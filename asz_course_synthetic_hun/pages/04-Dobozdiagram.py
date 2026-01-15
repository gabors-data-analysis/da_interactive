# boxplot_by_category.py
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter
import utils


# Változók, amelyeket se X-nek, se Y-nak nem szeretnél felajánlani
INCLUDE_VARS = [
    "sales_clean","tanass_clean","eszk","persexp_clean","pretax",
    "ereduzem","export_value","liabilities","ranyag","jetok","grant_value",
    "emp","age","firm_owner","has_grant"
    #"has_export","exit","county_name"
]

# Kategorikus változók értékeinek "szép" feliratai
CATEGORICAL_VALUE_LABELS = {
    "firm_owner": {
        "domestic": "Belföldi tulajdon",
        "foreign": "Külföldi tulajdon",
        "local gov": "Önkormányzati tulajdon",
        "state":"Állami tulajdon"
    },
    "has_grant": {
        1:"Kapott támogatást",
        0:"Nem kapott támogatást"
    }
}

CATEGORY_VARS = {
    "Kapott támogatást":"has_grant",
    "Tulajdonos":"firm_owner"
}

# --------------------------- Setup ---------------------------
col_settings, col_viz = utils.setup_page(
    'Dobozdiagram — 2019 keresztmetszet',
    'Dobozdiagram — 2019 keresztmetszet (szimulált)'
)
cs = utils.load_cross_section(st.session_state['data_path'])
real_data = st.session_state.get('real_data', False)

st.markdown(
    "Válasszon egy **kategóriás/dummy** X-változót és egy **folytonos** Y-változót. "
    "Az ábra a Y eloszlását mutatja az X kategóriái szerint, "
    "a piros pont jelzi az **átlagot**. Opcionálisan **ln(Y)** skálán is ábrázolhat."
)

# --------------------------- Beállítások (Bal oldal) ---------------------------
with col_settings:
    st.header("Beállítások")

    sync_on = utils.render_sync_option(st)

    # Ágazati szűrő
    lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
    lab_df["__code"] = pd.to_numeric(
        lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
        errors="coerce"
    )
    lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
    opts = ["Összes ágazat"] + lab_df["label"].tolist()
    
    ind_idx = utils.get_synced_index(opts, "global_industry")
    sel_label = st.selectbox("Ágazat", opts, index=ind_idx)
    utils.update_synced_state("global_industry", sel_label)

scope_all = sel_label == "Összes ágazat"
df = cs.copy() if scope_all else cs[cs["nace2_name_code"] == sel_label].copy()

# Pénzügyi változók
MONETARY_VARS = utils.get_monetary_vars()
NAME2COL = {**MONETARY_VARS, **utils.NON_MONETARY_VARS, **CATEGORY_VARS}
COL2NAME = {v: k for k, v in NAME2COL.items()}

# --------------------------- Típus-ellenőrzés ---------------------------
def is_categorical_like(s: pd.Series) -> bool:
    nun = s.nunique(dropna=True)
    return (pd.api.types.is_bool_dtype(s) or
            s.dtype == "object" or
            pd.api.types.is_categorical_dtype(s) or
            nun <= 10)

def is_continuous_like(s: pd.Series) -> bool:
    return pd.api.types.is_numeric_dtype(s) and s.nunique(dropna=True) > 10

# X jelöltek: kategóriás, nem kizárt
x_candidates = []
for col in df.columns:
    if col not in INCLUDE_VARS:
        continue
    if is_categorical_like(df[col]):
        x_candidates.append(col)

# Y jelöltek: folytonos, nem kizárt
y_candidates = []
for col in df.columns:
    if col not in INCLUDE_VARS:
        continue
    if is_continuous_like(df[col]):
        y_candidates.append(col)

if not x_candidates or not y_candidates:
    st.error("Nem található megfelelő kategóriás (X) vagy folytonos (Y) változó a kijelölt mintában (a kizárt változók figyelembevételével).")
    st.stop()

# Felirathoz „szép” nevek, ha vannak
x_labels = [COL2NAME.get(c, c) for c in x_candidates]
y_labels = [COL2NAME.get(c, c) for c in y_candidates]

with col_settings:
    # Sync Categorical X (Separate key!)
    x_idx = utils.get_synced_index(x_labels, "global_categorical_var")
    x_label = st.selectbox("X (kategóriás/dummy)", x_labels, index=x_idx)

    # Sync Primary Continuous (Y)
    y_idx = utils.get_synced_index(y_labels, "global_primary_var")
    y_label = st.selectbox("Y (folytonos)", y_labels, index=y_idx)

    utils.update_synced_state("global_categorical_var", x_label)
    utils.update_synced_state("global_primary_var", y_label)

xvar = NAME2COL.get(x_label, x_label)
yvar = NAME2COL.get(y_label, y_label)

# --------------------------- Y szélsőérték-kezelés (ÚJ) ---------------------------
y_is_monetary = yvar in MONETARY_VARS.values()

with col_settings:
    with st.expander("Szélsőérték-kezelés (Y)"):
        y_filter = st.selectbox("Y szélsőérték-kezelése", utils.FILTER_OPTIONS, index=0)

        # Adattisztítás a határokhoz
        df = df.replace([np.inf, -np.inf], np.nan)
        df = df.dropna(subset=[xvar, yvar])

        # Pénzügyi skála (millió Ft -> /1000, ha kell)
        if y_is_monetary:
            df[yvar] = df[yvar] / 1000.0

        # Manuális határok Y-hoz (az aktuális, skálázott Y-on)
        if y_filter == "Kézi minimum/maximum":
            st.markdown("**Y kézi határok (a megjelenített egységben)**")
            current_min_y = float(np.nanmin(df[yvar]))
            current_max_y = float(np.nanmax(df[yvar]))
            y_low_manual = st.number_input(
                "Y minimum",
                value=current_min_y,
                step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
            )
            y_high_manual = st.number_input(
                "Y maximum",
                value=current_max_y,
                step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
            )
            if y_low_manual > y_high_manual:
                st.error("A minimum nem lehet nagyobb a maximum­nál.")
        else:
            y_low_manual = None
            y_high_manual = None

    # ln(Y) + további opciók
    with st.expander("Megjelenítés és Skála"):
        use_log_y = st.checkbox("Y log skála (ln)", value=False)
        hide_outliers = st.checkbox("Kiugrók elrejtése a dobozábrán", value=False)
        overlay_points = st.checkbox("Egyedi pontok megjelenítése", value=False)


# --------------------------- Y log feltétel + filter alkalmazása ---------------------------
# Ha log skála, csak pozitív Y maradhat
if use_log_y:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()

# Szélsőérték-kezelés az eredeti (skálázott) Y-on
y_series = df[yvar].astype(float)
y_filtered = utils.apply_filter(y_series, y_filter, y_low_manual, y_high_manual)

# Szűrt df (index-alapú szinkronizálás)
df = df.loc[y_filtered.index].copy()
df["__y_raw__"] = y_filtered

if df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# Y transzformáció (ln vagy eredeti)
if use_log_y:
    df["__y__"] = np.log(df["__y_raw__"])
    y_axis_label = f"ln({y_label})"
    fmt_monetary = False
else:
    df["__y__"] = df["__y_raw__"]
    y_axis_label = y_label
    fmt_monetary = y_is_monetary

# --------------------------- Kategória-feliratok (értékszint) ---------------------------
def pretty_cat_value(var_name, value):
    mapping = CATEGORICAL_VALUE_LABELS.get(var_name, {})
    if value in mapping:
        return mapping[value]
    if str(value) in mapping:
        return mapping[str(value)]
    return str(value)

df["__x_display__"] = df[xvar].apply(lambda v: pretty_cat_value(xvar, v))

# Kategóriák rendezése gyakoriság szerint
order = df["__x_display__"].value_counts(dropna=False).index.tolist()

# --------------------------- Ábra ---------------------------
with col_viz:
    fig, ax = plt.subplots(figsize=(9, 5))

    sns.boxplot(
        data=df,
        x="__x_display__", y="__y__",
        order=order,
        showfliers=not hide_outliers,
        width=0.6, color=utils.COLORS[0],
        ax=ax
    )

    # Piros pont az átlagokra (kategóriánként)
    mean_by_cat = df.groupby("__x_display__")["__y__"].mean().reindex(order)
    x_positions = np.arange(len(order))
    ax.scatter(
        x_positions,
        mean_by_cat.values,
        color="red",
        s=40,
        zorder=4,
        label="Átlag"
    )

    # Egyedi pontok
    if overlay_points:
        sns.stripplot(
            data=df,
            x="__x_display__", y="__y__",
            order=order,
            dodge=False, jitter=0.15,
            alpha=0.35, color="black",
            size=3, ax=ax
        )

    # Tengelyek, feliratok
    ax.set_xlabel(x_label)
    ax.set_ylabel(y_axis_label)
    ax.spines[['top', 'right']].set_visible(False)
    ax.tick_params(axis='x', rotation=20)

    # Y formázás, ha pénzügyi és nem log
    if fmt_monetary:
        ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))

    # Legenda (csak átlag pontokra)
    ax.legend(frameon=False, loc="best")

    plt.tight_layout()
    st.pyplot(fig)

    # --------------------------- Összegzés táblázat ---------------------------
    desc = df.groupby("__x_display__", observed=True)["__y__"].agg(
        n="count", átlag="mean", median="median"
    ).loc[order]
    desc = desc.rename_axis(x_label).reset_index()

    if use_log_y:
        st.caption("Megjegyzés: Y tengely természetes log skálán (ln). "
                   "Az összefoglaló is ezen a skálán értendő.")
    if y_filter != "Nincs szűrés":
        st.caption(f"Y szélsőérték-kezelés: **{y_filter}**.")

    st.subheader("Kategóriánkénti összefoglaló (ábrán használt skálán)")
    st.dataframe(desc, use_container_width=True)
