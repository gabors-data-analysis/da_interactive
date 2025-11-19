# boxplot_by_category.py
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from matplotlib.ticker import FuncFormatter

# --------------------------- Beállítások ---------------------------
color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

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

# Opció: pénzügyi változók (csak címkézéshez, formázáshoz)
MONETARY_VARS_REAL = {
    'Értékesítés (millió Ft)': 'sales_clean',
    'Tárgyi eszközök (millió Ft)': 'tanass_clean',
    'Eszközök összesen (millió Ft)': 'eszk',
    'Személyi jellegű ráfordítások (millió Ft)': 'persexp_clean',
    'Adózás előtti eredmény (millió Ft)': 'pretax',
    'EBIT (millió Ft)': 'ereduzem',
    'Export értéke (millió Ft)': 'export_value',
    'Kötelezettségek (millió Ft)': 'liabilities',
    'Anyag jellegű ráfordítások (millió Ft)': 'ranyag',
    'Jegyzett tőke (millió Ft)': 'jetok',
    'Támogatás mértéke (millió Ft)': 'grant_value'
}
MONETARY_VARS_SIM = {
    'Értékesítés (millió Ft)': 'sales_clean',
    'Tárgyi eszközök (millió Ft)': 'tanass_clean',
    'Eszközök összesen (millió Ft)': 'eszk',
    'Személyi jellegű ráfordítások (millió Ft)': 'persexp_clean',
    'Adózás előtti eredmény (millió Ft)': 'pretax',
    'EBIT (millió Ft)': 'ereduzem',
    'Export értéke (millió Ft)': 'export_value',
    'Kötelezettségek (millió Ft)': 'liabilities'
}
NON_MONETARY_VARS = {
    'Foglalkoztatottak száma (fő)': 'emp',
    'Kor (év)': 'age',
}
CATEGORY_VARS = {
    "Kapott támogatást":"has_grant",
    "Tulajdonos":"firm_owner"
}

# Szélsőérték-kezelési opciók (mint a szórásdiagramnál)
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

# --------------------------- Oldal beállítás ---------------------------
real_data = st.session_state.get('real_data', False)
if real_data:
    st.set_page_config(page_title='Dobozdiagram — Vállalatok (HU keresztmetszet)', layout='wide')
else:
    st.set_page_config(page_title='Dobozdiagram — Vállalatok (HU keresztmetszet, szimulált)', layout='wide')

# --------------------------- Adatbetöltés ---------------------------
@st.cache_data
def load_cross_section(path: str) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()
    need = {"nace2", "nace2_name_code"}
    missing = need - set(df.columns)
    if missing:
        st.error(f"Hiányzó oszlopok az adatban: {missing}")
        st.stop()
    df["nace2"] = df["nace2"].astype(str)
    df["nace2_name_code"] = df["nace2_name_code"].astype(str)
    return df

data_path = st.session_state['data_path']
cs = load_cross_section(data_path)

# --------------------------- Cím & leírás ---------------------------

BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if st.session_state['real_data']:
        st.title('Dobozdiagram — 2019 keresztmetszet')
    else:
        st.title('Dobozdiagram — 2019 keresztmetszet (szimulált)')

with col_right:
    # logó a jobb felső sarokban
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)



st.markdown(
    "Válasszon egy **kategóriás/dummy** X-változót és egy **folytonos** Y-változót. "
    "Az ábra a Y eloszlását mutatja az X kategóriái szerint, "
    "a piros pont jelzi az **átlagot**. Opcionálisan **ln(Y)** skálán is ábrázolhat."
)

# --------------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Ágazati szűrő
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(
    lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
    errors="coerce"
)
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
opts = ["Összes ágazat"] + lab_df["label"].tolist()

sel_label = st.sidebar.selectbox("Ágazat", opts, index=0)
scope_all = sel_label == "Összes ágazat"
df = cs.copy() if scope_all else cs[cs["nace2_name_code"] == sel_label].copy()

# Pénzügyi változók
MONETARY_VARS = MONETARY_VARS_REAL if real_data else MONETARY_VARS_SIM
NAME2COL = {**MONETARY_VARS, **NON_MONETARY_VARS, **CATEGORY_VARS}
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

x_label = st.sidebar.selectbox("X (kategóriás/dummy)", x_labels, index=0)
y_label = st.sidebar.selectbox("Y (folytonos)", y_labels, index=0)

xvar = NAME2COL.get(x_label, x_label)
yvar = NAME2COL.get(y_label, y_label)

# --------------------------- Y szélsőérték-kezelés (ÚJ) ---------------------------
y_is_monetary = yvar in MONETARY_VARS.values()
st.sidebar.subheader("Szélsőérték-kezelés (Y)")
y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

# Adattisztítás a határokhoz
df = df.replace([np.inf, -np.inf], np.nan)
df = df.dropna(subset=[xvar, yvar])

# Pénzügyi skála (millió Ft -> /1000, ha kell)
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

# Manuális határok Y-hoz (az aktuális, skálázott Y-on)
if y_filter == "Kézi minimum/maximum":
    st.sidebar.markdown("**Y kézi határok (a megjelenített egységben)**")
    current_min_y = float(np.nanmin(df[yvar]))
    current_max_y = float(np.nanmax(df[yvar]))
    y_low_manual = st.sidebar.number_input(
        "Y minimum",
        value=current_min_y,
        step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
    )
    y_high_manual = st.sidebar.number_input(
        "Y maximum",
        value=current_max_y,
        step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
    )
    if y_low_manual > y_high_manual:
        st.sidebar.error("A minimum nem lehet nagyobb a maximum­nál.")
        # csak vizuális hibaüzenet; a filter függvény majd kezeli
else:
    y_low_manual = None
    y_high_manual = None

# ln(Y) + további opciók
use_log_y = st.sidebar.checkbox("Y log skála (ln)", value=False)
hide_outliers = st.sidebar.checkbox("Kiugrók elrejtése a dobozábrán", value=False)
overlay_points = st.sidebar.checkbox("Egyedi pontok megjelenítése", value=False)





# --------------------------- Szélsőérték-kezelés függvény ---------------------------
def apply_filter(series: pd.Series, mode: str, low_val: float, high_val: float) -> pd.Series:
    """
    mode: one of FILTER_OPTIONS
    - Nincs szűrés: return as-is
    - Winsor top–bottom 2%: clip at 2/98
    - Levágás top–bottom 2%: drop outside 2/98
    - Kézi minimum/maximum: drop outside low_val/high_val
    """
    s = series.dropna()
    if len(s) < 5 or mode == "Nincs szűrés":
        return s

    if mode == "Winsor top–bottom 2%":
        q_low, q_high = np.percentile(s, [2, 98])
        return s.clip(q_low, q_high)

    if mode == "Levágás top–bottom 2%":
        q_low, q_high = np.percentile(s, [2, 98])
        # st.write(q_low,q_high)
        return s[(s > q_low) & (s < q_high)]

    if mode == "Kézi minimum/maximum" and low_val is not None and high_val is not None:
        return s[(s > low_val) & (s < high_val)]

    return s

# --------------------------- Y log feltétel + filter alkalmazása ---------------------------
# Ha log skála, csak pozitív Y maradhat
if use_log_y:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()

# Szélsőérték-kezelés az eredeti (skálázott) Y-on
y_series = df[yvar].astype(float)
y_filtered = apply_filter(y_series, y_filter, y_low_manual, y_high_manual)

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
fig, ax = plt.subplots(figsize=(9, 5))

sns.boxplot(
    data=df,
    x="__x_display__", y="__y__",
    order=order,
    showfliers=not hide_outliers,
    width=0.6, color=color[0],
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
