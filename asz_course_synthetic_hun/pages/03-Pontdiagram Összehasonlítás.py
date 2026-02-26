# streamlit_scatter_two_industries.py
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter, LogLocator
import textwrap
import utils

# Opcionális LOWESS
try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    HAS_LOWESS = True
except Exception:
    HAS_LOWESS = False

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# ----------------------- Setup ------------------------
col_settings, col_viz = utils.setup_page(
    'Két-ágazatos szórásdiagram — 2019 keresztmetszet',
    'Két-ágazatos szórásdiagram — 2019 keresztmetszet (szimulált)'
)
cs = utils.load_cross_section(st.session_state['data_path'])

st.markdown("Válasszon **két ágazatot**, két **változót**, és egy opcionális **illesztést**. "
            "A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------- Beállítások (Bal oldal) ---------------------------
with col_settings:
    st.header("Beállítások")

    # Ágazati opciók: ÖSSZES elöl, majd kódszám szerint
    lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
    lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
                                     errors="coerce")
    lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
    options = ["Összes ágazat"] + lab_df["label"].tolist()

    # Két ágazat kiválasztása
    sel_label_A = st.selectbox(
        "Ágazat A",
        options,
        index=options.index(persist("p03_industry_A", options[0])),
        key="_p03_industry_A",
        on_change=save, args=("p03_industry_A",)
    )
    default_B = options[1] if len(options) > 1 else options[0]
    sel_label_B = st.selectbox(
        "Ágazat B",
        options,
        index=options.index(persist("p03_industry_B", default_B)),
        key="_p03_industry_B",
        on_change=save, args=("p03_industry_B",)
    )
    # Változók (pénzügyi: millió Ft)
    MONETARY_VARS = utils.get_monetary_vars()
    var_map = {**MONETARY_VARS, **utils.NON_MONETARY_VARS}

    available = {k: v for k, v in var_map.items() if v in cs.columns}
    avail_keys = list(available.keys())

    # Sync Secondary (X)
    default_x = avail_keys[0] if avail_keys else None
    x_label = st.selectbox(
        "X változó",
        avail_keys,
        index=avail_keys.index(persist("p03_x_var", default_x)) if default_x and persist("p03_x_var", default_x) in avail_keys else 0,
        key="_p03_x_var",
        on_change=save, args=("p03_x_var",)
    )

    # Sync Primary (Y)
    default_y = avail_keys[4] if len(avail_keys) > 4 else (avail_keys[0] if avail_keys else None)
    y_label = st.selectbox(
        "Y változó",
        avail_keys,
        index=avail_keys.index(persist("p03_y_var", default_y)) if default_y and persist("p03_y_var", default_y) in avail_keys else 0,
        key="_p03_y_var",
        on_change=save, args=("p03_y_var",)
    )
xvar = available[x_label]; yvar = available[y_label]
x_is_monetary = xvar in MONETARY_VARS.values()
y_is_monetary = yvar in MONETARY_VARS.values()

# ----------------------- Szélsőérték-kezelés: 4 opció / tengely -----------------------
with col_settings:
    with st.expander("Szélsőérték-kezelés"):
        saved_xf = persist("p03_xfilter", "Nincs szűrés (összes érték)")
        xf_idx = utils.FILTER_OPTIONS.index(saved_xf) if saved_xf in utils.FILTER_OPTIONS else 0
        x_filter = st.selectbox("X szélsőérték-kezelése", utils.FILTER_OPTIONS, index=xf_idx, key="_p03_xfilter", on_change=save, args=("p03_xfilter",))

        # Calculate global min/max for defaults
        df_global = cs.copy()
        if x_is_monetary:
            df_global[xvar] = df_global[xvar] / 1000.0
        
        current_min_x = float(np.nanmin(df_global[xvar]))
        current_max_x = float(np.nanmax(df_global[xvar]))

        x_low_manual, x_high_manual = None, None
        y_low_manual, y_high_manual = None, None

        if x_filter == "Kézi minimum/maximum":
            st.markdown("**X kézi határok (a megjelenített egységben)**")
            x_low_manual = st.number_input("X minimum", value=current_min_x)
            x_high_manual = st.number_input("X maximum", value=current_max_x)
            
            if x_low_manual > x_high_manual:
                st.error("A minimum nem lehet nagyobb a maximumnál.")
                x_low_manual, x_high_manual = x_high_manual, x_low_manual

# ----------------------- Bin scatter opció (mint az első scriptben) -----------------------
with col_settings:
    with st.expander("Ábra beállítások"):
        BIN_SCATTER_OPTIONS = [
            "Eredeti",
            "5 bin",
            "10 bin",
            "20 bin",
            "100 bin"
        ]
        saved_bin = persist("p03_bin", "Eredeti")
        bin_idx = BIN_SCATTER_OPTIONS.index(saved_bin) if saved_bin in BIN_SCATTER_OPTIONS else 0
        bin_scatter_choice = st.selectbox("Pontdiagram típusa", BIN_SCATTER_OPTIONS, index=bin_idx, key="_p03_bin", on_change=save, args=("p03_bin",))
        bin_scatter_map = {
            "5 bin": 5,
            "10 bin": 10,
            "20 bin": 20,
            "100 bin": 100
        }
        n_bins = bin_scatter_map.get(bin_scatter_choice, None)
        use_bin_scatter = n_bins is not None

        # Illesztés típusa
        fit_opts = ["Nincs", "Lineáris", "Kvadratikus", "Köbös", "LOWESS", "Lépcsőzetes (5 bin)", "Lépcsőzetes (20 bin)"]
        saved_fit = persist("p03_fit", "Nincs")
        fit_idx = fit_opts.index(saved_fit) if saved_fit in fit_opts else 0
        fit_type = st.selectbox(
            "Illesztés rárajzolása",
            fit_opts,
            index=fit_idx, key="_p03_fit", on_change=save, args=("p03_fit",)
        )
        if fit_type == "LOWESS" and not HAS_LOWESS:
            st.warning("A statsmodels LOWESS nem elérhető; válasszon másik illesztést.")

        # Ábra megjelenés
        st.markdown("#### Megjelenítés")
        alpha = st.slider("Pontok átlátszósága", 0.1, 1.0, persist("p03_alpha", 0.5), 0.05, key="_p03_alpha", on_change=save, args=("p03_alpha",))
        size  = st.slider("Pontméret", 5, 100, persist("p03_size", 20), 1, key="_p03_size", on_change=save, args=("p03_size",))

        # Log skálák
        st.markdown("#### Skála")
        logx = st.checkbox("Logaritmikus skála X", value=persist("p03_logx", False), key="_p03_logx", on_change=save, args=("p03_logx",))
        logy = st.checkbox("Logaritmikus skála Y", value=persist("p03_logy", False), key="_p03_logy", on_change=save, args=("p03_logy",))

# ----------------------- Segédfüggvények ---------------------------
def filter_scope(df: pd.DataFrame, label: str) -> pd.DataFrame:
    if label == "Összes ágazat":
        return df.copy()
    return df[df["nace2_name_code"] == label].copy()

def fwd_x(v):
    return np.log(v) if logx else v

def inv_x(v):
    return (np.exp(v)) if logx else v

def fwd_y(v):
    return np.log(v) if logy else v

def inv_y(v):
    return (np.exp(v)) if logy else v



def plot_one(scope_df: pd.DataFrame, title: str):
    # változók kiválasztása & tisztítás
    df = scope_df.replace([np.inf, -np.inf], np.nan).dropna(subset=[xvar, yvar])
    # skálázás millió Ft-ra, ha pénzügyi
    if x_is_monetary:
        df[xvar] = df[xvar] / 1000.0
    if y_is_monetary:
        df[yvar] = df[yvar] / 1000.0
    # log skála esetén nem-pozitívak eldobása
    if logx:
        df = df[df[xvar] > 0]
    if logy:
        df = df[df[yvar] > 0]
    if df.empty:
        st.warning(f"Nincs adat a szűrés után: {title}")
        return None, None, 0

    # szélsőérték-kezelés (X)
    x = utils.apply_filter(df[xvar], x_filter, x_low_manual, x_high_manual)
    y = df[yvar]
    idx = x.index.intersection(y.index)
    plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})
    if plot_df.empty:
        st.warning(f"Nincs adat a szélsőérték-kezelés után: {title}")
        return None, None, 0

    # ----------------------- Bin scatter előkészítés -----------------------
    # Illesztés mindig plot_df-en, a pontdiagram pedig scatter_df-en (binelt vagy eredeti)
    scatter_df = plot_df.copy()
    if use_bin_scatter and len(plot_df) >= 2:
        try:
            plot_df["__bin_scatter"] = pd.qcut(plot_df[xvar], q=n_bins, duplicates="drop")
            gb_scatter = plot_df.groupby("__bin_scatter", observed=True)
            scatter_df = gb_scatter[[xvar, yvar]].mean().reset_index(drop=True)
            plot_df.drop(columns="__bin_scatter", inplace=True)
        except Exception:
            scatter_df = plot_df.copy()

    # Ábra
    fig, ax = plt.subplots()
    sns.scatterplot(
        data=scatter_df, x=xvar, y=yvar, s=size, alpha=alpha,
        edgecolor='white', linewidth=0.2, color=utils.COLORS[0], ax=ax
    )

    coef_text = None
    # Illesztések: a transzformált térben számolunk, majd visszatranszformálunk a rajzhoz
    if fit_type in {"Lineáris", "Kvadratikus", "Köbös"} and len(plot_df) >= 10:
        deg = {"Lineáris": 1, "Kvadratikus": 2, "Köbös": 3}[fit_type]
        xx_tr = fwd_x(plot_df[xvar].to_numpy())
        yy_tr = fwd_y(plot_df[yvar].to_numpy())
        order = np.argsort(xx_tr); xx_sorted = xx_tr[order]; yy_sorted = yy_tr[order]
        coef = np.polyfit(xx_sorted, yy_sorted, deg=deg)
        poly = np.poly1d(coef)

        xs_orig = np.linspace(plot_df[xvar].min(), plot_df[xvar].max(), 400)
        xs_tr = fwd_x(xs_orig)
        ys_tr = poly(xs_tr)
        ys_orig = inv_y(ys_tr)
        ax.plot(xs_orig, ys_orig, color=utils.COLORS[1], linewidth=2, alpha=0.9, label=f"{fit_type} illesztés")

        # Egyenlet a transzformált térben
        if deg == 1:
            a, b = coef[1], coef[0]
            xname = "ln(x)" if logx else "x"
            yname = "ln(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname}"
        elif deg == 2:
            a, b, c = coef[2], coef[1], coef[0]
            xname = "ln(x)" if logx else "x"
            yname = "ln(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}²"
        else:
            a, b, c, d = coef[3], coef[2], coef[1], coef[0]
            xname = "ln(x)" if logx else "x"
            yname = "ln(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}² + {d:.4g}·{xname}³"

    elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
        x_tr = fwd_x(plot_df[xvar].values)
        y_tr = fwd_y(plot_df[yvar].values)
        z = lowess(y_tr, x_tr, frac=0.25, return_sorted=True)
        x_orig = inv_x(z[:, 0])
        y_orig = inv_y(z[:, 1])
        ax.plot(x_orig, y_orig, color=utils.COLORS[1], linewidth=2, alpha=0.9, label="LOWESS illesztés")

    elif fit_type.startswith("Lépcsőzetes") and len(plot_df) >= 10:
        steps = 5 if "5" in fit_type else 20
        plot_df["__bin"] = pd.qcut(plot_df[xvar], q=steps, duplicates="drop")
        gb = plot_df.groupby("__bin", observed=True)
        # X határok az eredeti skálán
        bin_edges = gb[xvar].agg(x_min="min", x_max="max")
        # Y átlag a transzformált térben, majd vissza
        y_mean_tr = fwd_y(gb[yvar].mean())
        y_mean = inv_y(y_mean_tr)
        bin_stats = pd.concat([bin_edges, y_mean.rename("y_mean")], axis=1).reset_index(drop=True)
        for _, r in bin_stats.iterrows():
            ax.hlines(y=r["y_mean"], xmin=r["x_min"], xmax=r["x_max"],
                      colors=utils.COLORS[1], linewidth=3, alpha=0.9)
        plot_df.drop(columns="__bin", inplace=True)

    # címkék & formázás
    wrapped_title = textwrap.fill(title, width=40)  # próbáld 35–45 között finomhangolni
    ax.set_title(wrapped_title, loc='left', fontsize=12)

    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)
    ax.spines[['top', 'right']].set_visible(False)

    # --- X axis ---
    if logx:
        # log scale with base e
        ax.set_xscale('log', base=np.e)
        ax.xaxis.set_major_locator(LogLocator(base=np.e))

        # show ln(x) as tick labels
        ax.xaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{np.log(v):.2f}")  # ln(v)
        )
    else:
        ax.ticklabel_format(style='plain', axis='x')
        ax.xaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{v:,.2f}" if x_is_monetary else f"{v:,.0f}")
        )

    ax.tick_params(axis='x', labelrotation=25)

    # --- Y axis ---
    if logy:
        ax.set_yscale('log', base=np.e)
        ax.yaxis.set_major_locator(LogLocator(base=np.e))

        # show ln(y) as tick labels
        ax.yaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{np.log(v):.2f}")
        )
    else:
        ax.yaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{v:,.2f}" if y_is_monetary else f"{v:,.0f}")
        )

    if logx:
        ax.set_xlabel(f"ln({x_label})")
    if logy:
        ax.set_ylabel(f"ln({y_label})")

    if fit_type != "Nincs":
        ax.legend(frameon=False)

    plt.tight_layout()
    return fig, coef_text, len(plot_df)

# ----------------------- Két oszlop ----------------------
with col_viz:
    colA, colB = st.columns(2, gap="large")

    with colA:
        scope_A = filter_scope(cs, sel_label_A)
        figA, coefA, nA = plot_one(scope_A, f"A: {sel_label_A}")
        if figA is not None:
            st.pyplot(figA)
            if coefA is not None and fit_type in {"Lineáris", "Kvadratikus", "Köbös"}:
                st.markdown(f"**Illesztési egyenlet:** {coefA}")

    with colB:
        scope_B = filter_scope(cs, sel_label_B)
        figB, coefB, nB = plot_one(scope_B, f"B: {sel_label_B}")
        if figB is not None:
            st.pyplot(figB)
            if coefB is not None and fit_type in {"Lineáris", "Kvadratikus", "Köbös"}:
                st.markdown(f"**Illesztési egyenlet:** {coefB}")

# ----------------------- Lábléc összegzés -------------------
tail_note_x = utils.tail_note_txt(x_filter, x_low_manual, x_high_manual)
bin_note = bin_scatter_choice

with col_viz:
    st.markdown(
        f"**Ágazatok:** A = {sel_label_A} · B = {sel_label_B} · "
       # f"**X:** `{x_label}` · **Y:** `{y_label}` · **Illesztés:** {fit_type} · "
       # f"**Log (X/Y):** {logx} / {logy} · "
       # f"**Szélső értékek (X/Y):** {tail_note_x} · "
       # f"**Bin scatter:** {bin_note}"
    )
