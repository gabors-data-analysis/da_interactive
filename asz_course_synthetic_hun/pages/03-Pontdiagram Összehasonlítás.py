# streamlit_scatter_two_industries.py
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from matplotlib.ticker import FuncFormatter

# Opcionális LOWESS
try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    HAS_LOWESS = True
except Exception:
    HAS_LOWESS = False

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

if st.session_state['real_data'] == True:
    st.set_page_config(page_title='Két-ágazatos szórásdiagram — Vállalatok (HU keresztmetszet)', layout='wide')
else:
    st.set_page_config(page_title='Két-ágazatos szórásdiagram — Vállalatok (HU keresztmetszet, szimulált)', layout='wide')


@st.cache_data
def load_cross_section(path: str = st.session_state['data_path']) -> pd.DataFrame:
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

cs = load_cross_section()

if st.session_state['real_data'] == True:
    st.title('Két-ágazatos szórásdiagram — 2019 keresztmetszet')
else:
    st.title('Két-ágazatos szórásdiagram — 2019 keresztmetszet (szimulált)')

st.markdown("Válasszon **két ágazatot**, két **változót**, és egy opcionális **illesztést**. A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Ágazati opciók: ÖSSZES elöl, majd kódszám szerint
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
                                 errors="coerce")
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
options = ["Összes ágazat"] + lab_df["label"].tolist()

# Két ágazat kiválasztása
sel_label_A = st.sidebar.selectbox("Ágazat A", options, index=0)
sel_label_B = st.sidebar.selectbox("Ágazat B", options, index=min(1, len(options)-1))

# Változók (pénzügyi: millió Ft)
if st.session_state['real_data'] == True:
    MONETARY_VARS = {
        'Értékesítés (millió Ft)': 'sales_clean',
        'Tárgyi eszközök (millió Ft)': 'tanass_clean',
        'Eszközök összesen (millió Ft)': 'eszk',
        'Személyi jellegű ráfordítások (millió Ft)': 'persexp_clean',
        'Adózás előtti eredmény (millió Ft)': 'pretax',
        'EBIT (millió Ft)': 'ereduzem',
        'Export értéke (millió Ft)': 'export_value',
        'Kötelezettségek (millió Ft)': 'liabilities',
        'Anyag jellegű ráfordítások (millió Ft)':'ranyag',
        'Jegyzett tőke (millió Ft)':'jetok',
        'Támogatás mértéke (millió Ft)':'grant_value'
    }
else:
    MONETARY_VARS = {
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
var_map = {**MONETARY_VARS, **NON_MONETARY_VARS}

available = {k: v for k, v in var_map.items() if v in cs.columns}
x_label = st.sidebar.selectbox("X változó", list(available.keys()), index=0)
y_label = st.sidebar.selectbox("Y változó", list(available.keys()), index=min(4, len(available)-1))
xvar = available[x_label]; yvar = available[y_label]
x_is_monetary = xvar in MONETARY_VARS.values()
y_is_monetary = yvar in MONETARY_VARS.values()

# ----------------------- Szélsőérték-kezelés: 4 opció / tengely -----------------------
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Levágás (kézi megadás)"
]
st.sidebar.subheader("Szélsőérték-kezelés")
x_filter = st.sidebar.selectbox("X szélsőérték-kezelése", FILTER_OPTIONS, index=0)
# y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

# Kézi százalékok megadása, ha szükséges
x_low_manual, x_high_manual = 2.0, 98.0
y_low_manual, y_high_manual = 2.0, 98.0
if x_filter == "Levágás (kézi megadás)":
    x_low_manual = st.sidebar.number_input("X alsó percentil (%)", min_value=0.0, max_value=49.0, value=2.0, step=0.5)
    x_high_manual = st.sidebar.number_input("X felső percentil (%)", min_value=51.0, max_value=100.0, value=98.0, step=0.5)

# Illesztés típusa
fit_type = st.sidebar.selectbox(
    "Illesztés rárajzolása",
    ["Nincs", "Lineáris", "Kvadratikus", "Köbös", "LOWESS", "Lépcsőzetes (5 bin)", "Lépcsőzetes (20 bin)"],
    index=0
)
if fit_type == "LOWESS" and not HAS_LOWESS:
    st.sidebar.warning("A statsmodels LOWESS nem elérhető; válasszon másik illesztést.")

# Ábra megjelenés
alpha = st.sidebar.slider("Pontok átlátszósága", 0.1, 1.0, 0.5, 0.05)
size  = st.sidebar.slider("Pontméret", 5, 100, 20, 1)

# Log skálák
logx = st.sidebar.checkbox("Logaritmikus skála X", value=False)
logy = st.sidebar.checkbox("Logaritmikus skála Y", value=False)

# ----------------------- Segédfüggvények ---------------------------
def filter_scope(df: pd.DataFrame, label: str) -> pd.DataFrame:
    if label == "Összes ágazat":
        return df.copy()
    return df[df["nace2_name_code"] == label].copy()

def apply_filter(series: pd.Series, mode: str, low_pct: float = 2.0, high_pct: float = 98.0) -> pd.Series:
    """
    mode ∈ FILTER_OPTIONS
    - Nincs szűrés: változatlan
    - Winsor top–bottom 2%: 2/98 percentilen klippelés
    - Levágás top–bottom 2%: 2/98 percentilen kívüliek eldobása
    - Levágás (kézi megadás): low_pct/high_pct percentilen kívüliek eldobása
    """
    s = series.dropna()
    if len(s) < 5 or mode == "Nincs szűrés":
        return s

    if mode == "Winsor top–bottom 2%":
        ql, qh = np.percentile(s, [2, 98])
        return s.clip(ql, qh)

    if mode == "Levágás top–bottom 2%":
        ql, qh = np.percentile(s, [2, 98])
        return s[(s > ql) & (s < qh)]

    if mode == "Levágás (kézi megadás)":
        ql, qh = np.percentile(s, [low_pct, high_pct])
        return s[(s > ql) & (s < qh)]

    return s

def fwd_x(v):
    return np.log10(v) if logx else v

def inv_x(v):
    return (10 ** v) if logx else v

def fwd_y(v):
    return np.log10(v) if logy else v

def inv_y(v):
    return (10 ** v) if logy else v

def tail_note_txt(mode, low=None, high=None):
    if mode == "Nincs szűrés":
        return "nincs"
    if mode == "Winsor top–bottom 2%":
        return "winsor 2–98%"
    if mode == "Levágás top–bottom 2%":
        return "levágás 2–98%"
    if mode == "Levágás (kézi megadás)":
        return f"levágás {low:.1f}–{high:.1f}%"
    return "—"

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

    # szélsőérték-kezelés (új, 4 opció)
    x = apply_filter(df[xvar], x_filter, x_low_manual, x_high_manual)
    y = df[yvar]
    idx = x.index.intersection(y.index)
    plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})
    if plot_df.empty:
        st.warning(f"Nincs adat a szélsőérték-kezelés után: {title}")
        return None, None, 0

    # Ábra
    fig, ax = plt.subplots()
    sns.scatterplot(data=plot_df, x=xvar, y=yvar, s=size, alpha=alpha,
                    edgecolor='white', linewidth=0.2, color=color[0], ax=ax)

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
        ax.plot(xs_orig, ys_orig, color=color[1], linewidth=2, alpha=0.9, label=f"{fit_type} illesztés")

        # Egyenlet a transzformált térben
        if deg == 1:
            a, b = coef[1], coef[0]
            xname = "log10(x)" if logx else "x"
            yname = "log10(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname}"
        elif deg == 2:
            a, b, c = coef[2], coef[1], coef[0]
            xname = "log10(x)" if logx else "x"
            yname = "log10(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}²"
        else:
            a, b, c, d = coef[3], coef[2], coef[1], coef[0]
            xname = "log10(x)" if logx else "x"
            yname = "log10(y)" if logy else "y"
            coef_text = f"{yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}² + {d:.4g}·{xname}³"

    elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
        x_tr = fwd_x(plot_df[xvar].values)
        y_tr = fwd_y(plot_df[yvar].values)
        z = lowess(y_tr, x_tr, frac=0.25, return_sorted=True)
        x_orig = inv_x(z[:, 0])
        y_orig = inv_y(z[:, 1])
        ax.plot(x_orig, y_orig, color=color[1], linewidth=2, alpha=0.9, label="LOWESS illesztés")

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
                      colors=color[1], linewidth=3, alpha=0.9)
        plot_df.drop(columns="__bin", inplace=True)

    # címkék & formázás
    ax.set_title(title, loc='left', fontsize=14)
    ax.set_xlabel(x_label); ax.set_ylabel(y_label)
    ax.spines[['top', 'right']].set_visible(False)

    # tengelyek
    ax.ticklabel_format(style='plain', axis='x')
    ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}" if x_is_monetary else f"{v:,.0f}"))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}" if y_is_monetary else f"{v:,.0f}"))
    ax.tick_params(axis='x', labelrotation=25)

    # log skálák
    if logx:
        ax.set_xscale('log')
    if logy:
        ax.set_yscale('log')

    if fit_type != "Nincs":
        ax.legend(frameon=False)

    plt.tight_layout()
    return fig, coef_text, len(plot_df)

# ----------------------- Két oszlop ----------------------
colA, colB = st.columns(2, gap="large")

with colA:
    scope_A = filter_scope(cs, sel_label_A)
    figA, coefA, nA = plot_one(scope_A, f"A: {sel_label_A}")
    if figA is not None:
        st.pyplot(figA)
        if coefA is not None and fit_type in {"Lineáris", "Kvadratikus", "Köbös"}:
            st.markdown(f"**Illesztési egyenlet (A, transzformált térben):** {coefA}")

with colB:
    scope_B = filter_scope(cs, sel_label_B)
    figB, coefB, nB = plot_one(scope_B, f"B: {sel_label_B}")
    if figB is not None:
        st.pyplot(figB)
        if coefB is not None and fit_type in {"Lineáris", "Kvadratikus", "Köbös"}:
            st.markdown(f"**Illesztési egyenlet (B, transzformált térben):** {coefB}")

# ----------------------- Lábléc összegzés -------------------
tail_note_x = tail_note_txt(x_filter, x_low_manual, x_high_manual)
#tail_note_y = tail_note_txt(y_filter, y_low_manual, y_high_manual)

st.markdown(
    f"**Ágazatok:** A = {sel_label_A} · B = {sel_label_B} · "
    f"**X:** `{x_label}` · **Y:** `{y_label}` · **Illesztés:** {fit_type} · "
    f"**Log (X/Y):** {logx} / {logy} · "
    f"**Szélső értékek (X/Y):** {tail_note_x}"
)
