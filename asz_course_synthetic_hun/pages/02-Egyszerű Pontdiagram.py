import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from matplotlib.ticker import FuncFormatter

# Optional for LOWESS
try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    HAS_LOWESS = True
except Exception:
    HAS_LOWESS = False

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]
st.set_page_config(page_title='Pontdiagram — Vállalatok (HU keresztmetszet, szimulált)', layout='wide')

@st.cache_data
def load_cross_section(path: str = 'data/synthetic/sim_cs2019_by_nace2_withcats.parquet') -> pd.DataFrame:
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

# ----------------------- UI: fejlécek ------------------------
st.title('Pontdiagram — 2019 keresztmetszet (szimulált)')
st.markdown("Válasszon egy **ágazatot**, két **változót**, és egy opcionális **illesztést**. A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Ágazati opciók
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False), errors="coerce")
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
opts = ["Összes ágazat (ALL)"] + lab_df["label"].tolist()

sel_label = st.sidebar.selectbox("Ágazat", opts, index=0)
scope_all = sel_label == "Összes ágazat (ALL)"

# Változók
MONETARY_VARS = {
    'Értékesítés (millió Ft)': 'sales_clean',
    'Tárgyi eszközök (millió Ft)': 'tanass_clean',
    'Eszközök összesen (millió Ft)': 'eszk',
    'Személyi jellegű ráfordítások (millió Ft)': 'persexp_clean',
    'Adózás előtti eredmény (millió Ft)': 'pretax',
    'EBIT (millió Ft)': 'ereduzem',
    'Export értéke (millió Ft)': 'export_value',
    'Kötelezettségek (millió Ft)': 'liabilities',
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

# Szélsőérték-kezelés
st.sidebar.subheader("Szélsőérték-kezelés (2%)")
winsor_x = st.sidebar.checkbox("X winsorizálása", value=True)
trim_x   = st.sidebar.checkbox("X szélsők levágása", value=False)
winsor_y = st.sidebar.checkbox("Y winsorizálása", value=True)
trim_y   = st.sidebar.checkbox("Y szélsők levágása", value=False)

# Illesztés
fit_type = st.sidebar.selectbox(
    "Illesztés rárajzolása",
    ["Nincs", "Lineáris", "Kvadratikus", "Köbös", "LOWESS", "Lépcsős (5 kosár)", "Lépcsős (20 kosár)"],
    index=1
)
if fit_type == "LOWESS" and not HAS_LOWESS:
    st.sidebar.warning("A statsmodels LOWESS nem elérhető; válasszon másik illesztést.")

# Plot megjelenés
alpha = st.sidebar.slider("Pontok átlátszósága", 0.1, 1.0, 0.5, 0.05)
size  = st.sidebar.slider("Pontméret", 5, 100, 20, 1)

# Log skálák
logx = st.sidebar.checkbox("Logaritmikus skála X", value=False)
logy = st.sidebar.checkbox("Logaritmikus skála Y", value=False)

# ----------------------- Szűrés és előkészítés ---------------------
df = cs.copy() if scope_all else cs[cs["nace2_name_code"] == sel_label].copy()
df = df.replace([np.inf, -np.inf], np.nan).dropna(subset=[xvar, yvar])

if x_is_monetary:
    df[xvar] = df[xvar] / 1000.0
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

if logx:
    df = df[df[xvar] > 0]
if logy:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()

def apply_tails(s: pd.Series, do_winsor: bool, do_trim: bool) -> pd.Series:
    s = s.dropna()
    if len(s) < 5:
        return s
    q2, q98 = np.percentile(s, [2, 98])
    if do_trim:
        return s[(s > q2) & (s < q98)]
    return s.clip(q2, q98) if do_winsor else s

x = apply_tails(df[xvar], winsor_x, trim_x)
y = apply_tails(df[yvar], winsor_y, trim_y)

idx = x.index.intersection(y.index)
plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})

if plot_df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# ----------------------- Ábra -----------------------------
fig, ax = plt.subplots()
sns.scatterplot(data=plot_df, x=xvar, y=yvar, s=size, alpha=alpha,
                edgecolor='white', linewidth=0.2, color=color[0], ax=ax)

coef_text = None

# Illesztések
if fit_type in {"Lineáris", "Kvadratikus", "Köbös"} and len(plot_df) >= 10:
    deg = {"Lineáris": 1, "Kvadratikus": 2, "Köbös": 3}[fit_type]
    xx = plot_df[xvar].to_numpy(); yy = plot_df[yvar].to_numpy()
    order = np.argsort(xx); xx_sorted = xx[order]; yy_sorted = yy[order]
    coef = np.polyfit(xx_sorted, yy_sorted, deg=deg)
    poly = np.poly1d(coef)
    xs = np.linspace(xx_sorted.min(), xx_sorted.max(), 400)
    ax.plot(xs, poly(xs), color=color[1], linewidth=2, alpha=0.9, label=f"{fit_type} illesztés")

    if deg == 1:
        a, b = coef[1], coef[0]
        coef_text = f"Lineáris: y = {a:.4g} + {b:.4g}·x"
    elif deg == 2:
        a, b, c = coef[2], coef[1], coef[0]
        coef_text = f"Kvadratikus: y = {a:.4g} + {b:.4g}·x + {c:.4g}·x²"
    else:
        a, b, c, d = coef[3], coef[2], coef[1], coef[0]
        coef_text = f"Köbös: y = {a:.4g} + {b:.4g}·x + {c:.4g}·x² + {d:.4g}·x³"

elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
    z = lowess(plot_df[yvar].values, plot_df[xvar].values, frac=0.25, return_sorted=True)
    ax.plot(z[:, 0], z[:, 1], color=color[1], linewidth=2, alpha=0.9, label="LOWESS illesztés")

elif fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
    steps = 5 if "5" in fit_type else 20
    plot_df["__bin"] = pd.qcut(plot_df[xvar], q=steps, duplicates="drop")
    gb = plot_df.groupby("__bin", observed=True)[[xvar, yvar]]
    bin_stats = gb.agg(x_min=(xvar, "min"), x_max=(xvar, "max"), y_mean=(yvar, "mean")).reset_index(drop=True)
    for _, r in bin_stats.iterrows():
        ax.hlines(y=r["y_mean"], xmin=r["x_min"], xmax=r["x_max"],
                  colors=color[1], linewidth=3, alpha=0.9)
    plot_df.drop(columns="__bin", inplace=True)

ax.set_xlabel(x_label); ax.set_ylabel(y_label)
ax.spines[['top', 'right']].set_visible(False)

ax.ticklabel_format(style='plain', axis='x')
ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}" if x_is_monetary else f"{v:,.0f}"))
ax.tick_params(axis='x', labelrotation=25)
ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}" if y_is_monetary else f"{v:,.0f}"))

if logx:
    ax.set_xscale('log')
if logy:
    ax.set_yscale('log')

plt.tight_layout()
st.pyplot(fig)

# ----------------------- Összegzés --------------------------
scope_label = sel_label
tail_note_x = "szűrés" if trim_x else ("winsorizálás" if winsor_x else "nyers")
tail_note_y = "szűrés" if trim_y else ("winsorizálás" if winsor_y else "nyers")

st.markdown(
    f"**Minta:** {scope_label} · **X:** `{x_label}` · **Y:** `{y_label}` · "
    f"**Megfigyelések:** {len(plot_df):,} · **Szélső értékek (X/Y):** {tail_note_x} / {tail_note_y} · "
    f"**Illesztés:** {fit_type} · **Log (X/Y):** {logx} / {logy}"
)

if coef_text is not None:
    st.markdown(f"**Illesztési egyenlet:** {coef_text}")
