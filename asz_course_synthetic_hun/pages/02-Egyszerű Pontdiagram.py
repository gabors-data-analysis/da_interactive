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

if st.session_state['real_data'] == True:
    st.set_page_config(page_title='Pontdiagram — Vállalatok (HU keresztmetszet)', layout='wide')
else:
    st.set_page_config(page_title='Pontdiagram — Vállalatok (HU keresztmetszet, szimulált)', layout='wide')


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

# ----------------------- UI: fejlécek ------------------------
if st.session_state['real_data'] == True:
    st.title('Pontdiagram — 2019 keresztmetszet')
else:
    st.title('Pontdiagram — 2019 keresztmetszet (szimulált)')

st.markdown("Válasszon egy **ágazatot**, két **változót**, és egy opcionális **illesztést**. A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Ágazati opciók
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False), errors="coerce")
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
opts = ["Összes ágazat"] + lab_df["label"].tolist()

sel_label = st.sidebar.selectbox("Ágazat", opts, index=0)
scope_all = sel_label == "Összes ágazat"

# Változók
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

# ----------------------- Szélsőérték-kezelés (új, 4 opció) -----------------------
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

st.sidebar.subheader("Szélsőérték-kezelés")
x_filter = st.sidebar.selectbox("X szélsőérték-kezelése", FILTER_OPTIONS, index=0)
# y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

# Kézi százalékok, ha szükséges
# x_low_manual, x_high_manual = 2.0, 98.0
# y_low_manual, y_high_manual = 2.0, 98.0

if x_filter == "Kézi minimum/maximum":
    st.sidebar.markdown("**Kézi határok (a megjelenített egységben)**")
    # sensible defaults: current min/max of x
    current_min = float(np.nanmin(xvar))
    current_max = float(np.nanmax(xvar))
    x_low_manual = st.sidebar.number_input("Minimum", value=current_min, step=(current_max - current_min)/100 if current_max > current_min else 1.0)
    x_high_manual = st.sidebar.number_input("Maximum", value=current_max, step=(current_max - current_min)/100 if current_max > current_min else 1.0)
    if x_low_manual > x_high_manual:
        st.sidebar.error("A minimum nem lehet nagyobb a maximum­nál.")
        # swap to avoid crash and still continue
        manual_min, manual_max = x_high_manual, x_low_manual
else:
    x_low_manual = None
    x_high_manual = None

#if x_filter == "Levágás (kézi megadás)":
#    x_low_manual = st.sidebar.number_input("X alsó percentil (%)", min_value=0.0, max_value=49.0, value=2.0, step=0.5)
#    x_high_manual = st.sidebar.number_input("X felső percentil (%)", min_value=51.0, max_value=100.0, value=98.0, step=0.5)
# if y_filter == "Levágás (kézi megadás)":
#    y_low_manual = st.sidebar.number_input("Y alsó percentil (%)", min_value=0.0, max_value=49.0, value=2.0, step=0.5)
#    y_high_manual = st.sidebar.number_input("Y felső percentil (%)", min_value=51.0, max_value=100.0, value=98.0, step=0.5)

# ----------------------- Illesztés ---------------------------
fit_type = st.sidebar.selectbox(
    "Illesztés rárajzolása",
    ["Nincs", "Lineáris", "Kvadratikus", "Köbös", "LOWESS", "Lépcsős (5 bin)", "Lépcsős (20 bin)"],
    index=0
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

# pénzügyiek ezres megjelenítés (millió Ft -> ezer millió? Itt marad, ahogy nálad volt: /1000)
if x_is_monetary:
    df[xvar] = df[xvar] / 1000.0
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

# Log-hoz csak pozitívak maradnak
if logx:
    df = df[df[xvar] > 0]
if logy:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()

def apply_filter(series: pd.Series, mode: str, low_val: float , high_val: float) -> pd.Series:
    """
    mode: one of FILTER_OPTIONS
    - Nincs szűrés: return as-is
    - Winsor top–bottom 2%: clip at 2/98
    - Levágás top–bottom 2%: drop outside 2/98
    - Levágás (kézi megadás): drop outside low_pct/high_pct
    """
    s = series.dropna()
    if len(s) < 5 or mode == "Nincs szűrés":
        return s

    if mode == "Winsor top–bottom 2%":
        q_low, q_high = np.percentile(s, [2, 98])
        return s.clip(q_low, q_high)

    if mode == "Levágás top–bottom 2%":
        q_low, q_high = np.percentile(s, [2, 98])
        return s[(s > q_low) & (s < q_high)]

    if mode == "Kézi minimum/maximum":
        return s[(s > low_val) & (s < high_val)]

    return s

x = apply_filter(df[xvar], x_filter, x_low_manual, x_high_manual)
y = df[yvar]

# közös index a két sorozatra
idx = x.index.intersection(y.index)
plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})

if plot_df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# ----------------------- Transzformációk (fit a transzformált térben) -----------------------
def fwd_x(v):
    return np.log10(v) if logx else v

def inv_x(v):
    return (10 ** v) if logx else v

def fwd_y(v):
    return np.log10(v) if logy else v

def inv_y(v):
    return (10 ** v) if logy else v

# ----------------------- Ábra -----------------------------
fig, ax = plt.subplots()
sns.scatterplot(
    data=plot_df, x=xvar, y=yvar, s=size, alpha=alpha,
    edgecolor='white', linewidth=0.2, color=color[0], ax=ax
)

coef_text = None

# Illesztések (mindig a transzformált térben számoljuk, majd visszatérünk az eredeti skálára)
if fit_type in {"Lineáris", "Kvadratikus", "Köbös"} and len(plot_df) >= 10:
    deg = {"Lineáris": 1, "Kvadratikus": 2, "Köbös": 3}[fit_type]
    xx = fwd_x(plot_df[xvar].to_numpy())
    yy = fwd_y(plot_df[yvar].to_numpy())
    order = np.argsort(xx); xx_sorted = xx[order]; yy_sorted = yy[order]
    coef = np.polyfit(xx_sorted, yy_sorted, deg=deg)
    poly = np.poly1d(coef)

    # Rajzolás: x rács az EREDETI skálán, de a fit a transzformáltban készül
    xs_orig = np.linspace(plot_df[xvar].min(), plot_df[xvar].max(), 400)
    xs_tr = fwd_x(xs_orig)
    ys_tr = poly(xs_tr)
    ys_orig = inv_y(ys_tr)
    ax.plot(xs_orig, ys_orig, color=color[1], linewidth=2, alpha=0.9, label=f"{fit_type} illesztés")

    # Koef szöveg a transzformált tér egyenletére (ez „lineáris” ott)
    if deg == 1:
        a, b = coef[1], coef[0]
        # Megnevezés jelzi, melyik tengely(ek) logban vannak
        trafo_note = []
        if logy: trafo_note.append("log10(y)")
        else:    trafo_note.append("y")
        if logx: xname = "log10(x)"
        else:    xname = "x"
        coef_text = f"{fit_type}: {trafo_note[0]} = {a:.4g} + {b:.4g}·{xname}"
    elif deg == 2:
        a, b, c = coef[2], coef[1], coef[0]
        xname = "log10(x)" if logx else "x"
        yname = "log10(y)" if logy else "y"
        coef_text = f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}²"
    else:
        a, b, c, d = coef[3], coef[2], coef[1], coef[0]
        xname = "log10(x)" if logx else "x"
        yname = "log10(y)" if logy else "y"
        coef_text = f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}² + {d:.4g}·{xname}³"

elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
    x_tr = fwd_x(plot_df[xvar].values)
    y_tr = fwd_y(plot_df[yvar].values)
    z = lowess(y_tr, x_tr, frac=0.25, return_sorted=True)
    # vissza eredeti skálára a plothoz
    x_orig = inv_x(z[:, 0])
    y_orig = inv_y(z[:, 1])
    ax.plot(x_orig, y_orig, color=color[1], linewidth=2, alpha=0.9, label="LOWESS illesztés")

elif fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
    steps = 5 if "5" in fit_type else 20
    # Binnelés mindig az eredeti X skálán történik (kvantilisek),
    # de az Y átlagát a transzformált térben számoljuk, majd visszatranszformáljuk,
    # hogy a „vonal” a log skálán is értelmes legyen.
    plot_df["__bin"] = pd.qcut(plot_df[xvar], q=steps, duplicates="drop")
    gb = plot_df.groupby("__bin", observed=True)
    # X határok az eredeti skálán
    bin_edges = gb[xvar].agg(x_min="min", x_max="max")
    # Y átlag a transzformált térben
    y_mean_tr = fwd_y(gb[yvar].mean())
    # vissza eredeti skálára
    y_mean = inv_y(y_mean_tr)
    bin_stats = pd.concat([bin_edges, y_mean.rename("y_mean")], axis=1).reset_index(drop=True)

    for _, r in bin_stats.iterrows():
        ax.hlines(y=r["y_mean"], xmin=r["x_min"], xmax=r["x_max"], colors=color[1], linewidth=3, alpha=0.9)
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

if fit_type != "Nincs":
    ax.legend(frameon=False)

plt.tight_layout()
st.pyplot(fig)

# ----------------------- Összegzés --------------------------
scope_label = sel_label

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

tail_note_x = tail_note_txt(x_filter, x_low_manual, x_high_manual)
# tail_note_y = tail_note_txt(y_filter, y_low_manual, y_high_manual)

st.markdown(
    f"**Minta:** {scope_label} · **X:** `{x_label}` · **Y:** `{y_label}` · "
    f"**Megfigyelések:** {len(plot_df):,} · **Szélső értékek (X/Y):** {tail_note_x} · "
    f"**Illesztés:** {fit_type} · **Log (X/Y):** {logx} / {logy}"
)

if coef_text is not None:
    st.markdown(f"**Illesztési egyenlet (transzformált térben):** {coef_text}")
