import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from matplotlib.ticker import FuncFormatter, LogLocator

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

    reg_outcomes = {"sales_growth_perc","sales_growth_log_diff"}
    missing = reg_outcomes - set(df.columns)
    if missing:
        df["sales_growth_perc"] = (df["sales_lead_sim"] - df["sales_clean"]) / df["sales_clean"] * 100
        df["sales_growth_log_diff"] = df["ln_sales_lead_sim"] - df["ln_sales"]
    
    else:
        df["sales_growth_perc"] = df["sales_growth_perc"] * 100
    

    return df


cs = load_cross_section(st.session_state['data_path'])


# ----------------------- UI: fejlécek ------------------------
BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if st.session_state['real_data'] == True:
        st.title('Pontdiagram — 2019 keresztmetszet')
    else:
        st.title('Pontdiagram — 2019 keresztmetszet (szimulált)')

with col_right:
    # logó a jobb felső sarokban
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)

st.markdown(
    """
    Az adatok forrása **OPTEN**.  
    Minden ábra és adat oktatási céllal készült és tájékoztató jellegű.  
    """
)
st.markdown("Válasszon egy **ágazatot**, két **változót**, és egy opcionális **illesztést**. "
            "A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Ágazati opciók
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(
    lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
    errors="coerce"
)
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
        'Anyag jellegű ráfordítások (millió Ft)': 'ranyag',
        'Jegyzett tőke (millió Ft)': 'jetok',
        'Támogatás mértéke (millió Ft)': 'grant_value'
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
    "Relatív növekedés (%)":"sales_growth_perc",
    "Log növekedés (log-diff)":"sales_growth_log_diff",
    'Foglalkoztatottak száma (fő)': 'emp',
    'Kor (év)': 'age',
}
var_map = { **NON_MONETARY_VARS,**MONETARY_VARS}

available = {k: v for k, v in var_map.items() if v in cs.columns}
x_label = st.sidebar.selectbox("X változó", list(available.keys()), index=2)
y_label = st.sidebar.selectbox("Y változó", list(available.keys()), index=0)
xvar = available[x_label]
yvar = available[y_label]
x_is_monetary = xvar in MONETARY_VARS.values()
y_is_monetary = yvar in MONETARY_VARS.values()

# ----------------------- Szélsőérték-kezelés -----------------------
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

st.sidebar.subheader("Szélsőérték-kezelés")
x_filter = st.sidebar.selectbox("X szélsőérték-kezelése", FILTER_OPTIONS, index=0)
y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

df = cs.copy() if scope_all else cs[cs["nace2_name_code"] == sel_label].copy()
df = df.replace([np.inf, -np.inf], np.nan).dropna(subset=[xvar, yvar])

# pénzügyiek ezres megjelenítés (millió Ft -> /1000)
if x_is_monetary:
    df[xvar] = df[xvar] / 1000.0
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

# Manuális X-határok (változó egységében)
if x_filter == "Kézi minimum/maximum":
    st.sidebar.markdown("**X kézi határok (a megjelenített egységben)**")
    current_min_x = float(np.nanmin(df[xvar]))
    current_max_x = float(np.nanmax(df[xvar]))
    x_low_manual = st.sidebar.number_input(
        "X minimum",
        value=current_min_x,
        step=(current_max_x - current_min_x)/100 if current_max_x > current_min_x else 1.0
    )
    x_high_manual = st.sidebar.number_input(
        "X maximum",
        value=current_max_x,
        step=(current_max_x - current_min_x)/100 if current_max_x > current_min_x else 1.0
    )
    if x_low_manual > x_high_manual:
        st.sidebar.error("X esetén a minimum nem lehet nagyobb a maximumnál.")
        x_low_manual, x_high_manual = x_high_manual, x_low_manual
else:
    x_low_manual = None
    x_high_manual = None

# Manuális Y-határok (változó egységében)
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
        st.sidebar.error("Y esetén a minimum nem lehet nagyobb a maximumnál.")
        y_low_manual, y_high_manual = y_high_manual, y_low_manual
else:
    y_low_manual = None
    y_high_manual = None

# ----------------------- Bin scatter opció (új) -----------------------
st.sidebar.subheader("Ábra beállítások")

BIN_SCATTER_OPTIONS = [
    "Eredeti",
    "5 bin",
    "10 bin",
    "20 bin",
    "100 bin"
]
bin_scatter_choice = st.sidebar.selectbox("Pontdiagram típusa", BIN_SCATTER_OPTIONS, index=0)
bin_scatter_map = {
    "5 bin": 5,
    "10 bin": 10,
    "20 bin": 20,
    "100 bin": 100
}
n_bins = bin_scatter_map.get(bin_scatter_choice, None)
use_bin_scatter = n_bins is not None

# --- NEW: plot size controls (inches) ---

# ----------------------- Illesztés ---------------------------
fit_type = st.sidebar.selectbox(
    "Illesztés rárajzolása (eredeti adaton, nem a bin-eken)",
    ["Nincs", "Lineáris", "Kvadratikus", "Köbös", "LOWESS", "Lépcsős (5 bin)", "Lépcsős (20 bin)"],
    index=0
)
if fit_type == "LOWESS" and not HAS_LOWESS:
    st.sidebar.warning("A statsmodels LOWESS nem elérhető; válasszon másik illesztést.")



# Log skálák
logx = st.sidebar.checkbox("Logaritmikus skála X", value=False)
logy = st.sidebar.checkbox("Logaritmikus skála Y", value=False)

st.sidebar.subheader("Megjelenítés")
fig_width = st.sidebar.slider("Ábra szélessége", 4.0, 16.0, 8.0, 0.5)
fig_height = st.sidebar.slider("Ábra magassága", 2.0, 8.0, 4.0, 0.5)
# Plot megjelenés
alpha = st.sidebar.slider("Pontok átlátszósága", 0.1, 1.0, 0.5, 0.05)
size = st.sidebar.slider("Pontméret", 5, 100, 20, 1)

# ----------------------- Szűrés és előkészítés ---------------------

# Log-hoz csak pozitívak maradnak
if logx:
    df = df[df[xvar] > 0]
if logy:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()


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
        return s[(s > q_low) & (s < q_high)]

    if mode == "Kézi minimum/maximum":
        if low_val is None or high_val is None:
            return s
        return s[(s > low_val) & (s < high_val)]

    return s


x = apply_filter(df[xvar], x_filter, x_low_manual, x_high_manual)
y = apply_filter(df[yvar], y_filter, y_low_manual, y_high_manual)

# közös index a két sorozatra
idx = x.index.intersection(y.index)
plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})

if plot_df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# ----------------------- Bin scatter előkészítés -----------------------
scatter_df = plot_df.copy()
if use_bin_scatter and len(plot_df) >= 2:
    try:
        plot_df["__bin_scatter"] = pd.qcut(plot_df[xvar], q=n_bins, duplicates="drop")
        gb_scatter = plot_df.groupby("__bin_scatter", observed=True)
        scatter_df = gb_scatter[[xvar, yvar]].mean().reset_index(drop=True)
        plot_df.drop(columns="__bin_scatter", inplace=True)
    except Exception:
        scatter_df = plot_df.copy()

# ----------------------- Transzformációk (fit a transzformált térben) -----------------------
def fwd_x(v):
    return np.log(v) if logx else v


def inv_x(v):
    return np.exp(v) if logx else v


def fwd_y(v):
    return np.log(v) if logy else v


def inv_y(v):
    return np.exp(v) if logy else v


# ----------------------- Ábra -----------------------------
fig, ax = plt.subplots(figsize=(fig_width, fig_height))
sns.scatterplot(
    data=scatter_df, x=xvar, y=yvar, s=size, alpha=alpha,
    edgecolor='white', linewidth=0.2, color=color[0], ax=ax
)

coef_text = None

# Illesztések (mindig a transzformált térben számoljuk, majd visszatérünk az eredeti skálára)
if fit_type in {"Lineáris", "Kvadratikus", "Köbös"} and len(plot_df) >= 10:
    deg = {"Lineáris": 1, "Kvadratikus": 2, "Köbös": 3}[fit_type]
    xx = fwd_x(plot_df[xvar].to_numpy())
    yy = fwd_y(plot_df[yvar].to_numpy())
    order = np.argsort(xx)
    xx_sorted = xx[order]
    yy_sorted = yy[order]
    coef = np.polyfit(xx_sorted, yy_sorted, deg=deg)
    poly = np.poly1d(coef)

    xs_orig = np.linspace(plot_df[xvar].min(), plot_df[xvar].max(), 400)
    xs_tr = fwd_x(xs_orig)
    ys_tr = poly(xs_tr)
    ys_orig = inv_y(ys_tr)
    ax.plot(xs_orig, ys_orig, color=color[1], linewidth=2, alpha=0.9, label=f"{fit_type} illesztés")

    if deg == 1:
        a, b = coef[1], coef[0]
        trafo_note = []
        if logy:
            trafo_note.append("ln(y)")
        else:
            trafo_note.append("y")
        if logx:
            xname = "ln(x)"
        else:
            xname = "x"
        coef_text = f"{fit_type}: {trafo_note[0]} = {a:.4g} + {b:.4g}·{xname}"
    elif deg == 2:
        a, b, c = coef[2], coef[1], coef[0]
        xname = "ln(x)" if logx else "x"
        yname = "ln(y)" if logy else "y"
        coef_text = f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}²"
    else:
        a, b, c, d = coef[3], coef[2], coef[1], coef[0]
        xname = "ln(x)" if logx else "x"
        yname = "ln(y)" if logy else "y"
        coef_text = (
            f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + "
            f"{c:.4g}·{xname}² + {d:.4g}·{xname}³"
        )

elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
    x_tr = fwd_x(plot_df[xvar].values)
    y_tr = fwd_y(plot_df[yvar].values)
    z = lowess(y_tr, x_tr, frac=0.25, return_sorted=True)
    x_orig = inv_x(z[:, 0])
    y_orig = inv_y(z[:, 1])
    ax.plot(x_orig, y_orig, color=color[1], linewidth=2, alpha=0.9, label="LOWESS illesztés")

elif fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
    steps = 5 if "5" in fit_type else 20

    # 1. Quantile edges on filtered x
    x_vals = plot_df[xvar].to_numpy()
    q_grid = np.linspace(0, 1, steps + 1)
    edges = np.quantile(x_vals, q_grid)

    # 2. Keep only strictly increasing edges (positive-width bins)
    edges_clean = [edges[0]]
    for e in edges[1:]:
        if e > edges_clean[-1]:
            edges_clean.append(e)

    if len(edges_clean) <= 1:
        st.warning("Lépcsős illesztés: nem sikerült pozitív szélességű kvantilis bin-eket képezni.")
    else:
        bins = np.array(edges_clean)

        # 3. Cut using our quantile bins; bins are Interval objects
        plot_df["__bin"] = pd.cut(
            plot_df[xvar],
            bins=bins,
            include_lowest=True
        )

        gb = plot_df.groupby("__bin", observed=False)

        # mean y in transformed space
        y_mean_tr = fwd_y(gb[yvar].mean())
        y_mean = inv_y(y_mean_tr)

        # 4. Draw a segment from bin.left to bin.right for each non-empty bin
        for interval, y_m in y_mean.dropna().items():
            left = interval.left
            right = interval.right
            ax.hlines(
                y=y_m,
                xmin=left,
                xmax=right,
                colors=color[1],
                linewidth=3,
                alpha=0.9
            )

        plot_df.drop(columns="__bin", inplace=True)


ax.set_xlabel(x_label)
ax.set_ylabel(y_label)
ax.spines[['top', 'right']].set_visible(False)

# --- X axis ---
if logx:
    ax.set_xscale('log', base=np.e)
    ax.xaxis.set_major_locator(LogLocator(base=np.e))
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
    ax.yaxis.set_major_formatter(
        FuncFormatter(lambda v, _: f"{np.log(v):.2f}")
    )
else:
    ax.yaxis.set_major_formatter(
        FuncFormatter(lambda v, _: f"{v:,.2f}" if y_is_monetary else f"{v:,.2f}")
    )

if logx:
    ax.set_xlabel(f"ln({x_label})")
if logy:
    ax.set_ylabel(f"ln({y_label})")

if fit_type != "Nincs":
    ax.legend(frameon=False)

plt.tight_layout()
st.pyplot(fig)

# ---------- Bin boundary table for stepwise fits (no duplicate dropping) ----------
if fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
    steps = 5 if "5" in fit_type else 20

    # Use the x-values actually used for the fit (after all filters)
    x_vals = plot_df[xvar].to_numpy()

    # Quantile positions for the requested number of bins
    q_grid = np.linspace(0, 1, steps + 1)

    # Bin edges by quantiles (this keeps duplicates!)
    edges = np.quantile(x_vals, q_grid)

    bin_table = pd.DataFrame({
        "Rekesz": np.arange(1, steps + 1),
        "Alsó Határ": edges[:-1],
        "Felső Határ": edges[1:]
    })

    st.markdown("**Lépcsős illesztés – Rekeszhatárok:**")
    st.dataframe(bin_table.style.format({
        "Alsó Határ": "{:.3f}",
        "Felső Határ": "{:.3f}",
    }))


# ----------------------- Összegzés --------------------------
scope_label = sel_label


def tail_note_txt(mode, low=None, high=None):
    if mode == "Nincs szűrés":
        return "nincs"
    if mode == "Winsor top–bottom 2%":
        return "winsor 2–98%"
    if mode == "Levágás top–bottom 2%":
        return "levágás 2–98%"
    if mode == "Kézi minimum/maximum" and low is not None and high is not None:
        return f"levágás {low:.1f}–{high:.1f}"
    return "—"


tail_note_x = tail_note_txt(x_filter, x_low_manual, x_high_manual)
tail_note_y = tail_note_txt(y_filter, y_low_manual, y_high_manual)
bin_note = bin_scatter_choice

st.markdown(
    f"**Minta:** {scope_label} · **X:** `{x_label}` · **Y:** `{y_label}`"
    # + f" · **Szélső értékek (X/Y):** {tail_note_x} / {tail_note_y} · **Bin scatter:** {bin_note}"
)

if coef_text is not None:
    st.markdown(f"**Illesztési egyenlet (transzformált térben):** {coef_text}")
