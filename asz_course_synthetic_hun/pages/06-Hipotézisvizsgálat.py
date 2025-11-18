# mean_test_by_grant.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
from matplotlib.ticker import FuncFormatter
from scipy import stats

# próbálunk SciPy-t használni pontos t-próbához
try:
    
    HAS_SCIPY = True
except Exception:
    HAS_SCIPY = False

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

# --------------------------- Oldal beállítás ---------------------------
real_data = st.session_state.get('real_data', False)
if real_data:
    st.set_page_config(
        page_title='Átlagteszt támogatott / nem támogatott — Vállalatok (HU keresztmetszet)',
        layout='wide'
    )
else:
    st.set_page_config(
        page_title='Átlagteszt támogatott / nem támogatott — Vállalatok (HU keresztmetszet, szimulált)',
        layout='wide'
    )

# pénzügyi változók (mint a szórásdiagramokban)
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

MONETARY_VARS = MONETARY_VARS_REAL if real_data else MONETARY_VARS_SIM
VAR_MAP = {**MONETARY_VARS, **NON_MONETARY_VARS}
VAR_LABELS = list(VAR_MAP.keys())

# --------------------------- Adatbetöltés ---------------------------
@st.cache_data
def load_cross_section(path: str) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()
    need = {"nace2", "nace2_name_code", "has_grant"}
    missing = need - set(df.columns)
    if missing:
        st.error(f"Hiányzó oszlop(ok) az adatban: {missing}")
        st.stop()
    df["nace2"] = df["nace2"].astype(str)
    df["nace2_name_code"] = df["nace2_name_code"].astype(str)
    return df

data_path = st.session_state.get('data_path', 'data/synthetic/sim_cs2019_by_nace2_withcats.parquet')
cs = load_cross_section(data_path)

# --------------------------- Cím & leírás ---------------------------
BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if st.session_state['real_data']:
        st.title('Átlagteszt támogatott / nem támogatott — 2019 keresztmetszet')
    else:
        st.title('Átlagteszt támogatott / nem támogatott — 2019 keresztmetszet (szimulált)')

with col_right:
    # logó a jobb felső sarokban
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)



st.markdown(
    "Válasszon egy **folytonos változót**. A teszt H₀ hipotézise: "
    "a változó átlaga **azonos** a támogatást kapott és nem kapott vállalatoknál. "
    "Hₐ: az átlagok **nem egyenlőek** (kétoldali teszt)."
)

# --------------------------- Oldalsáv ---------------------------
st.sidebar.header("Beállítások")

# Változó kiválasztása
available_vars = {k: v for k, v in VAR_MAP.items() if v in cs.columns}
if not available_vars:
    st.error("Nincs elérhető folytonos változó az adatokban.")
    st.stop()

y_label = st.sidebar.selectbox("Változó", list(available_vars.keys()), index=0)
yvar = available_vars[y_label]
y_is_monetary = yvar in MONETARY_VARS.values()

# Ágazat választás (baseline)
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(
    lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
    errors="coerce"
)
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
sector_options = ["Összes ágazat"] + lab_df["label"].tolist()
sel_label = st.sidebar.selectbox("Ágazat (baseline sorhoz)", sector_options, index=0)
scope_all = sel_label == "Összes ágazat"

# Ugyanaz a szélsőérték-kezelés, mint a szórásdiagramnál
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

st.sidebar.subheader("Szélsőérték-kezelés (Y)")
y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

# Manuális min/max (a megjelenített egységben)
# Előbb minimális adat-előkészítés, hogy legyen tartomány
tmp_df = cs.replace([np.inf, -np.inf], np.nan)
if yvar in tmp_df.columns:
    tmp_vals = tmp_df[yvar].dropna()
else:
    tmp_vals = pd.Series(dtype=float)

if y_is_monetary and not tmp_vals.empty:
    tmp_vals = tmp_vals / 1000.0

if y_filter == "Kézi minimum/maximum" and not tmp_vals.empty:
    st.sidebar.markdown("**Y kézi határok (a megjelenített egységben)**")
    current_min_y = float(np.nanmin(tmp_vals))
    current_max_y = float(np.nanmax(tmp_vals))
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
else:
    y_low_manual = None
    y_high_manual = None

# Log skála, mint a szórásdiagramnál
use_log_y = st.sidebar.checkbox("Y log skála (ln)", value=False)

# Eredmény csak baseline ágazatra vagy minden ágazatra?
scope_mode = st.sidebar.radio(
    "Eredmény típusa",
    ["Csak kiválasztott ágazat", "Minden ágazat külön (plusz összes)"],
    index=0
)

# --------------------------- Segédfüggvények ---------------------------
def apply_filter(series: pd.Series, mode: str, low_val: float, high_val: float) -> pd.Series:
    """
    Szélsőérték-kezelés Y-re:
    - Nincs szűrés
    - Winsor top–bottom 2%
    - Levágás top–bottom 2%
    - Kézi minimum/maximum: low_val/high_val alapján vág
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

    if mode == "Kézi minimum/maximum" and low_val is not None and high_val is not None:
        return s[(s > low_val) & (s < high_val)]

    return s

def one_ttest(sub: pd.DataFrame) -> pd.Series:
    """
    sub: olyan df, amely tartalmazza yvar-t és has_grant-et.
    Kimenet: mean1, se1, n1, mean0, se0, n0, t, p, N
    (1 = kapott támogatást, 0 = nem kapott)
    """
    tmp = sub[[yvar, "has_grant"]].dropna()
    if tmp.empty:
        return pd.Series({
            "mean_grant": np.nan, "se_grant": np.nan, "n_grant": 0,
            "mean_nongrant": np.nan, "se_nongrant": np.nan, "n_nongrant": 0,
            "t": np.nan, "p": np.nan, "N": 0
        })

    g1 = tmp[tmp["has_grant"] == 1][yvar].astype(float)
    g0 = tmp[tmp["has_grant"] == 0][yvar].astype(float)
    n1, n0 = len(g1), len(g0)
    N = n1 + n0

    if n1 == 0 or n0 == 0:
        return pd.Series({
            "mean_grant": g1.mean() if n1 > 0 else np.nan,
            "se_grant": g1.std(ddof=1)/np.sqrt(n1) if n1 > 1 else np.nan,
            "n_grant": n1,
            "mean_nongrant": g0.mean() if n0 > 0 else np.nan,
            "se_nongrant": g0.std(ddof=1)/np.sqrt(n0) if n0 > 1 else np.nan,
            "n_nongrant": n0,
            "t": np.nan, "p": np.nan, "N": N
        })

    mean1, mean0 = g1.mean(), g0.mean()
    se1 = g1.std(ddof=1) / np.sqrt(n1) if n1 > 1 else np.nan
    se0 = g0.std(ddof=1) / np.sqrt(n0) if n0 > 1 else np.nan

    # Welch t-próba
    if HAS_SCIPY:
        t_stat, p_val = stats.ttest_ind(g1, g0, equal_var=False, nan_policy="omit")
    else:
        # kézi t és normálközelítéses p-érték (ha nincs SciPy)
        s1_sq = g1.var(ddof=1)
        s0_sq = g0.var(ddof=1)
        se_diff = np.sqrt(s1_sq/n1 + s0_sq/n0)
        if se_diff == 0 or np.isnan(se_diff):
            t_stat = np.nan
            p_val = np.nan
        else:
            t_stat = (mean1 - mean0) / se_diff
            # normálközelítés
            p_val = 2 * (1 - 0.5 * (1 + np.math.erf(abs(t_stat) / np.sqrt(2))))

    return pd.Series({
        "mean_grant": mean1, "se_grant": se1, "n_grant": n1,
        "mean_nongrant": mean0, "se_nongrant": se0, "n_nongrant": n0,
        "t": t_stat, "p": p_val, "N": N
    })

# --------------------------- Adatelőkészítés a teszthez ---------------------------
df = cs.copy()
df = df.replace([np.inf, -np.inf], np.nan)

needed_cols = ["nace2_name_code", yvar, "has_grant"]
missing_cols = [c for c in needed_cols if c not in df.columns]
if missing_cols:
    st.error(f"Hiányzó oszlop(ok) az adatban: {missing_cols}")
    st.stop()

df = df[needed_cols].dropna(subset=[yvar, "has_grant"])
if df.empty:
    st.error("Nincs olyan megfigyelés, ahol a kiválasztott változó és a támogatottság is ismert.")
    st.stop()

# pénzügyi skálázás (millió Ft -> /1000)
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

# log skála -> csak pozitívak maradnak
if use_log_y:
    df = df[df[yvar] > 0].copy()
if df.empty:
    st.error("A log-transzformáció után nincs megjeleníthető adat.")
    st.stop()

# szélsőérték-kezelés Y-ra
y_filtered = apply_filter(df[yvar], y_filter, y_low_manual, y_high_manual)
df = df.loc[y_filtered.index].copy()
df[yvar] = y_filtered

if df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# log transzformáció ténylegesen (ha kérte a felhasználó)
if use_log_y:
    df[yvar] = np.log(df[yvar].astype(float))
    y_axis_label = f"ln({y_label})"
else:
    y_axis_label = y_label

# --------------------------- Eredmények számítása ---------------------------
# baseline: kiválasztott ágazat vagy összes
if scope_all:
    baseline_df = df.copy()
else:
    baseline_df = df[df["nace2_name_code"] == sel_label].copy()

baseline_stats = one_ttest(baseline_df)
baseline_row = pd.DataFrame({
    "Ágazat": [sel_label if not scope_all else "Összes ágazat"],
    "Átlag (kapott)": [baseline_stats["mean_grant"]],
    "SE (kapott)": [baseline_stats["se_grant"]],
    "n (kapott)": [baseline_stats["n_grant"]],
    "Átlag (nem kapott)": [baseline_stats["mean_nongrant"]],
    "SE (nem kapott)": [baseline_stats["se_nongrant"]],
    "n (nem kapott)": [baseline_stats["n_nongrant"]],
    "t-érték": [baseline_stats["t"]],
    "p-érték": [baseline_stats["p"]],
    "N": [baseline_stats["N"]],
})

if scope_mode == "Csak kiválasztott ágazat":
    result = baseline_row
else:
    # minden ágazat + összes
    # iparáganként, NACE-only nevek elhagyása, kód szerinti rendezés
    by_ind = df.groupby("nace2_name_code", observed=True).apply(one_ttest).reset_index()

    mask_valid = ~by_ind["nace2_name_code"].str.startswith("NACE", na=False)
    by_ind = by_ind[mask_valid].copy()

    by_ind["__code"] = pd.to_numeric(
        by_ind["nace2_name_code"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
        errors="coerce"
    )
    by_ind = by_ind.sort_values(["__code", "nace2_name_code"]).drop(columns="__code")

    table_ind = pd.DataFrame({
        "Ágazat": by_ind["nace2_name_code"],
        "Átlag (kapott)": by_ind["mean_grant"],
        "SE (kapott)": by_ind["se_grant"],
        "n (kapott)": by_ind["n_grant"],
        "Átlag (nem kapott)": by_ind["mean_nongrant"],
        "SE (nem kapott)": by_ind["se_nongrant"],
        "n (nem kapott)": by_ind["n_nongrant"],
        "t-érték": by_ind["t"],
        "p-érték": by_ind["p"],
        "N": by_ind["N"],
    })

    # Összes ágazat sor: minden megfigyelés egyben
    overall_stats = one_ttest(df)
    overall_row = pd.DataFrame({
        "Ágazat": ["Összes ágazat"],
        "Átlag (kapott)": [overall_stats["mean_grant"]],
        "SE (kapott)": [overall_stats["se_grant"]],
        "n (kapott)": [overall_stats["n_grant"]],
        "Átlag (nem kapott)": [overall_stats["mean_nongrant"]],
        "SE (nem kapott)": [overall_stats["se_nongrant"]],
        "n (nem kapott)": [overall_stats["n_nongrant"]],
        "t-érték": [overall_stats["t"]],
        "p-érték": [overall_stats["p"]],
        "N": [overall_stats["N"]],
    })

    result = pd.concat([overall_row, table_ind], ignore_index=True)

# --------------------------- Megjelenítés ---------------------------
st.subheader("Átlagteszt eredményei")

st.dataframe(
    result.style.format({
        "Átlag (kapott)": "{:,.3f}",
        "SE (kapott)": "{:,.3f}",
        "Átlag (nem kapott)": "{:,.3f}",
        "SE (nem kapott)": "{:,.3f}",
        "t-érték": "{:,.3f}",
        "p-érték": "{:,.3f}",
        "n (kapott)": "{:,.0f}",
        "n (nem kapott)": "{:,.0f}",
        "N": "{:,.0f}",
    }),
    use_container_width=True
)

caption_extra = []
if use_log_y:
    caption_extra.append("A teszt és az átlagok ln-skálán értendők.")
if y_filter != "Nincs szűrés":
    caption_extra.append(f"Szélsőérték-kezelés Y-ra: {y_filter}.")
if caption_extra:
    st.caption(" ".join(caption_extra))

st.caption(
    "H₀: a kiválasztott változó átlaga azonos a támogatást kapott és nem kapott vállalatok között "
    "(Welch-féle kétmintás t-próba, kétoldali)."
)
