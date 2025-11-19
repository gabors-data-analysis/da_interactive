# mean_test_by_grant_nace1.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
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
        page_title='Átlagteszt támogatott / nem támogatott — NACE1 csoportok',
        layout='wide'
    )
else:
    st.set_page_config(
        page_title='Átlagteszt támogatott / nem támogatott — NACE1 csoportok (szimulált)',
        layout='wide'
    )

# --------- NACE1 csoportok (ugyanúgy, mint a korrelációs dashboardban) ---------
NACE1_LABELS = {
    "01-04": "NACE 01–04 (MEZŐGAZDASÁG)",
    "05-09": "NACE 05–09 (BÁNYÁSZAT, KŐFEJTÉS)",
    "10-34": "NACE 10–34 (FELDOLGOZÓIPAR)",
    "35":    "NACE 35 (VILLAMOSENERGIA-, GÁZ-, GŐZELLÁTÁS)",
    "36-39": "NACE 36–39 (VÍZELLÁTÁS)",
    "40-44": "NACE 40–44 (ÉPÍTŐIPAR)",
    "45-47": "NACE 45–47 (KERESKEDELEM, GÉPJÁRMŰJAVÍTÁS)",
    "48-54": "NACE 48–54 (SZÁLLÍTÁS, RAKTÁROZÁS)",
    "55-57": "NACE 55–57 (SZÁLLÁSHELY-SZOLGÁLTATÁS, VENDÉGLÁTÁS)",
    "58-63": "NACE 58–63 (INFORMÁCIÓ, KOMMUNIKÁCIÓ)",
    "64-66": "NACE 64–66 (PÉNZÜGYI, BIZTOSÍTÁSI TEVÉKENYSÉG)",
    "67-68": "NACE 67–68 (INGATLANÜGYLETEK)",
    "69-76": "NACE 69–76 (SZAKMAI, TUDOMÁNYOS, MŰSZAKI TEVÉKENYSÉG)",
    "77-83": "NACE 77–83 (ADMINISZTRATÍV ÉS SZOLGÁLTATÁST TÁMOGATÓ TEVÉKENYSÉG)",
    "84":    "NACE 84 (KÖZIGAZGATÁS, VÉDELEM; KÖTELEZŐ TÁRSADALOMBIZTOSÍTÁS)",
    "85":    "NACE 85 (OKTATÁS)",
    "86-89": "NACE 86–89 (HUMÁN-EGÉSZSÉGÜGYI, SZOCIÁLIS ELLÁTÁS)",
    "90-99": "NACE 90–99 (EGYÉB)",
}

BINS = [0, 4, 9, 34, 35, 39, 44, 47, 54, 57, 63, 66, 68, 76, 83, 84, 85, 89, 99]
LABELS = [
    "01-04",
    "05-09",
    "10-34",
    "35",
    "36-39",
    "40-44",
    "45-47",
    "48-54",
    "55-57",
    "58-63",
    "64-66",
    "67-68",
    "69-76",
    "77-83",
    "84",
    "85",
    "86-89",
    "90-99",
]

# --------------------------- Változó-készlet ---------------------------
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

# --------------------------- Adatbetöltés (NACE1) ---------------------------
@st.cache_data
def load_cross_section_with_nace1(path: str) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()
    need = {"nace2", "has_grant"}
    missing = need - set(df.columns)
    if missing:
        st.error(f"Hiányzó oszlop(ok) az adatban: {missing}")
        st.stop()

    df["nace2"] = df["nace2"].astype(str)
    nace2_num = pd.to_numeric(df["nace2"].str[:2], errors="coerce")

    df["nace1_code"] = pd.cut(
        nace2_num,
        bins=BINS,
        labels=LABELS,
        right=True,
        include_lowest=True
    )
    df["nace1_name"] = df["nace1_code"].astype(str).map(NACE1_LABELS).fillna(df["nace1_code"].astype(str))

    return df

data_path = st.session_state['data_path']
cs = load_cross_section_with_nace1(data_path)

# --------------------------- Cím & leírás ---------------------------
BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if real_data:
        st.title('Átlagteszt támogatott / nem támogatott — NACE1 csoportok, 2019 keresztmetszet')
    else:
        st.title('Átlagteszt támogatott / nem támogatott — NACE1 csoportok, 2019 keresztmetszet (szimulált)')

with col_right:
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)

st.markdown(
    "Válasszon egy **folytonos változót**. A teszt H₀ hipotézise: "
    "a változó átlaga **azonos** a támogatást kapott és nem kapott vállalatoknál. "
    "Hₐ: az átlagok **nem egyenlőek** (kétoldali teszt). "
    "Az eredmények NACE1 csoportonként számolódnak."
)

# --------------------------- Oldalsáv (változó, NACE1, log, scope) ---------------------------
st.sidebar.header("Beállítások")

available_vars = {k: v for k, v in VAR_MAP.items() if v in cs.columns}
if not available_vars:
    st.error("Nincs elérhető folytonos változó az adatokban.")
    st.stop()

y_label = st.sidebar.selectbox("Változó", list(available_vars.keys()), index=0)
yvar = available_vars[y_label]
y_is_monetary = yvar in MONETARY_VARS.values()

# NACE1 baseline választás
sector_options = ["Összes ágazat"] + [NACE1_LABELS[k] for k in LABELS if k in NACE1_LABELS]
sel_label = st.sidebar.selectbox("NACE1 csoport (baseline sorhoz)", sector_options, index=0)
scope_all = sel_label == "Összes ágazat"

# Log skála (ln)
use_log_y = st.sidebar.checkbox("Y log skála (ln)", value=False)

# Eredmény típusa
scope_mode = st.sidebar.radio(
    "Eredmény típusa",
    ["Csak kiválasztott NACE1 csoport", "Minden NACE1 csoport külön (plusz összes)"],
    index=0
)

# --------------------------- Segédfüggvények ---------------------------
def apply_filter(series: pd.Series, mode: str, manual_min: float, manual_max: float) -> pd.Series:
    """
    Szélsőérték-kezelés Y-ra:
    - Nincs szűrés
    - Winsor top–bottom 2%
    - Levágás top–bottom 2%
    - Kézi minimum/maximum: manual_min/manual_max alapján vág
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

    if mode == "Kézi minimum/maximum" and manual_min is not None and manual_max is not None:
        return s[(s > manual_min) & (s < manual_max)]

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
        s1_sq = g1.var(ddof=1)
        s0_sq = g0.var(ddof=1)
        se_diff = np.sqrt(s1_sq/n1 + s0_sq/n0)
        if se_diff == 0 or np.isnan(se_diff):
            t_stat = np.nan
            p_val = np.nan
        else:
            t_stat = (mean1 - mean0) / se_diff
            p_val = 2 * (1 - 0.5 * (1 + np.math.erf(abs(t_stat) / np.sqrt(2))))

    return pd.Series({
        "mean_grant": mean1, "se_grant": se1, "n_grant": n1,
        "mean_nongrant": mean0, "se_nongrant": se0, "n_nongrant": n0,
        "t": t_stat, "p": p_val, "N": N
    })

# --------------------------- Adatelőkészítés ---------------------------
df = cs.copy()
df = df.replace([np.inf, -np.inf], np.nan)

needed_cols = ["nace1_code", "nace1_name", yvar, "has_grant"]
missing_cols = [c for c in needed_cols if c not in df.columns]
if missing_cols:
    st.error(f"Hiányzó oszlop(ok) az adatban: {missing_cols}")
    st.stop()

df = df[needed_cols].dropna(subset=[yvar, "has_grant", "nace1_code"])
if df.empty:
    st.error("Nincs olyan megfigyelés, ahol a kiválasztott változó, a támogatottság és a NACE1 kód is ismert.")
    st.stop()

# pénzügyi skálázás (millió Ft -> /1000)
if y_is_monetary:
    df[yvar] = df[yvar] / 1000.0

# log skála -> csak pozitívak maradnak, utána ln
if use_log_y:
    df = df[df[yvar] > 0].copy()
if df.empty:
    st.error("A log-transzformáció után nincs megjeleníthető adat.")
    st.stop()

if use_log_y:
    df[yvar] = np.log(df[yvar].astype(float))
    y_axis_label = f"ln({y_label})"
else:
    y_axis_label = y_label

# --------------------------- Szélsőérték kezelés (Y) – az általad használt minta ---------------------------
st.sidebar.subheader("Szélsőérték kezelés")
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]
tail_mode = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

y_vec = df[yvar].replace([np.inf, -np.inf], np.nan).dropna()
if tail_mode == "Kézi minimum/maximum" and not y_vec.empty:
    st.sidebar.markdown("**Kézi határok (a megjelenített egységben)**")
    current_min = float(np.nanmin(y_vec))
    current_max = float(np.nanmax(y_vec))
    manual_min = st.sidebar.number_input(
        "Minimum",
        value=current_min,
        step=(current_max - current_min)/100 if current_max > current_min else 1.0
    )
    manual_max = st.sidebar.number_input(
        "Maximum",
        value=current_max,
        step=(current_max - current_min)/100 if current_max > current_min else 1.0
    )
    if manual_min > manual_max:
        st.sidebar.error("A minimum nem lehet nagyobb a maximum­nál.")
        manual_min, manual_max = manual_max, manual_min
else:
    manual_min = None
    manual_max = None

# alkalmazzuk a szűrést
y_filtered = apply_filter(df[yvar], tail_mode, manual_min, manual_max)
df = df.loc[y_filtered.index].copy()
df[yvar] = y_filtered

if df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# ... mindened marad, egészen idáig:

# --------------------------- Eredmények számítása ---------------------------
if scope_all:
    baseline_df = df.copy()
    baseline_name = "Összes ágazat"
else:
    baseline_df = df[df["nace1_name"] == sel_label].copy()
    baseline_name = sel_label

baseline_stats = one_ttest(baseline_df)
baseline_row = pd.DataFrame({
    "Ágazat": [baseline_name],
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

if scope_mode == "Csak kiválasztott NACE1 csoport":
    result = baseline_row
else:
    by_ind = df.groupby("nace1_code", observed=True).apply(one_ttest).reset_index()
    by_ind["Ágazat"] = by_ind["nace1_code"].astype(str).map(NACE1_LABELS).fillna(by_ind["nace1_code"].astype(str))
    by_ind = by_ind.sort_values("nace1_code")

    table_ind = pd.DataFrame({
        "Ágazat": by_ind["Ágazat"],
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

# --------------------------- 95%-os CI hozzáadása ---------------------------
# 95%-os konfidencia-intervallum: mean ± 1.96 * SE
result["Alsó 95% CI (kapott)"] = result["Átlag (kapott)"] - 1.96 * result["SE (kapott)"]
result["Felső 95% CI (kapott)"] = result["Átlag (kapott)"] + 1.96 * result["SE (kapott)"]
result["Alsó 95% CI (nem kapott)"] = result["Átlag (nem kapott)"] - 1.96 * result["SE (nem kapott)"]
result["Felső 95% CI (nem kapott)"] = result["Átlag (nem kapott)"] + 1.96 * result["SE (nem kapott)"]

# --------------------------- 1. táblázat: mintaösszehasonlítás ---------------------------
st.subheader("Mintaösszehasonlítás: átlagok és 95%-os konfidencia-intervallumok")

if scope_mode == "Csak kiválasztott NACE1 csoport":
    # ---- EGY NACE1: nincs Ágazat oszlop, csak statisztikák soronként ----
    r = result.iloc[0]

    rows = [
        {"Statisztika": "Átlag",
         "Támogatott": r["Átlag (kapott)"],
         "Nem támogatott": r["Átlag (nem kapott)"]},
        {"Statisztika": "SE",
         "Támogatott": r["SE (kapott)"],
         "Nem támogatott": r["SE (nem kapott)"]},
        {"Statisztika": "n",
         "Támogatott": r["n (kapott)"],
         "Nem támogatott": r["n (nem kapott)"]},
        {"Statisztika": "Alsó 95% CI",
         "Támogatott": r["Alsó 95% CI (kapott)"],
         "Nem támogatott": r["Alsó 95% CI (nem kapott)"]},
        {"Statisztika": "Felső 95% CI",
         "Támogatott": r["Felső 95% CI (kapott)"],
         "Nem támogatott": r["Felső 95% CI (nem kapott)"]},
    ]

    stats_table = pd.DataFrame(rows)

    st.dataframe(
        stats_table.style.format({
            "Támogatott": "{:,.3f}",
            "Nem támogatott": "{:,.3f}",
        }),
        use_container_width=True
    )

else:
    # ---- MINDEN NACE1: egy sor / ágazat, sok oszlop (mint korábban) ----
    stats_cols = [
        "Ágazat",
        "Átlag (kapott)", "SE (kapott)", "n (kapott)",
        "Alsó 95% CI (kapott)", "Felső 95% CI (kapott)",
        "Átlag (nem kapott)", "SE (nem kapott)", "n (nem kapott)",
        "Alsó 95% CI (nem kapott)", "Felső 95% CI (nem kapott)",
    ]
    stats_table = result[stats_cols].copy()

    st.dataframe(
        stats_table.style.format({
            "Átlag (kapott)": "{:,.3f}",
            "SE (kapott)": "{:,.3f}",
            "n (kapott)": "{:,.0f}",
            "Alsó 95% CI (kapott)": "{:,.3f}",
            "Felső 95% CI (kapott)": "{:,.3f}",
            "Átlag (nem kapott)": "{:,.3f}",
            "SE (nem kapott)": "{:,.3f}",
            "n (nem kapott)": "{:,.0f}",
            "Alsó 95% CI (nem kapott)": "{:,.3f}",
            "Felső 95% CI (nem kapott)": "{:,.3f}",
        }),
        use_container_width=True
    )

st.caption(
    "A konfidencia-intervallumok 95%-os szinten, normál-közelítéssel, ±1.96·SE alapján számítódnak."
)



# --------------------------- Hipotézisek (középre, „egyenlet-szerűen”) ---------------------------
st.markdown("---")
st.markdown("<div style='text-align:center; font-size:1.1em;'><b>Hipotézisek</b></div>", unsafe_allow_html=True)

st.latex(r"H_0: \mu_{\text{támogatott}} = \mu_{\text{nem támogatott}}")
st.latex(r"H_A: \mu_{\text{támogatott}} \neq \mu_{\text{nem támogatott}}")

# --------------------------- 2. táblázat: t-próba eredményei ---------------------------
st.markdown("---")
st.subheader("Hipotézisvizsgálat: t-próba eredményei")

test_table = result[["Ágazat", "t-érték", "p-érték", "N"]].copy()

st.dataframe(
    test_table.style.format({
        "t-érték": "{:,.3f}",
        "p-érték": "{:,.3f}",
        "N": "{:,.0f}",
    }),
    use_container_width=True
)

caption_extra = []
# log & szűrés jelzése maradhat, ha a fenti változók megvannak a scriptben:
if use_log_y:
    caption_extra.append("A teszt és az átlagok ln-skálán értendők.")
if tail_mode != "Nincs szűrés":
    caption_extra.append(f"Szélsőérték-kezelés Y-ra: {tail_mode}.")

if caption_extra:
    st.caption(" ".join(caption_extra))

st.caption(
    "Welch-féle kétmintás t-próba, kétoldali teszt. "
    "H₀ szerint a kiválasztott változó átlaga azonos a támogatást kapott és nem kapott vállalatok között."
)
