# correlations_by_industry_nace1.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path

# --------------------------- Beállítások ---------------------------
color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

real_data = st.session_state.get('real_data', False)

if real_data:
    st.set_page_config(
        page_title='Korrelációk Ágazatonként — Vállalatok (HU keresztmetszet)',
        layout='wide'
    )
else:
    st.set_page_config(
        page_title='Korrelációk Ágazatonként — Vállalatok (HU keresztmetszet, szimulált)',
        layout='wide'
    )

# --------- PÉLDA / HELYKITÖLTŐ: NACE1 KÓDOK NEVEI ---------
# Itt tudod egyenként átírni, hogyan jelenjenek meg a NACE1 csoportok a táblázatban.
# A kulcsok a 2 jegyű tartományokat jelölik (01-09, 10-19, ...).
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
# Ha nem akarod mindet átírni, nyugodtan hagyd a helykitöltő szöveget,
# vagy csak azokat módosítsd, amiket tényleg használsz.

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
OTHER_VARS = {
    "Kapott támogatást": "has_grant",
    "Tulajdonos": "firm_owner"
}

MONETARY_VARS = MONETARY_VARS_REAL if real_data else MONETARY_VARS_SIM
VAR_MAP = {**MONETARY_VARS, **NON_MONETARY_VARS, **OTHER_VARS}

# ---------- MANUÁLIS ALAPÉRTELMEZETT (baseline) VÁLTOZÓ ----------
BASELINE_LABEL = 'Kapott támogatást'

# --------------------------- Adatbetöltés + NACE1 szint képzése ---------------------------
@st.cache_data
def load_cross_section_with_nace1(path: str) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()

    # csak a 'nace2' oszlop a kötelező (stringként kezeljük)
    if "nace2" not in df.columns:
        st.error("Hiányzik a `nace2` oszlop az adatban (NACE2 kódok).")
        st.stop()
    df["nace2"] = df["nace2"].astype(str)

    # NACE2 első két számjegye -> szám
    nace2_num = pd.to_numeric(df["nace2"].str[:2], errors="coerce")

    # NACE1 tartományok (01–09, 10–19, ..., 90–99)
    bins = [0, 4, 9, 34,35,39,44,47,54,57,63,66,68,76,83,84,85,89,99]
    labels = [
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

    df["nace1_code"] = pd.cut(
        nace2_num,
        bins=bins,
        labels=labels,
        right=True,
        include_lowest=True
    )

    # Szöveges név a NACE1_LABELS alapján (ha nincs megadva, marad a kód)
    df["nace1_name"] = df["nace1_code"].astype(str).map(NACE1_LABELS).fillna(df["nace1_code"].astype(str))

    return df

data_path = st.session_state['data_path']
cs = load_cross_section_with_nace1(data_path)

# --------------------------- Cím & leírás ---------------------------
BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if real_data:
        st.title('Korrelációk Ágazatonként — 2019 keresztmetszet')
    else:
        st.title('Korrelációk Ágazatonként — 2019 keresztmetszet (szimulált)')

with col_right:
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)
st.markdown(
    """
    Az adatok forrása **OPTEN**.  
    Minden ábra és adat oktatási céllal készült és tájékoztató jellegű.  
    """
)
st.markdown(
    "Válasszon két **változót**. Az alkalmazás kiszámítja a **Pearson-féle korrelációt** "
    "minden **NACE1 csoportban** külön-külön, valamint **az összes ágazatra együtt**. "
    "A táblázat sorai a NACE1 csoportok, az oszlopok: korreláció és elemszám (*n*)."
)

col_settings, col_sep, col_viz = st.columns([4, 2, 12])

with col_sep:
    st.markdown(
        '<div style="border-left: 1px solid #e0e0e0; height: 100vh; margin: 0 auto;"></div>',
        unsafe_allow_html=True,
    )

# --------------------------- Beállítások (Bal oldal) ---------------------------
with col_settings:
    st.header("Beállítások")

    available = {k: v for k, v in VAR_MAP.items() if v in cs.columns}
    if len(available) < 2:
        st.error("Nincs elég elérhető változó az adatokban a korrelációhoz.")
        st.stop()

    available_keys = list(available.keys())

    # baseline index a Változó 1-hez
    if BASELINE_LABEL in available_keys:
        x_index = available_keys.index(BASELINE_LABEL)
    else:
        x_index = 0

    x_label = st.selectbox("Változó 1", available_keys, index=x_index)

    y_options = [k for k in available_keys if k != x_label]
    y_label = st.selectbox("Változó 2", y_options, index=0)

    xvar = available[x_label]
    yvar = available[y_label]

    with st.expander("Megjelenítés"):
        sort_choice = st.radio(
            "Rendezés a táblázatban",
            ["Ágazat szerint", "Korreláció szerint (csökkenő)"],
            index=0
        )

# --------------------------- Előkészítés ---------------------------
df = cs.copy()
df = df.replace([np.inf, -np.inf], np.nan)

# NACE1 mezők + kiválasztott változók
needed_cols = ["nace1_code", "nace1_name", xvar, yvar]
missing_cols = [c for c in needed_cols if c not in df.columns]
if missing_cols:
    st.error(f"Hiányzó oszlop(ok) az adatban: {missing_cols}")
    st.stop()

df = df[needed_cols].dropna(subset=[xvar, yvar, "nace1_code"])

if df.empty:
    st.error("Nincs olyan megfigyelés, ahol mindkét kiválasztott változó és a NACE1 kód nem hiányzik.")
    st.stop()

# --------------------------- Korreláció számítás ---------------------------
def corr_and_n(sub: pd.DataFrame) -> pd.Series:
    tmp = sub[[xvar, yvar]].dropna()
    n = len(tmp)
    if n < 2:
        return pd.Series({"corr": np.nan, "n": n})
    return pd.Series({"corr": tmp[xvar].corr(tmp[yvar]), "n": n})

# NACE1 csoportonként (nace1_code szerint, ami rendezett kategória)
by_ind = df.groupby("nace1_code", observed=True).apply(corr_and_n).reset_index()

# Hozzáadjuk a név oszlopot is (NACE1_LABELS alapján)
by_ind["Ágazat"] = by_ind["nace1_code"].astype(str).map(NACE1_LABELS).fillna(by_ind["nace1_code"].astype(str))

# Rendezés NACE1 kód sorrendben (kategória sorrend)
by_ind = by_ind.sort_values("nace1_code")

# Összes ágazat (minden megfigyelés együtt)
overall = corr_and_n(df)
overall_row = pd.DataFrame({
    "nace1_code": [np.nan],
    "corr": [overall["corr"]],
    "n": [overall["n"]],
    "Ágazat": ["Összes ágazat"],
})

# Összefűzés
result = pd.concat([overall_row, by_ind[["nace1_code","Ágazat", "corr", "n" ]]], ignore_index=True)

# Szép oszlopnév a korrelációs oszlopnak
corr_col_name = "Korreláció"
result = result.rename(columns={
    "Ágazat":"Ágazat",
    "corr": corr_col_name,
    "n": "n"
})

# --------------------------- Rendezési opció ---------------------------
if sort_choice == "Korreláció szerint (csökkenő)":
    overall_part = result[result["Ágazat"] == "Összes ágazat"]
    sector_part = result[result["Ágazat"] != "Összes ágazat"].copy()
    sector_part = sector_part.sort_values(
        by=corr_col_name,
        ascending=False,
        na_position="last"
    )
    result = pd.concat([overall_part, sector_part], ignore_index=True)
else:
    # NACE1 kód szerint: overall sor az elején, a többi NACE1 kód (kategória sorrend)
    overall_part = result[result["Ágazat"] == "Összes ágazat"]
    sector_part = result[result["Ágazat"] != "Összes ágazat"].copy()
    # sector_part már nace1_code szerint rendezve jött, de biztos, ami biztos:
    sector_part = sector_part.sort_values("nace1_code")
    result = pd.concat([overall_part, sector_part], ignore_index=True)

# result_display = result
# Felesleges technikai oszlop elrejtése a megjelenítésből
result_display = result.drop(columns=["nace1_code"])

# --------------------------- Megjelenítés ---------------------------
with col_viz:
    st.subheader("Korrelációk NACE1 csoportonként")

    st.dataframe(
        result_display.style.format({
            corr_col_name: "{:.3f}",
            "n": "{:,.0f}"
        }),
        width="stretch",
        column_order=["Ágazat",corr_col_name,"n"]
    )

    st.caption(
        "Megjegyzés: Pearson-féle korrelációs együttható. "
    )
