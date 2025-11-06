# home.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path

st.set_page_config(page_title="Kezdőlap — Szimulált", layout="wide")

# ---------------------------- Beállítás ----------------------------
st.session_state['real_data'] = False

if st.session_state['real_data'] == True:
    st.session_state['data_path'] = "real_data/balance_cross_section_2019.parquet"
else:
    st.session_state['data_path'] = "data/synthetic/sim_cs2019_by_nace2_withcats.parquet"

# ---------------------------- Változónevek (HU -> belső) ----------------------------
HUN_TO_INTERNAL = {
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

# ---------------------------- Felhasználói beállítások ----------------------------
VARS_TO_SHOW = [
    "row_id", "year", "nace2_name_code",
    "sales_clean", "emp", "age",
    "pretax", "ereduzem", "export_value", "liabilities",
    "tanass_clean", "eszk", "persexp_clean", "ranyag", "jetok",
    "grant_value", "has_grant",
    "county", "exit_2021", "firm_owner"
]

DISPLAY_NAMES = {
    "row_id": "ID",
    "year": "Év",
    "nace2_name_code": "Ágazat (NACE2)",
    "sales_clean": "Értékesítés (millió Ft)",
    "emp": "Foglalkoztatottak (fő)",
    "age": "Kor (év)",
    "pretax": "Adózás előtti eredmény (millió Ft)",
    "ereduzem": "EBIT (millió Ft)",
    "export_value": "Export értéke (millió Ft)",
    "liabilities": "Kötelezettségek (millió Ft)",
    "tanass_clean": "Tárgyi eszközök (millió Ft)",
    "eszk": "Eszközök összesen (millió Ft)",
    "persexp_clean": "Személyi jellegű ráfordítások (millió Ft)",
    "ranyag": "Anyag jellegű ráfordítások (millió Ft)",
    "jetok": "Jegyzett tőke (millió Ft)",
    "grant_value": "Támogatás mértéke (millió Ft)",
    "has_grant": "Kapott támogatást",
    "county": "Megye",
    "exit_2021": "Kilépés 2021-ig",
    "firm_owner": "Tulajdonosi forma"
}

# ---------------------------- Demo adat előállítás ----------------------------
@st.cache_data
def make_demo_df(n_rows: int = 100) -> pd.DataFrame:
    rng = np.random.default_rng(2025)

    nace_opts = [
        "Élelmiszeripar", "Vegyipar", "Kiskereskedelem", "IT szolgáltatás",
        "Építőipar", "Szállítás", "Üzletviteli tanácsadás", "Vendéglátás"
    ]
    owner_opts = ["Privát, belföld", "Állami", "Privát, külföld"]

    counties = [
        "Budapest", "Pest", "Baranya", "Bács-Kiskun", "Békés", "Borsod-Abaúj-Zemplén",
        "Csongrád-Csanád", "Fejér", "Győr-Moson-Sopron", "Hajdú-Bihar", "Heves",
        "Jász-Nagykun-Szolnok", "Komárom-Esztergom", "Nógrád", "Somogy",
        "Szabolcs-Szatmár-Bereg", "Tolna", "Vas", "Veszprém", "Zala"
    ]

    rows = []
    for i in range(n_rows):
        # ---- Év (2010–2022) ----
        year = int(rng.integers(2010, 2023))

        # ---- Alap skálák: millió Ft ----
        # Értékesítés (lehet 0 is)
        if rng.random() < 0.30:
            sales = 0.0
        else:
            # 5–50 000 millió Ft (azaz 5 mrd-ig), erős jobbferdeség
            sales = float(int(rng.lognormal(mean=8.7, sigma=1.0)))  # ~ 6k–50k tipikusan
            sales = max(5.0, min(sales, 50_000.0))

        # Foglalkoztatott (hiányzó ~20%)
        emp = None if rng.random() < 0.20 else int(rng.integers(1, 250))

        # Export (gyakran 0; ha van, a sales 5–60%-a)
        if rng.random() < 0.70 or sales == 0:
            export_val = 0.0
        else:
            export_val = float(int(sales * rng.uniform(0.05, 0.60)))

        # Kor
        age = int(rng.integers(1, 51))

        # Kötelezettségek: 0.2–1.5 × sales + zaj
        liabilities = float(int(max(0, sales * rng.uniform(0.2, 1.5) + rng.normal(0, 200))))

        # Összes eszköz
        eszk = float(int(max(0, sales * rng.uniform(0.5, 2.0) + liabilities * rng.uniform(0.0, 0.5))))

        # Tárgyi eszközök
        tanass_clean = float(int(eszk * rng.uniform(0.10, 0.70)))

        # Anyag jellegű ráfordítások
        ranyag = float(int(max(0, sales * rng.uniform(0.30, 0.85) + (0 if sales > 0 else rng.uniform(0, 50)))))

        # Személyi jellegű ráfordítások
        persexp_clean = float(int(max(0, sales * rng.uniform(0.05, 0.30) + rng.normal(0, 50))))

        # EBIT (üzemi eredmény)
        op_margin = rng.uniform(-0.10, 0.20)
        ereduzem = float(int(sales * op_margin - rng.normal(0, 50)))

        # Adózás előtti eredmény
        interest_exp = liabilities * rng.uniform(0.02, 0.08)
        pretax = float(int(ereduzem - interest_exp + rng.uniform(-50, 50)))

        # Jegyzett tőke
        jetok = float(int(max(3, eszk * rng.uniform(0.01, 0.30))))

        # Támogatás
        has_grant = int(rng.random() < 0.40)
        if has_grant:
            cap = max(5.0, sales * 0.30)
            grant_value = float(int(min(cap, rng.lognormal(mean=3.5, sigma=1.0))))
        else:
            grant_value = 0.0

        # Megye
        county = rng.choice(counties)

        # Kilépés 2021-ig (dummy)
        exit_prob = 0.08
        if sales < 50 or pretax < 0:
            exit_prob += 0.07
        if emp is not None and emp <= 5:
            exit_prob += 0.05
        exit_2021 = int(rng.random() < min(0.5, exit_prob))

        rows.append({
            "row_id": i + 1,
            "year": year,
            "nace2_name_code": rng.choice(nace_opts),
            "sales_clean": sales,
            "emp": emp,
            "age": age,
            "pretax": pretax,
            "ereduzem": ereduzem,
            "export_value": export_val,
            "liabilities": liabilities,
            "tanass_clean": tanass_clean,
            "eszk": eszk,
            "persexp_clean": persexp_clean,
            "ranyag": ranyag,
            "jetok": jetok,
            "grant_value": grant_value,
            "firm_owner": rng.choice(owner_opts),
            "has_grant": has_grant,
            "county": county,
            "exit_2021": exit_2021
        })

    d = pd.DataFrame(rows)

    # Típusok rendezése
    d["emp"] = d["emp"].astype("Int64")
    d["year"] = d["year"].astype("Int64")
    for c in [
        "sales_clean", "pretax", "ereduzem", "export_value", "liabilities",
        "tanass_clean", "eszk", "persexp_clean", "ranyag", "jetok", "grant_value"
    ]:
        d[c] = d[c].astype("float64")

    d["has_grant"] = d["has_grant"].astype("Int64")
    d["exit_2021"] = d["exit_2021"].astype("Int64")

    return d

df = make_demo_df()

# ---------------------------- Rövid leírás -----------------------------
st.title("Adattábla példa - Szimulált adatok")

# ---------------------------- Változótípusok ---------------------------
def classify_series(s: pd.Series) -> str:
    nun = s.nunique(dropna=True)
    if nun == 2:
        return "bináris"
    if nun <= 10:
        return "kategória"
    if pd.api.types.is_numeric_dtype(s):
        return "folytonos"
    return "kategória"

# ---------------------------- Oszlopok kiválasztása --------------------
present_cols = [c for c in VARS_TO_SHOW if c in df.columns]
missing_cols = [c for c in VARS_TO_SHOW if c not in df.columns]

if missing_cols:
    st.caption("A következő, megjelenítésre kért oszlopok nem találhatók az adatban: " + ", ".join(missing_cols))

if not present_cols:
    st.warning("A megjelenítéshez kijelölt oszlopok egyike sem található az adatban. Mutatjuk az összes elérhető oszlopot.")
    present_cols = list(df.columns)

# ---------------------------- 100 véletlen sor --------------------------
if df.empty:
    st.warning("Az adatállomány üres.")
else:
    n_show = len(df)
    sample_df = df[present_cols].sample(n=n_show, random_state=42).reset_index(drop=True)

    to_show = sample_df.rename(columns=lambda c: DISPLAY_NAMES.get(c, c))

    st.subheader("100 véletlen vállalat")
    st.dataframe(to_show, use_container_width=True, hide_index=True)

    st.caption("Tipp: Az oszlopok szélessége a fejléc szélén húzással állítható.")
