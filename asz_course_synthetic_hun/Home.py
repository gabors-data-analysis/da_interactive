# home.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path

st.set_page_config(page_title="Kezdőlap — Szimulált", layout="wide")

# ---------------------------- Felhasználói beállítások ----------------------------
VARS_TO_SHOW = [
    "row_id", "nace2_name_code",
    "sales_clean", "emp", "age",
    "pretax", "export_value", "liabilities", "firm_owner", "has_grant"
]

DISPLAY_NAMES = {
    "row_id": "ID",
    "nace2_name_code": "Ágazat (NACE2)",
    "sales_clean": "Sales (ezer HUF)",
    "emp": "Foglalkoztatottak (fő)",
    "age": "Kor (év)",
    "pretax": "Adózás előtti eredmény (ezer HUF)",
    "export_value": "Export érték (ezer HUF)",
    "liabilities": "Kötelezettségek (ezer HUF)",
    "firm_owner": "Tulajdonosi forma",
    "has_grant": "Kapott támogatást"
}

# ---------------------------- Adat betöltés ----------------------------
@st.cache_data
def load_cs(path: str = "data/synthetic/sim_cs2019_by_nace2_withcats.parquet") -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    return pd.read_parquet(p).copy()

df = load_cs()

# ---------------------------- Rövid leírás -----------------------------
st.title("Vállalati adatok — kezdőlap")
st.markdown(
    """
Ez az alkalmazás a **2019-es vállalati keresztmetszeti** adaton végez interaktív vizualizációt és regressziókat.  
Itt a kezdőlapon **10 véletlen vállalatot** mutatunk be a teljes adatállományból.

Az oszlopfejlécekben a változó **típusát** is jelezzük:
- *folytonos*: numerikus, sokféle érték
- *bináris*: pontosan 2 különböző érték
- *kategória*: legfeljebb 10 különböző érték (de nem bináris)
"""
)

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

types = {col: classify_series(df[col]) for col in present_cols}

# ---------------------------- 10 véletlen sor --------------------------
if df.empty:
    st.warning("Az adatállomány üres.")
else:
    n_show = min(10, len(df))
    sample_df = df[present_cols].sample(n=n_show, random_state=None).reset_index(drop=True)

    # Két szintű fejléc: (megjelenített név, típus)
    multi_cols = pd.MultiIndex.from_tuples(
        [(DISPLAY_NAMES.get(col, col), types[col]) for col in sample_df.columns],
        names=["Változó", "Típus"]
    )
    to_show = sample_df.copy()
    to_show.columns = multi_cols

    st.subheader("10 véletlen vállalat")
    # Tipp: az oszlopok szélessége az egérrel, a fejléc szélét húzva állítható
    st.dataframe(to_show, use_container_width=True, hide_index=True)

    st.caption(
        "Megjegyzés: *kategória* = ≤10 különböző érték; *bináris* = pontosan 2 érték; "
        "*folytonos* = numerikus változó sok különböző értékkel. "
        "Az oszlopok szélessége a fejléc szélén húzással állítható."
    )
