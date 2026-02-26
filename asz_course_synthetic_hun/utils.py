import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
from typing import List, Optional

# -------------------------------------------------------------------------
# 1. Constants
# -------------------------------------------------------------------------
COLORS = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

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

FILTER_OPTIONS = ["Nincs szűrés", "Kézi minimum/maximum"]

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

# -------------------------------------------------------------------------
# 2. Session State Management
# -------------------------------------------------------------------------
def init_session_state():
    st.session_state.setdefault("real_data", False)

def get_monetary_vars():
    return MONETARY_VARS_REAL if st.session_state.get("real_data") else MONETARY_VARS_SIM

# -------------------------------------------------------------------------
# 3. Data Loading & Efficiency
# -------------------------------------------------------------------------
@st.cache_data
def load_cross_section(path: str, columns: Optional[List[str]] = None) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    
    # Load specific columns if requested to save memory
    df = pd.read_parquet(p, columns=columns).copy()
    
    # Ensure required columns exist
    if "nace2_name_code" not in df.columns and "nace2" in df.columns:
         # Fallback or error handling could go here, but assuming standard dataset structure
         pass

    if "nace2" in df.columns:
        df["nace2"] = df["nace2"].astype(str)
    if "nace2_name_code" in df.columns:
        df["nace2_name_code"] = df["nace2_name_code"].astype(str)

    # Common calculated fields
    if "sales_lead_sim" in df.columns and "sales_clean" in df.columns:
        df["sales_growth_perc"] = (df["sales_lead_sim"] - df["sales_clean"]) / df["sales_clean"] * 100
    
    if "ln_sales_lead_sim" in df.columns and "ln_sales" in df.columns:
        df["sales_growth_log_diff"] = df["ln_sales_lead_sim"] - df["ln_sales"]
    elif "sales_growth_perc" in df.columns:
        df["sales_growth_perc"] = df["sales_growth_perc"] * 100

    return df

def add_nace1_columns(df: pd.DataFrame) -> pd.DataFrame:
    """Adds NACE1 grouping columns to the dataframe."""
    if "nace2" not in df.columns:
        return df
    
    nace2_num = pd.to_numeric(df["nace2"].str[:2], errors="coerce")
    bins = [0, 4, 9, 34, 35, 39, 44, 47, 54, 57, 63, 66, 68, 76, 83, 84, 85, 89, 99]
    labels = [
        "01-04", "05-09", "10-34", "35", "36-39", "40-44", "45-47", "48-54",
        "55-57", "58-63", "64-66", "67-68", "69-76", "77-83", "84", "85", "86-89", "90-99"
    ]
    df["nace1_code"] = pd.cut(nace2_num, bins=bins, labels=labels, right=True, include_lowest=True)
    df["nace1_name"] = df["nace1_code"].astype(str).map(NACE1_LABELS).fillna(df["nace1_code"].astype(str))
    return df

# -------------------------------------------------------------------------
# 4. UI Component Abstraction
# -------------------------------------------------------------------------
def setup_page(title_real: str, title_sim: str):
    init_session_state()
    
    title = title_real if st.session_state["real_data"] else title_sim
    st.set_page_config(page_title=title, layout="wide")

    # Header Layout
    col_left, col_right = st.columns([4, 1])
    with col_left:
        st.title(title)
    
    with col_right:
        # Logo logic
        # Assuming utils.py is in root, images is in ./images
        logo_path = Path(__file__).parent / "images/logo_opten_horizontal_black.png"
        if logo_path.exists():
            st.image(str(logo_path), use_container_width=True)

    st.markdown(
        """
        Az adatok forrása **OPTEN**.  
        Minden ábra és adat oktatási céllal készült és tájékoztató jellegű.  
        """
    )

    # Main 3-column Layout
    col_settings, col_sep, col_viz = st.columns([4, 2, 12])
    with col_sep:
        st.markdown(
            '<div style="border-left: 1px solid #e0e0e0; height: 100vh; margin: 0 auto;"></div>',
            unsafe_allow_html=True,
        )
    
    return col_settings, col_viz

# -------------------------------------------------------------------------
# 5. Helper Functions
# -------------------------------------------------------------------------
def apply_filter(series: pd.Series, mode: str, low_val: float, high_val: float) -> pd.Series:
    s = series.dropna()
    if len(s) < 5 or mode == "Nincs szűrés":
        return s

    if mode == "Kézi minimum/maximum":
        if low_val is not None and high_val is not None:
            return s[(s > low_val) & (s < high_val)]
    return s

def lspline(series: pd.Series, knots: List[float]) -> np.ndarray:
    """Linear spline design matrix."""
    vector = series.values.astype(float)
    columns = []
    for i, knot in enumerate(knots):
        column = np.minimum(vector, knot if i == 0 else knot - knots[i - 1])
        columns.append(column)
        vector = vector - column
    columns.append(vector)
    return np.column_stack(columns)

def tail_note_txt(mode, low=None, high=None):
    if mode == "Nincs szűrés":
        return "nincs"
    if mode == "Kézi minimum/maximum":
        return f"kézi [{low:.2f}, {high:.2f}]"
    return "—"