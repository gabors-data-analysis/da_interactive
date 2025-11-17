# correlations_by_industry.py
import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path

# --------------------------- Beállítások ---------------------------
color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]
if st.session_state.get('real_data', False):
    st.set_page_config(
        page_title='Korrelációk ágazatonként — Vállalatok (HU keresztmetszet)',
        layout='wide'
    )
else:
    st.set_page_config(
        page_title='Korrelációk ágazatonként — Vállalatok (HU keresztmetszet, szimulált)',
        layout='wide'
    )

# pénzügyi változók (mint a korábbi szórásdiagramokban)
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
    "Kapott támogatást":"has_grant",
    "Tulajdonos":"firm_owner"
}

real_data = st.session_state.get('real_data', False)
MONETARY_VARS = MONETARY_VARS_REAL if real_data else MONETARY_VARS_SIM
VAR_MAP = {**MONETARY_VARS, **NON_MONETARY_VARS,**OTHER_VARS}


# ---------- MANUÁLIS ALAPÉRTELMEZETT (baseline) VÁLTOZÓ ----------
# Ezt a feliratot írd át, ha másik legyen az első selectbox alapértelmezése.
BASELINE_LABEL = 'Kapott támogatást'

# --------------------------- Adatbetöltés ---------------------------
@st.cache_data
def load_cross_section(path: str) -> pd.DataFrame:
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

data_path = st.session_state.get('data_path', 'data/synthetic/sim_cs2019_by_nace2_withcats.parquet')
cs = load_cross_section(data_path)

# --------------------------- Cím & leírás ---------------------------
if real_data:
    st.title('Korrelációk ágazatonként — 2019 keresztmetszet')
else:
    st.title('Korrelációk ágazatonként — 2019 keresztmetszet (szimulált)')

st.markdown(
    "Válasszon két **változót**. Az alkalmazás kiszámítja a **Pearson-féle korrelációt** "
    "minden ágazatban külön-külön, valamint **az összes ágazatra együtt**. "
    "A táblázat sorai az ágazatok, az oszlopok: korreláció és elemszám (*n*)."
)

# --------------------------- Oldalsáv: változók ---------------------------
st.sidebar.header("Beállítások")

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

# Változó 1
x_label = st.sidebar.selectbox("Változó 1", available_keys, index=x_index)

# Változó 2 – az első választást kizárjuk, így nem lehet ugyanaz
y_options = [k for k in available_keys if k != x_label]
# ha a baseline-t választottuk 1-nek, akkor a 2. legyen az első másik opció
y_label = st.sidebar.selectbox("Változó 2", y_options, index=0)

xvar = available[x_label]
yvar = available[y_label]

# --------- Rendezési opció a táblázathoz (új) ----------
sort_choice = st.sidebar.radio(
    "Rendezés a táblázatban",
    ["Ágazati kód szerint", "Korreláció szerint (csökkenő)"],
    index=0
)

# --------------------------- Előkészítés ---------------------------
df = cs.copy()
df = df.replace([np.inf, -np.inf], np.nan)

# Csak a kiválasztott két változó + ágazat mező
needed_cols = ["nace2_name_code", xvar, yvar]
missing_cols = [c for c in needed_cols if c not in df.columns]
if missing_cols:
    st.error(f"Hiányzó oszlop(ok) az adatban: {missing_cols}")
    st.stop()

df = df[needed_cols].dropna(subset=[xvar, yvar])

if df.empty:
    st.error("Nincs olyan megfigyelés, ahol mindkét kiválasztott változó nem hiányzik.")
    st.stop()

# --------------------------- Korreláció számítás ---------------------------
def corr_and_n(sub: pd.DataFrame) -> pd.Series:
    tmp = sub[[xvar, yvar]].dropna()
    n = len(tmp)
    if n < 2:
        return pd.Series({"corr": np.nan, "n": n})
    return pd.Series({"corr": tmp[xvar].corr(tmp[yvar]), "n": n})

# Ágazatonként
by_ind = df.groupby("nace2_name_code", observed=True).apply(corr_and_n).reset_index()

# 1) Azok az ágazatok, melyek neve "NACE..." – elrejtjük
mask_valid = ~by_ind["nace2_name_code"].str.startswith("NACE", na=False)
by_ind = by_ind[mask_valid].copy()

# 2) Ágazatok rendezése NACE-szám szerint (a zárójelben lévő kód alapján)
by_ind["__code"] = pd.to_numeric(
    by_ind["nace2_name_code"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
    errors="coerce"
)
by_ind = by_ind.sort_values(["__code", "nace2_name_code"]).drop(columns="__code")

# Összes ágazat külön sorban (minden megfigyeléssel, akkor is, ha van NACE-only ágazat)
overall = corr_and_n(df)
overall_row = pd.DataFrame({
    "nace2_name_code": ["Összes ágazat"],
    "corr": [overall["corr"]],
    "n": [overall["n"]],
})

# Összefűzés: egyelőre ágazati kód szerinti sorrenddel
result = pd.concat([overall_row, by_ind], ignore_index=True)

# Szép oszlopnevek
corr_col_name = f"Korreláció ({x_label}, {y_label})"
result = result.rename(columns={
    "nace2_name_code": "Ágazat",
    "corr": corr_col_name,
    "n": "n"
})

# --------- Rendezés korreláció szerint, ha a felhasználó ezt kérte ----------
if sort_choice == "Korreláció szerint (csökkenő)":
    overall_part = result[result["Ágazat"] == "Összes ágazat"]
    sector_part = result[result["Ágazat"] != "Összes ágazat"].copy()
    sector_part = sector_part.sort_values(
        by=corr_col_name,
        ascending=False,
        na_position="last"
    )
    result = pd.concat([overall_part, sector_part], ignore_index=True)
# ha az első opció van kiválasztva, már eleve kód szerint vannak rendezve

# --------------------------- Megjelenítés ---------------------------
st.subheader("Korrelációk ágazatonként")

st.dataframe(
    result.style.format({
        corr_col_name: "{:.3f}",
        "n": "{:,.0f}"
    }),
    use_container_width=True
)

st.caption(
    "Megjegyzés: Pearson-féle korrelációs együttható. "
    "Az \"Összes ágazat\" sor minden vállalatot együtt kezel. "
    
)