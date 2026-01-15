# correlations_by_industry_nace1.py
import streamlit as st
import pandas as pd
import numpy as np
import utils

# --------------------------- Setup ---------------------------
col_settings, col_viz = utils.setup_page(
    'Korrelációk Ágazatonként — 2019 keresztmetszet',
    'Korrelációk Ágazatonként — 2019 keresztmetszet (szimulált)'
)
cs = utils.load_cross_section(st.session_state['data_path'])
cs = utils.add_nace1_columns(cs)

OTHER_VARS = {
    "Kapott támogatást": "has_grant",
    "Tulajdonos": "firm_owner"
}
MONETARY_VARS = utils.get_monetary_vars()
VAR_MAP = {**MONETARY_VARS, **utils.NON_MONETARY_VARS, **OTHER_VARS}

# ---------- MANUÁLIS ALAPÉRTELMEZETT (baseline) VÁLTOZÓ ----------
BASELINE_LABEL = 'Kapott támogatást'

st.markdown(
    "Válasszon két **változót**. Az alkalmazás kiszámítja a **Pearson-féle korrelációt** "
    "minden **NACE1 csoportban** külön-külön, valamint **az összes ágazatra együtt**. "
    "A táblázat sorai a NACE1 csoportok, az oszlopok: korreláció és elemszám (*n*)."
)

# --------------------------- Beállítások (Bal oldal) ---------------------------
with col_settings:
    st.header("Beállítások")

    sync_on = utils.render_sync_option(st)

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

    # Sync Secondary (X)
    x_index = utils.get_synced_index(available_keys, "global_secondary_var")
    x_label = st.selectbox("Változó 1", available_keys, index=x_index)

    y_options = [k for k in available_keys if k != x_label]
    
    # Sync Primary (Y)
    y_index = utils.get_synced_index(y_options, "global_primary_var")
    y_label = st.selectbox("Változó 2", y_options, index=y_index)

    utils.update_synced_state("global_secondary_var", x_label)
    utils.update_synced_state("global_primary_var", y_label)

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
by_ind["Ágazat"] = by_ind["nace1_code"].astype(str).map(utils.NACE1_LABELS).fillna(by_ind["nace1_code"].astype(str))

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
