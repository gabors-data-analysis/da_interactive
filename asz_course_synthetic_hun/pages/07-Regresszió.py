import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
import statsmodels.api as sm

# ------------------------------------------------------
# Beállítások
# ------------------------------------------------------
if st.session_state['real_data'] == True:
    st.set_page_config(page_title="Növekedési regressziók — Vállalatok", layout="wide")
    st.title("Növekedési regressziók — 2019 keresztmetszet")
else:
    st.set_page_config(page_title="Növekedési regressziók — Vállalatok (szimulált)", layout="wide")
    st.title("Növekedési regressziók — 2019 keresztmetszet (szimulált)")




@st.cache_data
def load_cs(path: str = st.session_state['data_path']) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()

    # ellenőrzés: legyen iparági címke a szűréshez
    if "nace2_name_code" not in df.columns:
        st.error("Hiányzik a `nace2_name_code` oszlop az adatban.")
        st.stop()

    # típusok
    if "nace2" in df.columns:
        df["nace2"] = df["nace2"].astype(str)

    # ln_sales előállítása, ha szükséges
    if "sales_clean" in df.columns:
        df["ln_sales"] = np.log(np.clip(df["sales_clean"].astype(float), 1e-9, None))

    return df

df = load_cs()

st.markdown(
    """
Válasszon **ágazatot**, **kimeneti változót** és **magyarázó változókat**.

Támogatott kimenetek:

- **Relatív növekedés**: (sales_lead − sales) / sales  
- **Lognövekedés**: ln(sales_lead) − ln(sales)
"""
)

# ------------------------------------------------------
# Oldalsáv: ágazati szűrő
# ------------------------------------------------------
st.sidebar.header("Szűrők és modell")

# Ágazatok rendezése a címkében szereplő számszerű kód szerint; „ÖSSZES” az elején
lab_df = pd.DataFrame({"label": df["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
                                 errors="coerce")
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
industry_opts = ["Összes ágazat"] + lab_df["label"].tolist()

sel_industry = st.sidebar.selectbox("Ágazat", industry_opts, index=0)
if sel_industry == "Összes ágazat":
    d = df.copy()
else:
    d = df[df["nace2_name_code"] == sel_industry].copy()

# --- Foglalkoztatási méretkategóriák az `emp` alapján
if "emp" in d.columns:
    emp_float = pd.to_numeric(d["emp"], errors="coerce")
    d["emp_size"] = pd.cut(
        emp_float,
        bins=[-np.inf, 5, 50, 250, np.inf],
        labels=["≤5", "5–50", "50–250", "250+"],
        right=True,
        ordered=True
    ).astype("category")
else:
    d["emp_size"] = pd.Series(pd.Categorical([np.nan]*len(d)))

# ------------------------------------------------------
# Kimenetek
# ------------------------------------------------------
outcome_labels = [
    "Relatív növekedés: (sales_lead - sales)/sales",
    "Lognövekedés: ln(sales_lead) - ln(sales)",
]
label_to_col = {
    outcome_labels[0]: "sales_growth_perc",
    outcome_labels[1]: "sales_growth_log_diff",
}

# Build choices only for outcomes that actually exist
available = [lbl for lbl, col in label_to_col.items() if col in d.columns]
if not available:
    st.error(
        "Hiányoznak az előre számolt kimenetek. "
        "Kérlek futtasd a data generator-t (add_outcomes), hogy létrejöjjenek a kimeneti oszlopok."
    )
    st.stop()

outcome_choice = st.sidebar.selectbox("Kimenet", available, index=min(1, len(available)-1))
y_col = label_to_col[outcome_choice]

y = d[y_col].astype(float)  # safe; NaN stays NaN

# ------------------------------------------------------
# Magyarázó változók (kis kategóriaszámú oszlopok → kategóriák; per-változó négyzet/log; interakciók)
# ------------------------------------------------------
exclude_cols = {
    "nace2_name_code", "nace2", "growth_sim", "ln_sales_lead_sim", "sales_lead_sim",
    "ln_sales22_lead2", "sales22_lead2", "ln_sales", "name_hu", "row_id", "exit", "county"
}
MAX_CAT_LEVELS = 10

is_num  = pd.api.types.is_numeric_dtype
is_bool = pd.api.types.is_bool_dtype

# Osztályozás
candidate_cols = [c for c in d.columns if c not in exclude_cols]
categorical_cols, numeric_cols = [], []
for c in candidate_cols:
    s = d[c]
    nun = s.nunique(dropna=True)
    if (is_bool(s) or s.dtype == "object" or pd.api.types.is_categorical_dtype(s) or nun <= MAX_CAT_LEVELS):
        categorical_cols.append(c)
    elif is_num(s):
        numeric_cols.append(c)
# Megjegyzés: az emp_size természetesen a kategóriák közé kerül (4 szint)

st.sidebar.subheader("Magyarázó változók")
cont_vars = st.sidebar.multiselect("Folytonos magyarázó változók", options=sorted(numeric_cols))
cat_vars  = st.sidebar.multiselect("Kategorikus magyarázó változók", options=sorted(categorical_cols))

# --- Per-változó négyzetes tag
st.sidebar.markdown("**Négyzetes tagok (változónként):**")
quad_selected = []
for v in cont_vars:
    if st.sidebar.checkbox(f"{v}²", value=False, key=f"quad__{v}"):
        quad_selected.append(v)
quad_set = set(quad_selected)

# --- Per-változó log transzformáció
st.sidebar.markdown("**Log transzformációk (változónként):**")
log_selected = []
for v in cont_vars:
    if st.sidebar.checkbox(f"log({v})", value=False, key=f"log__{v}"):
        log_selected.append(v)
log_set = set(log_selected)

st.sidebar.markdown("**Interakció:**")
# --- Opcionális interakció: emp_size × tulajdonosi forma
ownership_default_idx = 0
ownership_options = [c for c in sorted(set(["firm_owner"]) | set(categorical_cols)) if c in d.columns]
if "firm_owner" in ownership_options:
    ownership_default_idx = ownership_options.index("firm_owner")

interact_emp_owner = st.sidebar.checkbox("Méretkategória × tulajdonosi forma interakció", value=False)
owner_var = "firm_owner"

rhs_cols = cont_vars + cat_vars
if not rhs_cols:
    st.info("Válasszon legalább egy magyarázó változót a jobb oldalon a modell futtatásához.")
    st.stop()

# Munkatábla (nyers bemenetekből hiányzók kidobása)
needed_cols = set(rhs_cols) | {"__y__"}
if interact_emp_owner:
    if "emp_size" not in d.columns:
        st.error("Az `emp_size` nem érhető el az interakcióhoz. (Korábban az `emp` alapján kell létrehozni.)")
        st.stop()
    needed_cols |= {"emp_size", owner_var}

dwork = d.copy()
dwork["__y__"] = y
dwork = dwork[list(needed_cols)].replace([np.inf, -np.inf], np.nan).dropna()

if dwork.empty:
    st.error("Nem maradt megfigyelés a kimenet/magyarázók hiányzóinak eldobása után.")
    st.stop()

# ------------------------------------------------------
# Dizájnmátrix
# ------------------------------------------------------
X_parts = []

# Folytonos változók (+ négyzet + log)
for v in cont_vars:
    x = pd.to_numeric(dwork[v], errors="coerce").astype(float)
    X_parts.append(x.rename(v))
    if v in quad_set:
        X_parts.append((x**2).rename(f"{v}^2"))
    if v in log_set:
        x_pos = x.where(x > 0, np.nan)         # nempozitív → NA a loghoz
        X_parts.append(np.log(x_pos).rename(f"log({v})"))

# Kategorikus változók → egyhot (numerikus dummy-k)
dummy_cache = {}  # interakcióhoz eltesszük
for v in cat_vars:
    dummies = pd.get_dummies(
        dwork[v].astype("category"),
        prefix=v,
        drop_first=True,
        dtype=float
    )
    dummy_cache[v] = dummies
    X_parts.append(dummies)

# Interakció: emp_size × owner_var
if interact_emp_owner:
    if "emp_size" not in dummy_cache:
        emp_dum = pd.get_dummies(
            dwork["emp_size"].astype("category"),
            prefix="emp_size",
            drop_first=True,
            dtype=float
        )
    else:
        emp_dum = dummy_cache["emp_size"]

    own_dum = pd.get_dummies(
        dwork[owner_var].astype("category"),
        prefix=owner_var,
        drop_first=True,
        dtype=float
    )

    inter_cols = {}
    for e_name in emp_dum.columns:
        for o_name in own_dum.columns:
            inter = (emp_dum[e_name] * own_dum[o_name]).astype(float)
            inter_cols[f"{e_name} × {o_name}"] = inter
    if inter_cols:
        X_parts.append(pd.DataFrame(inter_cols, index=dwork.index))

# Összefűzés, numerikus kényszerítés, Y igazítás
X = pd.concat(X_parts, axis=1)
X = X.apply(pd.to_numeric, errors="coerce").replace([np.inf, -np.inf], np.nan)
Y = pd.to_numeric(dwork["__y__"], errors="coerce").replace([np.inf, -np.inf], np.nan)

valid = X.notnull().all(axis=1) & Y.notnull()
X = X.loc[valid]
Y = Y.loc[valid]

if X.shape[0] < 5 or X.shape[1] == 0:
    st.error("Nincs elegendő felhasználható adat a kódolás/koerció után. Próbáljon más változókat vagy ágazatot.")
    st.stop()

X = sm.add_constant(X, has_constant="add")
model = sm.OLS(Y.astype(float), X.astype(float))
res = model.fit(cov_type="HC1")

# ------------------------------------------------------
# Szépített kimenet (csillagok + SE zárójelben)
# ------------------------------------------------------
st.subheader("Regressziós eredmények")

def sig_stars(p):
    if p < 0.01:
        return "***"
    elif p < 0.05:
        return "**"
    elif p < 0.1:
        return "*"
    return ""

def fmt_coef(x):
    try:
        if x != 0 and abs(x) < 0.001:
            return f"{x:.3g}"
        return f"{x:.3f}"
    except Exception:
        return str(x)

def fmt_se(x):
    try:
        if x != 0 and abs(x) < 0.001:
            return f"{x:.3g}"
        return f"{x:.3f}"
    except Exception:
        return str(x)

def prettify_name(name: str) -> str:
    if name == "const":
        return "Konstans"
    # hatványok
    name = name.replace("^2", " négyzet").replace("^3", " köb").replace("^4", " negyedik hatvány")
    # Dummyk: "var_Level" -> "var = Level"
    for cat in cat_vars:
        prefix = f"{cat}_"
        if name.startswith(prefix):
            level = name[len(prefix):]
            return f"{cat} = {level}"
    return name.replace("_", " ")

# Sorok összeállítása a paraméterrendben
rows = []
for term in res.params.index:
    coef = res.params[term]
    se   = res.bse[term]
    pval = res.pvalues[term]
    entry = f"{fmt_coef(coef)}{sig_stars(pval)} ({fmt_se(se)})"
    rows.append((prettify_name(term), entry))

# Egy modell táblázat
table_df = pd.DataFrame(rows, columns=["", "1. modell"])

# Megjelenítés
st.table(table_df)

# Választóvonal + alul: statok és bázisszintek
st.markdown("---")
st.markdown(f"**R²:** {res.rsquared:.3f}")

note = "Zárójelben a robusztus (HC1) standad hibák szerepelnek."

# --- Bázisszintek (drop-first) minden felhasznált kategóriára
def _baseline_of(series: pd.Series) -> str:
    s = series.astype("category")
    cats = list(s.cat.categories)
    return "—" if len(cats) == 0 else str(cats[0])

cats_in_model = set(cat_vars)
if 'interact_emp_owner' in locals() and interact_emp_owner:
    cats_in_model |= {"emp_size", owner_var}

baseline_lines = []
for v in sorted(cats_in_model):
    if v in dwork.columns:
        baseline_lines.append(f"{v}: {_baseline_of(dwork[v])}")

baselines_text = " | ".join(baseline_lines) if baseline_lines else "Nincs"

st.markdown(
    f"<span style='font-size:0.9em'><em>Megjegyzés:</em> {note} "
    "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
    unsafe_allow_html=True,
)

st.markdown(
    f"<span style='font-size:0.9em'><em>Kategóriák referencia-szintjei (drop-first):</em> {baselines_text}</span>",
    unsafe_allow_html=True,
)
