import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
import statsmodels.api as sm

# ------------------------------------------------------
# Beállítások
# ------------------------------------------------------

BASE_DIR = Path(__file__).resolve().parent.parent
col_left, col_right = st.columns([4, 1])

with col_left:
    if st.session_state['real_data'] == True:
        st.set_page_config(page_title="Növekedési regressziók — Vállalatok", layout="wide")
        st.title("Növekedési regressziók — 2019 keresztmetszet")
    else:
        st.set_page_config(page_title="Növekedési regressziók — Vállalatok (szimulált)", layout="wide")
        st.title("Növekedési regressziók — 2019 keresztmetszet (szimulált)")
with col_right:
    logo_path = BASE_DIR / "images/logo_opten_horizontal_black.png"
    if logo_path.exists():
        st.image(str(logo_path), use_container_width=True)


@st.cache_data
def load_cs(path: str = st.session_state['data_path']) -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()

    if "nace2_name_code" not in df.columns:
        st.error("Hiányzik a `nace2_name_code` oszlop az adatban.")
        st.stop()

    if "nace2" in df.columns:
        df["nace2"] = df["nace2"].astype(str)

    if "sales_clean" in df.columns:
        df["ln_sales"] = np.log(np.clip(df["sales_clean"].astype(float), 1e-9, None))

    return df

df = load_cs(st.session_state['data_path'])

# ------------------------------------------------------
# Változó-label szótárak (más dashboardokkal konzisztens)
# ------------------------------------------------------
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
    "Relatív növekedés (%)": "sales_growth_perc",
    "Log növekedés (log-diff)": "sales_growth_log_diff",
    "Foglalkoztatottak száma (fő)": "emp",
    "Kor (év)": "age",
}

# Fordított mapping: oszlopnév -> label (alapértelmezett)
VAR_LABELS_BY_COL = {}
for label, col in {**MONETARY_VARS, **NON_MONETARY_VARS}.items():
    VAR_LABELS_BY_COL[col] = label

# Kézi label további változókra
VAR_LABELS_BY_COL.update({
    "emp_size": "Foglalkoztatási méretkategória",
    "firm_owner": "Tulajdonosi forma",
    "has_export":"Exportőr",
    "has_grant":"Kapott támogatást"
   # "county_name":"Megye"
})

def col_to_label(col: str) -> str:
    """Oszlopnév -> megjelenített label."""
    return VAR_LABELS_BY_COL.get(col, col.replace("_", " "))
st.markdown(
    """
    Az adatok forrása **OPTEN**.  
    Minden ábra és adat oktatási céllal készült és tájékoztató jellegű.  
    """
)
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

lab_df = pd.DataFrame({"label": df["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(
    lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
    errors="coerce"
)
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
lead_vars_sim = [col for col in ["sales_lead_sim", "ln_sales_lead_sim"] if col in d.columns]

available = [lbl for lbl, col in label_to_col.items() if col in d.columns]
if not available:
    if lead_vars_sim:
        d["sales_growth_perc"] = (d["sales_lead_sim"] - d["sales_clean"]) / d["sales_clean"] * 100
        d["sales_growth_log_diff"] = d["ln_sales_lead_sim"] - d["ln_sales"]
        available = [lbl for lbl, col in label_to_col.items() if col in d.columns]
    else:
        st.error(
            "Hiányoznak az előre számolt kimenetek. "
            "Kérlek futtasd a data generator-t (add_outcomes), hogy létrejöjjenek a kimeneti oszlopok."
        )
        st.stop()

if st.session_state["real_data"] == True:
    d["sales_growth_perc"] = d["sales_growth_perc"] * 100
outcome_choice = st.sidebar.selectbox("Kimenet", available, index=0)
y_col = label_to_col[outcome_choice]
y = d[y_col].astype(float)

# ------------------------------------------------------
# Magyarázó változók
# ------------------------------------------------------
exclude_cols = {
    "nace2_name_code", "nace2", "growth_sim", "ln_sales_lead_sim", "sales_lead_sim",
    "ln_sales22_lead2", "sales22_lead2", "ln_sales", "name_hu", "row_id", "exit", "county",
    "sales_growth_log_diff", "sales_growth_perc", "is_ginop", "is_gop", "is_other",
    "is_tamop", "is_vp","foundyear","jetok","grant_value","ranyag", "county_name", "satok"
}
MAX_CAT_LEVELS = 10

is_num  = pd.api.types.is_numeric_dtype
is_bool = pd.api.types.is_bool_dtype

candidate_cols = [c for c in d.columns if c not in exclude_cols]
categorical_cols, numeric_cols = [], []
for c in candidate_cols:
    s = d[c]
    nun = s.nunique(dropna=True)
    if (is_bool(s) or s.dtype == "object" or pd.api.types.is_categorical_dtype(s) or nun <= MAX_CAT_LEVELS):
        categorical_cols.append(c)
    elif is_num(s):
        numeric_cols.append(c)

# label -> col MAP-ek a választáshoz
num_label_to_col = {col_to_label(c): c for c in numeric_cols}
cat_label_to_col = {col_to_label(c): c for c in categorical_cols}

st.sidebar.subheader("Magyarázó változók")

# A felhasználó label alapján választ folytonos változókat
cont_labels = st.sidebar.multiselect(
    "Folytonos magyarázó változók",
    options=sorted(num_label_to_col.keys())
)
cont_vars = [num_label_to_col[l] for l in cont_labels]

# Kategorikus változók label alapján
cat_labels = st.sidebar.multiselect(
    "Kategorikus magyarázó változók",
    options=sorted(cat_label_to_col.keys())
)
cat_vars = [cat_label_to_col[l] for l in cat_labels]

# --- Per-változó négyzetes tag (label látszik, key a col-név alapján)
st.sidebar.markdown("**Négyzetes tagok (változónként):**")
quad_selected = []
for v in cont_vars:
    label = col_to_label(v)
    if st.sidebar.checkbox(f"{label}²", value=False, key=f"quad__{v}"):
        quad_selected.append(v)
quad_set = set(quad_selected)

# --- Per-változó log transzformáció (label látszik, key a col-név)
st.sidebar.markdown("**Log transzformációk (változónként):**")
log_selected = []
for v in cont_vars:
    label = col_to_label(v)
    if st.sidebar.checkbox(f"log({label})", value=False, key=f"log__{v}"):
        log_selected.append(v)
log_set = set(log_selected)

st.sidebar.markdown("**Interakció:**")
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

# ------------------------------------------------------
# Szélsőérték-kezelés (Y és folytonos X-ek)
# ------------------------------------------------------
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

st.sidebar.subheader("Szélsőérték-kezelés (kimenet és folytonos magyarázók)")

y_filter = st.sidebar.selectbox("Y szélsőérték-kezelése", FILTER_OPTIONS, index=0)

if y_filter == "Kézi minimum/maximum":
    st.sidebar.markdown("**Y kézi határok (a kimenet egységében)**")
    y_clean = y.replace([np.inf, -np.inf], np.nan).dropna()
    if len(y_clean) > 0:
        current_min_y = float(np.nanmin(y_clean))
        current_max_y = float(np.nanmax(y_clean))
    else:
        current_min_y, current_max_y = 0.0, 1.0
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

x_filter = st.sidebar.selectbox("Folytonos X-ek szélsőérték-kezelése", FILTER_OPTIONS, index=0)

if x_filter == "Kézi minimum/maximum" and cont_vars:
    st.sidebar.markdown("**X kézi határok (folytonos magyarázók egységében)**")
    all_x_vals = []
    for v in cont_vars:
        all_x_vals.append(pd.to_numeric(d[v], errors="coerce"))
    all_x_vals = pd.concat(all_x_vals, ignore_index=True).replace([np.inf, -np.inf], np.nan).dropna()
    if len(all_x_vals) > 0:
        current_min_x = float(np.nanmin(all_x_vals))
        current_max_x = float(np.nanmax(all_x_vals))
    else:
        current_min_x, current_max_x = 0.0, 1.0

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


def apply_filter(series: pd.Series, mode: str, low_val: float, high_val: float) -> pd.Series:
    s = series.replace([np.inf, -np.inf], np.nan).dropna()
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

# ------------------------------------------------------
# Munkatábla
# ------------------------------------------------------
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

# Y filter
y_filtered = apply_filter(dwork["__y__"], y_filter, y_low_manual, y_high_manual)
if y_filtered.empty:
    st.error("A kimenet (Y) szélsőérték-kezelése után nem maradt megfigyelés.")
    st.stop()
idx_keep = y_filtered.index

# X filters
for v in cont_vars:
    x_series = dwork.loc[idx_keep, v]
    x_filtered = apply_filter(x_series, x_filter, x_low_manual, x_high_manual)
    if x_filtered.empty:
        st.error(f"A(z) `{col_to_label(v)}` változó szélsőérték-kezelése után nem maradt megfigyelés.")
        st.stop()
    idx_keep = idx_keep.intersection(x_filtered.index)

dwork = dwork.loc[idx_keep].copy()
dwork["__y__"] = y_filtered.loc[idx_keep]

if dwork.empty:
    st.error("A szélsőérték-kezelés után nem maradt elegendő megfigyelés.")
    st.stop()

# ------------------------------------------------------
# Dizájnmátrix
# ------------------------------------------------------
X_parts = []

X_parts = []

for v in cont_vars:
    x_raw = pd.to_numeric(dwork[v], errors="coerce").astype(float)

    # If user chose log(v), then use ONLY log(v) (no original).
    if v in log_set:
        x_trans = x_raw.where(x_raw > 0, np.nan)
        x_trans = np.log(x_trans)
        base_name = f"log({v})"
    else:
        x_trans = x_raw
        base_name = v

    # Add the (possibly transformed) base variable
    X_parts.append(x_trans.rename(base_name))

    # If user asked for a quadratic term, it’s the square of the variable actually used.
    # So with log selected, this will be [log(v)]²; without log, it’s v².
    if v in quad_set:
        X_parts.append((x_trans**2).rename(f"{base_name}^2"))
        
dummy_cache = {}
for v in cat_vars:
    dummies = pd.get_dummies(
        dwork[v].astype("category"),
        prefix=v,
        drop_first=True,
        dtype=float
    )
    dummy_cache[v] = dummies
    X_parts.append(dummies)

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
# Szépített journal-style kimenet, label-ekkel
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
    # Konstans
    if name == "const":
        return "Konstans"

    # Interakciók
    if " × " in name:
        left, right = name.split(" × ", 1)
        return f"{prettify_name(left)} × {prettify_name(right)}"

    # Négyzet
    if name.endswith("^2"):
        base = name[:-2]
        base_label = prettify_name(base)
        return f"{base_label} négyzet"

    # log()
    if name.startswith("log(") and name.endswith(")"):
        inner = name[4:-1]
        inner_label = prettify_name(inner)
        return f"log({inner_label})"

    # Dummyk: var_level
    for cat in cat_vars:
        prefix = f"{cat}_"
        if name.startswith(prefix):
            level = name[len(prefix):]
            var_label = col_to_label(cat)
            return f"{var_label} = {level}"

    # Eredeti változók
    if name in VAR_LABELS_BY_COL:
        return VAR_LABELS_BY_COL[name]

    return name.replace("_", " ")

rows = []
for term in res.params.index:
    coef = res.params[term]
    se   = res.bse[term]
    pval = res.pvalues[term]
    coef_str = f"{fmt_coef(coef)}{sig_stars(pval)}"
    se_str   = f"({fmt_se(se)})"
    rows.append((prettify_name(term), coef_str, se_str))

table_df = pd.DataFrame(rows, columns=["Változó", "1. modell", "SE"])

css = """
<style>
.reg-table-container {
    display: flex;
    justify-content: center;
    margin-top: 0.5rem;
    margin-bottom: 1.5rem;
}
.reg-table {
    border-collapse: collapse;
    font-size: 0.9rem;
    font-family: "Times New Roman", serif;
    max-width: 650px;
}
.reg-table thead tr th {
    border-bottom: 1px solid #000;
    padding: 4px 10px;
}
.reg-table tbody tr td {
    padding: 2px 10px;
    border-bottom: 1px solid #ddd;
}
.reg-table tbody tr:last-child td {
    border-bottom: 1px solid #000;
}
.reg-table th:first-child,
.reg-table td:first-child {
    text-align: left;
}
.reg-table th:not(:first-child),
.reg-table td:not(:first-child) {
    text-align: right;
}
</style>
"""

html_table = table_df.to_html(
    index=False,
    classes="reg-table",
    border=0,
    escape=False
)

st.markdown(css + f'<div class="reg-table-container">{html_table}</div>', unsafe_allow_html=True)

# ----------------- Meta-információk -----------------
st.markdown(f"**R²:** {res.rsquared:.3f}")

note = "Zárójelben a robusztus (HC1) standard hibák szerepelnek."

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
    f"<span style='font-size:0.85rem'><em>Megjegyzés:</em> {note} "
    "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
    unsafe_allow_html=True,
)

st.markdown(
    f"<span style='font-size:0.85rem'><em>Kategóriák referencia-szintjei (drop-first):</em> {baselines_text}</span>",
    unsafe_allow_html=True,
)
