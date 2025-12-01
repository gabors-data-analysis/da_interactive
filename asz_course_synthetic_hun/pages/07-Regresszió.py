import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
import statsmodels.api as sm
from typing import List  # <-- added for lspline type hints

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
def load_cross_section(path: str = st.session_state['data_path']) -> pd.DataFrame:
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

    reg_outcomes = {"sales_growth_perc","sales_growth_log_diff"}
    missing = reg_outcomes - set(df.columns)
    if missing:
        df["sales_growth_perc"] = (df["sales_lead_sim"] - df["sales_clean"]) / df["sales_clean"] * 100
        df["sales_growth_log_diff"] = df["ln_sales_lead_sim"] - df["ln_sales"]
    
    else:
        df["sales_growth_perc"] = df["sales_growth_perc"] * 100

    if "sales_clean" in df.columns:
        df["ln_sales"] = np.log(np.clip(df["sales_clean"].astype(float), 1e-9, None))

    return df


df = load_cross_section(st.session_state['data_path'])

# ------------------------------------------------------
# Változó-label szótárak
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

VAR_LABELS_BY_COL = {}
for label, col in {**MONETARY_VARS, **NON_MONETARY_VARS}.items():
    VAR_LABELS_BY_COL[col] = label

VAR_LABELS_BY_COL.update({
    "emp_size": "Foglalkoztatási méretkategória",
    "firm_owner": "Tulajdonosi forma",
    "has_export": "Exportőr",
    "has_grant": "Kapott támogatást",
    "ln_sales": "Log értékesítés"
})


def col_to_label(col: str) -> str:
    return VAR_LABELS_BY_COL.get(col, col.replace("_", " "))


# ------------------------------------------------------
# >>> CONFIG LISTS YOU CAN EDIT <<<
# ------------------------------------------------------

# List of base variables for which you want SQUARED versions.
TRANSFORM_VARS = [
    'sales_clean',
    'tanass_clean',
    'emp',
    'age',
]

# List of base variables for which you want LOG versions.
LOG_VARS = [
    'sales_clean',
    'tanass_clean',
    'emp',
    'age',
]

# List of ALL potential X variables (originals, squared versions, categorical, etc.)
POTENTIAL_X_VARS = [
    'emp',
    'log_emp',
    'sales_clean',
    'sales_clean_sq',
    'tanass_clean',
    'tanass_clean_sq',
    'emp',
    'emp_sq',
    'age',
    'age_sq',
    'firm_owner',
    'emp_size',
    'has_export',
    'has_grant',
    'log_sales_clean',
    'log_tanass_clean',
    'log_age'
]

# ------------------------------------------------------
# Main text
# ------------------------------------------------------
st.markdown(
    """
    Az adatok forrása **OPTEN**.  
    Minden ábra és adat oktatási céllal készült és tájékoztató jellegű.  
    """
)
st.markdown(
    """
Válasszon **ágazatot**, **kimeneti változót** és **magyarázó változókat**.

Lehetséges kimenetek:

- **Relatív növekedés**: (Árbevétel_2021 - Árbevétel_2019) / Árbevétel_2019  
- **Lognövekedés**: ln(Árbevétel_2021) − ln(Árbevétel_2019)
- **Árbevétel**
- **ln(Árbevétel)**
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
    d["emp_size"] = pd.Series(pd.Categorical([np.nan] * len(d)))

# ------------------------------------------------------
# Kimenetek (Y) – growth block as before
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


# ---- EXTRA: add sales and log sales as additional Y options ----
extra_y_labels = []

if "sales_clean" in d.columns:
    lab_sales = "Értékesítés (millió Ft)"
    extra_y_labels.append(lab_sales)
    label_to_col[lab_sales] = "sales_clean"

if "ln_sales" in d.columns:
    lab_lnsales = "Log értékesítés"
    extra_y_labels.append(lab_lnsales)
    label_to_col[lab_lnsales] = "ln_sales"

available = list(available) + extra_y_labels

outcome_choice = st.sidebar.selectbox("Kimenet", available, index=0)
y_col = label_to_col[outcome_choice]

# ------------------------------------------------------
# *** NEW: pénzügyiek ezres megjelenítés (millió Ft -> /1000)
# Apply to ALL monetary variables in d, including Y if monetary
# ------------------------------------------------------
for col in MONETARY_VARS.values():
    if col in d.columns:
        d[col] = d[col] / 1000.0

# Now build Y from the (possibly rescaled) d
y = d[y_col].astype(float)

# ------------------------------------------------------
# Négyzet + log transzformációk
# ------------------------------------------------------
for v in TRANSFORM_VARS:
    if v not in d.columns:
        continue
    s = pd.to_numeric(d[v], errors="coerce")
    base_label = VAR_LABELS_BY_COL.get(v, v.replace("_", " "))
    sq_name = f"{v}_sq"
    if sq_name not in d.columns:
        d[sq_name] = s ** 2
        VAR_LABELS_BY_COL[sq_name] = f"{base_label}²"

for v in LOG_VARS:
    if v not in d.columns:
        continue
    s = pd.to_numeric(d[v], errors="coerce")
    base_label = VAR_LABELS_BY_COL.get(v, v.replace("_", " "))
    log_name = f"log_{v}"
    if log_name not in d.columns:
        with np.errstate(divide='ignore', invalid='ignore'):
            log_s = np.where(s > 0, np.log(s), np.nan)
        d[log_name] = log_s
        VAR_LABELS_BY_COL[log_name] = f"log({base_label})"

# ------------------------------------------------------
# Jelölhető X-változók csak a POTENTIAL_X_VARS listából
# + tiltsuk le a Y-t és log(Y)-t
# ------------------------------------------------------
is_num = pd.api.types.is_numeric_dtype
is_bool = pd.api.types.is_bool_dtype

disallowed_x = {y_col}

log_of_y = f"log_{y_col}"
if log_of_y in d.columns:
    disallowed_x.add(log_of_y)

if y_col.startswith("log_"):
    base_y = y_col[4:]
    if base_y in d.columns:
        disallowed_x.add(base_y)

if y_col in ("sales_clean", "ln_sales"):
    for v in ("sales_clean", "ln_sales", "log_sales_clean"):
        if v in d.columns:
            disallowed_x.add(v)

candidate_cols = []
for c in POTENTIAL_X_VARS:
    if c in disallowed_x:
        continue
    if c in d.columns:
        candidate_cols.append(c)

categorical_cols, numeric_cols = [], []
for c in candidate_cols:
    s = d[c]
    nun = s.nunique(dropna=True)
    if (is_bool(s) or s.dtype == "object" or pd.api.types.is_categorical_dtype(s) or nun <= 10):
        categorical_cols.append(c)
    elif is_num(s):
        numeric_cols.append(c)

num_label_to_col = {col_to_label(c): c for c in numeric_cols}
cat_label_to_col = {col_to_label(c): c for c in categorical_cols}

# ------------------------------------------------------
# Modelleket építő UI (Modell 1 & Modell 2)
# ------------------------------------------------------
st.sidebar.subheader("Magyarázó változók — Modell 1")
cont_labels_1 = st.sidebar.multiselect(
    "Folytonos magyarázó változók – Modell 1",
    options=sorted(num_label_to_col.keys()),
    key="cont_m1"
)
cat_labels_1 = st.sidebar.multiselect(
    "Kategorikus magyarázó változók – Modell 1",
    options=sorted(cat_label_to_col.keys()),
    key="cat_m1"
)
cont_vars_1 = [num_label_to_col[l] for l in cont_labels_1]
cat_vars_1 = [cat_label_to_col[l] for l in cat_labels_1]

st.sidebar.subheader("Magyarázó változók — Modell 2")
cont_labels_2 = st.sidebar.multiselect(
    "Folytonos magyarázó változók – Modell 2",
    options=sorted(num_label_to_col.keys()),
    key="cont_m2"
)
cat_labels_2 = st.sidebar.multiselect(
    "Kategorikus magyarázó változók — Modell 2",
    options=sorted(cat_label_to_col.keys()),
    key="cat_m2"
)
cont_vars_2 = [num_label_to_col[l] for l in cont_labels_2]
cat_vars_2 = [cat_label_to_col[l] for l in cat_labels_2]

rhs_cols_all = cont_vars_1 + cat_vars_1 + cont_vars_2 + cat_vars_2
if not rhs_cols_all:
    st.info("Válasszon legalább egy magyarázó változót valamelyik modellhez a jobb oldalon.")
    st.stop()

# ------------------------------------------------------
# Lineáris spline beállítások (folytonos X-ek)
# ------------------------------------------------------
spline_specs = {}  # v -> list of knots (possibly empty)
spline_candidates = sorted(set(cont_vars_1) | set(cont_vars_2))

with st.sidebar.expander("Lineáris spline beállítások", expanded=False):
    if not spline_candidates:
        st.markdown("*Nincs kiválasztott folytonos magyarázó változó.*")
    for v in spline_candidates:
        label = col_to_label(v)
        use_spline = st.checkbox(f"{label} lineáris spline", value=False, key=f"spline_use__{v}")
        if not use_spline:
            continue

        series = pd.to_numeric(d[v], errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
        if len(series) < 5:
            st.warning(f"Nincs elég adat a spline-hoz: {label}")
            continue

        n_knots = st.selectbox(
            f"Határpontok száma – {label}",
            options=[1, 2],
            index=0,
            key=f"spline_nk__{v}"
        )

        knots = []
        q1 = float(series.quantile(1/3))
        q2 = float(series.quantile(2/3))

        k1 = st.number_input(
            f"{label} 1. határ",
            value=q1,
            key=f"spline_k1__{v}"
        )
        knots.append(k1)

        if n_knots == 2:
            k2 = st.number_input(
                f"{label} 2. határ",
                value=q2,
                key=f"spline_k2__{v}"
            )
            knots.append(k2)

        knots = sorted(knots)
        spline_specs[v] = knots

        # --- Label-ek a spline szegmensekhez (piecewise) ---
        base_label = col_to_label(v)
        n_segments = len(knots) + 1
        for i in range(n_segments):
            if i == 0:
                if len(knots) > 0:
                    desc = f"≤ {knots[0]:.2f}"
                else:
                    desc = ""
            elif i < len(knots):
                desc = f"({knots[i-1]:.2f}, {knots[i]:.2f}]"
            else:
                desc = f"> {knots[-1]:.2f}"
            VAR_LABELS_BY_COL[f"{v}_spline_{i+1}"] = f"{base_label} spline szakasz {i+1} ({desc})"

# ------------------------------------------------------
# Szélsőérték-kezelés (Y + választható folytonos X-ek)
# ------------------------------------------------------
FILTER_OPTIONS = [
    "Nincs szűrés",
    "Winsor top–bottom 2%",
    "Levágás top–bottom 2%",
    "Kézi minimum/maximum"
]

with st.sidebar.expander("Szélsőérték-kezelés (Y és X-ek)", expanded=False):
    # --- Y filter ---
    y_filter = st.selectbox("**Y szélsőérték-kezelése**", FILTER_OPTIONS, index=0)

    if y_filter == "Kézi minimum/maximum":
        st.markdown("**Y kézi határok (a kimenet egységében)**")
        y_clean = y.replace([np.inf, -np.inf], np.nan).dropna()
        if len(y_clean) > 0:
            current_min_y = float(np.nanmin(y_clean))
            current_max_y = float(np.nanmax(y_clean))
        else:
            current_min_y, current_max_y = 0.0, 1.0
        y_low_manual = st.number_input(
            "Y minimum",
            value=current_min_y,
            step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
        )
        y_high_manual = st.number_input(
            "Y maximum",
            value=current_max_y,
            step=(current_max_y - current_min_y)/100 if current_max_y > current_min_y else 1.0
        )
        if y_low_manual > y_high_manual:
            st.error("Y esetén a minimum nem lehet nagyobb a maximumnál.")
            y_low_manual, y_high_manual = y_high_manual, y_low_manual
    else:
        y_low_manual = None
        y_high_manual = None

    # --- X filters: user chooses which continuous vars to filter ---
    st.markdown("**X-ek szűrése:**")

    x_filterable_labels = sorted(num_label_to_col.keys())
    x_filter_labels_selected = st.multiselect(
        "Változók a szűréshez",
        options=x_filterable_labels,
        key="x_filter_vars"
    )

    x_filters = {}  # col_name -> (mode, low, high)
    for lbl in x_filter_labels_selected:
        col = num_label_to_col[lbl]
        mode = st.selectbox(
            f"{lbl} szélsőérték-kezelése",
            FILTER_OPTIONS,
            index=0,
            key=f"x_filter_mode__{col}"
        )

        low_val = high_val = None
        if mode == "Kézi minimum/maximum":
            series = pd.to_numeric(d[col], errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
            if len(series) > 0:
                cur_min = float(np.nanmin(series))
                cur_max = float(np.nanmax(series))
            else:
                cur_min, cur_max = 0.0, 1.0
            st.markdown(f"**{lbl} kézi határok**")
            low_val = st.number_input(
                f"{lbl} minimum",
                value=cur_min,
                step=(cur_max - cur_min)/100 if cur_max > cur_min else 1.0,
                key=f"x_filter_low__{col}"
            )
            high_val = st.number_input(
                f"{lbl} maximum",
                value=cur_max,
                step=(cur_max - cur_min)/100 if cur_max > cur_min else 1.0,
                key=f"x_filter_high__{col}"
            )
            if low_val > high_val:
                st.error(f"{lbl} esetén a minimum nem lehet nagyobb a maximumnál.")
                low_val, high_val = high_val, low_val

        x_filters[col] = (mode, low_val, high_val)

if "y_filter" not in locals():
    y_filter = "Nincs szűrés"
    y_low_manual = y_high_manual = None
if "x_filters" not in locals():
    x_filters = {}

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

    if mode == "Kézi minimum/maximum":
        if low_val is None or high_val is None:
            return s
        return s[(s > low_val) & (s < high_val)]

    return s

# ------------------------------------------------------
# Linear spline helper: piecewise design matrix (lspline)
# ------------------------------------------------------
def lspline(series: pd.Series, knots: List[float]) -> np.ndarray:
    """
    Generate a linear spline design matrix for the input series based on knots.
    Piecewise form: coefficients correspond to segment-specific slopes.

    Parameters
    ----------
    series : pd.Series
        The input series to generate the design matrix for.
    knots : List[float]
        The list of knots to use for the linear spline.

    Returns
    -------
    np.ndarray
        The design matrix for the linear spline with shape (n, len(knots)+1).
    """
    vector = series.values.astype(float)
    columns = []

    for i, knot in enumerate(knots):
        column = np.minimum(vector, knot if i == 0 else knot - knots[i - 1])
        columns.append(column)
        vector = vector - column

    # Add the remainder as the last column
    columns.append(vector)

    return np.column_stack(columns)

# ------------------------------------------------------
# Munkatábla + szűrés
# ------------------------------------------------------
needed_cols = set(rhs_cols_all) | {"__y__"} | set(x_filters.keys())

dwork = d.copy()
dwork["__y__"] = y
dwork = dwork[list(needed_cols)].replace([np.inf, -np.inf], np.nan).dropna()

if dwork.empty:
    st.error("Nem maradt megfigyelés a kimenet/magyarázók hiányzóinak eldobása után.")
    st.stop()

y_filtered = apply_filter(dwork["__y__"], y_filter, y_low_manual, y_high_manual)
if y_filtered.empty:
    st.error("A kimenet (Y) szélsőérték-kezelése után nem maradt megfigyelés.")
    st.stop()
idx_keep = y_filtered.index

for col, (mode, low_val, high_val) in x_filters.items():
    if col not in dwork.columns:
        continue
    x_series = pd.to_numeric(dwork.loc[idx_keep, col], errors="coerce")
    x_filtered = apply_filter(x_series, mode, low_val, high_val)
    if x_filtered.empty:
        st.error(f"A(z) `{col_to_label(col)}` változó szélsőérték-kezelése után nem maradt megfigyelés.")
        st.stop()
    idx_keep = idx_keep.intersection(x_filtered.index)

dwork = dwork.loc[idx_keep].copy()
dwork["__y__"] = y_filtered.loc[idx_keep]

if dwork.empty:
    st.error("A szélsőérték-kezelés után nem maradt elegendő megfigyelés.")
    st.stop()

Y_full = pd.to_numeric(dwork["__y__"], errors="coerce").replace([np.inf, -np.inf], np.nan)

# ------------------------------------------------------
# Dizájnmátrix-építő, modellek becslése (spline_specs-szel)
# ------------------------------------------------------
def build_design_matrix(dwork: pd.DataFrame,
                        cont_vars,
                        cat_vars,
                        spline_specs: dict):
    X_parts = []
    dummy_cache = {}

    for v in cont_vars:
        x_raw = pd.to_numeric(dwork[v], errors="coerce").astype(float)

        knots = spline_specs.get(v, [])
        if knots:
            # Proper piecewise linear spline expansion using lspline
            spline_mat = lspline(x_raw, knots)  # shape: (n, len(knots)+1)
            col_names = [f"{v}_spline_{i+1}" for i in range(spline_mat.shape[1])]
            spline_df = pd.DataFrame(spline_mat, index=dwork.index, columns=col_names)
            X_parts.append(spline_df)
        else:
            # No spline: just the raw variable
            X_parts.append(x_raw.rename(v))

    for v in cat_vars:
        dummies = pd.get_dummies(
            dwork[v].astype("category"),
            prefix=v,
            drop_first=True,
            dtype=float
        )
        dummy_cache[v] = dummies
        X_parts.append(dummies)

    if not X_parts:
        return None

    X = pd.concat(X_parts, axis=1)
    X = X.apply(pd.to_numeric, errors="coerce").replace([np.inf, -np.inf], np.nan)
    return X


def fit_model(X: pd.DataFrame, Y: pd.Series):
    if X is None:
        return None
    valid = X.notnull().all(axis=1) & Y.notnull()
    X_valid = X.loc[valid]
    Y_valid = Y.loc[valid]
    if X_valid.shape[0] < 5 or X_valid.shape[1] == 0:
        return None
    X_valid = sm.add_constant(X_valid, has_constant="add")
    model = sm.OLS(Y_valid.astype(float), X_valid.astype(float))
    res = model.fit(cov_type="HC1")
    return res


X1 = build_design_matrix(dwork, cont_vars_1, cat_vars_1, spline_specs)
X2 = build_design_matrix(dwork, cont_vars_2, cat_vars_2, spline_specs)

res1 = fit_model(X1, Y_full)
res2 = fit_model(X2, Y_full)

if (res1 is None) and (res2 is None):
    st.error("Egyik modellben sincs elegendő felhasználható adat.")
    st.stop()

# ------------------------------------------------------
# Eredmények táblázata – CPS-stílus, keskeny, középre igazítva
# ------------------------------------------------------
st.subheader("Regressziós eredmények (Modell 1 vs. Modell 2)")

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

all_cat_vars_in_models = set(cat_vars_1) | set(cat_vars_2)

def prettify_name(name: str) -> str:
    if name == "const":
        return "Konstans"
    if " × " in name:
        left, right = name.split(" × ", 1)
        return f"{prettify_name(left)} × {prettify_name(right)}"
    for cat in all_cat_vars_in_models:
        prefix = f"{cat}_"
        if name.startswith(prefix):
            level = name[len(prefix):]
            var_label = col_to_label(cat)
            return f"{var_label} = {level}"
    if name in VAR_LABELS_BY_COL:
        return VAR_LABELS_BY_COL[name]
    return name.replace("_", " ")

def build_summary_table(models, model_labels):
    summary_tables = []
    used_labels = []

    for reg, label in zip(models, model_labels):
        if reg is None:
            continue

        df = pd.DataFrame({
            "coef": reg.params,
            "se": reg.bse,
            "p": reg.pvalues
        })
        df["summary"] = df.apply(
            lambda row: f"{fmt_coef(row['coef'])}{sig_stars(row['p'])} ({fmt_se(row['se'])})",
            axis=1
        )

        df.index = [prettify_name(idx) for idx in df.index]

        extra = pd.DataFrame(
            {"summary": ["-------", f"{int(reg.nobs):,}".replace(",", " "), f"{reg.rsquared:.3f}"]},
            index=["-------", "Megfigyelések", "R²"]
        )
        df = pd.concat([df[["summary"]], extra])

        summary_tables.append(df)
        used_labels.append(label)

    if not summary_tables:
        return None

    combined = pd.concat(summary_tables, axis=1)
    combined.columns = used_labels

    rows = list(combined.index)
    ordered = []
    if "Konstans" in rows:
        ordered.append("Konstans")
    for r in sorted(r for r in rows if r not in ["Konstans", "Megfigyelések", "R²", "-------"]):
        ordered.append(r)
    if "-------" in rows:
        ordered.append("-------")
    if "Megfigyelések" in rows:
        ordered.append("Megfigyelések")
    if "R²" in rows:
        ordered.append("R²")

    return combined.reindex(ordered)

summary_table = build_summary_table([res1, res2], ["Modell 1", "Modell 2"])

if summary_table is not None:
    left_spacer, mid_col, right_spacer = st.columns([1, 3, 1])
    with mid_col:
        st.table(summary_table, border=False)
else:
    st.error("Nem sikerült regressziós összefoglalót készíteni.")

note = "Zárójelben a robusztus (HC1) standard hibák szerepelnek."

st.markdown(
    f"<span style='font-size:0.85rem'><em>Megjegyzés:</em> {note} "
    "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
    unsafe_allow_html=True,
)
