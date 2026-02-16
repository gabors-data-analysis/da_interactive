import streamlit as st
import pandas as pd
import numpy as np
import statsmodels.api as sm
import utils
import matplotlib.pyplot as plt
import seaborn as sns
try:
    from sklearn.metrics import roc_auc_score
except ImportError:
    roc_auc_score = None

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# ----------------------- Setup ------------------------
col_settings, col_viz = utils.setup_page(
    "Klasszifikáció — 2019 keresztmetszet",
    "Klasszifikáció — 2019 keresztmetszet (szimulált)"
)
df = utils.load_cross_section(st.session_state['data_path'])

# Ensure target variable exists or map it
if "has_grant_2020" not in df.columns:
    st.error("A 'has_grant_2020' (célváltozó) nem található az adatban.")
    st.stop()

target_var = "has_grant_2020"

# ------------------------------------------------------
# Változó-label szótárak
# ------------------------------------------------------
MONETARY_VARS = utils.get_monetary_vars()
NON_MONETARY_VARS = utils.NON_MONETARY_VARS.copy()

VAR_LABELS_BY_COL = {}
for label, col in {**MONETARY_VARS, **NON_MONETARY_VARS}.items():
    VAR_LABELS_BY_COL[col] = label

VAR_LABELS_BY_COL.update({
    "emp_size": "Foglalkoztatási méretkategória",
    "firm_owner": "Tulajdonosi forma",
    "has_export": "Exportőr",
    "has_grant": "Kapott támogatást",
    target_var: "Kapott támogatást (2020)",
    "ln_sales": "Log értékesítés"
})

def col_to_label(col: str) -> str:
    return VAR_LABELS_BY_COL.get(col, col.replace("_", " "))

# ------------------------------------------------------
# CONFIG LISTS
# ------------------------------------------------------
TRANSFORM_VARS = ['sales_clean', 'tanass_clean', 'emp', 'age']
LOG_VARS = ['sales_clean', 'tanass_clean', 'emp', 'age']
POTENTIAL_X_VARS = [
    'emp', 'log_emp', 'sales_clean', 'sales_clean_sq',
    'tanass_clean', 'tanass_clean_sq', 'emp_sq', 'age', 'age_sq',
    'firm_owner', 'emp_size', 'has_export',
    'log_sales_clean', 'log_tanass_clean', 'log_age'
]

# ------------------------------------------------------
# Main text
# ------------------------------------------------------


# ------------------------------------------------------
# Beállítások (Bal oldal)
# ------------------------------------------------------
with col_settings:
    st.header("Beállítások")

    sync_on = utils.render_sync_option(st)

    lab_df = pd.DataFrame({"label": df["nace2_name_code"].dropna().unique()})
    lab_df["__code"] = pd.to_numeric(
        lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
        errors="coerce"
    )
    lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
    industry_opts = ["Összes ágazat"] + lab_df["label"].tolist()
    
    ind_idx = utils.get_synced_index(industry_opts, "global_industry")
    sel_industry = st.selectbox("Ágazat", industry_opts, index=ind_idx)
    utils.update_synced_state("global_industry", sel_industry)

    # Model Selection
    m_opts = ["LPM", "Logit", "LPM és Logit"]
    model_choice = st.radio(
        "Modell típus", 
        m_opts, 
        index=m_opts.index(persist("p10_model_type", "LPM és Logit")),
        key="_p10_model_type", on_change=save, args=("p10_model_type",)
    )

if sel_industry == "Összes ágazat":
    d = df.copy()
else:
    d = df[df["nace2_name_code"] == sel_industry].copy()

# --- Preprocessing ---
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

# Monetary scaling
for col in MONETARY_VARS.values():
    if col in d.columns:
        d[col] = d[col] / 1000.0

# Transformations
for v in TRANSFORM_VARS:
    if v not in d.columns: continue
    s = pd.to_numeric(d[v], errors="coerce")
    base_label = VAR_LABELS_BY_COL.get(v, v.replace("_", " "))
    sq_name = f"{v}_sq"
    if sq_name not in d.columns:
        d[sq_name] = s ** 2
        VAR_LABELS_BY_COL[sq_name] = f"{base_label}²"

for v in LOG_VARS:
    if v not in d.columns: continue
    s = pd.to_numeric(d[v], errors="coerce")
    base_label = VAR_LABELS_BY_COL.get(v, v.replace("_", " "))
    log_name = f"log_{v}"
    if log_name not in d.columns:
        with np.errstate(divide='ignore', invalid='ignore'):
            log_s = np.where(s > 0, np.log(s), np.nan)
        d[log_name] = log_s
        VAR_LABELS_BY_COL[log_name] = f"log({base_label})"

# ------------------------------------------------------
# X Selection
# ------------------------------------------------------
disallowed_x = {target_var, "has_grant", "has_grant_2020", "grant_value"} 

candidate_cols = []
for c in POTENTIAL_X_VARS:
    if c in disallowed_x: continue
    if c in d.columns:
        candidate_cols.append(c)

categorical_cols, numeric_cols = [], []
for c in candidate_cols:
    s = d[c]
    nun = s.nunique(dropna=True)
    if (pd.api.types.is_bool_dtype(s) or s.dtype == "object" or pd.api.types.is_categorical_dtype(s) or nun <= 10):
        categorical_cols.append(c)
    elif pd.api.types.is_numeric_dtype(s):
        numeric_cols.append(c)

num_label_to_col = {col_to_label(c): c for c in numeric_cols}
cat_label_to_col = {col_to_label(c): c for c in categorical_cols}

with col_settings:
    with st.expander("Magyarázó változók", expanded=True):
        cont_labels = st.multiselect("Folytonos változók", options=sorted(num_label_to_col.keys()), default=persist("p10_cont", []), key="_p10_cont", on_change=save, args=("p10_cont",))
        cat_labels = st.multiselect("Kategorikus változók", options=sorted(cat_label_to_col.keys()), default=persist("p10_cat", []), key="_p10_cat", on_change=save, args=("p10_cat",))

cont_vars = [num_label_to_col[l] for l in cont_labels]
cat_vars = [cat_label_to_col[l] for l in cat_labels]

rhs_cols = cont_vars + cat_vars
if not rhs_cols:
    st.info("Válasszon legalább egy magyarázó változót.")
    st.stop()

# ------------------------------------------------------
# Splines
# ------------------------------------------------------
spline_specs = {}
with col_settings:
    with st.expander("Lineáris spline beállítások", expanded=False):
        for v in cont_vars:
            label = col_to_label(v)
            use_spline = st.checkbox(f"{label} lineáris spline", value=False, key=f"spline_{v}")
            if use_spline:
                series = pd.to_numeric(d[v], errors="coerce").dropna()
                if len(series) < 5: continue
                
                n_knots = st.selectbox(f"Határpontok száma – {label}", [1, 2], key=f"nk_{v}")
                q1 = float(series.quantile(1/3))
                k1 = st.number_input(f"{label} 1. határ", value=q1, key=f"k1_{v}")
                knots = [k1]
                if n_knots == 2:
                    q2 = float(series.quantile(2/3))
                    k2 = st.number_input(f"{label} 2. határ", value=q2, key=f"k2_{v}")
                    knots.append(k2)
                spline_specs[v] = sorted(knots)
                
                # Labels
                base_label = col_to_label(v)
                for i in range(len(knots) + 1):
                    desc = f"≤ {knots[0]:.2f}" if i==0 else (f"> {knots[-1]:.2f}" if i==len(knots) else f"({knots[i-1]:.2f}, {knots[i]:.2f}]")
                    VAR_LABELS_BY_COL[f"{v}_spline_{i+1}"] = f"{base_label} spline {i+1} ({desc})"

# ------------------------------------------------------
# Outlier Handling
# ------------------------------------------------------
with col_settings:
    with st.expander("Szélsőérték-kezelés (X-ek)", expanded=False):
        x_filters = {}
        for lbl in cont_labels:
            col = num_label_to_col[lbl]
            mode = st.selectbox(f"{lbl} kezelése", utils.FILTER_OPTIONS, index=0, key=f"filt_{col}")
            low, high = None, None
            if mode == "Kézi minimum/maximum":
                series = pd.to_numeric(d[col], errors="coerce").dropna()
                cmin, cmax = float(series.min()), float(series.max())
                low = st.number_input(f"{lbl} min", value=cmin, key=f"min_{col}")
                high = st.number_input(f"{lbl} max", value=cmax, key=f"max_{col}")
            x_filters[col] = (mode, low, high)

# ------------------------------------------------------
# Build Data
# ------------------------------------------------------
dwork = d.copy()
dwork["__y__"] = pd.to_numeric(dwork[target_var], errors="coerce")
dwork = dwork.dropna(subset=["__y__"] + rhs_cols)

# Apply filters
for col, (mode, low, high) in x_filters.items():
    dwork = dwork.loc[utils.apply_filter(dwork[col], mode, low, high).index]

if dwork.empty:
    st.error("Nincs elegendő adat.")
    st.stop()

Y = dwork["__y__"]

# Build X
X_parts = []
for v in cont_vars:
    x_raw = dwork[v].astype(float)
    knots = spline_specs.get(v, [])
    if knots:
        spline_mat = utils.lspline(x_raw, knots)
        cols = [f"{v}_spline_{i+1}" for i in range(spline_mat.shape[1])]
        X_parts.append(pd.DataFrame(spline_mat, index=dwork.index, columns=cols))
    else:
        X_parts.append(x_raw.rename(v))

for v in cat_vars:
    X_parts.append(pd.get_dummies(dwork[v].astype("category"), prefix=v, drop_first=True, dtype=float))

X = pd.concat(X_parts, axis=1)
X = sm.add_constant(X)

# ------------------------------------------------------
# Fit Models
# ------------------------------------------------------

# 1. LPM (OLS)
res_lpm = None
if "LPM" in model_choice:
    model_lpm = sm.OLS(Y, X)
    res_lpm = model_lpm.fit(cov_type="HC1")

# 2. Logit
res_logit = None
logit_margeff_df = None

if "Logit" in model_choice:
    model_logit = sm.Logit(Y, X)
    try:
        res_logit = model_logit.fit(disp=0)
        
        # Calculate Logit AME
        margeff = res_logit.get_margeff(at='overall')
        summary = margeff.summary_frame()
        
        # Defensively find the correct column names from the summary frame
        effect_col = next((c for c in summary.columns if 'dy/dx' in c), summary.columns[0])
        se_col = next((c for c in summary.columns if 'Std. Err' in c), summary.columns[1])
        p_col = next((c for c in summary.columns if 'P>|z|' in c), summary.columns[3])

        logit_margeff_df = pd.DataFrame({
            'ame': summary[effect_col],
            'ame_se': summary[se_col],
            'ame_p': summary[p_col]
        }, index=summary.index)
        
    except Exception as e:
        st.warning(f"Logit modell figyelmeztetés: {e}")

# ------------------------------------------------------
# Output
# ------------------------------------------------------
with col_viz:
    st.subheader("Regressziós eredmények")
    
    def sig_stars(p):
        if p < 0.01: return "***"
        elif p < 0.05: return "**"
        elif p < 0.1: return "*"
        return ""

    def fmt(x):
        try:
            if x != 0 and abs(x) < 0.001:
                return f"{x:.3g}"
            return f"{x:.3f}"
        except Exception:
            return str(x)

    def prettify(name):
        if name == "const": return "Konstans"
        for cat in cat_vars:
            if name.startswith(f"{cat}_"):
                return f"{col_to_label(cat)} = {name[len(cat)+1:]}"
        return VAR_LABELS_BY_COL.get(name, name)

    # Build comparison table
    summary_rows = []
    
    # Determine which params to iterate (use whichever model is available)
    params_index = res_lpm.params.index if res_lpm is not None else (res_logit.params.index if res_logit is not None else [])

    for name in params_index:
        row_vals = {"Változó": prettify(name)}
        row_se = {"Változó": ""}

        # LPM Columns
        if res_lpm is not None:
            lpm_val = res_lpm.params[name]
            lpm_se = res_lpm.bse[name]
            lpm_p = res_lpm.pvalues[name]
            
            val_str = f"{fmt(lpm_val)}{sig_stars(lpm_p)}"
            se_str = f"({fmt(lpm_se)})"
            
            row_vals["LPM (Együttható)"] = val_str
            row_se["LPM (Együttható)"] = se_str
            
            # LPM AME is same as Coeff
            row_vals["LPM (AME)"] = val_str
            row_se["LPM (AME)"] = se_str

        # Logit Columns
        if res_logit is not None:
            # Coeffs
            logit_val = res_logit.params[name]
            try:
                logit_se = res_logit.bse[name]
                logit_p = res_logit.pvalues[name]
            except:
                logit_se = np.nan
                logit_p = np.nan
            
            row_vals["Logit (Együttható)"] = f"{fmt(logit_val)}{sig_stars(logit_p)}"
            row_se["Logit (Együttható)"] = f"({fmt(logit_se)})"

            # AME
            if logit_margeff_df is not None and name in logit_margeff_df.index:
                r = logit_margeff_df.loc[name]
                row_vals["Logit (AME)"] = f"{fmt(r['ame'])}{sig_stars(r['ame_p'])}"
                row_se["Logit (AME)"] = f"({fmt(r['ame_se'])})"
            else:
                row_vals["Logit (AME)"] = "—"
                row_se["Logit (AME)"] = ""

        summary_rows.append(row_vals)
        summary_rows.append(row_se)

    summary_df = pd.DataFrame(summary_rows)
    
    # Stats
    footer_vals = {"Változó": "Brier-score"}
    obs_vals = {"Változó": "Megfigyelések"}
    sep_vals = {"Változó": "-------"}

    lpm_brier = np.nan
    if res_lpm is not None:
        lpm_pred = res_lpm.predict()
        lpm_brier = np.mean((lpm_pred - Y) ** 2)
        for col in ["LPM (Együttható)", "LPM (AME)"]:
            footer_vals[col] = f"{lpm_brier:.3f}"
            obs_vals[col] = f"{int(res_lpm.nobs):,}".replace(",", " ")
            sep_vals[col] = "-------"

    logit_brier = np.nan
    if res_logit is not None:
        logit_pred = res_logit.predict()
        logit_brier = np.mean((logit_pred - Y) ** 2)
        for col in ["Logit (Együttható)", "Logit (AME)"]:
            footer_vals[col] = f"{logit_brier:.3f}"
            obs_vals[col] = f"{int(res_logit.nobs):,}".replace(",", " ")
            sep_vals[col] = "-------"

    summary_df = pd.concat([summary_df, pd.DataFrame([sep_vals, obs_vals, footer_vals])], ignore_index=True)
    final_table = summary_df.set_index("Változó")

    # Display table
    # Adjust column width based on number of columns
    n_cols = len(final_table.columns)
    width_ratio = 6 if n_cols > 2 else 4
    left_spacer, mid_col, right_spacer = st.columns([1, width_ratio, 1])
    
    with mid_col:
        st.table(final_table, border=False)
    
    st.markdown(
        "<span style='font-size:0.85rem'><em>Megjegyzés:</em> Zárójelben a standard hibák. "
        "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
        unsafe_allow_html=True,
    )

# ------------------------------------------------------
# Confusion Matrix
# ------------------------------------------------------
with col_viz:
    st.markdown("---")
    st.subheader("Klasszifikációs teljesítmény (Confusion Matrix)")

    def plot_confusion_matrix(y_true, y_prob, model_name, ax):
        # Predictions
        y_pred = (y_prob >= threshold).astype(int)
        
        # Calculate counts
        tp = ((y_true == 1) & (y_pred == 1)).sum()
        tn = ((y_true == 0) & (y_pred == 0)).sum()
        fp = ((y_true == 0) & (y_pred == 1)).sum()
        fn = ((y_true == 1) & (y_pred == 0)).sum()
        
        # Matrix for heatmap
        cm = np.array([[tn, fp], [fn, tp]])
        
        # Labels
        group_names = ['Helyes Negatív','Álpozitív','Álnegatív','Helyes Pozitív']
        group_counts = ["{0:0.0f}".format(value) for value in cm.flatten()]
        group_percentages = ["{0:.2%}".format(value) for value in cm.flatten()/np.sum(cm)]
        
        labels = [f"{v1}\n{v2}\n{v3}" for v1, v2, v3 in zip(group_names, group_counts, group_percentages)]
        labels = np.asarray(labels).reshape(2,2)
        
        sns.heatmap(cm, annot=labels, fmt='', cmap='Blues', cbar=False, ax=ax)
        ax.set_title(f"{model_name} (Küszöb: {threshold:.2f})")
        ax.set_xlabel('Előrejelzett')
        ax.set_ylabel('Tény')
        ax.set_xticklabels(['0', '1'])
        ax.set_yticklabels(['0', '1'])

    available_models = {}
    if res_lpm is not None:
        available_models["LPM"] = res_lpm.predict()
    if res_logit is not None:
        available_models["Logit"] = res_logit.predict()

    if available_models:
        cm_model_choice = list(available_models.keys())[0]
        if len(available_models) > 1:
            cm_model_choice = st.radio("Válasszon modellt a mátrixhoz", list(available_models.keys()), horizontal=True)
        
        preds = available_models[cm_model_choice]

        # AUC
        if roc_auc_score is not None:
            try:
                auc_val = roc_auc_score(Y, preds)
                st.markdown(f"**AUC:** {auc_val:.4f}")
            except Exception as e:
                st.warning(f"Hiba az AUC számításánál: {e}")

        # Cost inputs & Optimal Threshold
        c1, c2 = st.columns(2)
        cost_fp = c1.number_input("Téves pozitív (FP) költsége", min_value=0.1, value=1.0, step=0.1)
        cost_fn = c2.number_input("Téves negatív (FN) költsége", min_value=0.1, value=1.0, step=0.1)
        
        opt_threshold = cost_fp / (cost_fp + cost_fn)
        st.info(f"Optimális küszöbérték (költségek alapján): {opt_threshold:.4f}")

        threshold = st.slider(
            "Klasszifikációs küszöbérték",
            min_value=0.0,
            max_value=1.0,
            value=float(opt_threshold),
            step=0.0001,
            format="%.4f",
            key=f"threshold_slider_{cost_fp}_{cost_fn}"
        )

        fig, ax = plt.subplots(figsize=(5, 4))
        plot_confusion_matrix(Y, preds, cm_model_choice, ax)
        st.pyplot(fig)

        # Calculate metrics
        y_pred = (preds >= threshold).astype(int)
        tp = ((Y == 1) & (y_pred == 1)).sum()
        tn = ((Y == 0) & (y_pred == 0)).sum()
        fp = ((Y == 0) & (y_pred == 1)).sum()
        fn = ((Y == 1) & (y_pred == 0)).sum()
        
        accuracy = (tp + tn) / len(Y) if len(Y) > 0 else 0
        recall = tp / (tp + fn) if (tp + fn) > 0 else 0
        precision = tp / (tp + fp) if (tp + fp) > 0 else 0
        specificity = tn / (tn + fp) if (tn + fp) > 0 else 0

        st.markdown("#### Metrikák")
        m1, m2, m3, m4 = st.columns(4)
        m1.metric("Pontosság (Accuracy)", f"{accuracy:.3f}")
        m2.metric("Érzékenység (Recall)", f"{recall:.3f}")
        m3.metric("Precizitás (Precision)", f"{precision:.3f}")
        m4.metric("Specificitás (Specificity)", f"{specificity:.3f}")