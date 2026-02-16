import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter
import utils
import statsmodels.api as sm

try:
    from arch.unitroot import PhillipsPerron
    HAS_ARCH = True
except ImportError:
    HAS_ARCH = False

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# -----------------------------------------------------------------------------
# Page Configuration
# -----------------------------------------------------------------------------
col_settings, col_viz = utils.setup_page(
    "Idősoros Elemzés",
    "Idősoros Elemzés"
)

# -----------------------------------------------------------------------------
# Data Loading & Preprocessing
# -----------------------------------------------------------------------------
if 'ts_data' not in st.session_state:
    st.warning("⚠️ Az idősoros adatfájl útvonala (ts_data) nincs beállítva. Kérlek, töltsd be az adatokat a főoldalon!")
    st.stop()

@st.cache_data
def load_and_prep_data(path):
    """Loads data and performs initial date parsing."""
    # utils.load_data doesn't exist in the provided utils.py, using read_parquet directly
    try:
        df = pd.read_parquet(path)
    except Exception:
        st.error(f"Nem sikerült betölteni a fájlt: {path}")
        st.stop()
    
    # Parse date string '2000.01.31' to datetime objects
    # errors='coerce' will turn invalid dates into NaT
    if 'tam_dont_datum' in df.columns:
        df['date'] = pd.to_datetime(df['tam_dont_datum'], format='%Y.%m.%d', errors='coerce')
    
    # Fill missing 'large program' values for filtering
    if 'large_program' in df.columns:
        df['large_program'] = df['large_program'].fillna("Egyéb / Ismeretlen")
        
    return df

try:
    df = load_and_prep_data(st.session_state['ts_data'])
except Exception as e:
    st.error(f"Hiba történt az adatok betöltése közben: {e}")
    st.stop()

# -----------------------------------------------------------------------------
# Sidebar Controls -> Left Column
# -----------------------------------------------------------------------------
with col_settings:
    st.header("Beállítások")
    

    # 1. Time Window (Fixed 2010-2022)
    df['year'] = df['date'].dt.year
    df = df[(df['year'] >= 2010) & (df['year'] <= 2019)].copy()

    # Scale grant_value to Million HUF (assuming input is Thousand HUF)
    if 'grant_value' in df.columns:
        df['grant_value'] = df['grant_value'] / 10

    # 2. Frequency (Gyakoriság)
    freq_map = {
        "Napi": "D",
        "Heti": "W",
        "Havi": "M",
        "Negyedéves": "Q",
        "Éves": "Y"
    }
    
    saved_freq = persist("p08_freq", "Havi")
    freq_idx = list(freq_map.keys()).index(saved_freq) if saved_freq in freq_map else 2
    
    freq_label = st.selectbox("Gyakoriság", list(freq_map.keys()), index=freq_idx, key="_p08_freq", on_change=save, args=("p08_freq",))
    freq_code = freq_map[freq_label]

    # 3. Metric Selection (Metrika)
    metric_options = {
        "n_grant": "Támogatások száma (db)",
        "grant_value": "Támogatási összeg (millió Ft)",
        "avg_grant_value": "Átlagos támogatási összeg (millió Ft)",
        "n_firms": "Támogatott cégek száma (db)"
    }
    selected_metrics = st.multiselect(
        "Változók", 
        list(metric_options.keys()), 
        default=persist("p08_metrics", ["n_grant"]),
        format_func=lambda x: metric_options[x],
        key="_p08_metrics", on_change=save, args=("p08_metrics",)
    )
    transform_mode = st.radio(
        "Változók átalakítása",
        ["Eredeti érték", "Változás (t - t-1)", "Kétszeres változás (Δ²)", "Százalékos változás (%)"],
        index=["Eredeti érték", "Változás (t - t-1)", "Kétszeres változás (Δ²)", "Százalékos változás (%)"].index(persist("p08_transform", "Eredeti érték")),
        key="_p08_transform", on_change=save, args=("p08_transform",)
    )

    # 4. Program Size Filter (n_program_year thresholds)
    size_opts = ["Összes adat", "Legalább 10 támogatás/év", "Legalább 25 támogatás/év", "Legalább 100 támogatás/év"]
    size_filter = st.radio(
        "Programméret",
        options=size_opts,
        index=size_opts.index(persist("p08_size", "Összes adat")),
        key="_p08_size", on_change=save, args=("p08_size",)
    )

# -----------------------------------------------------------------------------
# Filtering Logic
# -----------------------------------------------------------------------------
# Start with full mask
mask = pd.Series(True, index=df.index)

# Program Size Filter (using the pre-calculated flags)
if size_filter == "Legalább 10 támogatás/év":
    mask &= (df['n_program_year_10'] == 1)
elif size_filter == "Legalább 25 támogatás/év":
    mask &= (df['n_program_year_25'] == 1)
elif size_filter == "Legalább 100 támogatás/év":
    mask &= (df['n_program_year_100'] == 1)

# Apply filter
filtered_df = df[mask].copy()

# -----------------------------------------------------------------------------
# Aggregation & Visualization
# -----------------------------------------------------------------------------
with col_viz:
    if filtered_df.empty:
        st.info("A kiválasztott szűrők alapján nincs megjeleníthető adat.")
    else:
        # Resample by date and sum up the values
        # We group by Date only (summing up all selected counties/programs)
        
        agg_rules = {
            'n_grant': 'sum',
            'grant_value': 'sum'
        }
        
        # Handle exact distinct count for firms if 'firm_ids' exists (Surrogate Keys method)
        if 'firm_ids' in filtered_df.columns:
            # Union of all ID lists in the time bucket, then count unique elements
            agg_rules['firm_ids'] = lambda x: len(set().union(*x)) if len(x) > 0 else 0
        elif 'n_firms' in filtered_df.columns:
            # Fallback to simple sum (approximate if pre-aggregated)
            agg_rules['n_firms'] = 'sum'

        agg_df = filtered_df.set_index('date').resample(freq_code).agg(agg_rules)
        
        # Rename firm_ids to n_firms for consistency with metric_options
        if 'firm_ids' in agg_df.columns:
            agg_df = agg_df.rename(columns={'firm_ids': 'n_firms'})
        
        # Calculate Average (Weighted)
        # Avoid division by zero
        agg_df['avg_grant_value'] = agg_df.apply(
            lambda row: row['grant_value'] / row['n_grant'] if row['n_grant'] > 0 else 0, 
            axis=1
        )

        if transform_mode == "Változás (t - t-1)":
            agg_df = agg_df.diff().dropna()
        elif transform_mode == "Kétszeres változás (Δ²)":
            agg_df = agg_df.diff().diff().dropna()
        elif transform_mode == "Százalékos változás (%)":
            agg_df = agg_df.pct_change() * 100
            agg_df = agg_df.replace([np.inf, -np.inf], np.nan).dropna()
        
        # Reset index to make 'date' a column again for Altair
        agg_df = agg_df.reset_index()

        if not selected_metrics:
            st.warning("Válasszon legalább egy metrikát!")
        elif agg_df.empty:
            st.warning("Nincs elegendő adat a változás számításához (legalább 2 időszak szükséges).")
        else:
            st.subheader("Kiválasztott metrikák alakulása időben")
            
            # Filter for available columns
            plot_metrics = [m for m in selected_metrics if m in agg_df.columns]
            
            if not plot_metrics:
                st.warning("A kiválasztott metrikák nem állnak rendelkezésre.")
            else:
                # Adjust labels for diff
                display_opts = metric_options.copy()
                if transform_mode == "Változás (t - t-1)":
                    display_opts = {k: f"Δ {v}" for k, v in metric_options.items()}
                elif transform_mode == "Kétszeres változás (Δ²)":
                    display_opts = {k: f"Δ² {v}" for k, v in metric_options.items()}
                elif transform_mode == "Százalékos változás (%)":
                    display_opts = {k: f"% Δ {v}" for k, v in metric_options.items()}

                # --- Logic for Axes ---
                left_metrics = []
                right_metrics = []
                valid_selection = True
                error_msg = ""

                has_total = 'grant_value' in plot_metrics
                has_avg = 'avg_grant_value' in plot_metrics
                has_n_grant = 'n_grant' in plot_metrics
                has_n_firms = 'n_firms' in plot_metrics

                # Rule 1: Avg + Total -> No others, Split axes
                if has_total and has_avg:
                    if len(plot_metrics) > 2:
                        valid_selection = False
                        error_msg = "Ha az Átlagos és Teljes támogatási összeget egyszerre választja ki, más metrika nem szerepelhet."
                    else:
                        left_metrics = ['grant_value']
                        right_metrics = ['avg_grant_value']
                
                # Rule 2: n_firms + n_grant -> Same axis (Left), max 1 other (Right)
                elif has_n_grant and has_n_firms:
                    left_metrics = ['n_grant', 'n_firms']
                    others = [m for m in plot_metrics if m not in left_metrics]
                    if len(others) > 1:
                        valid_selection = False
                        error_msg = "A darabszámok (Cégek és Támogatások) mellé csak egy érték alapú metrika választható."
                    elif len(others) == 1:
                        right_metrics = others
                    else:
                        right_metrics = []
                
                # Default / Fallback for other cases
                else:
                    count_type = [m for m in plot_metrics if m in ['n_grant', 'n_firms']]
                    value_type = [m for m in plot_metrics if m in ['grant_value', 'avg_grant_value']]
                    
                    if count_type and value_type:
                        left_metrics = count_type
                        right_metrics = value_type
                    else:
                        left_metrics = plot_metrics
                        right_metrics = []

                if not valid_selection:
                    st.error(error_msg)
                else:
                    fig, ax = plt.subplots(figsize=(10, 6))
                    colors = utils.COLORS
                    
                    # Plot Left
                    for i, m in enumerate(left_metrics):
                        color = colors[i % len(colors)]
                        ax.plot(agg_df['date'], agg_df[m], label=display_opts[m], color=color, marker='o')
                    
                    ax.set_ylabel(" / ".join([display_opts[m] for m in left_metrics]))
                    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.0f}"))
                    
                    # Plot Right
                    ax2 = None
                    if right_metrics:
                        ax2 = ax.twinx()
                        start_idx = len(left_metrics)
                        for i, m in enumerate(right_metrics):
                            color = colors[(start_idx + i) % len(colors)]
                            ax2.plot(agg_df['date'], agg_df[m], label=display_opts[m], color=color, marker='o', linestyle='--')
                        
                        ax2.set_ylabel(" / ".join([display_opts[m] for m in right_metrics]))
                        ax2.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.0f}"))
                        ax2.spines['top'].set_visible(False)
                    
                    ax.set_xlabel("Dátum")
                    ax.spines['top'].set_visible(False)
                    ax.spines['right'].set_visible(False if not right_metrics else True)
                    
                    # Legend
                    h1, l1 = ax.get_legend_handles_labels()
                    if ax2:
                        h2, l2 = ax2.get_legend_handles_labels()
                        ax.legend(h1+h2, l1+l2, loc='upper left')
                    else:
                        ax.legend(h1, l1, loc='upper left')
                        
                    plt.xticks(rotation=45)
                    plt.tight_layout()
                    st.pyplot(fig)

                    # Phillips-Perron Test
                    with st.expander("Phillips-Perron teszt"):
                        if HAS_ARCH:
                            st.markdown("H₀: A folyamat egységgyököt tartalmaz (nem stacioner).")
                            
                            pp_results = []
                            for m in plot_metrics:
                                series = agg_df[m].dropna()
                                # Ensure enough data points
                                if len(series) > 10:
                                    try:
                                        pp = PhillipsPerron(series)
                                        is_stationary = pp.pvalue < 0.05
                                        pp_results.append({
                                            "Változó": display_opts[m],
                                            "Teszt statisztika": f"{pp.stat:.4f}",
                                            "P-érték": f"{pp.pvalue:.4f}",
                                            "Eredmény (5%)": "Stacioner" if is_stationary else "Egységgyök (Nem stacioner)"
                                        })
                                    except Exception as e:
                                        pp_results.append({
                                            "Változó": display_opts[m],
                                            "Teszt statisztika": "Hiba",
                                            "P-érték": "-",
                                            "Eredmény (5%)": str(e)
                                        })
                                else:
                                    pp_results.append({
                                        "Változó": display_opts[m],
                                        "Teszt statisztika": "N/A",
                                        "P-érték": "-",
                                        "Eredmény (5%)": "Túl kevés adat"
                                    })
                            
                            if pp_results:
                                st.dataframe(pd.DataFrame(pp_results), use_container_width=True)
                        else:
                            st.info("A Phillips-Perron teszt futtatásához szükséges az `arch` csomag telepítése (`pip install arch`).")

                    # --- Idősoros Regresszió ---
                    st.markdown("---")
                    with st.expander("Idősoros Regresszió"):
                        
                        # Available metrics in agg_df
                        reg_opts = [c for c in agg_df.columns if c in metric_options]
                        
                        if len(reg_opts) < 2:
                            st.warning("Legalább két változó szükséges a regresszióhoz.")
                        else:
                            rc1, rc2, rc3 = st.columns(3)
                            with rc1:
                                target_y = st.selectbox("Függő változó (y)", reg_opts, index=0, key="reg_y", format_func=lambda x: metric_options.get(x, x))
                            with rc2:
                                # Default to second option if available
                                def_x_idx = 1 if len(reg_opts) > 1 else 0
                                predictor_x = st.selectbox("Magyarázó változó (x)", reg_opts, index=def_x_idx, key="reg_x", format_func=lambda x: metric_options.get(x, x))
                            with rc3:
                                n_lags = st.number_input("Késleltetések száma", min_value=0, max_value=12, value=1, step=1)
                            
                            chk1, chk2 = st.columns(2)
                            with chk1:
                                seasonal_option = st.selectbox("Szezonalitás", ["Nincs", "Havi dummyk", "Negyedév dummyk"])
                            with chk2:
                                use_hac = st.checkbox("Newey-West (HAC) SE", value=True)
                                
                            # --- Structural Break ---
                            st.markdown("##### Strukturális törés")
                            use_break = st.checkbox("Strukturális törés bevonása", value=False)
                            if use_break:
                                sb_c1, sb_c2, sb_c3 = st.columns(3)
                                with sb_c1:
                                    break_start = st.date_input("Kezdete", value=pd.Timestamp("2016-01-01"))
                                with sb_c2:
                                    break_end = st.date_input("Vége", value=pd.Timestamp("2020-12-31"))
                                with sb_c3:
                                    interact_break = st.checkbox("Interakció X-szel", value=False)

                            reg_df = agg_df.copy()
                            
                            # Create lags for X
                            x_cols = [predictor_x]
                            lag_cols = []
                            for i in range(1, n_lags + 1):
                                col_name = f"{predictor_x}_lag{i}"
                                reg_df[col_name] = reg_df[predictor_x].shift(i)
                                x_cols.append(col_name)
                                lag_cols.append(col_name)
                            
                            # Seasonal dummies
                            if seasonal_option != "Nincs":
                                if reg_df['date'].dt.month.nunique() > 1:
                                    reg_df['month'] = reg_df['date'].dt.month
                                    
                                    if seasonal_option == "Havi dummyk":
                                        dummies = pd.get_dummies(reg_df['month'], prefix='month', drop_first=True).astype(int)
                                    else: # Negyedév dummyk
                                        reg_df['quarter'] = reg_df['date'].dt.quarter
                                        dummies = pd.get_dummies(reg_df['quarter'], prefix='quarter', drop_first=True).astype(int)
                                        
                                    reg_df = pd.concat([reg_df, dummies], axis=1)
                                    x_cols.extend(dummies.columns.tolist())
                                else:
                                    st.warning("Nem lehet szezonális dummykat generálni (nincs elegendő variáció a dátumokban).")

                            # Structural Break Dummy
                            if use_break:
                                reg_df['break_dummy'] = 0
                                if not pd.api.types.is_datetime64_any_dtype(reg_df['date']):
                                    reg_df['date'] = pd.to_datetime(reg_df['date'])
                                
                                mask_break = (reg_df['date'] >= pd.Timestamp(break_start)) & (reg_df['date'] <= pd.Timestamp(break_end))
                                reg_df.loc[mask_break, 'break_dummy'] = 1
                                x_cols.append('break_dummy')

                                if interact_break:
                                    vars_to_interact = [predictor_x] + lag_cols
                                    for v in vars_to_interact:
                                        int_col = f"{v}_x_break"
                                        reg_df[int_col] = reg_df[v] * reg_df['break_dummy']
                                        x_cols.append(int_col)

                            reg_data = reg_df[[target_y] + x_cols].dropna()
                            
                            if reg_data.empty:
                                st.error("Nincs elegendő adat a regresszióhoz.")
                            else:
                                Y = reg_data[target_y]
                                X = sm.add_constant(reg_data[x_cols])
                                
                                cov_type = 'HAC' if use_hac else 'nonrobust'
                                cov_kwds = {'maxlags': max(1, n_lags)} if use_hac else None
                                
                                model = sm.OLS(Y, X)
                                results = model.fit(cov_type=cov_type, cov_kwds=cov_kwds)
                                
                                # Formázott kimenet (hasonlóan a 07-Regresszió.py-hoz)
                                def sig_stars(p):
                                    if p < 0.01: return "***"
                                    elif p < 0.05: return "**"
                                    elif p < 0.1: return "*"
                                    return ""
                                
                                def fmt_val(x):
                                    return f"{x:.3f}"

                                # Calculate Cumulative Effect first
                                vars_of_interest = [predictor_x] + lag_cols
                                vars_of_interest = [v for v in vars_of_interest if v in results.params.index]
                                
                                cum_beta_str = ""
                                cum_se_str = ""
                                if vars_of_interest:
                                    # A kumulált hatás (összeg) és annak SE-jének számítása.
                                    # Ez matematikailag ekvivalens a tananyagban szereplő "segédregressziós" módszerrel,
                                    # de itt közvetlenül a kovarianciamátrixból számoljuk (Delta-módszer).
                                    r_matrix = pd.Series(0, index=results.params.index)
                                    r_matrix[vars_of_interest] = 1
                                    
                                    # t_test returns the effect (sum), SE, t, p
                                    t_test = results.t_test(r_matrix)
                                    c_beta = t_test.effect[0]
                                    c_se = t_test.sd.flat[0]
                                    c_p = float(t_test.pvalue)
                                    
                                    cum_beta_str = f"{fmt_val(c_beta)}{sig_stars(c_p)}"
                                    cum_se_str = f"({fmt_val(c_se)})"

                                rows_coef = []
                                rows_cum = []
                                indices = []
                                
                                for name in results.params.index:
                                    coef = results.params[name]
                                    se = results.bse[name]
                                    pval = results.pvalues[name]
                                    
                                    # Row 1: Coefficient
                                    rows_coef.append(f"{fmt_val(coef)}{sig_stars(pval)}")
                                    
                                    if name == predictor_x:
                                        rows_cum.append(cum_beta_str)
                                    else:
                                        rows_cum.append("")
                                    
                                    # Row 2: Standard Error
                                    rows_coef.append(f"({fmt_val(se)})")
                                    
                                    if name == predictor_x:
                                        rows_cum.append(cum_se_str)
                                    else:
                                        rows_cum.append("")
                                    
                                    # Index Label
                                    if name == "const":
                                        pretty_name = "Konstans"
                                    elif name.startswith("month_"):
                                        pretty_name = f"Hónap = {name.split('_')[1]}"
                                    elif name.startswith("quarter_"):
                                        pretty_name = f"Negyedév = {name.split('_')[1]}"
                                    elif "_lag" in name:
                                        parts = name.split("_lag")
                                        base = parts[0]
                                        lag = parts[1]
                                        base_label = metric_options.get(base, base)
                                        pretty_name = f"{base_label} (lag {lag})"
                                    elif "_x_break" in name:
                                        base = name.replace("_x_break", "")
                                        if "_lag" in base:
                                            parts = base.split("_lag")
                                            b = parts[0]
                                            l = parts[1]
                                            lbl = metric_options.get(b, b)
                                            pretty_name = f"{lbl} (lag {l}) × Törés"
                                        else:
                                            lbl = metric_options.get(base, base)
                                            pretty_name = f"{lbl} × Törés"
                                    elif name == "break_dummy":
                                        pretty_name = "Strukturális törés (Dummy)"
                                    else:
                                        pretty_name = metric_options.get(name, name)
                                    indices.append(pretty_name)
                                    indices.append("") # Empty label for SE row
                                
                                indices.append("----------")
                                rows_coef.append("----------")
                                rows_cum.append("----------")
                                
                                indices.append("Megfigyelések")
                                rows_coef.append(f"{int(results.nobs):,}".replace(",", " "))
                                rows_cum.append("")
                                
                                indices.append("R²")
                                rows_coef.append(f"{results.rsquared:.3f}")
                                rows_cum.append("")
                                
                                summary_df = pd.DataFrame({
                                    "Együttható": rows_coef,
                                    "Kumulált hatás": rows_cum
                                }, index=indices)
                                
                                left_spacer, mid_col, right_spacer = st.columns([1, 3, 1])
                                with mid_col:
                                    st.table(summary_df, border=False)
                                
                                st.markdown(
                                    "<span style='font-size:0.85rem'><em>Megjegyzés:</em> Zárójelben a standard hibák. "
                                    "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
                                    unsafe_allow_html=True,
                                )
