import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter
import utils
import statsmodels.api as sm
from statsmodels.tsa.arima.model import ARIMA

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
    "Idősoros Előrejelzés (ARIMA)",
    "Idősoros Előrejelzés (ARIMA)"
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
    try:
        df = pd.read_parquet(path)
    except Exception:
        st.error(f"Nem sikerült betölteni a fájlt: {path}")
        st.stop()
    
    if 'tam_dont_datum' in df.columns:
        df['date'] = pd.to_datetime(df['tam_dont_datum'], format='%Y.%m.%d', errors='coerce')
    
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
    st.header("Minta beállítások")
    
    # 1. Time Window (Fixed 2010-2020)
    df['year'] = df['date'].dt.year
    df = df[(df['year'] >= 2010) & (df['year'] <= 2020)].copy()

    # Scale grant_value to Million HUF
    if 'grant_value' in df.columns:
        df['grant_value'] = df['grant_value'] / 10

    # 2. Filters
    size_opts = ["Összes adat", "Legalább 10 támogatás/év", "Legalább 25 támogatás/év", "Legalább 100 támogatás/év"]
    size_filter = st.radio(
        "Programméret",
        options=size_opts,
        index=size_opts.index(persist("p09_size", "Összes adat")),
        key="_p09_size", on_change=save, args=("p09_size",)
    )

    st.markdown("---")
    st.subheader("ARIMA Beállítások")

    # Frequency (Gyakoriság)
    freq_map = {
        "Napi": "D",
        "Heti": "W",
        "Havi": "M",
        "Negyedéves": "Q",
        "Éves": "Y"
    }
    saved_freq = persist("p09_freq", "Havi")
    freq_idx = list(freq_map.keys()).index(saved_freq) if saved_freq in freq_map else 2
    freq_label = st.selectbox("Gyakoriság", list(freq_map.keys()), index=freq_idx, key="_p09_freq", on_change=save, args=("p09_freq",))
    freq_code = freq_map[freq_label]

    metric_options = {
        "n_grant": "Támogatások száma (db)",
        "grant_value": "Támogatási összeg (millió Ft)",
        "avg_grant_value": "Átlagos támogatási összeg (millió Ft)",
        "n_firms": "Támogatott cégek száma (db)"
    }
    
    saved_target = persist("p09_target", "n_grant")
    target_idx = list(metric_options.keys()).index(saved_target) if saved_target in metric_options else 0
    target_y = st.selectbox("Célváltozó", list(metric_options.keys()), index=target_idx, format_func=lambda x: metric_options[x], key="_p09_target", on_change=save, args=("p09_target",))
    
    c1, c2, c3 = st.columns(3)
    with c1:
        p_param = st.number_input("AR (p)", min_value=0, max_value=12, value=persist("p09_p", 1), key="_p09_p", on_change=save, args=("p09_p",))
    with c2:
        d_param = st.number_input("I (d)", min_value=0, max_value=2, value=persist("p09_d", 1), key="_p09_d", on_change=save, args=("p09_d",))
    with c3:
        q_param = st.number_input("MA (q)", min_value=0, max_value=12, value=persist("p09_q", 1), key="_p09_q", on_change=save, args=("p09_q",))

    min_d = df['date'].min()
    max_d = df['date'].max()
    default_split = min_d + (max_d - min_d) * 0.8
    
    split_date = st.date_input("Tanuló/Teszt vágás", value=persist("p09_split", default_split), min_value=min_d, max_value=max_d, key="_p09_split", on_change=save, args=("p09_split",))
    split_date = pd.Timestamp(split_date)

# -----------------------------------------------------------------------------
# Filtering Logic
# -----------------------------------------------------------------------------
mask = pd.Series(True, index=df.index)

if size_filter == "Legalább 10 támogatás/év":
    mask &= (df['n_program_year_10'] == 1)
elif size_filter == "Legalább 25 támogatás/év":
    mask &= (df['n_program_year_25'] == 1)
elif size_filter == "Legalább 100 támogatás/év":
    mask &= (df['n_program_year_100'] == 1)

filtered_df = df[mask].copy()

# -----------------------------------------------------------------------------
# Aggregation & Visualization
# -----------------------------------------------------------------------------
with col_viz:
    if filtered_df.empty:
        st.info("A kiválasztott szűrők alapján nincs megjeleníthető adat.")
    else:
        agg_rules = {
            'n_grant': 'sum',
            'grant_value': 'sum'
        }
        
        if 'firm_ids' in filtered_df.columns:
            agg_rules['firm_ids'] = lambda x: len(set().union(*x)) if len(x) > 0 else 0
        elif 'n_firms' in filtered_df.columns:
            agg_rules['n_firms'] = 'sum'

        agg_df = filtered_df.set_index('date').resample(freq_code).agg(agg_rules)
        
        if 'firm_ids' in agg_df.columns:
            agg_df = agg_df.rename(columns={'firm_ids': 'n_firms'})
        
        agg_df['avg_grant_value'] = agg_df.apply(
            lambda row: row['grant_value'] / row['n_grant'] if row['n_grant'] > 0 else 0, 
            axis=1
        )
        
        agg_df = agg_df.reset_index()

        if agg_df.empty:
            st.warning("Nincs elegendő adat.")
        else:
            # Prepare data for ARIMA
            arima_df = agg_df[['date', target_y]].dropna().set_index('date').sort_index()
            
            if arima_df.empty:
                st.error("Nincs elegendő adat az ARIMA modellhez.")
            else:
                # Try to infer frequency
                try:
                    arima_df = arima_df.asfreq(freq_code)
                except:
                    pass 

                # Always use original scale for plotting
                y_data = arima_df[target_y].dropna()
                final_order = (p_param, d_param, q_param)

                train_df = y_data[y_data.index <= split_date]
                test_df = y_data[y_data.index > split_date]

                if train_df.empty:
                    st.error("A tanuló adathalmaz üres. Válasszon későbbi dátumot.")
                else:
                    try:
                        model = ARIMA(train_df, order=final_order)
                        results = model.fit()
                    except Exception as e:
                        st.error(f"Hiba az ARIMA modell illesztésekor: {e}")
                        st.stop()

                    # Forecast Visualization (Moved to top)
                    st.subheader("Előrejelzés")
                    
                    if not test_df.empty:
                        forecast_steps = len(test_df)
                    else:
                        forecast_steps = 12
                        
                    forecast_res = results.get_forecast(steps=forecast_steps)
                    forecast_mean = forecast_res.predicted_mean
                    conf_int = forecast_res.conf_int(alpha=0.2)

                    fig_fc, ax_fc = plt.subplots(figsize=(10, 5))
                    
                    ax_fc.plot(train_df.index, train_df, label="Tényadat", color=utils.COLORS[0], marker='o')
                    
                    if not test_df.empty:
                        ax_fc.plot(test_df.index, test_df, color=utils.COLORS[0], marker='o')
                    
                    ax_fc.plot(forecast_mean.index, forecast_mean, label="Előrejelzés", color=utils.COLORS[2], marker='o')
                    ax_fc.fill_between(conf_int.index, conf_int.iloc[:, 0], conf_int.iloc[:, 1], color=utils.COLORS[2], alpha=0.2)
                    
                    ax_fc.axvline(split_date, color='gray', linestyle='--', label="Vágás")
                    ax_fc.legend()
                    title_prefix = ""
                    ax_fc.set_title(f"{title_prefix}{metric_options.get(target_y, target_y)} előrejelzése")
                    ax_fc.spines['top'].set_visible(False)
                    ax_fc.spines['right'].set_visible(False)
                    st.pyplot(fig_fc)

                    # Model Summary
                    with st.expander("Modell Összefoglaló", expanded=False):
                        def sig_stars(p):
                            if p < 0.01: return "***"
                            elif p < 0.05: return "**"
                            elif p < 0.1: return "*"
                            return ""
                        
                        def fmt_val(x):
                            return f"{x:.3f}"

                        rows_coef = []
                        indices = []
                        
                        for name in results.params.index:
                            if name == "sigma2":
                                continue

                            coef = results.params[name]
                            try:
                                se = results.bse[name]
                                pval = results.pvalues[name]
                            except:
                                se = np.nan
                                pval = np.nan
                            
                            # Row 1: Coefficient
                            rows_coef.append(f"{fmt_val(coef)}{sig_stars(pval)}")
                            
                            # Row 2: Standard Error
                            rows_coef.append(f"({fmt_val(se)})")
                            
                            # Index Label
                            if name == "const":
                                pretty_name = "Konstans"
                            elif name.startswith("ar.L"):
                                pretty_name = f"AR({name.split('.')[1].replace('L','')})"
                            elif name.startswith("ma.L"):
                                pretty_name = f"MA({name.split('.')[1].replace('L','')})"
                            else:
                                pretty_name = name
                            indices.append(pretty_name)
                            indices.append("") # Empty label for SE row

                        indices.append("----------")
                        rows_coef.append("----------")
                        
                        indices.append("Megfigyelések")
                        rows_coef.append(f"{int(results.nobs):,}".replace(",", " "))
                        
                        indices.append("Tanuló adat")
                        rows_coef.append("")

                        indices.append("AIC")
                        rows_coef.append(f"{results.aic:.2f}")
                        
                        indices.append("BIC")
                        rows_coef.append(f"{results.bic:.2f}")

                        # Calculate in-sample errors on original scale (levels)
                        # results.resid would be on differenced scale if d>0
                        train_pred = results.predict()
                        train_errors = train_df - train_pred
                        mse = np.nanmean(train_errors**2)
                        rmse = np.sqrt(mse)
                        indices.append("MSE")
                        rows_coef.append(f"{mse:.2f}")
                        
                        indices.append("RMSE")
                        rows_coef.append(f"{rmse:.2f}")

                        if not test_df.empty:
                            # Calculate Test Metrics
                            test_mse = np.mean((test_df.values - forecast_mean.values)**2)
                            test_rmse = np.sqrt(test_mse)
                            
                            indices.append("----------")
                            rows_coef.append("----------")
                            
                            indices.append("Teszt adat")
                            rows_coef.append("")
                            
                            indices.append("MSE")
                            rows_coef.append(f"{test_mse:.2f}")
                            
                            indices.append("RMSE")
                            rows_coef.append(f"{test_rmse:.2f}")
                        
                        summary_df = pd.DataFrame({
                            "Együttható": rows_coef
                        }, index=indices)
                        
                        left_spacer, mid_col, right_spacer = st.columns([1, 3, 1])
                        with mid_col:
                            st.table(summary_df, border=False)
                        
                        st.markdown(
                            "<span style='font-size:0.85rem'><em>Megjegyzés:</em> Zárójelben a standard hibák. "
                            "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
                            unsafe_allow_html=True,
                        )