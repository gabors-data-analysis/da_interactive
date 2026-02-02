import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter
import utils

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
    if 'large program' in df.columns:
        df['large program'] = df['large program'].fillna("Egyéb / Ismeretlen")
        
    return df

try:
    df = load_and_prep_data(st.session_state['ts_data'])
    st.write(df.head())
except Exception as e:
    st.error(f"Hiba történt az adatok betöltése közben: {e}")
    st.stop()

# -----------------------------------------------------------------------------
# Sidebar Controls -> Left Column
# -----------------------------------------------------------------------------
with col_settings:
    st.header("Beállítások")
    utils.render_sync_option(st)

    # 1. Time Window (Időablak)
    min_date = df['date'].min()
    max_date = df['date'].max()

    # Handle case where date parsing failed completely
    if pd.isna(min_date):
        st.error("Nem sikerült értelmezni a dátumokat a 'tam_dont_datum' oszlopban.")
        st.stop()

    date_range = st.date_input(
        "Időablak",
        value=(min_date, max_date),
        min_value=min_date,
        max_value=max_date
    )

    # 2. Frequency (Gyakoriság)
    freq_map = {
        "Napi": "D",
        "Heti": "W",
        "Havi": "M",
        "Éves": "Y"
    }
    freq_label = st.selectbox("Gyakoriság", list(freq_map.keys()), index=2) # Default: Monthly
    freq_code = freq_map[freq_label]

    # 3. Metric Selection (Metrika)
    metric_options = {
        "n_grant": "Támogatások száma (db)",
        "grant_value": "Támogatási összeg (ezer Ft)",
        "avg_grant_value": "Átlagos támogatási összeg (ezer Ft)"
    }
    selected_metric_key = st.selectbox(
        "Metrika", 
        list(metric_options.keys()), 
        format_func=lambda x: metric_options[x]
    )

    # 4. Program Size Filter (n_program_year thresholds)
    size_filter = st.radio(
        "Program méret szűrő",
        options=["Összes adat", "Legalább 10 támogatás/év", "Legalább 25 támogatás/év", "Legalább 100 támogatás/év"],
        index=0
    )

    # 5. County Filter (Megyék)
    # Default is empty list which implies "All"
    all_counties = sorted(df['megval_megye_nev'].dropna().unique())
    selected_counties = st.multiselect("Megyék (Üres = Összes)", all_counties)

    # 6. Large Program Filter (Optional extra)
    if 'large program' in df.columns:
        large_programs = sorted(df['large program'].unique())
        selected_large_programs = st.multiselect("Nagy Program Kategória", large_programs)
    else:
        selected_large_programs = []

# -----------------------------------------------------------------------------
# Filtering Logic
# -----------------------------------------------------------------------------
# Start with full mask
mask = pd.Series(True, index=df.index)

# Date Filter
if isinstance(date_range, tuple) and len(date_range) == 2:
    start_d, end_d = date_range
    mask &= (df['date'] >= pd.to_datetime(start_d)) & (df['date'] <= pd.to_datetime(end_d))

# County Filter
if selected_counties:
    mask &= df['megval_megye_nev'].isin(selected_counties)

# Large Program Filter
if selected_large_programs:
    mask &= df['large program'].isin(selected_large_programs)

# Program Size Filter (using the pre-calculated flags)
if size_filter == "> 10 támogatás/év":
    mask &= (df['n_program_year_10'] == 1)
elif size_filter == "> 25 támogatás/év":
    mask &= (df['n_program_year_25'] == 1)
elif size_filter == "> 100 támogatás/év":
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
        resampled = filtered_df.set_index('date').resample(freq_code)
        
        agg_df = pd.DataFrame()
        agg_df['n_grant'] = resampled['n_grant'].sum()
        agg_df['grant_value'] = resampled['grant_value'].sum()
        
        # Calculate Average (Weighted)
        # Avoid division by zero
        agg_df['avg_grant_value'] = agg_df.apply(
            lambda row: row['grant_value'] / row['n_grant'] if row['n_grant'] > 0 else 0, 
            axis=1
        )
        
        # Reset index to make 'date' a column again for Altair
        agg_df = agg_df.reset_index()

        st.subheader(f"{metric_options[selected_metric_key]} alakulása időben")
        
        # Dynamic Y-axis title
        y_title = metric_options[selected_metric_key]
        
        # Create Line Chart (Matplotlib + Seaborn)
        fig, ax = plt.subplots(figsize=(10, 6))
        
        sns.lineplot(
            data=agg_df, 
            x='date', 
            y=selected_metric_key, 
            marker='o', 
            color=utils.COLORS[0],
            ax=ax
        )
        
        ax.set_xlabel("Dátum")
        ax.set_ylabel(y_title)
        ax.spines[['top', 'right']].set_visible(False)
        
        # Format Y axis
        ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.0f}"))
        
        # Rotate x-axis labels for better readability
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        st.pyplot(fig)

        # Optional: Show Data Table
        with st.expander("Részletes adatok táblázata"):
            st.dataframe(agg_df.style.format({
                'n_grant': '{:,.0f}',
                'grant_value': '{:,.0f}',
                'avg_grant_value': '{:,.2f}'
            }))
