import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from matplotlib.ticker import FuncFormatter

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]
st.set_page_config(page_title='Eloszlások vizualizálása — Vállalatok (HU, szimulált)', layout='wide')

@st.cache_data
def load_cross_section(path: str = 'data/synthetic/sim_cs2019_by_nace2_withcats.parquet') -> pd.DataFrame:
    p = Path(path)
    if not p.exists():
        st.error(f"Fájl nem található: {p}")
        st.stop()
    df = pd.read_parquet(p).copy()
    if "nace2_name_code" not in df.columns:
        st.error("Hiányzik a `nace2_name_code` oszlop az adatból.")
        st.stop()
    return df

cs = load_cross_section()

# ----------------------------- Header ------------------------------
st.title('Eloszlások vizualizálása — 2019 keresztmetszet (szimulált)')
st.markdown("Válasszon egy **ágazatot** és egy **változót** a megjelenítéshez. A pénzügyi adatok **millió forintban** szerepelnek.")

# ----------------------------- Sidebar -----------------------------
st.sidebar.header('Beállítások')

# Build industry options: ALL first, then sorted by numeric code in the label
lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
lab_df["__code"] = pd.to_numeric(lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False), errors="coerce")
lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")

labels = ["Összes ágazat (ALL)"] + lab_df["label"].tolist()
def_idx = 1 if len(labels) > 1 else 0
selected_label = st.sidebar.selectbox("Ágazat", labels, index=def_idx)

# Variable selection — monetary variables will be displayed in million HUF
MONETARY_VARS = {
    'Értékesítés (millió Ft)': 'sales_clean',
    'Tárgyi eszközök (millió Ft)': 'tanass_clean',
    'Eszközök összesen (millió Ft)': 'eszk',
    'Személyi jellegű ráfordítások (millió Ft)': 'persexp_clean',
    'Adózás előtti eredmény (millió Ft)': 'pretax',
    'EBIT (millió Ft)': 'ereduzem',
    'Export értéke (millió Ft)': 'export_value',
    'Kötelezettségek (millió Ft)': 'liabilities',
}
NON_MONETARY_VARS = {
    'Foglalkoztatottak száma (fő)': 'emp',
    'Kor (év)': 'age',
}
var_map = {**MONETARY_VARS, **NON_MONETARY_VARS}

available = [k for k, v in var_map.items() if v in cs.columns]
if not available:
    st.error("A várt változók közül egyik sem található az adatban.")
    st.stop()

var_label = st.sidebar.selectbox('Megjelenítendő változó', available, index=0)
var = var_map[var_label]
is_monetary = var in MONETARY_VARS.values()

# Tail handling (2% each side)
st.sidebar.subheader("Szélsőérték kezelés (2% minden oldalon)")
winsorize = st.sidebar.checkbox("Winsorizálás (alsó/felső 2%)", value=True)
trim = st.sidebar.checkbox("Szélsők kizárása (alsó/felső 2%)", value=False)

# Bins only
st.sidebar.subheader('Hisztogram beállítások')
bins = st.sidebar.slider('Binek száma', min_value=5, max_value=60, value=25, step=1)

# ----------------------------- Filter ------------------------------
if selected_label == "Összes ágazat (ALL)":
    workset = cs.copy()
else:
    workset = cs[cs["nace2_name_code"] == selected_label].copy()

if workset.empty:
    st.error('Nincs adat a választott szűrési feltételekhez.')
    st.stop()

# scale to million HUF when monetary
x_raw = workset[var].replace([np.inf, -np.inf], np.nan).dropna()
x = (x_raw / 1000.0) if is_monetary else x_raw

if x.empty:
    st.warning("A kiválasztott változó nem tartalmaz érvényes adatot a szűrés után.")
    st.stop()

# Tails on the filtered set
if len(x) >= 5:
    q2, q98 = np.percentile(x, [2, 98])
else:
    q2, q98 = np.nanmin(x), np.nanmax(x)

if trim:
    x_plot = x[(x >= q2) & (x <= q98)]
else:
    x_plot = x.clip(q2, q98) if winsorize else x

# ----------------------------- Plot -------------------------------
fig, ax = plt.subplots()

# Compute histogram and hide small bins (<5 obs)
counts, edges = np.histogram(x_plot, bins=bins)
counts_masked = counts.copy()
counts_masked[counts_masked < 5] = 0

widths = np.diff(edges)
ax.bar(edges[:-1], counts_masked, width=widths, align='edge',
       color=color[0], edgecolor='white', linewidth=0.5)

st.subheader(f'{var_label} hisztogram — {bins} kosár (a <5 megfigyelésű oszlopok rejtve)')
ax.set_xlabel(var_label)
ax.set_ylabel('Gyakoriság')
ax.spines[['top', 'right']].set_visible(False)

# No scientific notation + tilted labels + separators
ax.ticklabel_format(style='plain', axis='x')
ax.xaxis.set_major_formatter(FuncFormatter(lambda x, _: f"{x:,.2f}" if is_monetary else f"{x:,.0f}"))
ax.tick_params(axis='x', labelrotation=25)

plt.tight_layout()
st.pyplot(fig)

# ----------------------- Descriptive statistics ----------------------
x_stats = x_plot.replace([np.inf, -np.inf], np.nan).dropna()
if not x_stats.empty:
    modes = x_stats.mode(dropna=True)
    mode_val = modes.iloc[0] if len(modes) > 0 else np.nan
    stats = {
        'minimum': float(np.nanmin(x_stats)),
        'maximum': float(np.nanmax(x_stats)),
        'terjedelem': float(np.nanmax(x_stats) - np.nanmin(x_stats)),
        'átlag': float(np.nanmean(x_stats)),
        'medián': float(np.nanmedian(x_stats)),
        'módusz': float(mode_val) if pd.api.types.is_numeric_dtype(x_stats) else mode_val,
        'szórás (minta)': float(x_stats.std(ddof=1)),
        'variancia (minta)': float(x_stats.var(ddof=1)),
        'ferdeség': float(x_stats.skew())
    }
    st.subheader('Leíró statisztikák (a megjelenített adaton)')
    stats_df = pd.DataFrame({'Statisztika': list(stats.keys()), 'Érték': list(stats.values())})
    def _fmt(v):
        try:
            return f"{v:,.2f}" if is_monetary else f"{v:,.0f}"
        except Exception:
            return str(v)
    stats_df['Érték'] = stats_df['Érték'].map(_fmt)
    st.dataframe(stats_df, use_container_width=True)
else:
    st.info('Nem maradt legalább 5 megfigyelés a statisztikához.')

# ----------------------------- Summary ------------------------------
scope_label = selected_label
tail_note = "Levágva" if trim else ("winsorizálva" if winsorize else "nyers")
unit_note = "millió Ft" if is_monetary else "nyers egység"
st.markdown(
    f"**Minta:** {scope_label} · **Változó:** `{var_label}` · "
    f"**Megfigyelések (megjelenítve):** {len(x_plot):,} · **Szélek:** {tail_note} · **Binek:** {bins} · "
    f"**Egység:** {unit_note} · **A 5-nél kevesebb megfigyelésű oszlopok rejtve vannak.**"
)
