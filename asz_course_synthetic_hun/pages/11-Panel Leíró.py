import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
from scipy.stats import gaussian_kde
import utils

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# ----------------------------- Setup ------------------------------
col_settings, col_viz = utils.setup_page(
    "Paneladatok leíró elemzése",
    "Paneladatok leíró elemzése (szimulált)"
)

# --- LOAD DATA FROM SESSION STATE ---
def get_panel_data():
    if "panel_data" not in st.session_state or st.session_state["panel_data"] is None:
        st.error("Paneladatok nincsenek betöltve. Nyissa meg először az Adattábla oldalt.")
        st.stop()
    return st.session_state["panel_data"].copy()


def load_data(panel_data):
    df = panel_data.copy()

    df = utils.add_sme_category(df)
    
    if 'received_grant' in df.columns:
        df['received_grant'] = df['received_grant'].fillna(0)
        treatment_years = df[df['received_grant'] == 1].groupby('id')['year'].min().reset_index()
        treatment_years.columns = ['id', 'cohort_year']
        df = pd.merge(df, treatment_years, on='id', how='left')
        df['cohort'] = df['cohort_year'].apply(lambda x: "Sosem kezelt" if pd.isna(x) else f"{int(x)}. évi kohorsz")
    else:
        df['cohort'] = "Sosem kezelt"
        
    return df

df = load_data(get_panel_data())

st.markdown("""
Válasszon **ágazatot**, **éveket** és egy **változót** a megjelenítéshez.
A pénzügyi változók **millió forintban** jelennek meg.
""")

# --- VAR CONFIG ---
MONETARY_VARS = ["sales_clean23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23", "pretax23","ereduzem23", "jetok23", "grant_value","export23"]
variable_list = ["emp", "sales_clean23", "received_grant", "foundyear", "county", "firm_owner","pretax23","ereduzem23",
                  "export23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23", "jetok23", "grant_value"]
available_variables = [v for v in variable_list if v in df.columns]

if not available_variables:
    st.error("Nincsenek megfelelő változók az adatban.")
    st.stop()

# ----------------------------- Sidebar / Settings -----------------------------
with col_settings:
    st.header('Beállítások')
    
    # 1. Choose an industry
    if 'nace2_in_2015' not in df.columns:
        st.error("'nace2_in_2015' oszlop nem található.")
        st.stop()
        
    industries = ["Összes ágazat"] + sorted(df['nace2_in_2015'].dropna().unique())
    default_industry = industries[0]
    selected_industry = st.selectbox(
        'Ágazat', 
        industries,
        index=industries.index(persist("p11_industry", default_industry)),
        key="_p11_industry", on_change=save, args=("p11_industry",)
    )
    
    # 2. Choose years
    if 'year' not in df.columns:
        st.error("'year' oszlop nem található.")
        st.stop()
    years = sorted(df['year'].dropna().unique())
    selected_years = st.multiselect(
        'Évek', 
        years, 
        default=persist("p11_years", years),
        key="_p11_years", on_change=save, args=("p11_years",)
    )
    
    # 3. Choose a variable
    selected_variable = st.selectbox(
        'Változó', 
        available_variables,
        index=available_variables.index(persist("p11_var", available_variables[0])),
        key="_p11_var", on_change=save, args=("p11_var",)
    )

    sme_all, sme_micro, sme_small, sme_medium, sme_large = utils.sme_filter_ui("p11", persist, save)

# Base filtering
if selected_industry == "Összes ágazat":
    base_filtered_df = df[df['year'].isin(selected_years)].copy()
else:
    base_filtered_df = df[(df['nace2_in_2015'] == selected_industry) & (df['year'].isin(selected_years))].copy()

base_filtered_df, allowed_cats = utils.apply_sme_filter(base_filtered_df, sme_all, sme_micro, sme_small, sme_medium, sme_large)

if base_filtered_df.empty:
    st.error("Nincs adat a választott szűrési feltételekhez.")
    st.stop()

is_monetary = selected_variable in MONETARY_VARS
is_continuous = pd.api.types.is_numeric_dtype(base_filtered_df[selected_variable]) and base_filtered_df[selected_variable].nunique() > 15

# Scale monetary variables (1000 HUF -> Million HUF)
if is_monetary:
    base_filtered_df[selected_variable] = base_filtered_df[selected_variable] / 1000.0

with col_settings:
    if is_continuous:
        with st.expander("Szélsőérték-kezelés"):
            saved_tail = persist("p11_tail", "Nincs szűrés (összes érték)")
            tail_idx = utils.FILTER_OPTIONS.index(saved_tail) if saved_tail in utils.FILTER_OPTIONS else 0
            tail_mode = st.selectbox("X szélsőérték-kezelése", utils.FILTER_OPTIONS, index=tail_idx, key="_p11_tail", on_change=save, args=("p11_tail",))

            x_raw = base_filtered_df[selected_variable].replace([np.inf, -np.inf], np.nan).dropna()
            
            if tail_mode == "Kézi minimum/maximum" and not x_raw.empty:
                st.markdown("**Kézi határok (a megjelenített egységben)**")
                current_min = float(np.nanmin(x_raw))
                current_max = float(np.nanmax(x_raw))
                manual_min = st.number_input(
                    "Minimum", value=current_min,
                    step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                )
                manual_max = st.number_input(
                    "Maximum", value=current_max,
                    step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                )
                if manual_min > manual_max:
                    st.error("A minimum nem lehet nagyobb a maximum­nál.")
                    manual_min, manual_max = manual_max, manual_min
            else:
                manual_min = None
                manual_max = None
                
        with st.expander("Skála"):
            use_log = st.checkbox("Logaritmikus (ln) transzformáció", value=persist("p11_log", False), key="_p11_log", on_change=save, args=("p11_log",))
    else:
        tail_mode = "Nincs szűrés"
        use_log = False
        manual_min = manual_max = None

# Apply filter / transforms
if is_continuous:
    filtered_series = utils.apply_filter(base_filtered_df[selected_variable], tail_mode, manual_min, manual_max)
    filtered_df = base_filtered_df.loc[filtered_series.index].copy()
    filtered_df[selected_variable] = filtered_series
else:
    filtered_df = base_filtered_df.dropna(subset=[selected_variable]).copy()

log_note = ""
if use_log and is_continuous:
    pos_mask = filtered_df[selected_variable] > 0
    if not np.any(pos_mask):
        st.warning("Logaritmikus ábrázoláshoz pozitív értékek szükségesek. A szűrés után nem maradt pozitív érték.")
        st.stop()
    filtered_df = filtered_df[pos_mask]
    filtered_df[selected_variable] = np.log(filtered_df[selected_variable])
    log_note = " (ln)"

if filtered_df.empty:
    st.warning("A kiválasztott beállításokkal nem maradt megjeleníthető adat.")
    st.stop()

# ----------------------------- Plot -------------------------------
with col_viz:
    st.header(f'"{selected_variable}" eloszlása évenként')

    fig, ax = plt.subplots(figsize=(10, 6))
    plotted_years = sorted(filtered_df['year'].unique())

    if is_continuous:
        for i, yr in enumerate(plotted_years):
            yr_data = filtered_df[filtered_df['year'] == yr][selected_variable].dropna()
            if len(yr_data) > 1 and yr_data.std() > 1e-6:
                try:
                    kde = gaussian_kde(yr_data)
                    x_grid = np.linspace(filtered_df[selected_variable].min(), filtered_df[selected_variable].max(), 500)
                    ax.plot(x_grid, kde(x_grid), label=str(yr), linewidth=2, color=utils.COLORS[i % len(utils.COLORS)])
                except Exception:
                    pass # Skip if matrix ends up singular somehow
        
        ax.set_ylabel('Sűrűség (Density)')
    else:
        # Bar plot for categories
        counts = filtered_df.groupby([selected_variable, 'year']).size().unstack(fill_value=0)
        counts = counts.reindex(columns=plotted_years, fill_value=0)
        
        colors = [utils.COLORS[i % len(utils.COLORS)] for i in range(len(plotted_years))]
        counts.plot(kind='bar', ax=ax, width=0.8, color=colors)
        
        ax.set_ylabel('Gyakoriság (Count)')
        plt.xticks(rotation=45, ha='right')

    ax.set_xlabel(f"{selected_variable}{log_note}")
    ax.spines[['top', 'right']].set_visible(False)

    if is_continuous:
        if use_log:
            ax.ticklabel_format(style="plain", axis="x")
            ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
        else:
            ax.ticklabel_format(style="plain", axis="x")
            ax.xaxis.set_major_formatter(
                FuncFormatter(lambda v, _: f"{v:,.2f}" if is_monetary else f"{v:,.0f}")
            )

    if len(plotted_years) > 0:
        ax.legend(title='Év', frameon=False)
        
    plt.tight_layout()
    st.pyplot(fig)

    # --- Summary text ---
    unit_note = "millió Ft" if (is_monetary and not use_log) else ("ln egység" if use_log else "nyers egység")
    tail_note = f"Kézi határok: [{manual_min:,.2f}, {manual_max:,.2f}]" if tail_mode == "Kézi minimum/maximum" else "Nincs szűrés"
    if not is_continuous:
        tail_note = "Kategorikus változó"
        
    kkv_note = ", ".join(allowed_cats)
    st.markdown(
        f"**Minta:** {selected_industry} · **KKV:** {kkv_note} · **Változó:** `{selected_variable}` · "
        f"**Megfigyelések:** {len(filtered_df):,} · **Szélek:** {tail_note} · **Egység:** {unit_note}"
    )

# ----------------------------- Section 2: Cohort Analysis -----------------------------

continuous_vars = [v for v in available_variables if pd.api.types.is_numeric_dtype(df[v]) and df[v].nunique() > 15]
if not continuous_vars:
    continuous_vars = available_variables

st.markdown("---")
col_settings_c, col_sep_c, col_viz_c = st.columns([4, 2, 12])

with col_sep_c:
    st.markdown(
        '<div style="border-left: 1px solid #e0e0e0; height: 100%; min-height: 400px; margin: 0 auto;"></div>',
        unsafe_allow_html=True,
    )

with col_settings_c:
    st.header('Kohorszok időbeli alakulása')
    
    selected_industry_c = st.selectbox(
        'Ágazat', 
        industries,
        index=industries.index(persist("p11_ind_c", default_industry)),
        key="_p11_ind_c", on_change=save, args=("p11_ind_c",)
    )
    
    selected_variable_c = st.selectbox(
        'Változó', 
        continuous_vars,
        index=continuous_vars.index(persist("p11_var_c", continuous_vars[0])) if persist("p11_var_c", continuous_vars[0]) in continuous_vars else 0,
        key="_p11_var_c", on_change=save, args=("p11_var_c",)
    )

    sme_all_c, sme_micro_c, sme_small_c, sme_medium_c, sme_large_c = utils.sme_filter_ui("p11_c", persist, save)

    only_once_c = st.checkbox(
        "Csak legfeljebb egyszer kezelt cégek",
        value=persist("p11_only_once_c", False),
        key="_p11_only_once_c", on_change=save, args=("p11_only_once_c",)
    )
    
if selected_industry_c == "Összes ágazat":
    base_c = df.copy()
else:
    base_c = df[df['nace2_in_2015'] == selected_industry_c].copy()

base_c, allowed_cats_c = utils.apply_sme_filter(base_c, sme_all_c, sme_micro_c, sme_small_c, sme_medium_c, sme_large_c)

if only_once_c and 'received_grant' in base_c.columns:
    grant_counts = base_c.groupby('id')['received_grant'].sum()
    valid_ids = grant_counts[grant_counts <= 1].index
    base_c = base_c[base_c['id'].isin(valid_ids)]

def sort_cohorts(cohorts):
    c_years = sorted([c for c in cohorts if c != "Sosem kezelt"])
    c_never = ["Sosem kezelt"] if "Sosem kezelt" in cohorts else []
    return c_years + c_never

available_cohorts = sort_cohorts(base_c['cohort'].unique())

with col_settings_c:
    default_cohorts = []
    first_treated = next((c for c in available_cohorts if c != "Sosem kezelt"), None)
    if first_treated:
        default_cohorts.append(first_treated)
    if "Sosem kezelt" in available_cohorts:
        default_cohorts.append("Sosem kezelt")

    saved_cohorts = persist("p11_cohorts", default_cohorts)
    valid_cohorts = [c for c in saved_cohorts if c in available_cohorts]
    if not valid_cohorts and available_cohorts:
        valid_cohorts = default_cohorts if default_cohorts else available_cohorts
        
    selected_cohorts = st.multiselect(
        'Megjelenítendő kohorszok',
        available_cohorts,
        default=valid_cohorts,
        key="_p11_cohorts", on_change=save, args=("p11_cohorts",)
    )

is_monetary_c = selected_variable_c in MONETARY_VARS

with col_settings_c:
    with st.expander("Szélsőérték-kezelés (Kohorsz)"):
        saved_tail_c = persist("p11_tail_c", "Nincs szűrés (összes érték)")
        tail_idx_c = utils.FILTER_OPTIONS.index(saved_tail_c) if saved_tail_c in utils.FILTER_OPTIONS else 0
        tail_mode_c = st.selectbox("Y szélsőérték-kezelése", utils.FILTER_OPTIONS, index=tail_idx_c, key="_p11_tail_c", on_change=save, args=("p11_tail_c",))
        
        x_raw_c = base_c[selected_variable_c].replace([np.inf, -np.inf], np.nan).dropna()
        if is_monetary_c:
            x_raw_c = x_raw_c / 1000.0

        if tail_mode_c == "Kézi minimum/maximum" and not x_raw_c.empty:
            st.markdown("**Kézi határok (a megjelenített egységben)**")
            current_min_c = float(np.nanmin(x_raw_c))
            current_max_c = float(np.nanmax(x_raw_c))
            manual_min_c = st.number_input("Minimum", value=current_min_c, step=(current_max_c - current_min_c) / 100 if current_max_c > current_min_c else 1.0, key="min_c")
            manual_max_c = st.number_input("Maximum", value=current_max_c, step=(current_max_c - current_min_c) / 100 if current_max_c > current_min_c else 1.0, key="max_c")
            if manual_min_c > manual_max_c:
                st.error("A minimum nem lehet nagyobb a maximum­nál.")
                manual_min_c, manual_max_c = manual_max_c, manual_min_c
        else:
            manual_min_c, manual_max_c = None, None

    with st.expander("Skála (Kohorsz)"):
        use_log_c = st.checkbox("Logaritmikus (ln) transzformáció", value=persist("p11_log_c", False), key="_p11_log_c", on_change=save, args=("p11_log_c",))

base_c = base_c[base_c['cohort'].isin(selected_cohorts)]

if is_monetary_c:
    base_c[selected_variable_c] = base_c[selected_variable_c] / 1000.0

filtered_series_c = utils.apply_filter(base_c[selected_variable_c], tail_mode_c, manual_min_c, manual_max_c)
filtered_df_c = base_c.loc[filtered_series_c.index].copy()
filtered_df_c[selected_variable_c] = filtered_series_c

log_note_c = ""
show_plot_c = True
if use_log_c:
    pos_mask_c = filtered_df_c[selected_variable_c] > 0
    if not np.any(pos_mask_c):
        st.warning("Logaritmikus ábrázoláshoz pozitív értékek szükségesek. (Kohorsz)")
        show_plot_c = False
    else:
        filtered_df_c = filtered_df_c[pos_mask_c]
        filtered_df_c[selected_variable_c] = np.log(filtered_df_c[selected_variable_c])
        log_note_c = " (ln)"

if filtered_df_c.empty:
    show_plot_c = False

with col_viz_c:
    st.header(f'"{selected_variable_c}" átlaga kohorszonként')

    if show_plot_c:
        fig_c, ax_c = plt.subplots(figsize=(10, 6))
        
        agg_df = filtered_df_c.groupby(['year', 'cohort'])[selected_variable_c].mean().unstack()
        
        if agg_df.empty:
            st.warning("Nincs elegendő adat az ábrázoláshoz.")
        else:
            sorted_cols = sort_cohorts(agg_df.columns)
            agg_df = agg_df[sorted_cols]
            
            color_idx = 0
            for col in sorted_cols:
                if not agg_df[col].isna().all():
                    valid_data = agg_df[col].dropna()
                    if col == "Sosem kezelt":
                        line_color = "red"
                    else:
                        line_color = utils.COLORS[color_idx % len(utils.COLORS)]
                        color_idx += 1
                    ax_c.plot(valid_data.index, valid_data, marker='o', linewidth=2, label=col, color=line_color)
                
            ax_c.set_ylabel(f"{selected_variable_c}{log_note_c} (Átlag)")
            ax_c.set_xlabel("Év")
            all_years_c = sorted(filtered_df_c['year'].unique())
            if len(all_years_c) > 0:
                ax_c.set_xticks(all_years_c)
            ax_c.spines[['top', 'right']].set_visible(False)
            
            if use_log_c:
                ax_c.ticklabel_format(style="plain", axis="y")
                ax_c.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
            else:
                ax_c.ticklabel_format(style="plain", axis="y")
                ax_c.yaxis.set_major_formatter(
                    FuncFormatter(lambda v, _: f"{v:,.2f}" if is_monetary_c else f"{v:,.0f}")
                )

            ax_c.legend(title='Kohorsz', frameon=False, bbox_to_anchor=(1.05, 1), loc='upper left')
            plt.tight_layout()
            st.pyplot(fig_c)

            unit_note_c = "millió Ft" if (is_monetary_c and not use_log_c) else ("ln egység" if use_log_c else "nyers egység")
            tail_note_c = f"Kézi határok: [{manual_min_c:,.2f}, {manual_max_c:,.2f}]" if tail_mode_c == "Kézi minimum/maximum" else "Nincs szűrés"
                
            kkv_note_c = ", ".join(allowed_cats_c)
            st.markdown(
                f"**Minta (Kohorsz):** {selected_industry_c} · **KKV:** {kkv_note_c} · **Változó:** `{selected_variable_c}` · "
                f"**Megfigyelések:** {len(filtered_df_c):,} · **Szélek:** {tail_note_c} · **Egység:** {unit_note_c}"
            )

            st.subheader("Cégek száma kohorszonként")
            cohort_counts = filtered_df_c.groupby('cohort')['id'].nunique().reindex(sorted_cols).reset_index()
            cohort_counts.columns = ['Kohorsz', 'Cégek száma (egyedi ID)']
            st.dataframe(cohort_counts, hide_index=True)

            # --- Event-Time Plot ---
            st.markdown("---")
            st.subheader(f'"{selected_variable_c}" átlaga eseményidő szerint')
            
            event_df = filtered_df_c[filtered_df_c['cohort'] != "Sosem kezelt"].copy()
            
            if event_df.empty:
                st.warning("Nincs elegendő (kezelt) adat az eseményidős ábrázoláshoz.")
            else:
                event_df['relative_year'] = event_df['year'] - event_df['cohort_year']
                agg_ev_df = event_df.groupby(['relative_year', 'cohort'])[selected_variable_c].mean().unstack()
                
                if agg_ev_df.empty:
                    st.warning("Nincs elegendő adat az eseményidős ábrázoláshoz.")
                else:
                    fig_ev, ax_ev = plt.subplots(figsize=(10, 6))
                    event_sorted_cols = [c for c in sorted_cols if c != "Sosem kezelt"]
                    
                    color_idx = 0
                    for col in event_sorted_cols:
                        if col in agg_ev_df.columns and not agg_ev_df[col].isna().all():
                            valid_data = agg_ev_df[col].dropna()
                            line_color = utils.COLORS[color_idx % len(utils.COLORS)]
                            ax_ev.plot(valid_data.index, valid_data, marker='o', linewidth=2, label=col, color=line_color)
                            color_idx += 1
                            
                    ax_ev.axvline(0, color='gray', linestyle='--', alpha=0.7, label='Kezelés éve (t=0)')
                    ax_ev.set_ylabel(f"{selected_variable_c}{log_note_c} (Átlag)")
                    ax_ev.set_xlabel("Relatív év (Eseményidő)")
                    
                    all_rel_years = sorted(event_df['relative_year'].dropna().unique())
                    if len(all_rel_years) > 0:
                        ax_ev.set_xticks(range(int(min(all_rel_years)), int(max(all_rel_years)) + 1))
                        
                    ax_ev.spines[['top', 'right']].set_visible(False)
                    
                    if use_log_c:
                        ax_ev.ticklabel_format(style="plain", axis="y")
                        ax_ev.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
                    else:
                        ax_ev.ticklabel_format(style="plain", axis="y")
                        ax_ev.yaxis.set_major_formatter(
                            FuncFormatter(lambda v, _: f"{v:,.2f}" if is_monetary_c else f"{v:,.0f}")
                        )

                    ax_ev.legend(title='Kohorsz', frameon=False, bbox_to_anchor=(1.05, 1), loc='upper left')
                    plt.tight_layout()
                    st.pyplot(fig_ev)
    else:
        st.warning("Nincs megjeleníthető adat a kohorsz ábrához.")
