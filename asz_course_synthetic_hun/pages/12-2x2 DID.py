import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import utils
import statsmodels.api as sm

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# ----------------------------- Setup ------------------------------
col_settings, col_viz = utils.setup_page(
    "Különbségek különbsége (2x2 DiD)",
    "Különbségek különbsége (2x2 DiD, szimulált)"
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
    else:
        df['cohort_year'] = np.nan
        
    return df

df = load_data(get_panel_data())

st.markdown("""
Válasszon **ágazatot**, egy **kezelési évet (t)**, egy **késleltetést (k)** és egy **kimeneti változót**.
Az oldal a kiválasztott változó átlagát hasonlítja össze a kezelés előtti évben *(t-1)* és a kezelés utáni évben *(t+k)* a kezelt és a kontrollcsoport között.
A pénzügyi adatok **millió forintban** jelennek meg.
""")

# --- VAR CONFIG ---
MONETARY_VARS = ["sales_clean23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23", "pretax23","ereduzem23", "jetok23", "grant_value","export23"]
variable_list = ["emp", "sales_clean23", "received_grant", "foundyear", "county", "firm_owner","pretax23","ereduzem23",
                  "export23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23", "jetok23", "grant_value"]
available_variables = [v for v in variable_list if v in df.columns]

if not available_variables:
    st.error("Nincsenek megfelelő folytonos változók az adatban.")
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
        index=industries.index(persist("p12_industry", default_industry)),
        key="_p12_industry", on_change=save, args=("p12_industry",)
    )
    
    # 2. Choose years
    if 'year' not in df.columns:
        st.error("'year' oszlop nem található.")
        st.stop()
    all_years = sorted(df['year'].dropna().unique())
    
    if len(all_years) < 3:
        st.error("Nincs elég év az adatban a DiD elemzéshez (legalább 3 év szükséges).")
        st.stop()
        
    # t-1 and t+k should be valid. Thus t can't be the first year.
    valid_t_years = all_years[1:]
    
    t_year = st.selectbox(
        'Kezelés éve (t)',
        valid_t_years,
        index=valid_t_years.index(persist("p12_tyear", valid_t_years[0])) if persist("p12_tyear", valid_t_years[0]) in valid_t_years else 0,
        key="_p12_tyear", on_change=save, args=("p12_tyear",)
    )
    
    max_k = max(all_years) - t_year
    if max_k < 0: max_k = 0
    
    k_range = list(range(0, max_k + 1))
    if len(k_range) == 0:
        st.error("Nincs elérhető későbbi év a választott kezelési évhez.")
        st.stop()
        
    k_val = st.selectbox(
        'Késleltetés (k év)',
        k_range,
        index=k_range.index(persist("p12_kval", min(2, max_k))) if persist("p12_kval", min(2, max_k)) in k_range else 0,
        key="_p12_kval", on_change=save, args=("p12_kval",)
    )
    
    # 3. Choose a variable
    selected_variable = st.selectbox(
        'Kimeneti Változó', 
        available_variables,
        index=available_variables.index(persist("p12_var", available_variables[0])),
        key="_p12_var", on_change=save, args=("p12_var",)
    )

    sme_all, sme_micro, sme_small, sme_medium, sme_large = utils.sme_filter_ui("p12", persist, save)

    only_once = st.checkbox(
        "Csak legfeljebb egyszer kezelt cégek",
        value=persist("p12_only_once", False),
        key="_p12_only_once", on_change=save, args=("p12_only_once",)
    )
    
    cg_options = ["Sosem kezelt", "Még nem kezelt"]
    control_group_type = st.radio(
        "Kontrollcsoport",
        options=cg_options,
        index=cg_options.index(persist("p12_control_type", cg_options[0])) if persist("p12_control_type", cg_options[0]) in cg_options else 0,
        key="_p12_control_type", on_change=save, args=("p12_control_type",)
    )

# Base filtering
if selected_industry == "Összes ágazat":
    base_df = df.copy()
else:
    base_df = df[df['nace2_in_2015'] == selected_industry].copy()

base_df, allowed_cats = utils.apply_sme_filter(base_df, sme_all, sme_micro, sme_small, sme_medium, sme_large)

if only_once and 'received_grant' in base_df.columns:
    grant_counts = base_df.groupby('id')['received_grant'].sum()
    valid_ids_once = grant_counts[grant_counts <= 1].index
    base_df = base_df[base_df['id'].isin(valid_ids_once)]

if base_df.empty:
    st.error("Nincs adat a választott szűrési feltételekhez.")
    st.stop()

is_monetary = selected_variable in MONETARY_VARS

# Scale monetary variables (1000 HUF -> Million HUF)
if is_monetary:
    base_df[selected_variable] = base_df[selected_variable] / 1000.0

with col_settings:
    with st.expander("Szélsőérték-kezelés"):
        saved_tail = persist("p12_tail", "Nincs szűrés (összes érték)")
        tail_idx = utils.FILTER_OPTIONS.index(saved_tail) if saved_tail in utils.FILTER_OPTIONS else 0
        tail_mode = st.selectbox("Y szélsőérték-kezelése", utils.FILTER_OPTIONS, index=tail_idx, key="_p12_tail", on_change=save, args=("p12_tail",))

        x_raw = base_df[selected_variable].replace([np.inf, -np.inf], np.nan).dropna()
        
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
        use_log = st.checkbox("Logaritmikus (ln) transzformáció", value=persist("p12_log", False), key="_p12_log", on_change=save, args=("p12_log",))

# Apply filter / transforms
filtered_series = utils.apply_filter(base_df[selected_variable], tail_mode, manual_min, manual_max)
filtered_df = base_df.loc[filtered_series.index].copy()
filtered_df[selected_variable] = filtered_series

log_note = ""
if use_log:
    pos_mask = filtered_df[selected_variable] > 0
    if not np.any(pos_mask):
        st.warning("Logaritmikus ábrázoláshoz pozitív értékek szükségesek. A szűrés után nem maradt pozitív érték.")
        st.stop()
    filtered_df = filtered_df[pos_mask]
    filtered_df[selected_variable] = np.log(filtered_df[selected_variable])
    log_note = " (ln)"

# --- Calculate DiD ---
year_pre = t_year - 1
year_post = t_year + k_val

# Filter firms observed in both periods
pre_ids = filtered_df[filtered_df['year'] == year_pre]['id'].unique()
post_ids = filtered_df[filtered_df['year'] == year_post]['id'].unique()
valid_ids = set(pre_ids).intersection(post_ids)

filtered_df = filtered_df[filtered_df['id'].isin(valid_ids)]

# Determine groups based on treatment year
if 'id' not in base_df.columns or 'cohort_year' not in base_df.columns:
    st.error("Hiányzik az 'id' vagy a 'cohort_year' oszlop az adatokból.")
    st.stop()

# Treatment assignment in year t
treated_ids = base_df[base_df['cohort_year'] == t_year]['id'].unique()

if control_group_type.startswith("Még nem kezelt"):
    control_ids = base_df[(base_df['cohort_year'] > year_post)  ]['id'].unique()
else:
    control_ids = base_df[base_df['cohort_year'].isna()]['id'].unique()

# Pre-period means
pre_data = filtered_df[filtered_df['year'] == year_pre]
mean_treated_pre = pre_data[pre_data['id'].isin(treated_ids)][selected_variable].mean()
mean_control_pre = pre_data[pre_data['id'].isin(control_ids)][selected_variable].mean()
n_treated_pre = pre_data[pre_data['id'].isin(treated_ids)][selected_variable].count()
n_control_pre = pre_data[pre_data['id'].isin(control_ids)][selected_variable].count()

# Post-period means
post_data = filtered_df[filtered_df['year'] == year_post]
mean_treated_post = post_data[post_data['id'].isin(treated_ids)][selected_variable].mean()
mean_control_post = post_data[post_data['id'].isin(control_ids)][selected_variable].mean()

# Check if data is sufficient
if np.isnan(mean_treated_pre) or np.isnan(mean_control_pre) or np.isnan(mean_treated_post) or np.isnan(mean_control_post):
    st.warning("Nincs elég adat a kiválasztott években az átlagok kiszámításához (lehet, hogy a szűrés miatt eltűntek a megfigyelések).")
else:
    # ----------------------------- Plot -------------------------------
    with col_viz:
        st.header(f'2x2 DiD eredmények: "{selected_variable}"')
        st.markdown(f"**Kezelési év (t):** {t_year} | **Összehasonlítás:** {year_pre} (t-1) vs. {year_post} (t+{k_val})")

        fig, ax = plt.subplots(figsize=(8, 5))
        
        years_plot = [year_pre, year_post]
        treated_means = [mean_treated_pre, mean_treated_post]
        control_means = [mean_control_pre, mean_control_post]
        
        control_label = "Sosem kezelt (Kontroll)" if control_group_type == "Sosem kezelt" else "Még nem kezelt (Kontroll)"
        
        # Plot lines
        ax.plot(years_plot, treated_means, marker='o', markersize=8, linewidth=2, color=utils.COLORS[0], label='Támogatott (Kezelt)')
        ax.plot(years_plot, control_means, marker='o', markersize=8, linewidth=2, color=utils.COLORS[1], label=control_label)
        
        # Counterfactual
        diff_control = mean_control_post - mean_control_pre
        counterfactual_post = mean_treated_pre + diff_control
        ax.plot([year_pre, year_post], [mean_treated_pre, counterfactual_post], marker='', linestyle='--', linewidth=2, color=utils.COLORS[0], alpha=0.6, label='Támogatott (Párhuzamos trend)')

        # Annotations for DiD
        did_effect = (mean_treated_post - mean_treated_pre) - diff_control
        
        ax.annotate(f"DiD Hatás:\n{did_effect:,.2f}", 
                    xy=(year_post, (mean_treated_post + counterfactual_post) / 2),
                    xytext=(year_post + 0.1, (mean_treated_post + counterfactual_post) / 2),
                    # arrowprops=dict(arrowstyle="<->", color='black'),
                    va='center')

        # Formatting
        ax.set_xticks(years_plot)
        ax.set_xticklabels([f"{year_pre}\n(t-1)", f"{year_post}\n(t+{k_val})"])
        
        ax.set_ylabel(f"{selected_variable}{log_note}")
        ax.spines[['top', 'right']].set_visible(False)
        
        if use_log:
            ax.ticklabel_format(style="plain", axis="y")
            ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
        else:
            ax.ticklabel_format(style="plain", axis="y")
            ax.yaxis.set_major_formatter(
                FuncFormatter(lambda v, _: f"{v:,.2f}" if is_monetary else f"{v:,.0f}")
            )

        # Relocate legend
        ax.legend(loc='lower center', bbox_to_anchor=(0.5, -0.2), ncol=3, frameon=False)
        plt.tight_layout()
        st.pyplot(fig)

        # --- Summary text ---
        unit_note = "millió Ft" if (is_monetary and not use_log) else ("ln egység" if use_log else "nyers egység")
        tail_note = f"Kézi határok: [{manual_min:,.2f}, {manual_max:,.2f}]" if tail_mode == "Kézi minimum/maximum" else "Nincs szűrés"
            
        kkv_note = ", ".join(allowed_cats)
        st.markdown(
            f"**Minta:** {selected_industry} · **KKV:** {kkv_note} · **Változó:** `{selected_variable}` · "
            f"**Szélek:** {tail_note} · **Egység:** {unit_note}"
        )
        
        st.subheader("Részletes átlagok")
        stats_df = pd.DataFrame({
            "Csoport": ["Támogatott (Kezelt)", control_label],
            f"Előtte ({year_pre})": [mean_treated_pre, mean_control_pre],
            f"Utána ({year_post})": [mean_treated_post, mean_control_post],
            "Különbség (Utána - Előtte)": [mean_treated_post - mean_treated_pre, mean_control_post - mean_control_pre],
            "Cégek száma": [n_treated_pre, n_control_pre]
        })
        
        def _fmt(v):
            if pd.isna(v): return "NaN"
            if use_log: return f"{v:,.3f}"
            return f"{v:,.2f}" if is_monetary else f"{v:,.1f}"
            
        stats_df[f"Előtte ({year_pre})"] = stats_df[f"Előtte ({year_pre})"].apply(_fmt)
        stats_df[f"Utána ({year_post})"] = stats_df[f"Utána ({year_post})"].apply(_fmt)
        stats_df["Különbség (Utána - Előtte)"] = stats_df["Különbség (Utána - Előtte)"].apply(_fmt)
        
        st.table(stats_df.set_index("Csoport"))
        st.markdown(f"**DiD Hatás (Különbségek különbsége):** `{_fmt(did_effect)}`")

        # --- Regression Table ---
        st.markdown("---")
        st.subheader("DiD regressziós eredmények")
        
        # Prepare data for regression
        reg_df = filtered_df[
            (filtered_df['year'].isin([year_pre, year_post])) &
            (filtered_df['id'].isin(set(treated_ids).union(control_ids)))
        ].copy()
        
        reg_df['Treated'] = reg_df['id'].isin(treated_ids).astype(int)
        reg_df['Post'] = (reg_df['year'] == year_post).astype(int)
        reg_df['Treated_x_Post'] = reg_df['Treated'] * reg_df['Post']
        
        # Drop NaNs for regression
        reg_data = reg_df[['id', selected_variable, 'Treated', 'Post', 'Treated_x_Post']].dropna()
        
        if len(reg_data) < 5:
            st.warning("Nincs elegendő adat a regresszió futtatásához.")
        else:
            X = sm.add_constant(reg_data[['Treated', 'Post', 'Treated_x_Post']])
            Y = reg_data[selected_variable]
            
            model = sm.OLS(Y, X)
            try:
                # Panel data -> cluster at the firm level
                res = model.fit(cov_type='cluster', cov_kwds={'groups': reg_data['id']})
                se_note = "Zárójelben a cég szinten klaszterezett robusztus standard hibák szerepelnek."
            except Exception:
                # Fallback to HC1
                res = model.fit(cov_type='HC1')
                se_note = "Zárójelben a robusztus (HC1) standard hibák szerepelnek."
            
            def sig_stars(p):
                if p < 0.01: return "***"
                elif p < 0.05: return "**"
                elif p < 0.1: return "*"
                return ""

            def fmt_val(x):
                try:
                    if x != 0 and abs(x) < 0.001:
                        return f"{x:.3g}"
                    return f"{x:.3f}"
                except Exception:
                    return str(x)

            indices = []
            rows_coef = []

            var_names = {
                "const": "Konstans",
                "Treated": "Kezelt (Treated)",
                "Post": "Utána (Post)",
                "Treated_x_Post": "DiD (Treated × Post)"
            }

            for name in res.params.index:
                coef = res.params[name]
                se = res.bse[name]
                pval = res.pvalues[name]

                rows_coef.append(f"{fmt_val(coef)}{sig_stars(pval)} ({fmt_val(se)})")

                pretty_name = var_names.get(name, name)
                indices.append(pretty_name)
            
            indices.append("-------")
            rows_coef.append("-------")
            
            indices.append("Megfigyelések")
            rows_coef.append(f"{int(res.nobs):,}".replace(",", " "))
            
            indices.append("R²")
            rows_coef.append(f"{res.rsquared:.3f}")
            
            summary_df = pd.DataFrame({"Modell 1": rows_coef}, index=indices)
            
            left_spacer, mid_col, right_spacer = st.columns([1, 3, 1])
            with mid_col:
                st.table(summary_df, border=False)
            
            st.markdown(
                f"<span style='font-size:0.85rem'><em>Megjegyzés:</em> {se_note} "
                "Szignifikanciaszintek: *** p&lt;0.01, ** p&lt;0.05, * p&lt;0.1.</span>",
                unsafe_allow_html=True,
            )
