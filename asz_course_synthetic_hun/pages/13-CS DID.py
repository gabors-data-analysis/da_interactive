import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter

import utils
from diff_diff import CallawaySantAnna
from src import figures


# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state:
        st.session_state[key] = default
    return st.session_state[key]


def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]


# ----------------------------- Helpers ------------------------------
MONETARY_VARS = [
    "sales_clean23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23",
    "pretax23", "ereduzem23", "jetok23", "grant_value", "export23"
]

VARIABLE_LIST = [
    "emp", "sales_clean23", "received_grant", "foundyear", "county", "firm_owner",
    "pretax23", "ereduzem23", "export23", "tanass_clean23", "persexp_clean23",
    "eszk23", "rlk23", "jetok23", "grant_value"
]


def get_panel_data():
    if "panel_data" not in st.session_state or st.session_state["panel_data"] is None:
        st.error("Paneladatok nincsenek betöltve. Nyissa meg először az Adattábla oldalt.")
        st.stop()
    return st.session_state["panel_data"].copy()


def load_data(panel_data):
    df = panel_data.copy()

    df = utils.add_sme_category(df)

    if "received_grant" not in df.columns:
        st.error("A 'received_grant' oszlop hiányzik, így a kezelés nem definiálható.")
        st.stop()

    df["received_grant"] = df["received_grant"].fillna(0)

    treatment_years = (
        df.loc[df["received_grant"] == 1]
        .groupby("id")["year"]
        .min()
        .reset_index()
        .rename(columns={"year": "cohort_year"})
    )

    df = pd.merge(df, treatment_years, on="id", how="left")
    df["cohort_year"] = df["cohort_year"].fillna(0).astype(int)

    return df


def get_available_numeric_outcomes(df):
    out = []
    for v in VARIABLE_LIST:
        if v in df.columns and pd.api.types.is_numeric_dtype(df[v]):
            out.append(v)
    return out


def build_base_sample(df, selected_industry, sme_filters, only_once):
    if selected_industry == "Összes ágazat":
        base_df = df.copy()
    else:
        base_df = df[df["nace2_in_2015"] == selected_industry].copy()

    base_df, allowed_cats = utils.apply_sme_filter(
        base_df,
        *sme_filters,
    )

    if only_once and "received_grant" in base_df.columns:
        grant_counts = base_df.groupby("id")["received_grant"].sum()
        valid_ids_once = grant_counts[grant_counts <= 1].index
        base_df = base_df[base_df["id"].isin(valid_ids_once)].copy()

    return base_df, allowed_cats


def apply_outcome_transform(
    df,
    outcome_var,
    is_monetary,
    tail_mode,
    manual_min,
    manual_max,
    use_log,
):
    out = df.copy()

    out["_outcome_raw"] = pd.to_numeric(out[outcome_var], errors="coerce")

    if is_monetary:
        out["_outcome_raw"] = out["_outcome_raw"] / 1000.0

    filtered_series = utils.apply_filter(out["_outcome_raw"], tail_mode, manual_min, manual_max)
    out = out.loc[filtered_series.index].copy()
    out["_outcome_raw"] = filtered_series

    if use_log:
        pos_mask = out["_outcome_raw"] > 0
        out = out[pos_mask].copy()
        out["_outcome"] = np.log(out["_outcome_raw"])
    else:
        out["_outcome"] = out["_outcome_raw"]

    return out


def balance_panel_for_outcome(df, unit="id", time="year", outcome_col="_outcome"):
    d = df.copy()

    if d.duplicated([unit, time]).any():
        raise ValueError("Duplicate id-year rows found.")

    all_years = np.sort(d[time].dropna().unique())
    n_years = len(all_years)

    d["row_ok"] = np.isfinite(d[outcome_col])

    firm_check = (
        d.groupby(unit)
        .agg(
            n_years=(time, "nunique"),
            all_rows_ok=("row_ok", "all"),
        )
    )

    valid_ids = firm_check.index[
        (firm_check["n_years"] == n_years) &
        (firm_check["all_rows_ok"])
    ]

    d_balanced = (
        d.loc[d[unit].isin(valid_ids)]
        .drop(columns="row_ok")
        .copy()
    )
    d_balanced["year_cat"] = d_balanced[time].astype(str)

    return d_balanced, all_years


def format_metric_value(value):
    if isinstance(value, float):
        return f"{value:,.2f}"
    return str(value)


# ----------------------------- Setup ------------------------------
col_settings, col_viz = utils.setup_page(
    "Callaway-Sant'Anna-féle DiD",
    "Callaway-Sant'Anna-féle DiD (szimulált)",
)

df = load_data(get_panel_data())

available_outcomes = get_available_numeric_outcomes(df)
if not available_outcomes:
    st.error("Nincs megfelelő numerikus kimeneti változó az adatban.")
    st.stop()

st.markdown("""
Válasszon **ágazatot**, **KKV-szűrést**, **kezelt kohorszokat** és egy **kimeneti változót**.
A pénzügyi változók **millió forintban** jelennek meg.
A kiválasztott minta a becslés előtt automatikusan **kiegyensúlyozásra** kerül a végső modellezett kimenet alapján.
""")


# ----------------------------- Sidebar / Settings -----------------------------
with col_settings:
    st.header("Beállítások")

    if "nace2_in_2015" not in df.columns:
        st.error("'nace2_in_2015' oszlop nem található.")
        st.stop()

    industries = ["Összes ágazat"] + sorted(df["nace2_in_2015"].dropna().unique())
    default_industry = industries[0]

    selected_industry = st.selectbox(
        "Ágazat",
        industries,
        index=industries.index(persist("p13_industry", default_industry)),
        key="_p13_industry", on_change=save, args=("p13_industry",),
    )

    selected_variable = st.selectbox(
        "Kimeneti változó",
        available_outcomes,
        index=available_outcomes.index(persist("p13_var", available_outcomes[0]))
        if persist("p13_var", available_outcomes[0]) in available_outcomes else 0,
        key="_p13_var", on_change=save, args=("p13_var",),
    )

    sme_all, sme_micro, sme_small, sme_medium, sme_large = utils.sme_filter_ui("p13", persist, save)

    only_once = st.checkbox(
        "Csak legfeljebb egyszer kezelt cégek",
        value=persist("p13_only_once", False),
        key="_p13_only_once", on_change=save, args=("p13_only_once",),
    )

    cg_options = ["Sosem kezelt", "Még nem kezelt"]
    control_group_ui = st.radio(
        "Kontrollcsoport",
        options=cg_options,
        index=cg_options.index(persist("p13_control_type", cg_options[0]))
        if persist("p13_control_type", cg_options[0]) in cg_options else 0,
        key="_p13_control_type", on_change=save, args=("p13_control_type",),
    )

    with st.expander("Szélsőérték-kezelés"):
        saved_tail = persist("p13_tail", "Nincs szűrés (összes érték)")
        tail_idx = utils.FILTER_OPTIONS.index(saved_tail) if saved_tail in utils.FILTER_OPTIONS else 0
        tail_mode = st.selectbox(
            "Y szélsőérték-kezelése",
            utils.FILTER_OPTIONS,
            index=tail_idx,
            key="_p13_tail", on_change=save, args=("p13_tail",),
        )

        x_raw = pd.to_numeric(df[selected_variable], errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
        is_monetary = selected_variable in MONETARY_VARS
        if is_monetary:
            x_raw = x_raw / 1000.0

        if tail_mode == "Kézi minimum/maximum" and not x_raw.empty:
            st.markdown("**Kézi határok (a megjelenített egységben)**")
            current_min = float(np.nanmin(x_raw))
            current_max = float(np.nanmax(x_raw))
            manual_min = st.number_input(
                "Minimum",
                value=current_min,
                step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                key="p13_manual_min",
            )
            manual_max = st.number_input(
                "Maximum",
                value=current_max,
                step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                key="p13_manual_max",
            )
            if manual_min > manual_max:
                st.error("A minimum nem lehet nagyobb a maximumnál.")
                manual_min, manual_max = manual_max, manual_min
        else:
            manual_min = None
            manual_max = None

    with st.expander("Skála"):
        use_log = st.checkbox(
            "Logaritmikus (ln) transzformáció",
            value=persist("p13_log", False),
            key="_p13_log", on_change=save, args=("p13_log",),
        )

    st.subheader("Event-study ablak")
    n_pre = st.number_input(
        "Megjelenített pre periódusok száma",
        min_value=1,
        value=int(persist("p13_n_pre", 3)),
        step=1,
        key="_p13_n_pre", on_change=save, args=("p13_n_pre",),
    )
    n_post = st.number_input(
        "Megjelenített post periódusok száma",
        min_value=1,
        value=int(persist("p13_n_post", 3)),
        step=1,
        key="_p13_n_post", on_change=save, args=("p13_n_post",),
    )


# ----------------------------- Build sample before cohort selection -----------------------------
sme_filters = (sme_all, sme_micro, sme_small, sme_medium, sme_large)
base_df, allowed_cats = build_base_sample(df, selected_industry, sme_filters, only_once)

if base_df.empty:
    st.error("Nincs adat a választott szűrési feltételekhez.")
    st.stop()

panel_start_year = int(base_df["year"].min())
available_cohorts = sorted([
    int(x) for x in base_df["cohort_year"].unique()
    if x > panel_start_year
])

with col_settings:
    if not available_cohorts:
        st.error("Nincs olyan kezelt kohorsz a választott mintában, amelyhez megfigyelhető a kezelés előtti év (t-1).")
        st.stop()

    st.caption(
        f"A {panel_start_year}. évi kohorsz nem választható, mert a panel {panel_start_year}-ben indul, "
        "így ehhez a kohorszhoz nem figyelhető meg a kezelés előtti év (t-1)."
    )

    saved_cohorts = persist("p13_cohorts", available_cohorts)
    valid_saved_cohorts = [c for c in saved_cohorts if c in available_cohorts]
    if not valid_saved_cohorts:
        valid_saved_cohorts = available_cohorts

    selected_cohorts = st.multiselect(
        "Becsléshez használt kezelt kohorszok",
        available_cohorts,
        default=valid_saved_cohorts,
        key="_p13_cohorts", on_change=save, args=("p13_cohorts",),
    )

if not selected_cohorts:
    selected_cohorts = available_cohorts

# Keep selected treated cohorts + never-treated
estimation_df = base_df[
    base_df["cohort_year"].isin(selected_cohorts) | (base_df["cohort_year"] == 0)
].copy()

if estimation_df.empty:
    st.error("A kiválasztott kohorszokkal nem maradt becslési minta.")
    st.stop()

is_monetary = selected_variable in MONETARY_VARS

# Apply trimming / transform first
estimation_df = apply_outcome_transform(
    estimation_df,
    outcome_var=selected_variable,
    is_monetary=is_monetary,
    tail_mode=tail_mode,
    manual_min=manual_min,
    manual_max=manual_max,
    use_log=use_log,
)

if estimation_df.empty:
    st.warning("A kiválasztott transzformáció és szűrés után nem maradt adat.")
    st.stop()

# Balance after trimming / transform
try:
    df_balanced, all_years = balance_panel_for_outcome(
        estimation_df,
        unit="id",
        time="year",
        outcome_col="_outcome",
    )
except ValueError as e:
    st.error(str(e))
    st.stop()

if df_balanced.empty:
    st.warning("A kiegyensúlyozás után nem maradt becsülhető minta.")
    st.stop()

balanced_treated_cohorts = sorted([int(x) for x in df_balanced["cohort_year"].unique() if x > 0])
if not balanced_treated_cohorts:
    st.warning("A kiegyensúlyozott mintában nem maradt kezelt kohorsz.")
    st.stop()

control_group_param = "never_treated" if control_group_ui == "Sosem kezelt" else "not_yet_treated"

# ----------------------------- Estimation -----------------------------
with st.spinner("Callaway-Sant'Anna becslés fut..."):
    try:
        cs_model = CallawaySantAnna(
            control_group=control_group_param,
            anticipation=0,
            base_period="universal",
        )

        results_cs = cs_model.fit(
            df_balanced,
            outcome="_outcome",
            unit="id",
            time="year",
            first_treat="cohort_year",
            aggregate="all",
        )

        gt_df = results_cs.to_dataframe()
        event_study_effects = results_cs.event_study_effects

    except Exception as e:
        st.error(f"A becslés nem sikerült: {e}")
        st.stop()


# ----------------------------- Results / Summary -----------------------------
with col_viz:
    st.header("Callaway-Sant'Anna eredmények")

    firms_before_balance = estimation_df["id"].nunique()
    firms_after_balance = df_balanced["id"].nunique()
    treated_firms_after_balance = df_balanced.loc[df_balanced["cohort_year"] > 0, "id"].nunique()
    never_treated_firms_after_balance = df_balanced.loc[df_balanced["cohort_year"] == 0, "id"].nunique()

    unit_note = "millió Ft" if (is_monetary and not use_log) else ("ln egység" if use_log else "nyers egység")
    tail_note = (
        f"Kézi határok: [{manual_min:,.2f}, {manual_max:,.2f}]"
        if tail_mode == "Kézi minimum/maximum" else tail_mode
    )
    kkv_note = ", ".join(allowed_cats)

    st.markdown(
        f"**Minta:** {selected_industry} | **KKV:** {kkv_note} | **Változó:** `{selected_variable}` | "
        f"**Kontroll:** {control_group_ui} | **Egység:** {unit_note}"
    )

    summary_df = pd.DataFrame(
        {
            "Mutató": [
                "Kiválasztott kezelt kohorszok",
                "Kiegyensúlyozott mintában maradt kezelt kohorszok",
                "Cégek száma a kiegyensúlyozás előtt",
                "Cégek száma a kiegyensúlyozás után",
                "Kezelt cégek száma a kiegyensúlyozott mintában",
                "Sosem kezelt cégek száma a kiegyensúlyozott mintában",
                "Megfigyelések száma a kiegyensúlyozott mintában",
                "Évek a kiegyensúlyozott panelben",
                "Szélsőérték-kezelés",
                "Logaritmikus transzformáció",
            ],
            "Érték": [
                ", ".join(map(str, selected_cohorts)),
                ", ".join(map(str, balanced_treated_cohorts)),
                f"{firms_before_balance:,}",
                f"{firms_after_balance:,}",
                f"{treated_firms_after_balance:,}",
                f"{never_treated_firms_after_balance:,}",
                f"{len(df_balanced):,}",
                f"{int(np.min(all_years))}-{int(np.max(all_years))} ({len(all_years)} év)",
                tail_note,
                "Igen" if use_log else "Nem",
            ],
        }
    )
    st.table(summary_df.set_index("Mutató"))

    cohort_counts = (
        df_balanced.groupby("cohort_year")["id"]
        .nunique()
        .reset_index(name="Cégek száma")
        .sort_values("cohort_year")
        .copy()
    )
    cohort_counts["Kohorsz"] = cohort_counts["cohort_year"].apply(
        lambda x: "Sosem kezelt" if x == 0 else str(int(x))
    )
    cohort_counts = cohort_counts[["Kohorsz", "Cégek száma"]]

    st.subheader("Kohorszméretek a kiegyensúlyozott mintában")
    st.dataframe(cohort_counts, hide_index=True, use_container_width=True)

    st.subheader("ATT(g,t) tábla")
    gt_display = gt_df.copy()
    if {"group", "time"}.issubset(gt_display.columns):
        gt_display = gt_display.sort_values(["group", "time"])
    st.dataframe(gt_display, use_container_width=True, hide_index=True)

    st.markdown("---")
    st.subheader("Kohorszspecifikus ATT(g,t) ábrák")

    gt_plot_df = gt_df.copy()
    if "group" in gt_plot_df.columns:
        gt_plot_df = gt_plot_df[gt_plot_df["group"].isin(balanced_treated_cohorts)].copy()

    try:
        fig_gt, _ = figures.plot_cs_cohorts(
            gt_plot_df,
            cohorts=balanced_treated_cohorts,
            x="event_time",
            n_pre=int(n_pre),
            n_post=int(n_post),
            ncols=3,
        )
        st.pyplot(fig_gt)
    except Exception as e:
        st.warning(f"A kohorszspecifikus ábra nem rajzolható ki: {e}")

    st.markdown("---")
    st.subheader("Eseményidős ábra")

    try:
        fig_es, ax_es, df_es_plot = figures.plot_cs_event_study(
            event_study_effects,
            n_pre=int(n_pre),
            n_post=int(n_post),
            title="Callaway-Sant'Anna-féle eseményidős hatások",
        )

        if use_log:
            ax_es.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
        else:
            ax_es.yaxis.set_major_formatter(
                FuncFormatter(lambda v, _: f"{v:,.2f}" if is_monetary else f"{v:,.2f}")
            )

        st.pyplot(fig_es)

        st.subheader("Eseményidős tábla")
        st.dataframe(df_es_plot, use_container_width=True, hide_index=True)

    except Exception as e:
        st.warning(f"Az eseményidős ábra nem rajzolható ki: {e}")
