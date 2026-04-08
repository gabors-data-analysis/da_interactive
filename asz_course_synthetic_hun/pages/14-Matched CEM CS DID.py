import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter

import utils
from src import figures
from src.matched_estimator_class import MatchedCEMCallawaySantAnna


def persist(key, default):
    if key not in st.session_state:
        st.session_state[key] = default
    return st.session_state[key]


def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]


def checkbox_setting(label, key, default):
    return st.checkbox(
        label,
        value=persist(key, default),
        key=f"_{key}",
        on_change=save,
        args=(key,),
    )


MONETARY_VARS = [
    "sales_clean23", "tanass_clean23", "persexp_clean23", "eszk23", "rlk23",
    "pretax23", "ereduzem23", "jetok23", "grant_value", "export23",
]

VARIABLE_LIST = [
    "emp", "sales_clean23", "received_grant", "foundyear", "county", "firm_owner",
    "pretax23", "ereduzem23", "export23", "tanass_clean23", "persexp_clean23",
    "eszk23", "rlk23", "jetok23", "grant_value",
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

    base_df, allowed_cats = utils.apply_sme_filter(base_df, *sme_filters)

    if only_once and "received_grant" in base_df.columns:
        grant_counts = base_df.groupby("id")["received_grant"].sum()
        valid_ids_once = grant_counts[grant_counts <= 1].index
        base_df = base_df[base_df["id"].isin(valid_ids_once)].copy()

    return base_df, allowed_cats


def apply_outcome_transform(df, outcome_var, is_monetary, tail_mode, manual_min, manual_max, use_log):
    out = df.copy()
    out["_outcome_raw"] = pd.to_numeric(out[outcome_var], errors="coerce")

    if is_monetary:
        out["_outcome_raw"] = out["_outcome_raw"] / 1000.0

    filtered_series = utils.apply_filter(out["_outcome_raw"], tail_mode, manual_min, manual_max)
    out = out.loc[filtered_series.index].copy()
    out["_outcome_raw"] = filtered_series

    if use_log:
        out = out[out["_outcome_raw"] > 0].copy()
        out["_outcome"] = np.log(out["_outcome_raw"])
    else:
        out["_outcome"] = out["_outcome_raw"]

    return out


def balance_panel_for_outcome(df, unit="id", time="year", outcome_col="_outcome"):
    d = df.copy()

    if d.duplicated([unit, time]).any():
        raise ValueError("Duplikált id-year sorok találhatók.")

    all_years = np.sort(d[time].dropna().unique())
    n_years = len(all_years)
    d["row_ok"] = np.isfinite(d[outcome_col])

    firm_check = (
        d.groupby(unit)
        .agg(n_years=(time, "nunique"), all_rows_ok=("row_ok", "all"))
    )

    valid_ids = firm_check.index[
        (firm_check["n_years"] == n_years) & (firm_check["all_rows_ok"])
    ]

    d_balanced = d.loc[d[unit].isin(valid_ids)].drop(columns="row_ok").copy()
    d_balanced["year_cat"] = d_balanced[time].astype(str)
    return d_balanced, all_years


def parse_cutpoint_text(text):
    vals = []
    for raw in text.split(","):
        token = raw.strip().lower()
        if not token:
            continue
        if token in {"-inf", "-infinity"}:
            vals.append(-np.inf)
        elif token in {"inf", "+inf", "infinity", "+infinity"}:
            vals.append(np.inf)
        else:
            vals.append(float(token))

    if len(vals) < 2:
        raise ValueError("Legalább két cellahatár szükséges.")
    if any(vals[i] >= vals[i + 1] for i in range(len(vals) - 1)):
        raise ValueError("A cellahatároknak szigorúan növekvőnek kell lenniük.")
    return vals


def make_quantile_edges(series, n_bins):
    x = pd.to_numeric(series, errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
    if x.empty:
        return None
    qs = np.linspace(0.0, 1.0, int(n_bins) + 1)
    edges = np.unique(x.quantile(qs).to_numpy(dtype=float))
    return edges.tolist() if len(edges) >= 2 else None


def make_equal_width_edges(series, n_bins):
    x = pd.to_numeric(series, errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
    if x.empty:
        return None
    lo = float(x.min())
    hi = float(x.max())
    if lo == hi:
        return None
    return np.linspace(lo, hi, int(n_bins) + 1).tolist()


def get_matching_candidates(df, selected_variable):
    excluded = {
        "id", "year", "year_cat", "received_grant", "cohort_year",
        "_outcome", "_outcome_raw",
    }
    candidates = [c for c in VARIABLE_LIST if c in df.columns and c not in excluded]
    if selected_variable in df.columns and selected_variable not in candidates:
        candidates.insert(0, selected_variable)
    return candidates


def render_matching_controls(df, matching_vars):
    cutpoints = {}
    config_rows = []

    for var in matching_vars:
        series = df[var]
        numeric = pd.api.types.is_numeric_dtype(series)

        with st.expander(f"Celladefiníció: {var}", expanded=False):
            if not numeric:
                st.caption("Nem numerikus változó: pontos illesztés történik.")
                config_rows.append({"Változó": var, "Módszer": "pontos értékek", "Cellahatárok": ""})
                continue

            method = st.selectbox(
                "Cellaképzés módja",
                ["Kvantilis alapú cellák", "Egyenlő szélességű cellák", "Kézi cellahatárok", "Pontos értékek"],
                index=0,
                key=f"p14_match_method_{var}",
            )

            edges = None
            if method in {"Kvantilis alapú cellák", "Egyenlő szélességű cellák"}:
                n_bins = st.number_input(
                    "Cellák száma",
                    min_value=2,
                    max_value=20,
                    value=5,
                    step=1,
                    key=f"p14_match_bins_{var}",
                )
                edges = (
                    make_quantile_edges(series, n_bins)
                    if method == "Kvantilis alapú cellák"
                    else make_equal_width_edges(series, n_bins)
                )

                if edges is None:
                    st.warning("Nem sikerült használható cellahatárokat létrehozni; pontos illesztés történik.")
                else:
                    st.caption("Generált cellahatárok: " + ", ".join(f"{x:,.4g}" for x in edges))

            elif method == "Kézi cellahatárok":
                text = st.text_input(
                    "Vesszővel elválasztott teljes cellahatárok",
                    value="-inf, 10, 50, 250, inf",
                    key=f"p14_match_manual_edges_{var}",
                )
                st.caption("Teljes cellahatárokat adjon meg. Példa: -inf, 10, 50, 250, inf")
                try:
                    edges = parse_cutpoint_text(text)
                except ValueError as e:
                    st.warning(f"Érvénytelen cellahatárok ehhez: {var}: {e}. Pontos illesztés történik.")
                    edges = None

            if edges is not None:
                cutpoints[var] = edges
                config_rows.append(
                    {
                        "Változó": var,
                        "Módszer": method,
                        "Cellahatárok": ", ".join(
                            "inf" if x == np.inf else "-inf" if x == -np.inf else f"{x:,.4g}"
                            for x in edges
                        ),
                    }
                )
            else:
                config_rows.append({"Változó": var, "Módszer": "pontos értékek", "Cellahatárok": ""})

    return cutpoints, pd.DataFrame(config_rows)


def show_plot(fig):
    st.pyplot(fig)
    plt.close(fig)


col_settings, col_viz = utils.setup_page(
    "Coarsened exact matchinggel illesztett CS-DID",
    "Coarsened exact matchinggel illesztett CS-DID (szimulált)",
)

df = load_data(get_panel_data())

available_outcomes = get_available_numeric_outcomes(df)
if not available_outcomes:
    st.error("Nincs megfelelő numerikus kimeneti változó az adatban.")
    st.stop()

st.markdown("""
Válasszon **ágazatot**, **KKV-szűrést**, **kezelt kohorszokat**, **kimeneti változót**
és **illesztési cellákat**. A becslő kohorszspecifikus coarsened exact matching
cellákat képez a kezelés előtti évben (g-1), cellán belüli ATT(g,t)-ket számol,
majd aggregálja az eredményeket.
""")


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
        index=industries.index(persist("p14_industry", default_industry))
        if persist("p14_industry", default_industry) in industries else 0,
        key="_p14_industry", on_change=save, args=("p14_industry",),
    )

    selected_variable = st.selectbox(
        "Kimeneti változó",
        available_outcomes,
        index=available_outcomes.index(persist("p14_var", available_outcomes[0]))
        if persist("p14_var", available_outcomes[0]) in available_outcomes else 0,
        key="_p14_var", on_change=save, args=("p14_var",),
    )

    sme_all, sme_micro, sme_small, sme_medium, sme_large = utils.sme_filter_ui("p14", persist, save)

    only_once = st.checkbox(
        "Csak legfeljebb egyszer kezelt cégek",
        value=persist("p14_only_once", False),
        key="_p14_only_once", on_change=save, args=("p14_only_once",),
    )

    st.subheader("Kontrollcsoport")
    st.caption("Ez az illesztett becslő jelenleg csak a sosem kezelt kontrollcsoportot támogatja.")
    control_group_ui = "Sosem kezelt"
    control_group_param = "never_treated"

    with st.expander("Szélsőérték-kezelés"):
        saved_tail = persist("p14_tail", utils.FILTER_OPTIONS[0])
        tail_idx = utils.FILTER_OPTIONS.index(saved_tail) if saved_tail in utils.FILTER_OPTIONS else 0
        tail_mode = st.selectbox(
            "Y szélsőérték-kezelése",
            utils.FILTER_OPTIONS,
            index=tail_idx,
            key="_p14_tail", on_change=save, args=("p14_tail",),
        )

        x_raw = pd.to_numeric(df[selected_variable], errors="coerce").replace([np.inf, -np.inf], np.nan).dropna()
        is_monetary = selected_variable in MONETARY_VARS
        if is_monetary:
            x_raw = x_raw / 1000.0

        if tail_mode == utils.FILTER_OPTIONS[1] and not x_raw.empty:
            st.markdown("**Kézi határok (a megjelenített egységben)**")
            current_min = float(np.nanmin(x_raw))
            current_max = float(np.nanmax(x_raw))
            manual_min = st.number_input(
                "Minimum",
                value=current_min,
                step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                key="p14_manual_min",
            )
            manual_max = st.number_input(
                "Maximum",
                value=current_max,
                step=(current_max - current_min) / 100 if current_max > current_min else 1.0,
                key="p14_manual_max",
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
            value=persist("p14_log", False),
            key="_p14_log", on_change=save, args=("p14_log",),
        )

    st.subheader("Event-study ablak")
    n_pre = st.number_input(
        "Megjelenített pre periódusok száma",
        min_value=1,
        value=int(persist("p14_n_pre", 3)),
        step=1,
        key="_p14_n_pre", on_change=save, args=("p14_n_pre",),
    )
    n_post = st.number_input(
        "Megjelenített post periódusok száma",
        min_value=1,
        value=int(persist("p14_n_post", 3)),
        step=1,
        key="_p14_n_post", on_change=save, args=("p14_n_post",),
    )


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
        st.error("Nincs olyan kezelt kohorsz a választott mintában, amelyhez megfigyelhető a kezelés előtti év (g-1).")
        st.stop()

    st.caption(
        f"A {panel_start_year}. évi kohorsz nem választható, mert a panel {panel_start_year}-ben indul, "
        "így ehhez a kohorszhoz nem figyelhető meg a kezelés előtti év (g-1)."
    )

    saved_cohorts = persist("p14_cohorts", available_cohorts)
    valid_saved_cohorts = [c for c in saved_cohorts if c in available_cohorts]
    if not valid_saved_cohorts:
        valid_saved_cohorts = available_cohorts

    selected_cohorts = st.multiselect(
        "Becsléshez használt kezelt kohorszok",
        available_cohorts,
        default=valid_saved_cohorts,
        key="_p14_cohorts", on_change=save, args=("p14_cohorts",),
    )

if not selected_cohorts:
    selected_cohorts = available_cohorts

estimation_df = base_df[
    base_df["cohort_year"].isin(selected_cohorts) | (base_df["cohort_year"] == 0)
].copy()

if estimation_df.empty:
    st.error("A kiválasztott kohorszokkal nem maradt becslési minta.")
    st.stop()

is_monetary = selected_variable in MONETARY_VARS
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

if df_balanced.loc[df_balanced["cohort_year"] == 0, "id"].nunique() == 0:
    st.warning("A kiegyensúlyozott mintában nem maradt sosem kezelt kontrollcég.")
    st.stop()


with col_settings:
    st.subheader("Illesztési cellák")
    matching_candidates = get_matching_candidates(df_balanced, selected_variable)
    default_matching_vars = ["emp"] if "emp" in matching_candidates else matching_candidates[:1]

    saved_matching_vars = [
        v for v in persist("p14_matching_vars", default_matching_vars)
        if v in matching_candidates
    ]

    matching_vars = st.multiselect(
        "CEM-cellákat definiáló változók a g-1 évben",
        matching_candidates,
        default=saved_matching_vars,
        key="_p14_matching_vars", on_change=save, args=("p14_matching_vars",),
    )

    if not matching_vars:
        st.caption("Nincs kiválasztott illesztési változó: egy közös cella kerül használatra.")

    cutpoints, matching_config_df = render_matching_controls(df_balanced, matching_vars)

    st.subheader("Megjelenített táblák")
    show_summary = checkbox_setting("Összefoglaló táblák", "p14_show_summary", True)
    show_att_table = checkbox_setting("ATT(g,t) tábla", "p14_show_att_table", True)
    show_match_report_table = checkbox_setting("Illesztési riport tábla", "p14_show_match_report", False)
    show_cell_debug_table = checkbox_setting("Cellaszintű diagnosztikai tábla", "p14_show_cell_debug", False)
    show_aggregate_tables = checkbox_setting("Aggregált táblák", "p14_show_aggregate_tables", True)

    st.subheader("Megjelenített ábrák")
    show_cohort_plot = checkbox_setting("Kohorszspecifikus ATT(g,t)", "p14_show_cohort_plot", True)
    show_event_study_plot = checkbox_setting("Eseményidős aggregált hatás", "p14_show_event_plot", True)
    show_match_heatmaps = checkbox_setting("Illesztési elemszám hőtérképek", "p14_show_match_heatmaps", True)
    show_match_support = checkbox_setting("Illesztési support összefoglalók", "p14_show_match_support", True)
    show_stratum_att = checkbox_setting("Cellaszintű aggregált ATT", "p14_show_stratum_att", True)
    show_stratum_dynamic = checkbox_setting("Cellaszintű dinamikus hatások", "p14_show_stratum_dynamic", False)
    st.caption("A standard hibák ezen az oldalon a gyors futás miatt ki vannak kapcsolva.")


with st.spinner("Illesztett CEM CS-DID becslés fut..."):
    try:
        matched_model = MatchedCEMCallawaySantAnna(
            matching_vars=matching_vars,
            cutpoints=cutpoints,
            control_group=control_group_param,
            anticipation=0,
            base_period="universal",
            bootstrap_iters=0,
            ci_level=0.95,
            compute_confidence_intervals=False,
            random_state=123,
        )

        results_matched = matched_model.fit(
            df_balanced,
            outcome="_outcome",
            unit="id",
            time="year",
            first_treat="cohort_year",
            aggregate="all",
            se_aggregate=None,
            compute_confidence_intervals=False,
        )

        gt_df = results_matched.to_dataframe()
        event_study_effects = results_matched.event_study_effects
        match_report_df = results_matched.to_dataframe("match_report")
        cell_debug_df = results_matched.to_dataframe("cell_debug")
        stratum_df = results_matched.to_dataframe("stratum")
        stratum_event_df = results_matched.to_dataframe("stratum_event_study")

    except Exception as e:
        st.error(f"A becslés nem sikerült: {e}")
        st.stop()


with col_viz:
    st.header("Illesztett CEM CS-DID eredmények")

    firms_before_balance = estimation_df["id"].nunique()
    firms_after_balance = df_balanced["id"].nunique()
    treated_firms_after_balance = df_balanced.loc[df_balanced["cohort_year"] > 0, "id"].nunique()
    never_treated_firms_after_balance = df_balanced.loc[df_balanced["cohort_year"] == 0, "id"].nunique()

    unit_note = "millió Ft" if (is_monetary and not use_log) else ("ln egység" if use_log else "nyers egység")
    tail_note = (
        f"Kézi határok: [{manual_min:,.2f}, {manual_max:,.2f}]"
        if tail_mode == utils.FILTER_OPTIONS[1] else tail_mode
    )
    kkv_note = ", ".join(allowed_cats)

    st.markdown(
        f"**Minta:** {selected_industry} | **KKV:** {kkv_note} | **Változó:** `{selected_variable}` | "
        f"**Kontroll:** {control_group_ui} | **Egység:** {unit_note}"
    )

    if show_summary:
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
                    "Illesztési változók",
                    "Illesztett cellasorok száma",
                    "Bootstrap standard hibák",
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
                    ", ".join(matching_vars) if matching_vars else "Nincs: egy közös cella",
                    f"{len(match_report_df):,}",
                    "Kikapcsolva",
                ],
            }
        )
        st.table(summary_df.set_index("Mutató"))

        st.subheader("Illesztési cellák beállítása")
        if matching_config_df.empty:
            st.info("Nincs kiválasztott illesztési változó.")
        else:
            st.dataframe(matching_config_df, hide_index=True, use_container_width=True)

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
        st.subheader("Kohorszméretek a kiegyensúlyozott mintában")
        st.dataframe(cohort_counts[["Kohorsz", "Cégek száma"]], hide_index=True, use_container_width=True)

    if show_att_table:
        st.subheader("ATT(g,t) tábla")
        gt_display = gt_df.copy()
        if {"group", "time"}.issubset(gt_display.columns):
            gt_display = gt_display.sort_values(["group", "time"])
        st.dataframe(gt_display, use_container_width=True, hide_index=True)

    if show_match_report_table:
        st.subheader("Illesztési riport tábla")
        st.dataframe(match_report_df, use_container_width=True, hide_index=True)

    if show_cell_debug_table:
        st.subheader("Cellaszintű diagnosztikai tábla")
        st.dataframe(cell_debug_df, use_container_width=True, hide_index=True)

    if show_aggregate_tables:
        st.subheader("Aggregált táblák")
        tab_event, tab_group, tab_calendar, tab_simple, tab_stratum, tab_stratum_event = st.tabs(
            ["Eseményidő", "Kohorsz", "Naptári év", "Egyszerű", "Cella", "Cella x eseményidő"]
        )
        with tab_event:
            st.dataframe(results_matched.to_dataframe("event_study"), use_container_width=True, hide_index=True)
        with tab_group:
            st.dataframe(results_matched.to_dataframe("group"), use_container_width=True, hide_index=True)
        with tab_calendar:
            st.dataframe(results_matched.to_dataframe("calendar"), use_container_width=True, hide_index=True)
        with tab_simple:
            st.dataframe(results_matched.to_dataframe("simple"), use_container_width=True, hide_index=True)
        with tab_stratum:
            st.dataframe(stratum_df, use_container_width=True, hide_index=True)
        with tab_stratum_event:
            st.dataframe(stratum_event_df, use_container_width=True, hide_index=True)

    if show_cohort_plot:
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
            show_plot(fig_gt)
        except Exception as e:
            st.warning(f"A kohorszspecifikus ábra nem rajzolható ki: {e}")

    if show_event_study_plot:
        st.markdown("---")
        st.subheader("Eseményidős aggregált hatás")
        try:
            fig_es, ax_es, df_es_plot = figures.plot_cs_event_study(
                event_study_effects,
                n_pre=int(n_pre),
                n_post=int(n_post),
                title="Illesztett CEM CS-DID eseményidős hatások",
            )
            ax_es.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
            show_plot(fig_es)
            st.subheader("Eseményidős ábra adatai")
            st.dataframe(df_es_plot, use_container_width=True, hide_index=True)
        except Exception as e:
            st.warning(f"Az eseményidős ábra nem rajzolható ki: {e}")

    if show_stratum_att:
        st.markdown("---")
        st.subheader("Cellaszintű aggregált ATT")
        try:
            fig_stratum, _, stratum_plot_df = figures.plot_stratum_aggregated_att(
                stratum_df,
                figsize=(10, 6),
                sort_by="stratum",
                ascending=True,
            )
            show_plot(fig_stratum)
            st.dataframe(stratum_plot_df, hide_index=True, use_container_width=True)
        except Exception as e:
            st.warning(f"A cellaszintű aggregált ATT ábra nem rajzolható ki: {e}")

    if show_stratum_dynamic:
        st.markdown("---")
        st.subheader("Cellaszintű dinamikus hatások")
        try:
            fig_dyn, _, dyn_plot_df = figures.plot_stratum_dynamic_effects(
                stratum_event_df,
                n_pre=int(n_pre),
                n_post=int(n_post),
                ncols=3,
                figsize_per_panel=(5, 4),
                sharey=True,
            )
            show_plot(fig_dyn)
            st.dataframe(dyn_plot_df, hide_index=True, use_container_width=True)
        except Exception as e:
            st.warning(f"A cellaszintű dinamikus hatásábra nem rajzolható ki: {e}")

    if show_match_heatmaps:
        st.markdown("---")
        st.subheader("Illesztési elemszám hőtérképek")
        try:
            fig_heat, _ = figures.plot_match_count_heatmaps(match_report_df)
            show_plot(fig_heat)
        except Exception as e:
            st.warning(f"Az illesztési elemszám hőtérkép nem rajzolható ki: {e}")

    if show_match_support:
        st.markdown("---")
        st.subheader("Illesztési support összefoglalók")
        try:
            fig_support, _, support_df = figures.plot_match_support_summaries(match_report_df)
            show_plot(fig_support)
            st.dataframe(support_df, hide_index=True, use_container_width=True)
        except Exception as e:
            st.warning(f"Az illesztési support összefoglaló nem rajzolható ki: {e}")
