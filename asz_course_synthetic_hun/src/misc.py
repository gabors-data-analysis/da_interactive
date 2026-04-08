import numpy as np
import pandas as pd


def compute_att_gt_cells(
    df,
    outcome,
    unit,
    time,
    first_treat,
    control_group="never_treated",   # "never_treated" or "not_yet_treated"
    post_only=True,
    check_unique_cohort=True,
):
    """
    Manually compute ATT(g,t) cells using universal base period g-1.

    For each treated cohort g and time t:
        ATT(g,t) = [E(Y_t | G=g) - E(Y_{g-1} | G=g)]
                 - [E(Y_t | C)   - E(Y_{g-1} | C)]

    where C is either:
      - never-treated units
      - not-yet-treated units at time t

    Parameters
    ----------
    df : pd.DataFrame
        Long panel.
    outcome : str
        Outcome variable.
    unit : str
        Unit id column.
    time : str
        Time column.
    first_treat : str
        First treatment period column.
        Convention: 0 or NaN = never treated.
    control_group : str
        "never_treated" or "not_yet_treated"
    post_only : bool
        If True, only compute post-treatment cells t >= g.
        If False, computes all t != g-1, still using g-1 as base.
    check_unique_cohort : bool
        If True, verify that each unit has a unique first_treat value.

    Returns
    -------
    pd.DataFrame
        One row per (g,t) cell.
    """
    d = df[[unit, time, outcome, first_treat]].copy()
    d = d.dropna(subset=[unit, time, outcome]).copy()

    d["_time"] = pd.to_numeric(d[time], errors="coerce")
    d["_y"] = pd.to_numeric(d[outcome], errors="coerce")
    d["_g_raw"] = pd.to_numeric(d[first_treat], errors="coerce")

    d = d.dropna(subset=["_time", "_y"]).copy()

    # 0 or NaN => never treated
    d["_g"] = d["_g_raw"]
    d.loc[d["_g"].isna() | (d["_g"] == 0), "_g"] = np.inf

    if check_unique_cohort:
        nunique_g = d.groupby(unit)["_g"].nunique()
        bad_units = nunique_g[nunique_g > 1]
        if len(bad_units) > 0:
            raise ValueError(
                f"{len(bad_units)} units have multiple first_treat values. "
                "Each unit must belong to exactly one cohort."
            )

    cohort_by_unit = d.groupby(unit)["_g"].first()
    times = np.array(sorted(d["_time"].unique()))
    treated_cohorts = np.array(sorted(cohort_by_unit[np.isfinite(cohort_by_unit)].unique()))

    def units_observed_in_both(unit_ids, t0, t1):
        ids0 = set(d.loc[(d[unit].isin(unit_ids)) & (d["_time"] == t0), unit])
        ids1 = set(d.loc[(d[unit].isin(unit_ids)) & (d["_time"] == t1), unit])
        return np.array(sorted(ids0 & ids1))

    def mean_y(unit_ids, tt):
        mask = d[unit].isin(unit_ids) & (d["_time"] == tt)
        return d.loc[mask, "_y"].mean()

    rows = []

    for g in treated_cohorts:
        base_period = g - 1

        if base_period not in times:
            # skip cohorts where g-1 is not in the observed time support
            continue

        treated_pool = cohort_by_unit[cohort_by_unit == g].index.to_numpy()

        if post_only:
            t_values = times[times >= g]
        else:
            t_values = times[times != base_period]

        for t in t_values:
            if control_group == "never_treated":
                control_pool = cohort_by_unit[cohort_by_unit == np.inf].index.to_numpy()
                control_cohorts_used = ["never_treated"]

            elif control_group == "not_yet_treated":
                # control units are untreated by time t
                control_pool = cohort_by_unit[cohort_by_unit > t].index.to_numpy()

                finite_control_cohorts = sorted(
                    int(x) for x in cohort_by_unit[cohort_by_unit > t].unique()
                    if np.isfinite(x)
                )
                has_never = np.isinf(cohort_by_unit[cohort_by_unit > t]).any()
                control_cohorts_used = finite_control_cohorts + (["never_treated"] if has_never else [])

            else:
                raise ValueError("control_group must be 'never_treated' or 'not_yet_treated'")

            if len(control_pool) == 0:
                continue

            treated_ids = units_observed_in_both(treated_pool, base_period, t)
            control_ids = units_observed_in_both(control_pool, base_period, t)

            if len(treated_ids) == 0 or len(control_ids) == 0:
                continue

            treated_base = mean_y(treated_ids, base_period)
            treated_t = mean_y(treated_ids, t)
            control_base = mean_y(control_ids, base_period)
            control_t = mean_y(control_ids, t)

            treated_change = treated_t - treated_base
            control_change = control_t - control_base
            effect = treated_change - control_change

            rows.append(
                {
                    "group": int(g),
                    "time": int(t),
                    "base_period": int(base_period),
                    "event_time": int(t - g),
                    "effect": effect,
                    "treated_base_mean": treated_base,
                    "treated_time_mean": treated_t,
                    "control_base_mean": control_base,
                    "control_time_mean": control_t,
                    "treated_change": treated_change,
                    "control_change": control_change,
                    "n_treated_units": int(len(treated_ids)),
                    "n_control_units": int(len(control_ids)),
                    "control_group_type": control_group,
                    "control_cohorts_used": control_cohorts_used,
                }
            )

    out = pd.DataFrame(rows)

    if not out.empty:
        out = out.sort_values(["group", "time"]).reset_index(drop=True)

    return out

def make_constant_log_dose(df, unit="id", grant="grant_value", out="dose_ln_grant"):
    d = df.copy()

    # row-level log grant: 0 stays 0
    d["_ln_grant_tmp"] = np.where(
        d[grant].isna(),
        np.nan,
        np.where(d[grant] == 0, 0, np.log(d[grant]))
    )

    # unit-level constant dose:
    # if a unit has one positive grant once and 0 otherwise,
    # max() gives that treated dose; never-treated stay 0
    d[out] = d.groupby(unit)["_ln_grant_tmp"].transform("max")

    # optional cleanup
    d = d.drop(columns=["_ln_grant_tmp"])
    return d    




def balance_panel_on_variables(
    df: pd.DataFrame,
    variables,
    unit: str = "id",
    time: str = "year",
    return_years: bool = False,
):
    """
    Keep only units that:
    1. are observed in every year present in df
    2. have finite values for `variables` in every row

    Parameters
    ----------
    df : pd.DataFrame
    variables : str or list[str]
        One variable name or a list of variable names to balance on.
    unit : str
        Unit identifier column.
    time : str
        Time column.
    return_years : bool
        If True, also return the sorted array of years used for balancing.

    Returns
    -------
    pd.DataFrame
        Balanced dataframe.
    or
    (pd.DataFrame, np.ndarray)
        Balanced dataframe and the full year vector if return_years=True.
    """
    vars_list = [variables] if isinstance(variables, str) else list(variables)

    missing = [col for col in [unit, time, *vars_list] if col not in df.columns]
    if missing:
        raise ValueError(f"Missing columns: {missing}")

    df_work = df.copy()

    all_years = np.sort(df_work[time].dropna().unique())
    n_years = len(all_years)

    df_work["row_ok"] = np.isfinite(df_work[vars_list]).all(axis=1)

    firm_check = (
        df_work.groupby(unit)
        .agg(
            n_years=(time, "nunique"),
            all_rows_ok=("row_ok", "all"),
        )
    )

    valid_ids = firm_check.index[
        (firm_check["n_years"] == n_years) &
        (firm_check["all_rows_ok"])
    ]

    df_balanced = (
        df_work.loc[df_work[unit].isin(valid_ids)]
        .drop(columns="row_ok")
        .copy()
    )

    if return_years:
        return df_balanced, all_years
    return df_balanced


def build_firm_panel_no_cohort_filter(
    df: pd.DataFrame,
    selected_industry: int | None = None,   # None = keep all industries
    sme_all: bool = True,
    sme_micro: bool = False,
    sme_small: bool = False,
    sme_medium: bool = False,
    sme_large: bool = False,
    only_once: bool = False,
    prepare_columns: bool = True,
) -> pd.DataFrame:
    """
    Build a firm-level panel without cohort-based sample filtering.

    Keeps all firms that pass the industry / SME / only_once filters,
    and keeps all years for those firms.

    Assumptions
    -----------
    - Industry is stored as integer in column `nace2_in_2015`
    - Treatment timing is derived from:
        cohort_year = first year with received_grant == 1

    Created variables
    -----------------
    ever_treated   : 1 if firm is ever treated, else 0
    post_treatment : 1 for treated firms in years >= cohort_year, else 0
    event_time     : year - cohort_year for ever-treated firms, else NaN
    """

    df = df.copy()

    # ------------------------------------------------------------
    # 1) Prepare SME category + cohort_year
    # ------------------------------------------------------------
    if prepare_columns:
        # SME category from first observed year, matching dashboard logic
        if {"emp", "id", "year"}.issubset(df.columns):
            cats = np.full(len(df), "Egyéb", dtype=object)
            cats[df["emp"] < 5] = "Mikró"
            cats[(df["emp"] >= 5) & (df["emp"] < 50)] = "Kis"
            cats[(df["emp"] >= 50) & (df["emp"] < 250)] = "Közép"
            cats[df["emp"] >= 250] = "Nagy"

            tmp = pd.DataFrame({
                "id": df["id"],
                "year": df["year"],
                "cat": cats,
            })

            first_year_cats = tmp.sort_values("year").drop_duplicates("id")
            cat_map = first_year_cats.set_index("id")["cat"].to_dict()
            df["sme_category"] = df["id"].map(cat_map).fillna("Egyéb")
        elif "sme_category" not in df.columns:
            df["sme_category"] = "Egyéb"

        # cohort_year = first treatment year
        if "received_grant" in df.columns:
            df["received_grant"] = df["received_grant"].fillna(0)

            treatment_years = (
                df.loc[df["received_grant"] == 1]
                .groupby("id")["year"]
                .min()
                .rename("cohort_year")
                .reset_index()
            )

            df = df.drop(columns=["cohort_year"], errors="ignore")
            df = df.merge(treatment_years, on="id", how="left")
        elif "cohort_year" not in df.columns:
            df["cohort_year"] = np.nan

    # ------------------------------------------------------------
    # 2) Basic checks
    # ------------------------------------------------------------
    required_cols = {"id", "year"}
    missing = required_cols - set(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {sorted(missing)}")

    # ------------------------------------------------------------
    # 3) Industry filter
    # ------------------------------------------------------------
    if selected_industry is None:
        base_df = df.copy()
    else:
        if "nace2_in_2015" not in df.columns:
            raise ValueError("'nace2_in_2015' column not found.")

        # industry is integer in the data
        selected_industry = int(selected_industry)
        base_df = df.loc[df["nace2_in_2015"] == selected_industry].copy()

    # ------------------------------------------------------------
    # 4) SME filter
    # ------------------------------------------------------------
    if sme_all:
        allowed_cats = ["Mikró", "Kis", "Közép", "Nagy", "Egyéb"]
    else:
        allowed_cats = []
        if sme_micro:
            allowed_cats.append("Mikró")
        if sme_small:
            allowed_cats.append("Kis")
        if sme_medium:
            allowed_cats.append("Közép")
        if sme_large:
            allowed_cats.append("Nagy")

        if not allowed_cats:
            raise ValueError("Select at least one SME category, or set sme_all=True.")

        base_df = base_df.loc[base_df["sme_category"].isin(allowed_cats)].copy()

    # ------------------------------------------------------------
    # 5) Optional: keep only firms treated at most once
    # ------------------------------------------------------------
    if only_once:
        if "received_grant" not in base_df.columns:
            raise ValueError("'received_grant' column not found, cannot apply only_once filter.")

        grant_counts = base_df.groupby("id")["received_grant"].sum()
        valid_ids_once = grant_counts[grant_counts <= 1].index
        base_df = base_df.loc[base_df["id"].isin(valid_ids_once)].copy()

    if base_df.empty:
        raise ValueError("No data left after filters.")

    # ------------------------------------------------------------
    # 6) Add treatment variables, keeping all years
    # ------------------------------------------------------------
    base_df["ever_treated"] = base_df["cohort_year"].notna().astype(int)

    base_df["post_treatment"] = np.where(
        base_df["cohort_year"].notna() & (base_df["year"] >= base_df["cohort_year"]),
        1,
        0
    )

    base_df["event_time"] = np.where(
        base_df["cohort_year"].notna(),
        base_df["year"] - base_df["cohort_year"],
        np.nan
    )

    base_df = base_df.sort_values(["id", "year"]).reset_index(drop=True)

    return base_df
