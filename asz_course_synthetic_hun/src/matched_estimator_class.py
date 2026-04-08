from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Optional, Sequence, Any
import numpy as np
import pandas as pd
from scipy.stats import norm


# ============================================================
# Result container
# ============================================================

@dataclass
class MatchedCEMCSResults:
    att_gt: pd.DataFrame
    event_study: pd.DataFrame
    group_effects_df: pd.DataFrame
    calendar_effects_df: pd.DataFrame
    simple_effect_df: pd.DataFrame
    stratum_effects_df: pd.DataFrame
    stratum_event_study_df: pd.DataFrame
    match_report_df: pd.DataFrame
    cell_debug_df: pd.DataFrame
    metadata: Dict[str, Any]

    def to_dataframe(self, kind: str = "att_gt") -> pd.DataFrame:
        """
        Similar spirit to diff_diff:
        - default: returns cohort-time ATT(g,t) table
        - other kinds:
            'event_study', 'group', 'calendar', 'simple',
            'stratum', 'stratum_event_study',
            'match_report', 'cell_debug'
        """
        kind = kind.lower()
        if kind in {"att_gt", "gt", "group_time"}:
            return self.att_gt.copy()
        if kind in {"event_study", "dynamic"}:
            return self.event_study.copy()
        if kind in {"group", "group_effects"}:
            return self.group_effects_df.copy()
        if kind in {"calendar", "calendar_effects"}:
            return self.calendar_effects_df.copy()
        if kind in {"simple", "overall"}:
            return self.simple_effect_df.copy()
        if kind in {"stratum", "stratum_effects", "cell_effects", "matching_cells"}:
            return self.stratum_effects_df.copy()
        if kind in {
            "stratum_event_study",
            "stratum_dynamic",
            "cell_event_study",
            "cell_dynamic",
            "matching_cell_event_time",
        }:
            return self.stratum_event_study_df.copy()
        if kind in {"match_report", "debug", "strata"}:
            return self.match_report_df.copy()
        if kind in {"cell_debug", "cell_report"}:
            return self.cell_debug_df.copy()
        raise ValueError(f"Unknown kind='{kind}'.")

    def get_match_report(self, cohort: Optional[int] = None) -> pd.DataFrame:
        df = self.match_report_df.copy()
        if cohort is not None:
            df = df[df["group"] == cohort].copy()
        return df

    def get_cell_debug(
        self,
        cohort: Optional[int] = None,
        time: Optional[int] = None,
    ) -> pd.DataFrame:
        df = self.cell_debug_df.copy()
        if cohort is not None:
            df = df[df["group"] == cohort].copy()
        if time is not None:
            df = df[df["time"] == time].copy()
        return df

    @property
    def event_study_effects(self) -> Dict[float, Dict[str, Any]]:
        out = {}
        df = self.event_study.copy().sort_values("event_time")

        for _, row in df.iterrows():
            out[float(row["event_time"])] = {
                "effect": float(row["effect"]) if pd.notna(row["effect"]) else np.nan,
                "se": float(row["se"]) if pd.notna(row["se"]) else np.nan,
                "t_stat": float(row["t_stat"]) if pd.notna(row["t_stat"]) else np.nan,
                "p_value": float(row["p_value"]) if pd.notna(row["p_value"]) else np.nan,
                "conf_int": (
                    float(row["conf_int_lower"]) if pd.notna(row["conf_int_lower"]) else np.nan,
                    float(row["conf_int_upper"]) if pd.notna(row["conf_int_upper"]) else np.nan,
                ),
                "n_groups": int(row["n_groups"]) if pd.notna(row["n_groups"]) else 0,
            }

        if -1.0 not in out:
            out[-1.0] = {
                "effect": 0.0,
                "se": np.nan,
                "t_stat": np.nan,
                "p_value": np.nan,
                "conf_int": (np.nan, np.nan),
                "n_groups": 0,
            }

        return dict(sorted(out.items(), key=lambda kv: kv[0]))

    @property
    def simple_effect(self) -> Dict[str, Any]:
        if self.simple_effect_df.empty:
            return {}
        return self.simple_effect_df.iloc[0].to_dict()


_ALL_AGGREGATIONS = {
    "event_study",
    "group",
    "calendar",
    "simple",
    "stratum",
    "stratum_event_study",
}

_BOOTSTRAPPED_AGGREGATIONS = {
    "event_study",
    "group",
    "calendar",
    "simple",
    "stratum",
    "stratum_event_study",
}


def _empty_att_gt_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "group", "time", "baseline", "event_time", "effect",
            "n_treated", "n_control", "n_treated_used", "n_control_used",
            "n_strata", "post",
        ]
    )


def _empty_event_study_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "event_time", "effect", "n_groups", "n_cells", "total_treated_weight",
        ]
    )


def _empty_group_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=["group", "effect", "n_periods", "total_treated_weight"]
    )


def _empty_calendar_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=["time", "effect", "n_groups", "n_cells", "total_treated_weight"]
    )


def _empty_simple_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=["estimand", "effect", "n_groups", "n_cells", "total_treated_weight"]
    )


def _empty_stratum_effects_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=["stratum", "effect", "total_treated_weight", "n_cells", "n_cohorts"]
    )


def _empty_stratum_event_study_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "stratum", "event_time", "effect", "n_cells",
            "n_cohorts", "total_treated_weight", "post",
        ]
    )


def _empty_match_report_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "stratum", "n_treated", "n_control", "treated_weight",
            "group", "baseline", "n_treated_total_matched", "n_control_total_matched",
        ]
    )


def _empty_cell_debug_df() -> pd.DataFrame:
    return pd.DataFrame(
        columns=[
            "group", "time", "baseline", "event_time", "stratum", "stratum_weight",
            "treated_base_mean", "treated_time_mean",
            "control_base_mean", "control_time_mean",
            "treated_change", "control_change",
            "stratum_effect", "weighted_contribution",
            "n_treated_stratum", "n_control_stratum", "post",
        ]
    )


def _normalize_aggregations(
    aggregations: Optional[str | Sequence[str]],
) -> set[str]:
    alias_map = {
        "event_study": "event_study",
        "dynamic": "event_study",
        "group": "group",
        "group_effects": "group",
        "cohort": "group",
        "cohorts": "group",
        "calendar": "calendar",
        "calendar_effects": "calendar",
        "simple": "simple",
        "overall": "simple",
        "stratum": "stratum",
        "strata": "stratum",
        "stratum_effects": "stratum",
        "cell": "stratum",
        "cell_effects": "stratum",
        "matching_cells": "stratum",
        "stratum_event_study": "stratum_event_study",
        "stratum_dynamic": "stratum_event_study",
        "cell_event_study": "stratum_event_study",
        "cell_dynamic": "stratum_event_study",
        "matching_cell_event_time": "stratum_event_study",
    }

    if aggregations is None:
        return set(_ALL_AGGREGATIONS)

    raw = [aggregations] if isinstance(aggregations, str) else list(aggregations)
    if len(raw) == 0:
        return set()

    normalized = set()
    for item in raw:
        key = str(item).lower()
        if key == "all":
            return set(_ALL_AGGREGATIONS)
        if key not in alias_map:
            allowed = ", ".join(sorted(["all"] + list(alias_map.keys())))
            raise ValueError(
                f"Unknown aggregation '{item}'. Allowed values: {allowed}"
            )
        normalized.add(alias_map[key])

    return normalized


def _normalize_se_aggregations(
    se_aggregate: Optional[str | Sequence[str]],
    requested_aggregations: set[str],
) -> set[str]:
    if se_aggregate is None:
        return set()

    if isinstance(se_aggregate, str) and se_aggregate.lower() == "all":
        return set(requested_aggregations)

    raw = list(se_aggregate)
    if any(str(item).lower() == "all" for item in raw):
        return set(requested_aggregations)

    requested_se_aggregations = _normalize_aggregations(raw)
    extra = requested_se_aggregations - requested_aggregations
    if extra:
        extra_str = ", ".join(sorted(extra))
        requested_str = ", ".join(sorted(requested_aggregations))
        raise ValueError(
            "se_aggregate cannot include aggregations that are not requested "
            f"in aggregate. Extra: {extra_str}. Requested aggregate set: {requested_str}."
        )

    return requested_se_aggregations


# ============================================================
# Internal helpers
# ============================================================

def _z_value(ci_level: float) -> float:
    alpha = 1.0 - ci_level
    return norm.ppf(1.0 - alpha / 2.0)


def _p_value_from_t(t_stat: float) -> float:
    if pd.isna(t_stat):
        return np.nan
    return 2.0 * norm.sf(abs(t_stat))


def _weighted_effect(
    sub: pd.DataFrame,
    effect_col: str,
    weight_col: str,
) -> float:
    w = pd.to_numeric(sub[weight_col], errors="coerce").fillna(0).to_numpy()
    y = pd.to_numeric(sub[effect_col], errors="coerce").to_numpy()
    mask = np.isfinite(w) & np.isfinite(y) & (w > 0)
    if mask.sum() == 0:
        return np.nan
    return float(np.average(y[mask], weights=w[mask]))


def _validate_inputs(
    control_group: str,
    base_period: str,
    anticipation: int,
):
    if control_group != "never_treated":
        raise NotImplementedError(
            "This draft currently supports only control_group='never_treated'."
        )
    if base_period != "universal":
        raise NotImplementedError(
            "This draft currently supports only base_period='universal'."
        )
    if anticipation != 0:
        raise NotImplementedError(
            "This draft currently supports only anticipation=0."
        )


def _prepare_estimation_data(
    data: pd.DataFrame,
    outcome: str,
    unit: str,
    time: str,
    first_treat: str,
    matching_vars: Sequence[str],
    cohorts: Optional[Sequence[int]] = None,
) -> pd.DataFrame:
    needed = [unit, time, outcome, first_treat] + list(matching_vars)
    missing = [c for c in needed if c not in data.columns]
    if missing:
        raise ValueError(f"Missing columns: {missing}")

    d = data[needed].copy()

    if d.duplicated([unit, time]).any():
        raise ValueError("Duplicate unit-time rows found.")

    d[time] = pd.to_numeric(d[time], errors="coerce")
    d[outcome] = pd.to_numeric(d[outcome], errors="coerce")
    d[first_treat] = pd.to_numeric(d[first_treat], errors="coerce")

    if d[time].isna().any():
        raise ValueError(f"Column '{time}' contains non-numeric or missing values.")

    if ~np.isfinite(d[outcome]).all():
        raise ValueError(
            f"Column '{outcome}' must be finite for all rows. "
            "Balance/clean the outcome before estimation."
        )

    d["_g"] = d[first_treat].copy()
    d.loc[d["_g"].isna() | (d["_g"] == 0), "_g"] = 0
    d["_g"] = d["_g"].astype(int)

    nunique_g = d.groupby(unit)["_g"].nunique()
    bad_units = nunique_g[nunique_g > 1]
    if len(bad_units) > 0:
        raise ValueError(
            f"{len(bad_units)} units have multiple first_treat values."
        )

    if cohorts is not None:
        cohort_set = set(int(c) for c in cohorts)
        d = d[d["_g"].isin(cohort_set | {0})].copy()

    if d.empty:
        raise ValueError("No data left after cohort restriction.")

    if (d["_g"] == 0).sum() == 0:
        raise ValueError("No never-treated units in the estimation sample.")

    return d


def _coarsen_series(
    s: pd.Series,
    bins: Optional[Sequence[float]] = None,
) -> pd.Series:
    """
    If bins is provided -> pd.cut
    Else -> exact matching on the raw values.
    Missing values are kept as a special category.
    """
    if bins is None:
        out = s.astype("string")
        out = out.fillna("__MISSING__")
        return out.astype(str)

    out = pd.cut(
        pd.to_numeric(s, errors="coerce"),
        bins=bins,
        include_lowest=True,
        duplicates="drop",
    ).astype("string")
    out = out.fillna("__MISSING__")
    return out.astype(str)


def _build_cohort_match_map(
    d: pd.DataFrame,
    cohort: int,
    unit: str,
    time: str,
    matching_vars: Sequence[str],
    cutpoints: Optional[Dict[str, Sequence[float]]] = None,
) -> Optional[Dict[str, Any]]:
    """
    Build CEM strata for one cohort g using baseline year g-1.
    Controls are never-treated only.
    """
    cutpoints = cutpoints or {}
    baseline = cohort - 1

    base = d.loc[
        (d[time] == baseline) &
        (d["_g"].isin([0, cohort])),
        [unit, "_g"] + list(matching_vars)
    ].copy()

    if base.empty:
        return None

    for var in matching_vars:
        bins = cutpoints.get(var, None)
        base[f"__cem_{var}"] = _coarsen_series(base[var], bins=bins)

    if matching_vars:
        base["__stratum"] = base[[f"__cem_{v}" for v in matching_vars]].agg("||".join, axis=1)
    else:
        base["__stratum"] = "__ALL__"

    treat = base[base["_g"] == cohort].copy()
    ctrl = base[base["_g"] == 0].copy()

    if treat.empty or ctrl.empty:
        return None

    common_strata = sorted(set(treat["__stratum"]).intersection(set(ctrl["__stratum"])))
    if len(common_strata) == 0:
        return None

    treat = treat[treat["__stratum"].isin(common_strata)].copy()
    ctrl = ctrl[ctrl["__stratum"].isin(common_strata)].copy()

    if treat.empty or ctrl.empty:
        return None

    treat_counts = treat.groupby("__stratum")[unit].nunique().rename("n_treated")
    ctrl_counts = ctrl.groupby("__stratum")[unit].nunique().rename("n_control")

    strata_info = (
        pd.concat([treat_counts, ctrl_counts], axis=1)
        .reset_index()
        .rename(columns={"__stratum": "stratum"})
        .sort_values("stratum")
        .reset_index(drop=True)
    )
    strata_info["treated_weight"] = strata_info["n_treated"] / strata_info["n_treated"].sum()

    treat_ids_by_stratum = (
        treat.groupby("__stratum")[unit]
        .apply(lambda x: list(pd.unique(x)))
        .to_dict()
    )
    ctrl_ids_by_stratum = (
        ctrl.groupby("__stratum")[unit]
        .apply(lambda x: list(pd.unique(x)))
        .to_dict()
    )
    weights = strata_info.set_index("stratum")["treated_weight"].to_dict()

    return {
        "cohort": cohort,
        "baseline": baseline,
        "strata_info": strata_info,
        "treat_ids_by_stratum": treat_ids_by_stratum,
        "ctrl_ids_by_stratum": ctrl_ids_by_stratum,
        "weights": weights,
        "matched_treated_ids": sorted(pd.unique(treat[unit])),
        "matched_control_ids": sorted(pd.unique(ctrl[unit])),
    }


def _compute_cell_effects_from_match_map(
    d: pd.DataFrame,
    match_map: Dict[str, Any],
    outcome: str,
    unit: str,
    time: str,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    For one cohort g, compute ATT(g,t) for all t != g-1,
    using universal base period g-1 and fixed matched strata.

    Returns
    -------
    cell_summary_df : one row per (g,t)
    cell_debug_df   : one row per (g,t,stratum)
    """
    cohort = match_map["cohort"]
    baseline = match_map["baseline"]

    y_wide = d.pivot(index=unit, columns=time, values=outcome)
    all_times = np.array(sorted(pd.unique(d[time])))

    if baseline not in y_wide.columns:
        return pd.DataFrame(), pd.DataFrame()

    summary_rows = []
    debug_rows = []

    for tt in all_times:
        if tt == baseline:
            continue

        cell_effect = 0.0
        used_strata = 0
        n_treated_used_total = 0
        n_control_used_total = 0

        for stratum, w in match_map["weights"].items():
            treat_ids = match_map["treat_ids_by_stratum"][stratum]
            ctrl_ids = match_map["ctrl_ids_by_stratum"][stratum]

            treated_base = pd.to_numeric(y_wide.loc[treat_ids, baseline], errors="coerce").dropna()
            treated_t = pd.to_numeric(y_wide.loc[treat_ids, tt], errors="coerce").dropna()
            control_base = pd.to_numeric(y_wide.loc[ctrl_ids, baseline], errors="coerce").dropna()
            control_t = pd.to_numeric(y_wide.loc[ctrl_ids, tt], errors="coerce").dropna()

            valid_treat_ids = sorted(set(treated_base.index).intersection(set(treated_t.index)))
            valid_ctrl_ids = sorted(set(control_base.index).intersection(set(control_t.index)))

            if len(valid_treat_ids) == 0 or len(valid_ctrl_ids) == 0:
                continue

            treated_base = pd.to_numeric(y_wide.loc[valid_treat_ids, baseline], errors="coerce").dropna()
            treated_t = pd.to_numeric(y_wide.loc[valid_treat_ids, tt], errors="coerce").dropna()
            control_base = pd.to_numeric(y_wide.loc[valid_ctrl_ids, baseline], errors="coerce").dropna()
            control_t = pd.to_numeric(y_wide.loc[valid_ctrl_ids, tt], errors="coerce").dropna()

            delta_treat = treated_t - treated_base
            delta_ctrl = control_t - control_base

            if len(delta_treat) == 0 or len(delta_ctrl) == 0:
                continue

            treated_base_mean = treated_base.mean()
            treated_time_mean = treated_t.mean()
            control_base_mean = control_base.mean()
            control_time_mean = control_t.mean()

            treated_change_mean = delta_treat.mean()
            control_change_mean = delta_ctrl.mean()

            att_m = treated_change_mean - control_change_mean
            weighted_contribution = w * att_m

            cell_effect += weighted_contribution
            used_strata += 1
            n_treated_used_total += len(delta_treat)
            n_control_used_total += len(delta_ctrl)

            debug_rows.append(
                {
                    "group": cohort,
                    "time": tt,
                    "baseline": baseline,
                    "event_time": tt - cohort,
                    "stratum": stratum,
                    "stratum_weight": w,
                    "treated_base_mean": treated_base_mean,
                    "treated_time_mean": treated_time_mean,
                    "control_base_mean": control_base_mean,
                    "control_time_mean": control_time_mean,
                    "treated_change": treated_change_mean,
                    "control_change": control_change_mean,
                    "stratum_effect": att_m,
                    "weighted_contribution": weighted_contribution,
                    "n_treated_stratum": len(delta_treat),
                    "n_control_stratum": len(delta_ctrl),
                    "post": int(tt >= cohort),
                }
            )

        if used_strata == 0:
            continue

        summary_rows.append(
            {
                "group": cohort,
                "time": tt,
                "baseline": baseline,
                "event_time": tt - cohort,
                "effect": cell_effect,
                "n_treated": len(match_map["matched_treated_ids"]),
                "n_control": len(match_map["matched_control_ids"]),
                "n_treated_used": n_treated_used_total,
                "n_control_used": n_control_used_total,
                "n_strata": used_strata,
                "post": int(tt >= cohort),
            }
        )

    cell_summary_df = pd.DataFrame(summary_rows)
    cell_debug_df = pd.DataFrame(debug_rows)

    if not cell_summary_df.empty:
        cell_summary_df = cell_summary_df.sort_values(["group", "time"]).reset_index(drop=True)

    if not cell_debug_df.empty:
        cell_debug_df = cell_debug_df.sort_values(["group", "time", "stratum"]).reset_index(drop=True)

    return cell_summary_df, cell_debug_df


def compute_matched_att_gt(
    data: pd.DataFrame,
    outcome: str,
    unit: str,
    time: str,
    first_treat: str,
    matching_vars: Sequence[str],
    cutpoints: Optional[Dict[str, Sequence[float]]] = None,
    cohorts: Optional[Sequence[int]] = None,
    control_group: str = "never_treated",
    base_period: str = "universal",
    anticipation: int = 0,
    return_debug: bool = False,
):
    """
    Compute matched ATT(g,t) cells using CEM at g-1 and never-treated controls.

    If return_debug=True, returns a dict with:
        {
            "att_gt": ...,
            "match_report": ...,
            "cell_debug": ...
        }
    else returns only att_gt.
    """
    _validate_inputs(
        control_group=control_group,
        base_period=base_period,
        anticipation=anticipation,
    )

    d = _prepare_estimation_data(
        data=data,
        outcome=outcome,
        unit=unit,
        time=time,
        first_treat=first_treat,
        matching_vars=matching_vars,
        cohorts=cohorts,
    )

    all_cohorts = sorted(int(x) for x in pd.unique(d["_g"]) if x > 0)

    att_rows = []
    match_rows = []
    cell_debug_rows = []

    for g in all_cohorts:
        match_map = _build_cohort_match_map(
            d=d,
            cohort=g,
            unit=unit,
            time=time,
            matching_vars=matching_vars,
            cutpoints=cutpoints,
        )
        if match_map is None:
            continue

        report = match_map["strata_info"].copy()
        report["group"] = g
        report["baseline"] = g - 1
        report["n_treated_total_matched"] = len(match_map["matched_treated_ids"])
        report["n_control_total_matched"] = len(match_map["matched_control_ids"])
        match_rows.append(report)

        cohort_df, cohort_debug_df = _compute_cell_effects_from_match_map(
            d=d,
            match_map=match_map,
            outcome=outcome,
            unit=unit,
            time=time,
        )

        if not cohort_df.empty:
            att_rows.append(cohort_df)

        if not cohort_debug_df.empty:
            cell_debug_rows.append(cohort_debug_df)

    if att_rows:
        att_gt = pd.concat(att_rows, ignore_index=True).sort_values(["group", "time"]).reset_index(drop=True)
    else:
        att_gt = pd.DataFrame(
            columns=[
                "group", "time", "baseline", "event_time", "effect",
                "n_treated", "n_control", "n_treated_used", "n_control_used",
                "n_strata", "post"
            ]
        )

    if match_rows:
        match_report = (
            pd.concat(match_rows, ignore_index=True)
            .sort_values(["group", "stratum"])
            .reset_index(drop=True)
        )
    else:
        match_report = pd.DataFrame(
            columns=[
                "stratum", "n_treated", "n_control", "treated_weight",
                "group", "baseline", "n_treated_total_matched", "n_control_total_matched"
            ]
        )

    if cell_debug_rows:
        cell_debug = (
            pd.concat(cell_debug_rows, ignore_index=True)
            .sort_values(["group", "time", "stratum"])
            .reset_index(drop=True)
        )
    else:
        cell_debug = pd.DataFrame(
            columns=[
                "group", "time", "baseline", "event_time", "stratum", "stratum_weight",
                "treated_base_mean", "treated_time_mean",
                "control_base_mean", "control_time_mean",
                "treated_change", "control_change",
                "stratum_effect", "weighted_contribution",
                "n_treated_stratum", "n_control_stratum", "post"
            ]
        )

    if return_debug:
        return {
            "att_gt": att_gt,
            "match_report": match_report,
            "cell_debug": cell_debug,
        }

    return att_gt


def aggregate_att_gt(att_gt: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """
    Aggregate ATT(g,t) in the spirit of Callaway-Sant'Anna.

    Weighting rule used here:
    - weights are proportional to matched treated counts in each cell
    """
    if att_gt.empty:
        return {
            "event_study": _empty_event_study_df(),
            "group": _empty_group_df(),
            "calendar": _empty_calendar_df(),
            "simple": _empty_simple_df(),
        }

    df = att_gt.copy()

    # Event study: all event times except -1 normalization point
    event_rows = []
    for e, sub in df.groupby("event_time", sort=True):
        event_rows.append(
            {
                "event_time": e,
                "effect": _weighted_effect(sub, effect_col="effect", weight_col="n_treated"),
                "n_groups": sub["group"].nunique(),
                "n_cells": len(sub),
                "total_treated_weight": sub["n_treated"].sum(),
            }
        )
    event_study = (
        pd.DataFrame(event_rows).sort_values("event_time").reset_index(drop=True)
        if event_rows else _empty_event_study_df()
    )

    # Group effects: average over post-treatment cells only
    post_df = df[df["post"] == 1].copy()

    if post_df.empty:
        group_df = _empty_group_df()
        calendar_df = _empty_calendar_df()
        simple_df = _empty_simple_df()
    else:
        group_rows = []
        for g, sub in post_df.groupby("group", sort=True):
            group_rows.append(
                {
                    "group": g,
                    "effect": _weighted_effect(sub, effect_col="effect", weight_col="n_treated"),
                    "n_periods": len(sub),
                    "total_treated_weight": sub["n_treated"].sum(),
                }
            )
        group_df = (
            pd.DataFrame(group_rows).sort_values("group").reset_index(drop=True)
            if group_rows else _empty_group_df()
        )

        # Calendar effects: average over active treated cohorts in each year
        cal_rows = []
        for tt, sub in post_df.groupby("time", sort=True):
            cal_rows.append(
                {
                    "time": tt,
                    "effect": _weighted_effect(sub, effect_col="effect", weight_col="n_treated"),
                    "n_groups": sub["group"].nunique(),
                    "n_cells": len(sub),
                    "total_treated_weight": sub["n_treated"].sum(),
                }
            )
        calendar_df = (
            pd.DataFrame(cal_rows).sort_values("time").reset_index(drop=True)
            if cal_rows else _empty_calendar_df()
        )

        # Simple overall ATT: average over all post-treatment cells
        simple_df = pd.DataFrame(
            [
                {
                    "estimand": "simple",
                    "effect": _weighted_effect(post_df, effect_col="effect", weight_col="n_treated"),
                    "n_groups": post_df["group"].nunique(),
                    "n_cells": len(post_df),
                    "total_treated_weight": post_df["n_treated"].sum(),
                }
            ]
        )

    return {
        "event_study": event_study,
        "group": group_df,
        "calendar": calendar_df,
        "simple": simple_df,
    }


def aggregate_cell_debug(cell_debug_df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
    """
    Aggregate stratum-level effects from cell_debug_df.

    Outputs
    -------
    stratum : pooled post-treatment effect for each stratum
    stratum_event_study : pooled effect for each (stratum, event_time)
    """
    if cell_debug_df.empty:
        return {
            "stratum": _empty_stratum_effects_df(),
            "stratum_event_study": _empty_stratum_event_study_df(),
        }

    df = cell_debug_df.copy()
    post_df = df[df["post"] == 1].copy()

    if post_df.empty:
        stratum_df = _empty_stratum_effects_df()
    else:
        stratum_rows = []
        for stratum, sub in post_df.groupby("stratum", sort=True):
            stratum_rows.append(
                {
                    "stratum": stratum,
                    "effect": _weighted_effect(
                        sub,
                        effect_col="stratum_effect",
                        weight_col="n_treated_stratum",
                    ),
                    "total_treated_weight": sub["n_treated_stratum"].sum(),
                    "n_cells": len(sub),
                    "n_cohorts": sub["group"].nunique(),
                }
            )
        stratum_df = (
            pd.DataFrame(stratum_rows).sort_values("stratum").reset_index(drop=True)
            if stratum_rows else _empty_stratum_effects_df()
        )

    dyn_rows = []
    for (stratum, event_time), sub in df.groupby(["stratum", "event_time"], sort=True):
        dyn_rows.append(
            {
                "stratum": stratum,
                "event_time": event_time,
                "effect": _weighted_effect(
                    sub,
                    effect_col="stratum_effect",
                    weight_col="n_treated_stratum",
                ),
                "n_cells": len(sub),
                "n_cohorts": sub["group"].nunique(),
                "total_treated_weight": sub["n_treated_stratum"].sum(),
                "post": int(event_time >= 0),
            }
        )
    stratum_event_study_df = (
        pd.DataFrame(dyn_rows)
        .sort_values(["stratum", "event_time"])
        .reset_index(drop=True)
        if dyn_rows else _empty_stratum_event_study_df()
    )

    return {
        "stratum": stratum_df,
        "stratum_event_study": stratum_event_study_df,
    }


def _bootstrap_resample_clusters(
    d: pd.DataFrame,
    unit: str,
    rng: np.random.Generator,
    draw_idx: int,
) -> pd.DataFrame:
    """
    Cluster bootstrap at the unit level.
    Each resampled firm keeps its full panel history.
    Duplicate draws get fresh bootstrap unit IDs.
    """
    ids = pd.unique(d[unit])
    sampled = rng.choice(ids, size=len(ids), replace=True)

    parts = []
    for j, uid in enumerate(sampled):
        tmp = d.loc[d[unit] == uid].copy()
        tmp[unit] = f"boot_{draw_idx}_{j}_{uid}"
        parts.append(tmp)

    return pd.concat(parts, ignore_index=True)


def _compute_point_estimates_and_aggregates(
    data: pd.DataFrame,
    outcome: str,
    unit: str,
    time: str,
    first_treat: str,
    matching_vars: Sequence[str],
    cutpoints: Optional[Dict[str, Sequence[float]]] = None,
    cohorts: Optional[Sequence[int]] = None,
    control_group: str = "never_treated",
    base_period: str = "universal",
    anticipation: int = 0,
    aggregations: Optional[str | Sequence[str]] = "all",
) -> Dict[str, pd.DataFrame]:
    requested_aggregations = _normalize_aggregations(aggregations)

    point = compute_matched_att_gt(
        data=data,
        outcome=outcome,
        unit=unit,
        time=time,
        first_treat=first_treat,
        matching_vars=matching_vars,
        cutpoints=cutpoints,
        cohorts=cohorts,
        control_group=control_group,
        base_period=base_period,
        anticipation=anticipation,
        return_debug=True,
    )

    out = {
        **point,
        "event_study": _empty_event_study_df(),
        "group": _empty_group_df(),
        "calendar": _empty_calendar_df(),
        "simple": _empty_simple_df(),
        "stratum": _empty_stratum_effects_df(),
        "stratum_event_study": _empty_stratum_event_study_df(),
    }

    if requested_aggregations & {"event_study", "group", "calendar", "simple"}:
        att_aggs = aggregate_att_gt(point["att_gt"])
        for key in requested_aggregations & {"event_study", "group", "calendar", "simple"}:
            out[key] = att_aggs[key]

    if requested_aggregations & {"stratum", "stratum_event_study"}:
        cell_aggs = aggregate_cell_debug(point["cell_debug"])
        for key in requested_aggregations & {"stratum", "stratum_event_study"}:
            out[key] = cell_aggs[key]

    return out


def bootstrap_matched_att(
    data: pd.DataFrame,
    outcome: str,
    unit: str,
    time: str,
    first_treat: str,
    matching_vars: Sequence[str],
    cutpoints: Optional[Dict[str, Sequence[float]]] = None,
    cohorts: Optional[Sequence[int]] = None,
    control_group: str = "never_treated",
    base_period: str = "universal",
    anticipation: int = 0,
    bootstrap_iters: int = 199,
    random_state: Optional[int] = None,
    aggregations: Optional[str | Sequence[str]] = "all",
) -> Dict[str, pd.DataFrame]:
    """
    Bootstrap the entire estimator:
    matching -> ATT(g,t) -> aggregation
    """
    _validate_inputs(
        control_group=control_group,
        base_period=base_period,
        anticipation=anticipation,
    )

    d = _prepare_estimation_data(
        data=data,
        outcome=outcome,
        unit=unit,
        time=time,
        first_treat=first_treat,
        matching_vars=matching_vars,
        cohorts=cohorts,
    )

    requested_aggregations = _normalize_aggregations(aggregations)
    requested_bootstrap_aggregations = requested_aggregations & _BOOTSTRAPPED_AGGREGATIONS

    rng = np.random.default_rng(random_state)

    att_gt_draws = []
    event_draws = []
    group_draws = []
    cal_draws = []
    simple_draws = []
    stratum_draws = []
    stratum_event_draws = []

    for b in range(bootstrap_iters):
        boot_df = _bootstrap_resample_clusters(d, unit=unit, rng=rng, draw_idx=b)

        try:
            out = _compute_point_estimates_and_aggregates(
                data=boot_df,
                outcome=outcome,
                unit=unit,
                time=time,
                first_treat=first_treat,
                matching_vars=matching_vars,
                cutpoints=cutpoints,
                cohorts=cohorts,
                control_group=control_group,
                base_period=base_period,
                anticipation=anticipation,
                aggregations=requested_bootstrap_aggregations,
            )
        except Exception:
            continue

        if not out["att_gt"].empty:
            tmp = out["att_gt"][["group", "time", "effect"]].copy()
            tmp["boot_iter"] = b
            att_gt_draws.append(tmp)

        if ("event_study" in requested_bootstrap_aggregations) and (not out["event_study"].empty):
            tmp = out["event_study"][["event_time", "effect"]].copy()
            tmp["boot_iter"] = b
            event_draws.append(tmp)

        if ("group" in requested_bootstrap_aggregations) and (not out["group"].empty):
            tmp = out["group"][["group", "effect"]].copy()
            tmp["boot_iter"] = b
            group_draws.append(tmp)

        if ("calendar" in requested_bootstrap_aggregations) and (not out["calendar"].empty):
            tmp = out["calendar"][["time", "effect"]].copy()
            tmp["boot_iter"] = b
            cal_draws.append(tmp)

        if ("simple" in requested_bootstrap_aggregations) and (not out["simple"].empty):
            tmp = out["simple"][["estimand", "effect"]].copy()
            tmp["boot_iter"] = b
            simple_draws.append(tmp)

        if ("stratum" in requested_bootstrap_aggregations) and (not out["stratum"].empty):
            tmp = out["stratum"][["stratum", "effect"]].copy()
            tmp["boot_iter"] = b
            stratum_draws.append(tmp)

        if (
            "stratum_event_study" in requested_bootstrap_aggregations
        ) and (not out["stratum_event_study"].empty):
            tmp = out["stratum_event_study"][["stratum", "event_time", "effect"]].copy()
            tmp["boot_iter"] = b
            stratum_event_draws.append(tmp)

    return {
        "att_gt": pd.concat(att_gt_draws, ignore_index=True) if att_gt_draws else pd.DataFrame(columns=["group", "time", "effect", "boot_iter"]),
        "event_study": pd.concat(event_draws, ignore_index=True) if event_draws else pd.DataFrame(columns=["event_time", "effect", "boot_iter"]),
        "group": pd.concat(group_draws, ignore_index=True) if group_draws else pd.DataFrame(columns=["group", "effect", "boot_iter"]),
        "calendar": pd.concat(cal_draws, ignore_index=True) if cal_draws else pd.DataFrame(columns=["time", "effect", "boot_iter"]),
        "simple": pd.concat(simple_draws, ignore_index=True) if simple_draws else pd.DataFrame(columns=["estimand", "effect", "boot_iter"]),
        "stratum": pd.concat(stratum_draws, ignore_index=True) if stratum_draws else pd.DataFrame(columns=["stratum", "effect", "boot_iter"]),
        "stratum_event_study": pd.concat(stratum_event_draws, ignore_index=True) if stratum_event_draws else pd.DataFrame(columns=["stratum", "event_time", "effect", "boot_iter"]),
    }


def _attach_bootstrap_stats(
    point_df: pd.DataFrame,
    boot_df: pd.DataFrame,
    key_cols: Sequence[str],
    ci_level: float = 0.95,
) -> pd.DataFrame:
    """
    Attaches:
    - se from bootstrap sd
    - percentile bootstrap CI
    - t_stat and p_value using normal approximation
    """
    out = point_df.copy()

    if out.empty:
        for col in ["se", "t_stat", "p_value", "conf_int_lower", "conf_int_upper", "n_boot"]:
            if col not in out.columns:
                out[col] = pd.Series(dtype=float)
        return out

    if boot_df.empty:
        out["se"] = np.nan
        out["t_stat"] = np.nan
        out["p_value"] = np.nan
        out["conf_int_lower"] = np.nan
        out["conf_int_upper"] = np.nan
        out["n_boot"] = 0
        return out

    alpha = 1.0 - ci_level

    grouped = (
        boot_df.groupby(list(key_cols))["effect"]
        .agg(
            se=lambda x: np.std(x, ddof=1),
            conf_int_lower=lambda x: np.quantile(x, alpha / 2.0),
            conf_int_upper=lambda x: np.quantile(x, 1.0 - alpha / 2.0),
            n_boot="count",
        )
        .reset_index()
    )

    out = out.merge(grouped, on=list(key_cols), how="left")

    out["t_stat"] = np.where(
        (pd.notna(out["se"])) & (out["se"] > 0),
        out["effect"] / out["se"],
        np.nan,
    )
    out["p_value"] = out["t_stat"].apply(_p_value_from_t)

    return out


# ============================================================
# Estimator class
# ============================================================

class MatchedCEMCallawaySantAnna:
    """
    Draft estimator with a diff_diff-like interface.

    Current restrictions:
    - control_group = 'never_treated' only
    - base_period = 'universal' only
    - anticipation = 0 only

    Matching:
    - cohort-specific CEM at g-1
    - controls are never-treated only
    """

    def __init__(
        self,
        matching_vars: Sequence[str],
        cutpoints: Optional[Dict[str, Sequence[float]]] = None,
        control_group: str = "never_treated",
        anticipation: int = 0,
        base_period: str = "universal",
        bootstrap_iters: int = 199,
        ci_level: float = 0.95,
        compute_confidence_intervals: bool = True,
        random_state: Optional[int] = None,
    ):
        _validate_inputs(
            control_group=control_group,
            base_period=base_period,
            anticipation=anticipation,
        )

        self.matching_vars = list(matching_vars)
        self.cutpoints = cutpoints or {}
        self.control_group = control_group
        self.anticipation = anticipation
        self.base_period = base_period
        self.bootstrap_iters = int(bootstrap_iters)
        self.ci_level = float(ci_level)
        self.compute_confidence_intervals = bool(compute_confidence_intervals)
        self.random_state = random_state

    def fit(
        self,
        data: pd.DataFrame,
        outcome: str,
        unit: str,
        time: str,
        first_treat: str,
        aggregate: str | Sequence[str] = "all",
        se_aggregate: Optional[str | Sequence[str]] = "all",
        cohorts: Optional[Sequence[int]] = None,
        compute_confidence_intervals: Optional[bool] = None,
    ) -> MatchedCEMCSResults:
        """
        aggregate selects which aggregate outputs should be computed.
        Use "all" for the full set, or pass a string / list of strings
        such as ["event_study", "simple", "stratum"].
        se_aggregate selects which requested aggregate outputs should get
        bootstrap standard errors. Use "all" for all selected aggregations,
        or pass None / [] for no aggregate-level bootstrap standard errors.
        If compute_confidence_intervals is False, bootstrap is skipped and
        inference columns are returned as NaN / 0-boot placeholders.
        """
        requested_aggregations = _normalize_aggregations(aggregate)
        requested_se_aggregations = _normalize_se_aggregations(
            se_aggregate=se_aggregate,
            requested_aggregations=requested_aggregations,
        )
        compute_ci = (
            self.compute_confidence_intervals
            if compute_confidence_intervals is None
            else bool(compute_confidence_intervals)
        )
        effective_bootstrap_iters = self.bootstrap_iters if compute_ci else 0

        point = _compute_point_estimates_and_aggregates(
            data=data,
            outcome=outcome,
            unit=unit,
            time=time,
            first_treat=first_treat,
            matching_vars=self.matching_vars,
            cutpoints=self.cutpoints,
            cohorts=cohorts,
            control_group=self.control_group,
            base_period=self.base_period,
            anticipation=self.anticipation,
            aggregations=requested_aggregations,
        )

        if effective_bootstrap_iters > 0:
            boots = bootstrap_matched_att(
                data=data,
                outcome=outcome,
                unit=unit,
                time=time,
                first_treat=first_treat,
                matching_vars=self.matching_vars,
                cutpoints=self.cutpoints,
                cohorts=cohorts,
                control_group=self.control_group,
                base_period=self.base_period,
                anticipation=self.anticipation,
                bootstrap_iters=effective_bootstrap_iters,
                random_state=self.random_state,
                aggregations=requested_se_aggregations,
            )
        else:
            boots = {
                "att_gt": pd.DataFrame(),
                "event_study": pd.DataFrame(),
                "group": pd.DataFrame(),
                "calendar": pd.DataFrame(),
                "simple": pd.DataFrame(),
                "stratum": pd.DataFrame(),
                "stratum_event_study": pd.DataFrame(),
            }

        att_gt = _attach_bootstrap_stats(
            point_df=point["att_gt"],
            boot_df=boots["att_gt"],
            key_cols=["group", "time"],
            ci_level=self.ci_level,
        )

        event_study = _attach_bootstrap_stats(
            point_df=point["event_study"],
            boot_df=boots["event_study"],
            key_cols=["event_time"],
            ci_level=self.ci_level,
        )

        group_df = _attach_bootstrap_stats(
            point_df=point["group"],
            boot_df=boots["group"],
            key_cols=["group"],
            ci_level=self.ci_level,
        )

        calendar_df = _attach_bootstrap_stats(
            point_df=point["calendar"],
            boot_df=boots["calendar"],
            key_cols=["time"],
            ci_level=self.ci_level,
        )

        simple_df = _attach_bootstrap_stats(
            point_df=point["simple"],
            boot_df=boots["simple"],
            key_cols=["estimand"],
            ci_level=self.ci_level,
        )

        stratum_df = _attach_bootstrap_stats(
            point_df=point["stratum"],
            boot_df=boots["stratum"],
            key_cols=["stratum"],
            ci_level=self.ci_level,
        )

        stratum_event_study_df = _attach_bootstrap_stats(
            point_df=point["stratum_event_study"],
            boot_df=boots["stratum_event_study"],
            key_cols=["stratum", "event_time"],
            ci_level=self.ci_level,
        )

        metadata = {
            "matching_vars": self.matching_vars,
            "cutpoints": self.cutpoints,
            "aggregations": sorted(requested_aggregations),
            "se_aggregations": sorted(requested_se_aggregations),
            "control_group": self.control_group,
            "base_period": self.base_period,
            "anticipation": self.anticipation,
            "bootstrap_iters": effective_bootstrap_iters,
            "ci_level": self.ci_level,
            "compute_confidence_intervals": compute_ci,
            "aggregate_argument": aggregate,
            "se_aggregate_argument": se_aggregate,
            "cohorts": None if cohorts is None else list(cohorts),
        }

        return MatchedCEMCSResults(
            att_gt=att_gt,
            event_study=event_study,
            group_effects_df=group_df,
            calendar_effects_df=calendar_df,
            simple_effect_df=simple_df,
            stratum_effects_df=stratum_df,
            stratum_event_study_df=stratum_event_study_df,
            match_report_df=point["match_report"],
            cell_debug_df=point["cell_debug"],
            metadata=metadata,
        )
