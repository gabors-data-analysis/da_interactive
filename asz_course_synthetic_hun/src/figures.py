import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import re
from matplotlib.ticker import FixedLocator
from utils import COLORS

PRIMARY_COLOR = COLORS[0]
SECONDARY_COLOR = COLORS[1]
ACCENT_COLOR = COLORS[2]
LINE_COLOR = COLORS[4]

def plot_match_count_heatmaps(
    match_report_df,
    figsize=(14, 10),
    annotate=False,
):
    """
    Two separate heatmaps:
    1) number of treated by cohort x stratum
    2) number of controls by cohort x stratum

    Input:
        match_report_df from res.to_dataframe("match_report")
    """
    df = match_report_df.copy()

    if df.empty:
        raise ValueError("match_report_df is empty.")

    treated_mat = (
        df.pivot(index="group", columns="stratum", values="n_treated")
          .fillna(0)
          .sort_index()
    )
    control_mat = (
        df.pivot(index="group", columns="stratum", values="n_control")
          .fillna(0)
          .sort_index()
    )
    sorted_strata = _sort_stratum_values(treated_mat.columns)
    treated_mat = treated_mat.reindex(columns=sorted_strata)
    control_mat = control_mat.reindex(columns=sorted_strata)

    fig, axes = plt.subplots(2, 1, figsize=figsize, constrained_layout=True)

    im0 = axes[0].imshow(treated_mat.values, aspect="auto")
    axes[0].set_title("Matched treated counts by cohort and stratum")
    axes[0].set_ylabel("Cohort")
    axes[0].set_xticks(range(len(treated_mat.columns)))
    axes[0].set_xticklabels(treated_mat.columns, rotation=90)
    axes[0].set_yticks(range(len(treated_mat.index)))
    axes[0].set_yticklabels(treated_mat.index)
    fig.colorbar(im0, ax=axes[0], shrink=0.9)

    im1 = axes[1].imshow(control_mat.values, aspect="auto")
    axes[1].set_title("Matched control counts by cohort and stratum")
    axes[1].set_ylabel("Cohort")
    axes[1].set_xlabel("Stratum")
    axes[1].set_xticks(range(len(control_mat.columns)))
    axes[1].set_xticklabels(control_mat.columns, rotation=90)
    axes[1].set_yticks(range(len(control_mat.index)))
    axes[1].set_yticklabels(control_mat.index)
    fig.colorbar(im1, ax=axes[1], shrink=0.9)

    if annotate:
        for ax, mat in zip(axes, [treated_mat, control_mat]):
            for i in range(mat.shape[0]):
                for j in range(mat.shape[1]):
                    ax.text(j, i, int(mat.iloc[i, j]), ha="center", va="center", fontsize=8)

    return fig, axes



def plot_match_support_summaries(
    match_report_df,
    figsize=(14, 12),
):
    """
    Four separate bar charts at the cohort level:
    1) number of matched strata
    2) total matched treated firms
    3) total matched controls
    4) Herfindahl concentration of stratum weights

    Input:
        match_report_df from res.to_dataframe("match_report")
    """
    df = match_report_df.copy()

    if df.empty:
        raise ValueError("match_report_df is empty.")

    cohort_summary = (
        df.groupby("group")
          .agg(
              n_matched_strata=("stratum", "nunique"),
              total_matched_treated=("n_treated_total_matched", "max"),
              total_matched_controls=("n_control_total_matched", "max"),
              herfindahl=("treated_weight", lambda x: np.sum(np.square(x))),
          )
          .reset_index()
          .sort_values("group")
    )

    metrics = [
        ("n_matched_strata", "Number of matched strata"),
        ("total_matched_treated", "Total matched treated firms"),
        ("total_matched_controls", "Total matched control firms"),
        ("herfindahl", "Weight concentration (Herfindahl)"),
    ]

    fig, axes = plt.subplots(2, 2, figsize=figsize, constrained_layout=True)
    axes = axes.ravel()

    for ax, (col, title) in zip(axes, metrics):
        ax.bar(cohort_summary["group"].astype(str), cohort_summary[col])
        ax.set_title(title)
        ax.set_xlabel("Cohort")
        ax.spines[["top", "right"]].set_visible(False)

    return fig, axes, cohort_summary


def plot_cell_change_scatter(
    cell_debug_df,
    cohort,
    time,
    figsize=(7, 6),
    size_scale=1200,
    annotate=True,
):
    """
    Scatter for one selected cell:
    x = control_change
    y = treated_change
    point size = stratum_weight

    Distance above the 45-degree line is the stratum effect.
    """
    df = cell_debug_df.copy()
    df = df[(df["group"] == cohort) & (df["time"] == time)].copy()

    if df.empty:
        raise ValueError(f"No rows found for cohort={cohort}, time={time}.")

    df = _sort_by_stratum(df)

    fig, ax = plt.subplots(figsize=figsize)

    sizes = size_scale * df["stratum_weight"].to_numpy()
    ax.scatter(df["control_change"], df["treated_change"], s=sizes)

    all_vals = pd.concat([df["control_change"], df["treated_change"]])
    lo, hi = all_vals.min(), all_vals.max()
    ax.plot([lo, hi], [lo, hi], linestyle="--", linewidth=1, color="0.5")

    if annotate:
        for _, row in df.iterrows():
            ax.annotate(
                row["stratum"],
                (row["control_change"], row["treated_change"]),
                fontsize=8,
                alpha=0.85,
            )

    ax.set_xlabel("Control change")
    ax.set_ylabel("Treated change")
    ax.set_title(f"Stratum-level changes: cohort {cohort}, year {time}")
    ax.spines[["top", "right"]].set_visible(False)

    return fig, ax, df


def plot_cell_waterfall(
    cell_debug_df,
    cohort,
    time,
    sort_by="weighted_contribution",
    ascending=False,
    figsize=(10, 5),
):
    """
    Waterfall chart for one selected cell (group, time).

    Uses weighted_contribution, which should sum to ATT(g,t).
    """
    df = cell_debug_df.copy()
    df = df[(df["group"] == cohort) & (df["time"] == time)].copy()

    if df.empty:
        raise ValueError(f"No rows found for cohort={cohort}, time={time}.")

    df = df.sort_values(sort_by, ascending=ascending).reset_index(drop=True)

    cumulative = np.cumsum(df["weighted_contribution"].to_numpy())
    starts = np.concatenate([[0], cumulative[:-1]])

    fig, ax = plt.subplots(figsize=figsize)

    for i, row in df.iterrows():
        ax.bar(
            x=i,
            height=row["weighted_contribution"],
            bottom=starts[i],
            width=0.8,
            label=row["stratum"] if i == 0 else None,
        )

    ax.axhline(0, linestyle="--", linewidth=1, color="0.5")
    ax.scatter(len(df), cumulative[-1], color="black", zorder=4)

    ax.set_xticks(list(range(len(df))))
    ax.set_xticklabels(df["stratum"], rotation=90)
    ax.set_xlabel("Stratum")
    ax.set_ylabel("Cumulative contribution")
    ax.set_title(f"Waterfall decomposition: cohort {cohort}, year {time}")
    ax.spines[["top", "right"]].set_visible(False)

    return fig, ax, df


def plot_cohort_reconciliation(
    att_gt_df,
    cell_debug_df,
    cohort,
    figsize=(11, 6),
    effect_color=PRIMARY_COLOR,
):
    """
    Calendar-year reconciliation for one cohort.

    - Line: final ATT(g,t)
    - Stacked bars: weighted_contribution by stratum
    """
    att = att_gt_df.copy()
    att = att[att["group"] == cohort].copy()
    if att.empty:
        raise ValueError(f"No ATT(g,t) rows found for cohort={cohort}.")

    dbg = cell_debug_df.copy()
    dbg = dbg[dbg["group"] == cohort].copy()
    if dbg.empty:
        raise ValueError(f"No cell debug rows found for cohort={cohort}.")

    contrib = (
        dbg.pivot_table(
            index="time",
            columns="stratum",
            values="weighted_contribution",
            aggfunc="sum",
            fill_value=0.0,
        )
        .sort_index()
    )
    contrib = contrib.reindex(columns=_sort_stratum_values(contrib.columns))

    years = contrib.index.to_numpy()

    fig, ax = plt.subplots(figsize=figsize)

    bottom_pos = np.zeros(len(years))
    bottom_neg = np.zeros(len(years))

    for stratum in contrib.columns:
        vals = contrib[stratum].to_numpy()
        pos = np.where(vals > 0, vals, 0)
        neg = np.where(vals < 0, vals, 0)

        ax.bar(years, pos, bottom=bottom_pos, width=0.7, label=stratum)
        ax.bar(years, neg, bottom=bottom_neg, width=0.7)

        bottom_pos += pos
        bottom_neg += neg

    att_line = att.sort_values("time")
    ax.plot(
        att_line["time"],
        att_line["effect"],
        marker="o",
        linewidth=2,
        color=effect_color,
        label="ATT(g,t)",
        zorder=4,
    )

    ax.axhline(0, linestyle="--", linewidth=1, color="0.5")
    ax.set_xlabel("Calendar year")
    ax.set_ylabel("ATT / weighted contribution")
    ax.set_title(f"Cohort-year reconciliation plot: cohort {cohort}")
    ax.spines[["top", "right"]].set_visible(False)
    ax.legend(frameon=False, bbox_to_anchor=(1.02, 1), loc="upper left")

    return fig, ax, contrib


def _effect_error_arrays(df, effect_col="effect", se_multiplier=1.96):
    """
    Return error arrays for Matplotlib from CI columns or an se column.

    If conf_int_lower/conf_int_upper exist, those are used directly.
    Otherwise, se_multiplier * se is used when se exists.
    """
    if effect_col not in df.columns:
        return None, None

    effect = pd.to_numeric(df[effect_col], errors="coerce")

    if {"conf_int_lower", "conf_int_upper"}.issubset(df.columns):
        lower = effect - pd.to_numeric(df["conf_int_lower"], errors="coerce")
        upper = pd.to_numeric(df["conf_int_upper"], errors="coerce") - effect
    elif "se" in df.columns:
        se = se_multiplier * pd.to_numeric(df["se"], errors="coerce")
        lower = se
        upper = se
    else:
        return None, None

    lower_arr = np.maximum(lower.to_numpy(dtype=float), 0.0)
    upper_arr = np.maximum(upper.to_numpy(dtype=float), 0.0)
    valid = np.isfinite(lower_arr) & np.isfinite(upper_arr)

    if not valid.any():
        return None, None

    return np.vstack([lower_arr, upper_arr]), valid


def _filter_event_window(df, n_pre=None, n_post=None, event_col="event_time"):
    if n_pre is None and n_post is None:
        return df.copy()

    out = df.copy()
    event_time = pd.to_numeric(out[event_col], errors="coerce")
    unique_times = sorted(event_time.dropna().unique())
    pre_times = [x for x in unique_times if x < 0]
    post_times = [x for x in unique_times if x >= 0]

    if n_pre is not None:
        pre_times = pre_times[-int(n_pre):]
    if n_post is not None:
        post_times = post_times[:int(n_post)]

    keep_times = set(pre_times + post_times)
    return out[event_time.isin(keep_times)].copy()


def _add_event_time_anchor(df, group_cols, event_col="event_time", effect_col="effect"):
    out = df.copy()
    out["_is_anchor"] = pd.to_numeric(out[event_col], errors="coerce") == -1
    out.loc[out["_is_anchor"], effect_col] = 0.0

    anchor_rows = []
    for group_key, group_df in out.groupby(group_cols, dropna=False, sort=False):
        if not isinstance(group_key, tuple):
            group_key = (group_key,)

        if group_df["_is_anchor"].any():
            continue

        row = {col: np.nan for col in out.columns}
        for col, value in zip(group_cols, group_key):
            row[col] = value
        row[event_col] = -1.0
        row[effect_col] = 0.0
        row["_is_anchor"] = True
        if "post" in row:
            row["post"] = 0
        anchor_rows.append(row)

    if anchor_rows:
        out = pd.concat([out, pd.DataFrame(anchor_rows)], ignore_index=True)

    return out


def _set_integer_event_ticks(ax, values, axis="x"):
    vals = pd.to_numeric(pd.Series(values), errors="coerce").dropna().to_numpy(dtype=float)
    vals = vals[np.isfinite(vals)]
    if vals.size == 0:
        return

    lo = int(np.floor(vals.min()))
    hi = int(np.ceil(vals.max()))
    ticks = np.arange(lo, hi + 1)

    if axis == "x":
        ax.xaxis.set_major_locator(FixedLocator(ticks))
    else:
        ax.yaxis.set_major_locator(FixedLocator(ticks))


def _stratum_sort_key(value):
    text = str(value)
    text = re.sub(r"(?<![A-Za-z])-inf(?:inity)?(?![A-Za-z])", "-1e309", text, flags=re.IGNORECASE)
    text = re.sub(r"(?<![A-Za-z])\+?inf(?:inity)?(?![A-Za-z])", "1e309", text, flags=re.IGNORECASE)
    parts = re.split(r"([-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?)", text)
    key = []

    for part in parts:
        if not part:
            continue
        lowered = part.lower()
        if lowered in {"-inf", "-infinity"}:
            key.append((0, -np.inf))
        elif lowered in {"inf", "+inf", "infinity", "+infinity"}:
            key.append((0, np.inf))
        else:
            try:
                key.append((0, float(part)))
            except ValueError:
                key.append((1, lowered))

    return key


def _sort_stratum_values(values):
    return sorted(values, key=_stratum_sort_key)


def _sort_by_stratum(df):
    if df.empty or "stratum" not in df.columns:
        return df.copy()

    out = df.copy()
    order = sorted(range(len(out)), key=lambda i: _stratum_sort_key(out.iloc[i]["stratum"]))
    return out.iloc[order].reset_index(drop=True)


def _plot_pre_post_line(
    ax,
    df,
    x_col,
    y_col,
    pre_color,
    post_color,
    event_col="event_time",
    anchor_col="_is_anchor",
    linewidth=1.5,
    zorder=1,
):
    if df.empty:
        return

    event_time = pd.to_numeric(df[event_col], errors="coerce")
    if anchor_col in df.columns:
        is_anchor = df[anchor_col].fillna(False).astype(bool)
    else:
        is_anchor = event_time == -1

    pre_line = df[(event_time < 0) | is_anchor].sort_values(x_col)
    post_line = df[(event_time >= 0) | is_anchor].sort_values(x_col)

    if len(pre_line) > 1:
        ax.plot(
            pre_line[x_col],
            pre_line[y_col],
            color=pre_color,
            linewidth=linewidth,
            zorder=zorder,
        )
    if len(post_line) > 1:
        ax.plot(
            post_line[x_col],
            post_line[y_col],
            color=post_color,
            linewidth=linewidth,
            zorder=zorder,
        )


def plot_stratum_aggregated_att(
    cell_debug_df,
    figsize=(10, 6),
    sort_by="stratum",
    ascending=True,
    point_color=PRIMARY_COLOR,
    se_multiplier=1.96,
):
    """
    Coeffplot of pooled post-period stratum-level ATT's.

    If a pre-aggregated result table is passed and it contains
    conf_int_lower/conf_int_upper or se, uncertainty bars are plotted.

    If a raw cell_debug_df is passed, the choice made is:
    pooled across all cohorts and all post periods,
    weighted by n_treated_stratum.

    When only se exists, bars use +/- se_multiplier * se.
    """
    df = cell_debug_df.copy()

    if {"stratum", "effect"}.issubset(df.columns):
        agg = df.copy()
    else:
        df = df[df["post"] == 1].copy()

        if df.empty:
            raise ValueError("No post-treatment rows found in cell_debug_df.")

        rows = []
        for stratum, x in df.groupby("stratum", sort=True):
            rows.append(
                {
                    "stratum": stratum,
                    "effect": np.average(
                        x["stratum_effect"],
                        weights=x["n_treated_stratum"],
                    ),
                    "total_treated_weight": x["n_treated_stratum"].sum(),
                    "n_cells": len(x),
                    "n_cohorts": x["group"].nunique(),
                }
            )
        agg = pd.DataFrame(rows)

    if sort_by == "stratum":
        agg = _sort_by_stratum(agg)
        if not ascending:
            agg = agg.iloc[::-1].reset_index(drop=True)
    else:
        agg = agg.sort_values(sort_by, ascending=ascending).reset_index(drop=True)

    fig, ax = plt.subplots(figsize=figsize)
    y = np.arange(len(agg))

    ax.scatter(agg["effect"], y, color=point_color)
    xerr, valid = _effect_error_arrays(agg, se_multiplier=se_multiplier)
    if xerr is not None:
        ax.errorbar(
            agg.loc[valid, "effect"],
            y[valid],
            xerr=xerr[:, valid],
            fmt="none",
            ecolor=point_color,
            capsize=4,
            linewidth=1.2,
            zorder=2,
        )
    ax.axvline(0, linestyle="--", linewidth=1, color="0.5")

    ax.set_yticks(y)
    ax.set_yticklabels(agg["stratum"])
    ax.invert_yaxis()
    ax.set_xlabel("Aggregated post-treatment ATT")
    ax.set_ylabel("Stratum")
    ax.set_title("Pooled stratum-level ATT coefficients")
    ax.spines[["top", "right"]].set_visible(False)

    return fig, ax, agg


def plot_stratum_dynamic_effects(
    cell_debug_df,
    n_pre=None,
    n_post=None,
    ncols=3,
    figsize_per_panel=(5, 4),
    sharey=True,
    effect_color=PRIMARY_COLOR,
    pre_color=None,
    post_color=SECONDARY_COLOR,
    anchor_color=None,
    se_multiplier=1.96,
):
    """
    Separate pooled event-study plot for each stratum.

    If a pre-aggregated result table is passed and it contains
    conf_int_lower/conf_int_upper or se, uncertainty bars are plotted.

    If a raw cell_debug_df is passed, the choice made is:
    pooled across cohorts at each event_time,
    weighted by n_treated_stratum.

    When only se exists, bars use +/- se_multiplier * se.
    """
    df = cell_debug_df.copy()
    pre_color = effect_color if pre_color is None else pre_color
    anchor_color = pre_color if anchor_color is None else anchor_color

    if df.empty:
        raise ValueError("cell_debug_df is empty.")

    if {"stratum", "event_time", "effect"}.issubset(df.columns):
        dyn = df.copy()
    else:
        rows = []
        for (stratum, event_time), x in df.groupby(["stratum", "event_time"], sort=True):
            rows.append(
                {
                    "stratum": stratum,
                    "event_time": event_time,
                    "effect": np.average(
                        x["stratum_effect"],
                        weights=x["n_treated_stratum"],
                    ),
                    "n_cells": len(x),
                    "n_cohorts": x["group"].nunique(),
                    "total_treated_weight": x["n_treated_stratum"].sum(),
                }
            )
        dyn = pd.DataFrame(rows)

    dyn = _add_event_time_anchor(dyn, ["stratum"])
    dyn = (
        pd.concat(
            [
                _filter_event_window(x, n_pre=n_pre, n_post=n_post)
                for _, x in dyn.groupby("stratum", sort=False)
            ],
            ignore_index=True,
        )
        if not dyn.empty
        else dyn
    )
    if dyn.empty:
        raise ValueError("No stratum event-time rows found in the selected window.")

    strata = _sort_by_stratum(dyn[["stratum"]].drop_duplicates())["stratum"].tolist()
    n = len(strata)
    nrows = int(np.ceil(n / ncols))

    fig, axes = plt.subplots(
        nrows=nrows,
        ncols=ncols,
        figsize=(figsize_per_panel[0] * ncols, figsize_per_panel[1] * nrows),
        sharey=sharey,
        squeeze=False,
    )
    axes = axes.ravel()

    for ax, stratum in zip(axes, strata):
        d = dyn[dyn["stratum"] == stratum].sort_values("event_time").copy()

        _plot_pre_post_line(
            ax,
            d,
            "event_time",
            "effect",
            pre_color=pre_color,
            post_color=post_color,
            anchor_col="_is_anchor",
        )

        event_time = pd.to_numeric(d["event_time"], errors="coerce")
        is_anchor = event_time == -1
        masks = [
            ((event_time < 0) & (~is_anchor), pre_color),
            (event_time >= 0, post_color),
            (is_anchor, anchor_color),
        ]
        for mask, color in masks:
            points = d.loc[mask].copy()
            if points.empty:
                continue
            yerr, valid = _effect_error_arrays(points, se_multiplier=se_multiplier)
            if yerr is None:
                ax.scatter(
                    points["event_time"],
                    points["effect"],
                    color=color,
                    s=28,
                    zorder=3,
                )
            else:
                if (~valid).any():
                    ax.scatter(
                        points.loc[~valid, "event_time"],
                        points.loc[~valid, "effect"],
                        color=color,
                        s=28,
                        zorder=3,
                    )
                ax.errorbar(
                    points.loc[valid, "event_time"],
                    points.loc[valid, "effect"],
                    yerr=yerr[:, valid],
                    fmt="o",
                    color=color,
                    ecolor=color,
                    capsize=4,
                    markersize=5,
                    linewidth=1.2,
                    zorder=3,
                )
        ax.axhline(0, linestyle="--", linewidth=1, color="0.5")
        ax.axvline(0, linestyle=":", linewidth=1, color="0.5")

        ax.set_title(stratum)
        ax.set_xlabel("Event time")
        ax.set_ylabel("Stratum-level dynamic effect")
        _set_integer_event_ticks(ax, d["event_time"], axis="x")
        ax.spines[["top", "right"]].set_visible(False)

    for ax in axes[len(strata):]:
        ax.remove()

    fig.suptitle("Pooled dynamic effects by stratum", y=1.02)
    fig.tight_layout()

    return fig, axes, dyn


def plot_cs_cohorts(
    gt_df,
    cohorts=None,
    x="event_time",              # "event_time" or "time"
    n_pre=None,
    n_post=None,
    ncols=3,
    figsize_per_panel=(5, 4),
    sharey=True,
    pre_color=PRIMARY_COLOR,
    post_color=SECONDARY_COLOR,
    anchor_color=PRIMARY_COLOR,
    line_color=PRIMARY_COLOR,
    ci_level=1.96,
):
    """
    Plot Callaway-Sant'Anna ATT(g,t) paths by cohort.
    """
    if isinstance(gt_df, tuple):
        gt_df = gt_df[0]

    df = gt_df.copy()

    required = {"group", "time", "effect"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Missing required columns: {missing}")

    df = df.dropna(subset=["group", "time", "effect"]).copy()
    df["group"] = pd.to_numeric(df["group"], errors="coerce")
    df["time"] = pd.to_numeric(df["time"], errors="coerce")
    df["effect"] = pd.to_numeric(df["effect"], errors="coerce")
    df = df.dropna(subset=["group", "time", "effect"]).copy()

    # keep treated cohorts only
    df = df[df["group"] > 0].copy()

    # derive CI from se if needed
    has_ci = {"conf_int_lower", "conf_int_upper"}.issubset(df.columns)
    if not has_ci:
        if "se" not in df.columns:
            raise ValueError(
                "Need either conf_int_lower/conf_int_upper or se in gt_df."
            )
        df["se"] = pd.to_numeric(df["se"], errors="coerce")
        df["conf_int_lower"] = df["effect"] - ci_level * df["se"]
        df["conf_int_upper"] = df["effect"] + ci_level * df["se"]

    df["event_time"] = df["time"] - df["group"]

    all_cohorts = np.sort(df["group"].unique())
    if cohorts is None or len(cohorts) == 0:
        cohorts = all_cohorts
    else:
        cohorts = np.array(sorted(set(cohorts)))
        missing_cohorts = [c for c in cohorts if c not in all_cohorts]
        if missing_cohorts:
            raise ValueError(f"These cohorts are not in gt_df: {missing_cohorts}")

    n = len(cohorts)
    nrows = math.ceil(n / ncols)

    fig, axes = plt.subplots(
        nrows=nrows,
        ncols=ncols,
        figsize=(figsize_per_panel[0] * ncols, figsize_per_panel[1] * nrows),
        sharey=sharey,
        squeeze=False,
    )
    axes = axes.ravel()

    for ax, cohort in zip(axes, cohorts):
        dfg = df[df["group"] == cohort].copy()

        dfg = dfg[dfg["event_time"] != -1].copy()

        anchor = pd.DataFrame(
            {
                "group": [cohort],
                "time": [cohort - 1],
                "effect": [0.0],
                "conf_int_lower": [np.nan],
                "conf_int_upper": [np.nan],
                "event_time": [-1],
                "_is_anchor": [True],
            }
        )
        dfg["_is_anchor"] = False
        dfg = pd.concat([dfg, anchor], ignore_index=True)
        dfg = _filter_event_window(dfg, n_pre=n_pre, n_post=n_post)

        if x not in {"event_time", "time"}:
            raise ValueError("x must be either 'event_time' or 'time'")

        dfg = dfg.sort_values(x).copy()

        is_anchor = dfg["_is_anchor"]
        is_post = (dfg["event_time"] >= 0) & (~is_anchor)
        is_pre = (dfg["event_time"] < 0) & (~is_anchor)

        _plot_pre_post_line(
            ax,
            dfg,
            x,
            "effect",
            pre_color=line_color,
            post_color=post_color,
        )

        pre = dfg[is_pre].copy()
        if not pre.empty:
            yerr_pre = np.vstack([
                pre["effect"] - pre["conf_int_lower"],
                pre["conf_int_upper"] - pre["effect"],
            ])
            ax.errorbar(
                pre[x],
                pre["effect"],
                yerr=yerr_pre,
                fmt="o",
                color=pre_color,
                ecolor=pre_color,
                capsize=4,
                markersize=5,
                linewidth=1.2,
                zorder=3,
            )

        post = dfg[is_post].copy()
        if not post.empty:
            yerr_post = np.vstack([
                post["effect"] - post["conf_int_lower"],
                post["conf_int_upper"] - post["effect"],
            ])
            ax.errorbar(
                post[x],
                post["effect"],
                yerr=yerr_post,
                fmt="o",
                color=post_color,
                ecolor=post_color,
                capsize=4,
                markersize=5,
                linewidth=1.2,
                zorder=3,
            )

        anc = dfg[is_anchor]
        ax.scatter(
            anc[x],
            anc["effect"],
            color=anchor_color,
            s=28,
            zorder=4,
        )

        ax.axhline(0, linestyle="--", linewidth=1, color="0.5")

        if x == "event_time":
            ax.axvline(0, linestyle=":", linewidth=1, color="0.5")
            ax.set_xlabel("Event time (t - g)")
            _set_integer_event_ticks(ax, dfg[x], axis="x")
        else:
            ax.axvline(cohort, linestyle=":", linewidth=1, color="0.5")
            ax.set_xlabel("Year")

        ax.set_title(f"Cohort {int(cohort)}")
        ax.set_ylabel("ATT(g,t)")
        ax.spines[["top", "right"]].set_visible(False)

    for ax in axes[len(cohorts):]:
        ax.remove()

    fig.suptitle("Callaway-Sant'Anna cohort-specific effects", y=1.02)
    fig.tight_layout()
    return fig, axes


def plot_cs_event_study(
    event_study_effects,
    n_pre=None,
    n_post=None,
    figsize=(8, 5),
    pre_color=PRIMARY_COLOR,    
    post_color=SECONDARY_COLOR,
    anchor_color=PRIMARY_COLOR,
    line_color=PRIMARY_COLOR,
    title="Callaway-Sant'Anna event-study effects",
):
    """
    Plot event-study coefficients from results_cs.event_study_effects.
    """
    rows = []
    for k, v in event_study_effects.items():
        et = float(k)
        ci = v.get("conf_int", (np.nan, np.nan))
        rows.append(
            {
                "event_time": et,
                "effect": float(v["effect"]) if pd.notna(v["effect"]) else np.nan,
                "se": float(v["se"]) if pd.notna(v["se"]) else np.nan,
                "t_stat": float(v["t_stat"]) if pd.notna(v["t_stat"]) else np.nan,
                "p_value": float(v["p_value"]) if pd.notna(v["p_value"]) else np.nan,
                "conf_int_lower": float(ci[0]) if pd.notna(ci[0]) else np.nan,
                "conf_int_upper": float(ci[1]) if pd.notna(ci[1]) else np.nan,
                "n_groups": v.get("n_groups", np.nan),
            }
        )

    df = pd.DataFrame(rows).sort_values("event_time").reset_index(drop=True)

    if (-1 not in df["event_time"].values) and (-1.0 not in df["event_time"].values):
        df = pd.concat(
            [
                df,
                pd.DataFrame(
                    [{
                        "event_time": -1.0,
                        "effect": 0.0,
                        "se": np.nan,
                        "t_stat": np.nan,
                        "p_value": np.nan,
                        "conf_int_lower": np.nan,
                        "conf_int_upper": np.nan,
                        "n_groups": 0,
                    }]
                ),
            ],
            ignore_index=True,
        )

    df.loc[df["event_time"] == -1, "effect"] = 0.0
    df.loc[df["event_time"] == -1, ["se", "t_stat", "p_value", "conf_int_lower", "conf_int_upper"]] = np.nan

    pre_times = sorted([x for x in df["event_time"].unique() if x < 0])
    post_times = sorted([x for x in df["event_time"].unique() if x >= 0])

    if n_pre is not None:
        pre_times = pre_times[-n_pre:]
    if n_post is not None:
        post_times = post_times[:n_post]

    keep_times = pre_times + post_times
    df_plot = df[df["event_time"].isin(keep_times)].copy().sort_values("event_time")

    is_anchor = df_plot["event_time"] == -1
    is_pre = (df_plot["event_time"] < 0) & (~is_anchor)
    is_post = df_plot["event_time"] >= 0

    fig, ax = plt.subplots(figsize=figsize)

    _plot_pre_post_line(
        ax,
        df_plot,
        "event_time",
        "effect",
        pre_color=line_color,
        post_color=post_color,
        anchor_col="_is_anchor",
    )

    pre = df_plot[is_pre]
    if not pre.empty:
        yerr_pre = np.vstack([
            pre["effect"] - pre["conf_int_lower"],
            pre["conf_int_upper"] - pre["effect"],
        ])
        ax.errorbar(
            pre["event_time"],
            pre["effect"],
            yerr=yerr_pre,
            fmt="o",
            color=pre_color,
            ecolor=pre_color,
            capsize=4,
            markersize=5,
            linewidth=1.2,
            zorder=3,
        )

    post = df_plot[is_post]
    if not post.empty:
        yerr_post = np.vstack([
            post["effect"] - post["conf_int_lower"],
            post["conf_int_upper"] - post["effect"],
        ])
        ax.errorbar(
            post["event_time"],
            post["effect"],
            yerr=yerr_post,
            fmt="o",
            color=post_color,
            ecolor=post_color,
            capsize=4,
            markersize=5,
            linewidth=1.2,
            zorder=3,
        )

    anc = df_plot[is_anchor]
    if not anc.empty:
        ax.scatter(
            anc["event_time"],
            anc["effect"],
            color=anchor_color,
            s=28,
            zorder=4,
        )

    ax.axhline(0, linestyle="--", linewidth=1, color="0.5")
    ax.axvline(0, linestyle=":", linewidth=1, color="0.5")

    ax.set_xlabel("Event time")
    ax.set_ylabel("ATT")
    ax.set_title(title)
    _set_integer_event_ticks(ax, df_plot["event_time"], axis="x")
    ax.spines[["top", "right"]].set_visible(False)

    fig.tight_layout()
    return fig, ax, df_plot
