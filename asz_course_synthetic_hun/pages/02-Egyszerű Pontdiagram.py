import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib.ticker import FuncFormatter, LogLocator
import statsmodels.api as sm  # for spline regression
import utils

# Optional for LOWESS
try:
    from statsmodels.nonparametric.smoothers_lowess import lowess
    HAS_LOWESS = True
except Exception:
    HAS_LOWESS = False

# ----------------------- Setup ------------------------
col_settings, col_viz = utils.setup_page(
    "Pontdiagram — 2019 keresztmetszet",
    "Pontdiagram — 2019 keresztmetszet (szimulált)"
)
cs = utils.load_cross_section(st.session_state["data_path"])

# ---- NEW: create ln_sales and ln_emp as extra variables ----
if "sales_clean" in cs.columns and "ln_sales" not in cs.columns:
    cs["ln_sales"] = np.log(np.clip(cs["sales_clean"].astype(float), 1e-9, None))
if "emp" in cs.columns and "ln_emp" not in cs.columns:
    emp_vals = pd.to_numeric(cs["emp"], errors="coerce")
    with np.errstate(divide="ignore", invalid="ignore"):
        cs["ln_emp"] = np.where(emp_vals > 0, np.log(emp_vals), np.nan)

st.markdown(
    "Válasszon egy **ágazatot**, két **változót**, és egy opcionális **illesztést**. "
    "A pénzügyi adatok **millió forintban** szerepelnek."
)

# ----------------------- Beállítások (Bal oldal) ---------------------------
with col_settings:
    st.header("Beállítások")

    sync_on = utils.render_sync_option(st)

    # Ágazati opciók
    lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
    lab_df["__code"] = pd.to_numeric(
        lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False), errors="coerce"
    )
    lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")
    opts = ["Összes ágazat"] + lab_df["label"].tolist()
    
    ind_idx = utils.get_synced_index(opts, "global_industry")
    sel_label = st.selectbox("Ágazat", opts, index=ind_idx)
    utils.update_synced_state("global_industry", sel_label)

scope_all = sel_label == "Összes ágazat"

# Változók
MONETARY_VARS = utils.get_monetary_vars()
extra_vars = {
    "Relatív növekedés (%)": "sales_growth_perc", "Log növekedés (log-diff)": "sales_growth_log_diff",
    "Log értékesítés": "ln_sales", "Log foglalkoztatottak száma": "ln_emp"
}
var_map = {**extra_vars, **utils.NON_MONETARY_VARS, **MONETARY_VARS}

available = {k: v for k, v in var_map.items() if v in cs.columns}

with col_settings:
    avail_keys = list(available.keys())
    
    # Sync Secondary Continuous (X)
    x_idx = utils.get_synced_index(avail_keys, "global_secondary_var")
    if x_idx == 0 and len(avail_keys) > 2: x_idx = 2 # Default fallback
    x_label = st.selectbox("X változó", avail_keys, index=x_idx)

    # Sync Primary Continuous (Y)
    y_idx = utils.get_synced_index(avail_keys, "global_primary_var")
    y_label = st.selectbox("Y változó", avail_keys, index=y_idx)

    utils.update_synced_state("global_secondary_var", x_label)
    utils.update_synced_state("global_primary_var", y_label)

xvar = available[x_label]
yvar = available[y_label]
x_is_monetary = xvar in MONETARY_VARS.values()
y_is_monetary = yvar in MONETARY_VARS.values()

# ----------------------- Szélsőérték-kezelés -----------------------
with col_settings:
    with st.expander("Szélsőérték-kezelés"):
        x_filter = st.selectbox("X szélsőérték-kezelése", utils.FILTER_OPTIONS, index=0)
        y_filter = st.selectbox("Y szélsőérték-kezelése", utils.FILTER_OPTIONS, index=0)

        df = cs.copy() if scope_all else cs[cs["nace2_name_code"] == sel_label].copy()
        df = df.replace([np.inf, -np.inf], np.nan).dropna(subset=[xvar, yvar])

        # pénzügyiek ezres megjelenítés (millió Ft -> /1000)
        if x_is_monetary:
            df[xvar] = df[xvar] / 1000.0
        if y_is_monetary:
            df[yvar] = df[yvar] / 1000.0

        # Manuális X-határok (változó egységében)
        if x_filter == "Kézi minimum/maximum":
            st.markdown("**X kézi határok (a megjelenített egységben)**")
            current_min_x = float(np.nanmin(df[xvar]))
            current_max_x = float(np.nanmax(df[xvar]))
            x_low_manual = st.number_input(
                "X minimum",
                value=current_min_x,
                step=(current_max_x - current_min_x) / 100
                if current_max_x > current_min_x
                else 1.0,
            )
            x_high_manual = st.number_input(
                "X maximum",
                value=current_max_x,
                step=(current_max_x - current_min_x) / 100
                if current_max_x > current_min_x
                else 1.0,
            )
            if x_low_manual > x_high_manual:
                st.error("X esetén a minimum nem lehet nagyobb a maximumnál.")
                x_low_manual, x_high_manual = x_high_manual, x_low_manual
        else:
            x_low_manual = None
            x_high_manual = None

        # Manuális Y-határok (változó egységében)
        if y_filter == "Kézi minimum/maximum":
            st.markdown("**Y kézi határok (a megjelenített egységben)**")
            current_min_y = float(np.nanmin(df[yvar]))
            current_max_y = float(np.nanmax(df[yvar]))
            y_low_manual = st.number_input(
                "Y minimum",
                value=current_min_y,
                step=(current_max_y - current_min_y) / 100
                if current_max_y > current_min_y
                else 1.0,
            )
            y_high_manual = st.number_input(
                "Y maximum",
                value=current_max_y,
                step=(current_max_y - current_min_y) / 100
                if current_max_y > current_min_y
                else 1.0,
            )
            if y_low_manual > y_high_manual:
                st.error("Y esetén a minimum nem lehet nagyobb a maximumnál.")
                y_low_manual, y_high_manual = y_high_manual, y_low_manual
        else:
            y_low_manual = None
            y_high_manual = None

# ----------------------- Bin scatter opció (új) -----------------------
with col_settings:
    with st.expander("Ábra beállítások"):
        BIN_SCATTER_OPTIONS = ["Eredeti", "5 bin", "10 bin", "20 bin", "100 bin"]
        bin_scatter_choice = st.selectbox("Pontdiagram típusa", BIN_SCATTER_OPTIONS, index=0)
        bin_scatter_map = {"5 bin": 5, "10 bin": 10, "20 bin": 20, "100 bin": 100}
        n_bins = bin_scatter_map.get(bin_scatter_choice, None)
        use_bin_scatter = n_bins is not None

        fit_type = st.selectbox(
            "Illesztés rárajzolása (eredeti adaton, nem a bin-eken)",
            [
                "Nincs",
                "Lineáris",
                "Kvadratikus",
                "Köbös",
                "Lépcsős (5 bin)",
                "Lépcsős (20 bin)",
                "LOWESS",
                "Lineáris spline",
            ],
            index=0,
        )
        if fit_type == "LOWESS" and not HAS_LOWESS:
            st.warning("A statsmodels LOWESS nem elérhető; válasszon másik illesztést.")

        # --- Lineáris spline beállítások (X-re) ---
        spline_use = False
        spline_knots = []

        if fit_type == "Lineáris spline":
            st.markdown("#### Lineáris spline (X változó)")
            series_for_knots = df[xvar].replace([np.inf, -np.inf], np.nan).dropna()
            if len(series_for_knots) < 5:
                st.warning("Nincs elég adat a spline-hoz.")
            else:
                spline_use = True
                if spline_use:
                    n_knots = st.selectbox("Határontok száma", [1, 2], index=0)
                    q1 = float(series_for_knots.quantile(1 / 3))
                    q2 = float(series_for_knots.quantile(2 / 3))
                    k1 = st.number_input(f"Első határ (X egységében)", value=q1)
                    spline_knots = [k1]
                    if n_knots == 2:
                        k2 = st.number_input(f"Második határ (X egységében)", value=q2)
                        spline_knots.append(k2)
                    spline_knots = sorted(spline_knots)

# Log skálák
with col_settings:
    with st.expander("Megjelenítés és Skála"):
        st.markdown("#### Skála")
        logx = st.checkbox("Logaritmikus skála X", value=False)
        logy = st.checkbox("Logaritmikus skála Y", value=False)

        st.markdown("#### Megjelenítés")
        fig_width = st.slider("Ábra szélessége", 4.0, 16.0, 8.0, 0.5)
        fig_height = st.slider("Ábra magassága", 2.0, 8.0, 4.0, 0.5)
        # Plot megjelenés
        alpha = st.slider("Pontok átlátszósága", 0.1, 1.0, 0.5, 0.05)
        size = st.slider("Pontméret", 5, 100, 20, 1)

# ----------------------- Szűrés és előkészítés ---------------------

# Log-hoz csak pozitívak maradnak
if logx:
    df = df[df[xvar] > 0]
if logy:
    df = df[df[yvar] > 0]

if df.empty:
    st.error("Nincs adat a megadott feltételekhez.")
    st.stop()

x = utils.apply_filter(df[xvar], x_filter, x_low_manual, x_high_manual)
y = utils.apply_filter(df[yvar], y_filter, y_low_manual, y_high_manual)

# közös index a két sorozatra
idx = x.index.intersection(y.index)
plot_df = pd.DataFrame({xvar: x.loc[idx], yvar: y.loc[idx]})

if plot_df.empty:
    st.error("A szélsőérték-kezelés után nincs megjeleníthető adat.")
    st.stop()

# ----------------------- Bin scatter előkészítés -----------------------
scatter_df = plot_df.copy()
if use_bin_scatter and len(plot_df) >= 2:
    try:
        plot_df["__bin_scatter"] = pd.qcut(plot_df[xvar], q=n_bins, duplicates="drop")
        gb_scatter = plot_df.groupby("__bin_scatter", observed=True)
        scatter_df = gb_scatter[[xvar, yvar]].mean().reset_index(drop=True)
        plot_df.drop(columns="__bin_scatter", inplace=True)
    except Exception:
        scatter_df = plot_df.copy()

# ----------------------- Transzformációk (fit a transzformált térben) -----------------------
def fwd_x(v):
    return np.log(v) if logx else v


def inv_x(v):
    return np.exp(v) if logx else v


def fwd_y(v):
    return np.log(v) if logy else v


def inv_y(v):
    return np.exp(v) if logy else v


# ----------------------- Ábra -----------------------------
with col_viz:
    fig, ax = plt.subplots(figsize=(fig_width, fig_height))
    sns.scatterplot(
        data=scatter_df,
        x=xvar,
        y=yvar,
        s=size,
        alpha=alpha,
        edgecolor="white",
        linewidth=0.2,
        color=utils.COLORS[0],
        ax=ax,
    )

    coef_text = None

    # Illesztések (mindig a transzformált térben számoljuk, majd visszatérünk az eredeti skálára)
    if fit_type in {"Lineáris", "Kvadratikus", "Köbös"} and len(plot_df) >= 10:
        deg = {"Lineáris": 1, "Kvadratikus": 2, "Köbös": 3}[fit_type]
        xx = fwd_x(plot_df[xvar].to_numpy())
        yy = fwd_y(plot_df[yvar].to_numpy())
        order = np.argsort(xx)
        xx_sorted = xx[order]
        yy_sorted = yy[order]
        coef = np.polyfit(xx_sorted, yy_sorted, deg=deg)
        poly = np.poly1d(coef)

        xs_orig = np.linspace(plot_df[xvar].min(), plot_df[xvar].max(), 400)
        xs_tr = fwd_x(xs_orig)
        ys_tr = poly(xs_tr)
        ys_orig = inv_y(ys_tr)
        ax.plot(
            xs_orig,
            ys_orig,
            color=utils.COLORS[1],
            linewidth=2,
            alpha=0.9,
            label=f"{fit_type} illesztés",
        )

        if deg == 1:
            a, b = coef[1], coef[0]
            yname = "ln(y)" if logy else "y"
            xname = "ln(x)" if logx else "x"
            coef_text = f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname}"
        elif deg == 2:
            a, b, c = coef[2], coef[1], coef[0]
            xname = "ln(x)" if logx else "x"
            yname = "ln(y)" if logy else "y"
            coef_text = (
                f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + {c:.4g}·{xname}²"
            )
        else:
            a, b, c, d = coef[3], coef[2], coef[1], coef[0]
            xname = "ln(x)" if logx else "x"
            yname = "ln(y)" if logy else "y"
            coef_text = (
                f"{fit_type}: {yname} = {a:.4g} + {b:.4g}·{xname} + "
                f"{c:.4g}·{xname}² + {d:.4g}·{xname}³"
            )

    elif fit_type == "LOWESS" and HAS_LOWESS and len(plot_df) >= 10:
        x_tr = fwd_x(plot_df[xvar].values)
        y_tr = fwd_y(plot_df[yvar].values)
        z = lowess(y_tr, x_tr, frac=0.25, return_sorted=True)
        x_orig = inv_x(z[:, 0])
        y_orig = inv_y(z[:, 1])
        ax.plot(
            x_orig,
            y_orig,
            color=utils.COLORS[1],
            linewidth=2,
            alpha=0.9,
            label="LOWESS illesztés",
        )

    elif (
        fit_type == "Lineáris spline"
        and spline_use
        and len(plot_df) >= 10
        and len(spline_knots) > 0
    ):
        # Spline illesztés a transzformált X-re
        xx_tr = fwd_x(plot_df[xvar].astype(float))
        yy_tr = fwd_y(plot_df[yvar].astype(float))

        # Kötések a transzformált térben is
        knots_tr = fwd_x(np.array(spline_knots, dtype=float))
        X_spline = utils.lspline(pd.Series(xx_tr, index=plot_df.index), knots_tr)
        X_design = sm.add_constant(X_spline)
        model = sm.OLS(yy_tr.values.astype(float), X_design.astype(float))
        res = model.fit(cov_type="HC1")

        # Előrejelzés a teljes X-tartományon
        xs_orig = np.linspace(plot_df[xvar].min(), plot_df[xvar].max(), 400)
        xs_tr = fwd_x(xs_orig)
        X_pred = utils.lspline(pd.Series(xs_tr), knots_tr)
        X_pred_design = sm.add_constant(X_pred)
        yhat_tr = res.predict(X_pred_design)
        yhat_orig = inv_y(yhat_tr)

        ax.plot(
            xs_orig,
            yhat_orig,
            color=utils.COLORS[1],
            linewidth=2,
            alpha=0.9,
            label="Lineáris spline illesztés",
        )

        # Koeficiens-egyenlet (szegmens-bázis formában)
        coefs = res.params
        intercept = coefs[0]
        betas = coefs[1:]
        yname = "ln(y)" if logy else "y"
        xname = "ln(x)" if logx else "x"
        terms = " + ".join([f"{b:.4g}·s{i+1}({xname})" for i, b in enumerate(betas)])
        coef_text = f"Lineáris spline: {yname} = {intercept:.4g}" + (
            f" + {terms}" if terms else ""
        )

    elif fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
        steps = 5 if "5" in fit_type else 20

        # 1. Quantile edges on filtered x
        x_vals = plot_df[xvar].to_numpy()
        q_grid = np.linspace(0, 1, steps + 1)
        edges = np.quantile(x_vals, q_grid)

        # 2. Keep only strictly increasing edges (positive-width bins)
        edges_clean = [edges[0]]
        for e in edges[1:]:
            if e > edges_clean[-1]:
                edges_clean.append(e)

        if len(edges_clean) <= 1:
            st.warning(
                "Lépcsős illesztés: nem sikerült pozitív szélességű kvantilis bin-eket képezni."
            )
        else:
            bins = np.array(edges_clean)

            # 3. Cut using our quantile bins; bins are Interval objects
            plot_df["__bin"] = pd.cut(plot_df[xvar], bins=bins, include_lowest=True)

            gb = plot_df.groupby("__bin", observed=False)

            # mean y in transformed space
            y_mean_tr = fwd_y(gb[yvar].mean())
            y_mean = inv_y(y_mean_tr)

            # 4. Draw a segment from bin.left to bin.right for each non-empty bin
            for interval, y_m in y_mean.dropna().items():
                left = interval.left
                right = interval.right
                ax.hlines(
                    y=y_m,
                    xmin=left,
                    xmax=right,
                    colors=utils.COLORS[1],
                    linewidth=3,
                    alpha=0.9,
                )

            plot_df.drop(columns="__bin", inplace=True)

    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)
    ax.spines[["top", "right"]].set_visible(False)

    # --- X axis ---
    if logx:
        ax.set_xscale("log", base=np.e)
        ax.xaxis.set_major_locator(LogLocator(base=np.e))
        ax.xaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{np.log(v):.2f}")  # ln(v)
        )
    else:
        ax.ticklabel_format(style="plain", axis="x")
        ax.xaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{v:,.2f}" if x_is_monetary else f"{v:,.0f}")
        )

    ax.tick_params(axis="x", labelrotation=25)

    # --- Y axis ---
    if logy:
        ax.set_yscale("log", base=np.e)
        ax.yaxis.set_major_locator(LogLocator(base=np.e))
        ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{np.log(v):.2f}"))
    else:
        ax.yaxis.set_major_formatter(
            FuncFormatter(lambda v, _: f"{v:,.2f}" if y_is_monetary else f"{v:,.2f}")
        )

    if logx:
        ax.set_xlabel(f"ln({x_label})")
    if logy:
        ax.set_ylabel(f"ln({y_label})")

    if fit_type != "Nincs":
        ax.legend(frameon=False)

    plt.tight_layout()
    st.pyplot(fig)

# ---------- Bin boundary table for stepwise fits (no duplicate dropping) ----------
if fit_type.startswith("Lépcsős") and len(plot_df) >= 10:
    steps = 5 if "5" in fit_type else 20

    # Use the x-values actually used for the fit (after all filters)
    x_vals = plot_df[xvar].to_numpy()

    # Quantile positions for the requested number of bins
    q_grid = np.linspace(0, 1, steps + 1)

    # Bin edges by quantiles (this keeps duplicates!)
    edges = np.quantile(x_vals, q_grid)

    # --- compute mean Y in each bin, using these edges ---
    y_means = []
    for i, (low, high) in enumerate(zip(edges[:-1], edges[1:])):
        # Handle NaNs in edges gracefully
        if np.isnan(low) or np.isnan(high):
            y_means.append(np.nan)
            continue

        # Include lower bound; upper bound is open except (optionally) last bin
        if i < len(edges) - 2:
            mask = (plot_df[xvar] >= low) & (plot_df[xvar] < high)
        else:
            mask = (plot_df[xvar] >= low) & (plot_df[xvar] <= high)

        y_vals = plot_df.loc[mask, yvar]
        y_means.append(y_vals.mean() if not y_vals.empty else np.nan)

    bin_table = pd.DataFrame({
        "Rekesz": np.arange(1, steps + 1),
        "Alsó Határ": edges[:-1],
        "Felső Határ": edges[1:],
        f"{y_label} átlag": y_means,
    })

    st.markdown("**Lépcsős illesztés – Rekeszhatárok és bin-átlagok (Y):**")

    # Formatting: pénzügyi Y változó esetén ezres csoportosítás, különben 3 tizedes
    format_dict = {
        "Alsó Határ": "{:.3f}",
        "Felső Határ": "{:.3f}",
    }
    if y_is_monetary:
        format_dict[f"{y_label} átlag"] = "{:,.2f}"
    else:
        format_dict[f"{y_label} átlag"] = "{:.3f}"

    with col_viz:
        st.dataframe(bin_table.style.format(format_dict))


# ----------------------- Összegzés --------------------------
scope_label = sel_label
tail_note_x = utils.tail_note_txt(x_filter, x_low_manual, x_high_manual)
tail_note_y = utils.tail_note_txt(y_filter, y_low_manual, y_high_manual)
bin_note = bin_scatter_choice

with col_viz:
    st.markdown(f"**Minta:** {scope_label} · **X:** `{x_label}` · **Y:** `{y_label}`")

    if coef_text is not None:
        st.markdown(f"**Illesztési egyenlet (transzformált térben):** {coef_text}")
