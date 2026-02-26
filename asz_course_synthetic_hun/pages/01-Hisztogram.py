import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import utils

# --- State Persistence Helper ---
def persist(key, default):
    if key not in st.session_state: st.session_state[key] = default
    return st.session_state[key]
def save(key):
    st.session_state[key] = st.session_state[f"_{key}"]

# ----------------------------- Setup ------------------------------
col_settings, col_viz = utils.setup_page(
    "Eloszlások vizualizálása",
    "Eloszlások vizualizálása — 2019 keresztmetszet (szimulált)"
)
cs = utils.load_cross_section(st.session_state["data_path"])

st.markdown(
    "Válasszon egy **ágazatot** és egy **változót** a megjelenítéshez. A pénzügyi adatok **millió forintban** szerepelnek."
)

# ----------------------------- Sidebar -----------------------------
with col_settings:
    st.header("Beállítások")

    # Build industry options: ALL first, then sorted by numeric code in the label
    lab_df = pd.DataFrame({"label": cs["nace2_name_code"].dropna().unique()})
    lab_df["__code"] = pd.to_numeric(
        lab_df["label"].str.extract(r"\((\d{1,2})\)\s*$", expand=False),
        errors="coerce",
    )
    lab_df = lab_df.sort_values(["__code", "label"]).drop(columns="__code")

    labels = ["Összes ágazat"] + lab_df["label"].tolist()
    default_industry = labels[1] if len(labels) > 1 else labels[0]
    selected_label = st.selectbox(
        "Ágazat",
        labels,
        index=labels.index(persist("p01_industry", default_industry)),
        key="_p01_industry",
        on_change=save, args=("p01_industry",)
    )
# Variable selection — monetary variables will be displayed in million HUF
MONETARY_VARS = utils.get_monetary_vars()
var_map = {**MONETARY_VARS, **utils.NON_MONETARY_VARS}

available = [k for k, v in var_map.items() if v in cs.columns]
if not available:
    st.error("A várt változók közül egyik sem található az adatban.")
    st.stop()

with col_settings:
    default_var = available[0] if available else None
    var_label = st.selectbox(
        "Megjelenítendő változó",
        available,
        index=available.index(persist("p01_var", default_var)) if default_var and persist("p01_var", default_var) in available else 0,
        key="_p01_var",
        on_change=save, args=("p01_var",)
    )

var = var_map[var_label]
is_monetary = var in MONETARY_VARS.values()

# ----------------------------- Filter ------------------------------
if selected_label == "Összes ágazat":
    workset = cs.copy()
else:
    workset = cs[cs["nace2_name_code"] == selected_label].copy()

if workset.empty:
    st.error("Nincs adat a választott szűrési feltételekhez.")
    st.stop()

# scale to million HUF when monetary
x_raw = workset[var].replace([np.inf, -np.inf], np.nan).dropna()
x = (x_raw / 1000.0) if is_monetary else x_raw

if x.empty:
    st.warning("A kiválasztott változó nem tartalmaz érvényes adatot a szűrés után.")
    st.stop()

# -------------------------- Tail handling (mutually exclusive) --------------------------
with col_settings:
    with st.expander("Szélsőérték-kezelés"):
        saved_tail = persist("p01_tail", "Nincs szűrés (összes érték)")
        tail_idx = utils.FILTER_OPTIONS.index(saved_tail) if saved_tail in utils.FILTER_OPTIONS else 0
        tail_mode = st.selectbox("X szélsőérték-kezelése", utils.FILTER_OPTIONS, index=tail_idx, key="_p01_tail", on_change=save, args=("p01_tail",))

        # Manual min/max controls (shown only when selected)
        if tail_mode == "Kézi minimum/maximum":
            st.markdown("**Kézi határok (a megjelenített egységben)**")
            # sensible defaults: current min/max of x
            current_min = float(np.nanmin(x))
            current_max = float(np.nanmax(x))
            manual_min = st.number_input(
                "Minimum",
                value=current_min,
                step=(current_max - current_min) / 100
                if current_max > current_min
                else 1.0,
            )
            manual_max = st.number_input(
                "Maximum",
                value=current_max,
                step=(current_max - current_min) / 100
                if current_max > current_min
                else 1.0,
            )
            if manual_min > manual_max:
                st.error("A minimum nem lehet nagyobb a maximum­nál.")
                # swap to avoid crash and still continue
                manual_min, manual_max = manual_max, manual_min
        else:
            manual_min = None
            manual_max = None

    # ----------------------------- Histogram settings -----------------------------
    with st.expander("Hisztogram beállítások"):
        bins = st.slider("Binek száma", min_value=5, max_value=60, value=persist("p01_bins", 25), step=1, key="_p01_bins", on_change=save, args=("p01_bins",))

    # ----------------------------- Log option -----------------------------
    with st.expander("Skála"):
        use_log = st.checkbox("Logaritmikus (ln) transzformáció", value=persist("p01_log", False), key="_p01_log", on_change=save, args=("p01_log",))

# Apply tail handling
if tail_mode == "Kézi minimum/maximum":
    x_filtered = x[(x >= manual_min) & (x <= manual_max)]
else:  # "Nincs szűrés (összes érték)"
    x_filtered = x

# Log transform (for plotting & stats)
log_note = ""
if use_log:
    # filter out non-positive values before log
    pos_mask = x_filtered > 0
    if not np.any(pos_mask):
        st.warning(
            "Logaritmikus ábrázoláshoz pozitív értékek szükségesek. A szűrés után nem maradt pozitív érték."
        )
        st.stop()
    x_plot = np.log(x_filtered[pos_mask])
    log_note = " (ln)"
else:
    x_plot = x_filtered

if x_plot.empty:
    st.warning("A kiválasztott beállításokkal nem maradt megjeleníthető adat.")
    st.stop()

# ----------------------------- Plot -------------------------------
with col_viz:
    fig, ax = plt.subplots()

    # Compute histogram and hide small bins (<5 obs)
    counts, edges = np.histogram(x_plot, bins=bins)
    counts_masked = counts.copy()
    counts_masked[counts_masked < 5] = 0

    widths = np.diff(edges)
    ax.bar(
        edges[:-1],
        counts_masked,
        width=widths,
        align="edge",
        color=utils.COLORS[0],
        edgecolor="white",
        linewidth=0.5,
    )

    st.header(
        f"{var_label} Hisztogram "
    )

    # Axis labels & formatting
    ax.set_xlabel(f"{var_label}{log_note}")
    ax.set_ylabel("Gyakoriság")
    ax.spines[["top", "right"]].set_visible(False)

    # Tick formatting
    if use_log:
        # log space usually small decimals
        ax.ticklabel_format(style="plain", axis="x")
        ax.xaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.2f}"))
    else:
        ax.ticklabel_format(style="plain", axis="x")
        ax.xaxis.set_major_formatter(
            FuncFormatter(
                lambda v, _: f"{v:,.2f}" if is_monetary else f"{v:,.0f}"
            )
        )
    ax.tick_params(axis="x", labelrotation=25)

    plt.tight_layout()
    st.pyplot(fig)

    # ----------------------- Descriptive statistics ----------------------
    x_stats = x_plot.replace([np.inf, -np.inf], np.nan).dropna()
    if not x_stats.empty:
        modes = x_stats.mode(dropna=True)
        mode_val = modes.iloc[0] if len(modes) > 0 else np.nan
        stats = {
            "minimum": float(np.nanmin(x_stats)),
            "maximum": float(np.nanmax(x_stats)),
            "terjedelem": float(np.nanmax(x_stats) - np.nanmin(x_stats)),
            "átlag": float(np.nanmean(x_stats)),
            "medián": float(np.nanmedian(x_stats)),
            "módusz": float(mode_val)
            if pd.api.types.is_numeric_dtype(x_stats)
            else mode_val,
            "szórás (minta)": float(x_stats.std(ddof=1)),
            "variancia (minta)": float(x_stats.var(ddof=1)),
            "ferdeség": float(x_stats.skew()),
        }
        st.subheader("Leíró statisztikák (a megjelenített adaton)")
        stats_df = pd.DataFrame(
            {"Statisztika": list(stats.keys()), "Érték": list(stats.values())}
        )

        def _fmt(v):
            try:
                # in log mode, keep two decimals for readability
                if use_log:
                    return f"{v:,.2f}"
                return f"{v:,.2f}" if is_monetary else f"{v:,.0f}"
            except Exception:
                return str(v)

        stats_df["Érték"] = stats_df["Érték"].map(_fmt)
        st.dataframe(stats_df, use_container_width=True)
    else:
        st.info("Nem maradt legalább 5 megfigyelés a statisztikához.")

    # ----------------------------- Summary ------------------------------
    scope_label = selected_label
    if tail_mode == "Kézi minimum/maximum":
        tail_note = f"Kézi határok: [{manual_min:,.2f}, {manual_max:,.2f}]"
    else:
        tail_note = "Nincs szűrés"

    unit_note = (
        "millió Ft"
        if is_monetary and not use_log
        else ("ln egység" if use_log else "nyers egység")
    )
    st.markdown(
        f"**Minta:** {scope_label} · **Változó:** `{var_label}` · "
        f"**Megfigyelések (megjelenítve):** {len(x_plot):,} · **Szélek:** {tail_note} · **Binek:** {bins} · "
        f"**Egység:** {unit_note} · **A 5-nél kevesebb megfigyelésű oszlopok rejtve vannak.**"
    )
