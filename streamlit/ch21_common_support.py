import streamlit as st
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn as sns
from sklearn.preprocessing import KBinsDiscretizer
import statsmodels.formula.api as smf

import warnings
warnings.filterwarnings("ignore")

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.set_page_config(page_title="Common Support", layout="wide")

st.title("Common Support")

st.markdown("""
### Introduction

This dashboard illustrates the concept of **common support** in causal inference using a synthetic dataset.  
We simulate the relationship between **log parental income** and **log annual income**, where having a **college degree** may change this relationship.  

### Key Concepts

- **Treatment & Outcome**  
  - **Treatment (`has_degree`)**: whether an individual has a college degree (1 = yes, 0 = no).  
  - **Outcome (`log_income`)**: log of annual income in USD.  
  - **Covariate (`log_parent_income`)**: log of parental annual income in USD.  

- **Common Support Regions**  
  - The **common support region** is the range of `log_parent_income` where both treated and control observations are common.  
  - **Low common support extensions** are ranges just outside the common support region where only a few treated observations exist. While these areas may contain some treated observations, they are not sufficient for reliable causal estimation, so these ranges should be left out of causal estimation with regression
  - Areas outside these ranges have **no support** and should be used for causal estimation.

- **Data-Generating Process**  
  - Log parental income is drawn from a normal distribution.  
  - The probability of having a degree depends on whether log parental income is in the high- or low-support regions.  
  - Log income is generated from **parabolic functions** of log parental income, with different intercepts, slopes, and curvature for treated and control groups.  
  - Random noise is added to reflect unobserved factors.

- **Estimation Methods**  
  1. **Naive OLS** – Fits a regression on the full dataset, ignoring common support.  
  2. **OLS with Common Support** – Restricts the regression to the overlap region.  
  3. **Coarsened Exact Matching (CEM)** – Groups data into bins of `log_parent_income` and compares treated and control units only within bins containing both groups.
  4. **Coarsened Exact Matching with Common Support** – Similar to CEM, but only includes bins that fall within the common support region.

### How to Use the Dashboard

- Adjust parameters in the **sidebar** to change:
  - Sample size and distribution of parental income.
  - Width and location of the high-support region.
  - Degree probabilities in high and low support areas.
  - Income curve parameters for treated and control groups.
  - Noise level and random seed.
  - Binning settings for CEM.

- Explore:
  - Scatterplot of `log_income` vs `log_parent_income` with shaded high/low support areas.
  - Distribution of parental income by degree status.
  - Numerical summary statistics.
  - A comparison of estimated effects from different methods versus the true effects.

Use these tools to see how **ignoring common support** can bias causal effect estimates, and how **restricting to the common support region** improves validity.
Observe that while regression results may provide biased results either because of the lack of common support (naive OLS) or because a linear model is a bad approximation of the true relationship (even if restricting to the common support region),
whereas CEM is more robust as it automatically excludes regions having no common support and can account for non-linear relationships as well.
""")

# Sidebar for settings
st.sidebar.header("Settings")
# data generation parameters
st.sidebar.subheader("Data Generation Parameters")
n_samples = st.sidebar.slider("Number of Samples", min_value=1000, max_value=10000, value=3000, step=100)
log_pincome_mean = st.sidebar.slider("Mean of Log Parental Income", value=10.4, min_value=8.0, max_value=14.0, step=0.1)
log_pincome_sd = st.sidebar.slider("Standard Deviation of Log Parental Income", value=0.4, min_value=0.1, max_value=1.0, step=0.05)

high_support_region = st.sidebar.slider("Common Support Region", min_value=9.0, max_value=14.0, value=(10.45, 11.25), step=0.05)
low_support_extension = st.sidebar.slider("Low  Common Support Extension", min_value=0.0, max_value=2.0, value=0.5, step=0.05)
high_support_treated_prob = st.sidebar.slider("Common Support Treated Probability", min_value=0.4, max_value=0.6, value=0.5, step=0.01)
low_support_treated_prob = st.sidebar.slider("Low Common Support Treated Probability", min_value=0.0, max_value=0.1, value=0.02, step=0.01)

st.sidebar.subheader("Treated and Control Group Data Generation")
treated_intercept = st.sidebar.number_input("Intercept for Treated Group", value=9.5, step=0.1)
treated_linear = st.sidebar.number_input("Linear Term for Treated Group", value=0.3, step=0.01)
treated_quadratic = st.sidebar.number_input("Quadratic Term for Treated Group", value=-0.05, step=0.01)

control_intercept = st.sidebar.number_input("Intercept for Control Group", value=8.5, step=0.1)
control_linear = st.sidebar.number_input("Linear Term for Control Group", value=0.3, step=0.01)
control_quadratic = st.sidebar.number_input("Quadratic Term for Control Group", value=-1.4, step=0.01)

parabola_center = st.sidebar.number_input("Parabola Center", value=11.0, step=0.1)

st.sidebar.subheader("Other Settings")
noise_std = st.sidebar.slider("Noise Standard Deviation", 0.0, 1.0, 0.12, 0.01)
random_seed = st.sidebar.number_input("Random Seed", value=42, step=1)
np.random.seed(random_seed)
st.sidebar.subheader("Binning Parameters")
nbins_parent_income = st.sidebar.slider("Number of Bins for Log Parental Income", 5, 500, 1000)
binning_strat = st.sidebar.selectbox("Binning Strategy",["Equal width", "Equal frequency"],index=1)
binning_strat = 'uniform' if binning_strat == "Equal width" else 'quantile'

# Generate synthetic data, may need to tweak parameters later to be more realistic
def generate_data():
    # Draw parental log-income
    log_pincome = np.random.normal(loc=log_pincome_mean, scale=log_pincome_sd, size=n_samples)

    # Initialize probabilities with zeros
    degree_prob = np.zeros(n_samples)

    # Assign probabilities by income region
    degree_prob[(log_pincome >= high_support_region[0] - low_support_extension) &
                (log_pincome < high_support_region[0])] = low_support_treated_prob

    degree_prob[(log_pincome >= high_support_region[0]) &
                (log_pincome < high_support_region[1])] = high_support_treated_prob

    degree_prob[(log_pincome >= high_support_region[1]) &
                (log_pincome < high_support_region[1] + low_support_extension)] = low_support_treated_prob

    # Draw degrees (vector)
    degree = np.random.binomial(1, degree_prob)

    # Income equation: treated vs control
    log_income = np.where(
        degree == 0,
        control_intercept + control_linear * log_pincome + control_quadratic * (log_pincome-parabola_center)**2,
        treated_intercept + treated_linear * log_pincome + treated_quadratic * (log_pincome-parabola_center)**2
    )

    # Add noise
    log_income += np.random.normal(0, noise_std, n_samples)

    # Create DataFrame
    data = pd.DataFrame({
        'log_income': log_income,
        'has_degree': degree,
        'log_parent_income': log_pincome
    })

    return data


# Calculate effect estimate using coarsaned exact matching
def cem_coef(data):
    # Discretize log parental income into bins
    discretizer = KBinsDiscretizer(
        n_bins=nbins_parent_income, 
        encode='ordinal', 
        strategy=binning_strat
    )
    data['parent_income_bin'] = discretizer.fit_transform(
        data[['log_parent_income']]
    ).astype(int)

    # Group by treatment status and bins
    treated = (
        data[data['has_degree'] == 1]
        .groupby('parent_income_bin')
        .agg(mean_log_income=('log_income', 'mean'),
             count=('log_income', 'count'))
        .rename(columns={'mean_log_income': 'treated_mean_log_income',
                         'count': 'treated_count'})
    )

    control = (
        data[data['has_degree'] == 0]
        .groupby('parent_income_bin')
        .agg(mean_log_income=('log_income', 'mean'),
             count=('log_income', 'count'))
        .rename(columns={'mean_log_income': 'control_mean_log_income',
                         'count': 'control_count'})
    )

    # Match bins with both treated and control observations
    matched = treated.join(control, how='inner')
    matched['total_count'] = matched['treated_count'] + matched['control_count']
    matched['treatment_effect'] = (
        matched['treated_mean_log_income'] - matched['control_mean_log_income']
    )

    # Weighted average treatment effect
    ate = np.average(
        matched['treatment_effect'], 
        weights=matched['total_count']
    ) if not matched.empty else np.nan

    income_min = high_support_region[0]
    income_max = high_support_region[1]

    treated_with_support = (
        data[(data['log_parent_income'] >= income_min) & (data['log_parent_income'] <= income_max) & (data['has_degree'] == 1)]
        .groupby('parent_income_bin')
        .agg(mean_log_income=('log_income', 'mean'),
             count=('log_income', 'count'))
        .rename(columns={'mean_log_income': 'treated_mean_log_income',
                         'count': 'treated_count'})
    )

    control_with_support = (
        data[(data['log_parent_income'] >= income_min) & (data['log_parent_income'] <= income_max) & (data['has_degree'] == 0)]
        .groupby('parent_income_bin')
        .agg(mean_log_income=('log_income', 'mean'),
             count=('log_income', 'count'))
        .rename(columns={'mean_log_income': 'control_mean_log_income',
                         'count': 'control_count'})
    )

    matched_with_support = treated_with_support.join(control_with_support, how='inner')
    matched_with_support['total_count'] = matched_with_support['treated_count'] + matched_with_support['control_count']
    matched_with_support['treatment_effect'] = (
        matched_with_support['treated_mean_log_income'] - matched_with_support['control_mean_log_income']
    )

    # Weighted average treatment effect
    ate_with_support = np.average(
        matched_with_support['treatment_effect'],
        weights=matched_with_support['total_count']
    ) if not matched_with_support.empty else np.nan

    return len(matched), ate, len(matched_with_support), ate_with_support


def ols_coef(data):
    model = smf.ols('log_income ~ has_degree + log_parent_income', data=data).fit()
    coef = model.params['has_degree']
    return coef

# calculate OLS effect estimate on the common support region
def ols_coef_with_support(data):
    income_min = high_support_region[0]
    income_max = high_support_region[1]

    mask = (
        (data["log_parent_income"] >= income_min) & (data["log_parent_income"] <= income_max)
    )

    data_matched = data[mask]

    model = smf.ols('log_income ~ has_degree + log_parent_income', data=data_matched).fit()
    coef = model.params['has_degree']
    return len(data_matched), coef

#def calc_ate(data):
#    ate_intercept = treated_intercept - control_intercept
#    ate_linear = treated_linear - control_linear
#    ate_quadratic = treated_quadratic - control_quadratic
#    effects = ate_intercept + ate_linear * data['log_parent_income'] + ate_quadratic * (data['log_parent_income'] - parabola_center)**2
#    return np.mean(effects)

data = generate_data()

st.sidebar.download_button(
    "Download Simulated Data",
    data.to_csv(index=False).encode("utf-8"),
    "ch21_common_support_data.csv",
    "text/csv"
)

st.sidebar.markdown('Code hosted on [Github](https://github.com/gabors-data-analysis/da_interactive/blob/main/streamlit/ch21_common_support.py).')

# summary statistics
st.subheader("Data Summary")
summary_frame = data.describe().T
summary_frame.index = ['Log Income', 'Has Degree', 'Log Parental Income']
st.dataframe(summary_frame)

data['Has degree'] = data['has_degree'].map({0: "No", 1: "Yes"})

def add_support_shading(ax):
    # Low-support left
    ax.axvspan(high_support_region[0] - low_support_extension, 
               high_support_region[0], 
               color="orange", alpha=0.2, label="Common Support (low)")
    # High-support
    ax.axvspan(high_support_region[0], 
               high_support_region[1], 
               color="green", alpha=0.15, label="Common Support")
    # Low-support right
    ax.axvspan(high_support_region[1], 
               high_support_region[1] + low_support_extension, 
               color="orange", alpha=0.2)

# Scatterplots of generated data
st.subheader("Scatterplot and Conditional Distribution")
col1, col2 = st.columns(2)
with col1:
    st.subheader("Log Income vs Log Parental Income")
    st.write("")
    st.write("")
    fig, ax = plt.subplots(figsize=(10, 6))
    sns.scatterplot(x=data['log_parent_income'], y=data['log_income'], hue=data['Has degree'],
                    alpha=0.6, ax=ax, palette=color)
    add_support_shading(ax)
    ax.annotate("Common\nSupport\n(low)", xy=(high_support_region[0] - low_support_extension / 2, np.quantile(data['log_income'], 0.01)), 
                ha="center", va="center", fontsize=12, color="orange")
    ax.annotate("Common\nSupport", xy=(high_support_region[0] + (high_support_region[1] - high_support_region[0]) / 2, np.quantile(data['log_income'], 0.01)), 
                ha="center", va="center", fontsize=12, color="green")
    ax.annotate("Common\nSupport\n(low)", xy=(high_support_region[1] + low_support_extension / 2, np.quantile(data['log_income'], 0.01)), 
                ha="center", va="center", fontsize=12, color="orange")
    ax.xaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val)/1000:.0f}K"))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val)/1000:.0f}K"))
    ax.set_xlabel("Parental Annual Income (USD)")
    ax.set_ylabel("Annual Income (USD)")
    ax.spines[['top', 'right']].set_visible(False)
    st.pyplot(fig)
with col2:
    st.subheader("Distribution of Log Parental Income by Degree Status")
    fig, ax = plt.subplots(figsize=(10, 6))
    sns.kdeplot(data=data, x='log_parent_income', hue=data['Has degree'], fill=False, ax=ax, palette=color)
    add_support_shading(ax)
    ax.annotate("Common\nSupport\n(low)", xy=(high_support_region[0] - low_support_extension / 2, 0.3), 
                ha="center", va="center", fontsize=12, color="orange")
    ax.annotate("Common\nSupport", xy=(high_support_region[0] + (high_support_region[1] - high_support_region[0]) / 2, 0.3), 
                ha="center", va="center", fontsize=12, color="green")
    ax.annotate("Common\nSupport\n(low)", xy=(high_support_region[1] + low_support_extension / 2, 0.3), 
                ha="center", va="center", fontsize=12, color="orange")
    ax.xaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val)/1000:.0f}K"))
    ax.set_xlabel("Parental Annual Income (USD)")
    ax.spines[['top', 'right']].set_visible(False)
    st.pyplot(fig)


# calculate estimates
bins_with_support, cem_estimate, bins_high_support, cem_high_support = cem_coef(data)
ols_full = ols_coef(data)
obs_with_support, ols_cs = ols_coef_with_support(data)
#full_ate = calc_ate(data)
#income_min = high_support_region[0]
#income_max = high_support_region[1]

#mask = (
#    (data["log_parent_income"] >= income_min) & (data["log_parent_income"] <= income_max)
#)

#common_support_ate = calc_ate(data[mask])

st.subheader("Effect Estimation Summary")

col1, col2 = st.columns(2)
# display written summary of results
with col1:
    st.markdown(f"""
    Out of {nbins_parent_income} possible bins, {bins_with_support} bins had matches for coarsaned matching, of which {bins_high_support} were in the common support region.

    Out of {n_samples} total observations, {obs_with_support} observations fall within the common support region of the distributions.

    The estimates of the treatment effect are as follows:

    - **Naive OLS (Full Sample)**: {ols_full:.3f}
    - **OLS (Common Support)**: {ols_cs:.3f}
    - **Coarsened Exact Matching (Full Sample)**: {cem_estimate:.3f}
    - **Coarsened Exact Matching (Common Support)**: {cem_high_support:.3f}
    """)

result_df = pd.DataFrame({
    "Estimator": [
        "Naive OLS\n(Full Sample)",
        "OLS\n(Common Support)",
        "Coarsened\nExact\nMatching\n(Full Sample)",
        "Coarsened\nExact\nMatching\n(Common Support)"
    ],
    "Estimate": [ols_full, ols_cs, cem_estimate, cem_high_support]
})

# Bar plot of estimates
with col2:
    fig, ax = plt.subplots()
    sns.barplot(data=result_df, x="Estimator", y="Estimate", ax=ax, palette=color)
    # add data labels
    for p in ax.patches:
        ax.annotate(f"{p.get_height():.2f}", (p.get_x() + p.get_width() / 2., p.get_height()),
                    ha='center', va='bottom', fontsize=12, color='black')
    ax.legend()
    ax.set_ylabel("Estimated Treatment Effect on Log Income")
    ax.spines[['top', 'right']].set_visible(False)
    st.pyplot(fig)