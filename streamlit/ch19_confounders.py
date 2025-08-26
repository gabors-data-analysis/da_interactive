import streamlit as st
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.set_page_config(layout="wide", page_title="Ch. 19: Confounders")

st.title("Chapter 19: Confounders and Bad Conditioners in Causal Inference")

st.markdown("""
## Introduction

This interactive dashboard illustrates **confounding** and **bad conditioning** in causal inference 
using a simulated study of the effect of **caffeine intake** on **cognitive ability**.

### Key Concepts
- **Treatment (X):** Caffeine consumed in the 2 hours before a cognitive test (mg).
- **Outcome (Y):** Cognitive test score (0–100).
- **Confounders:** Variables that influence both treatment and outcome (e.g., **sleep hours**, **age**).
- **Bad Conditioner:** A variable affected by both the treatment and the outcome (here: **heart rate**), 
  which can introduce **collider bias** if controlled for.
- **True Effect:** The actual causal effect of caffeine on cognitive ability, which you can set in the sidebar.

### How to Use the Dashboard
1. **Adjust Simulation Settings** in the sidebar:
   - Change the sample size, random seed, and noise level.
   - Set causal coefficients for each relationship in the system.
2. **Inspect Regression Models**:
   - Two separate models (A and B) allow you to compare different control variable choices.
   - Coefficients are shown with statistical significance stars (`***`, `**`, `*`) and standard errors.
   - R² values summarize model fit.
3. **Visualize Caffeine’s Effect**:
   - The coefficient plot compares the estimated caffeine effect across both models.
   - Error bars represent 95% confidence intervals.
   - The dashed vertical line shows the **true effect** for reference.
4. **Explore the Simulated Data**:
   - A summary table of the generated dataset is provided at the bottom.

### Goal
By experimenting with the controls, you can see how **including or excluding** certain variables 
affects the estimated treatment effect. This helps illustrate why controlling for **confounders** is 
essential and why controlling for **colliders** can be harmful.
""")


# Sidebar: Simulation Controls
st.sidebar.header("Simulation Settings")
seed = st.sidebar.number_input("Random Seed", value=42, step=1)
sample_size = st.sidebar.number_input("Sample Size", value=10000, step=100)
noise_level = st.sidebar.slider("Noise Level (SD)", 0.01, 5.0, 1.0, step=0.01)

np.random.seed(seed)

st.sidebar.subheader("Causal Coefficients")

coef_caffeine_to_cog = st.sidebar.slider("Caffeine → Cognitive Ability", -1.0, 1.0, 0.3, step=0.05)

coef_sleep_to_caffeine = st.sidebar.slider("Sleep → Caffeine", -1.0, 1.0, -0.5, step=0.05)
coef_age_to_caffeine = st.sidebar.slider("Age → Caffeine", -1.0, 1.0, 0.4, step=0.05)

coef_sleep_to_cog = st.sidebar.slider("Sleep → Cognitive Ability", -1.0, 1.0, 0.6, step=0.05)
coef_age_to_cog = st.sidebar.slider("Age → Cognitive Ability", -1.0, 1.0, 0.4, step=0.05)

coef_caffeine_to_hr = st.sidebar.slider("Caffeine → Heart Rate", 0.0, 2.0, 1.0, step=0.05)
coef_cog_to_hr = st.sidebar.slider("Cognitive Ability → Heart Rate", -2.0, 0.0, -0.5, step=0.05)

st.sidebar.subheader("Intercepts")
intercept_caffeine = st.sidebar.slider("Intercept: Caffeine", -50.0, 50.0, 0.0, step=1.0)
intercept_cognitive = st.sidebar.slider("Intercept: Cognitive Ability", -30.0, 70.0, 40.0, step=1.0)
intercept_hr = st.sidebar.slider("Intercept: Heart Rate", 40.0, 100.0, 60.0, step=1.0)

# --- Causal DAG (Graphviz) ---

label = lambda x: f"{x:.2f}"

dot = f"""
digraph G {{
  rankdir=TB;  // top-to-bottom layout
  node [shape=box, style=rounded, fontsize=12];

  // Node definitions with highlights
  caffeine [label="caffeine\\n(mg)", style="filled,rounded", fillcolor="#3a5e8c", fontcolor="white"];
  cognitive_ability [label="cognitive ability\\n(test score)", style="filled,rounded", fillcolor="#10a53d", fontcolor="white"];
  sleep [label="sleep"];
  age [label="age"];
  heart_rate [label="heart rate"];

  // Force row alignment
  {{ rank=same; sleep; age }}
  {{ rank=same; caffeine; cognitive_ability }}
  {{ rank=same; heart_rate }}

  // Invisible edges to center align across rows
  sleep -> caffeine [style=invis];
  age -> cognitive_ability [style=invis];
  caffeine -> heart_rate [style=invis];
  cognitive_ability -> heart_rate [style=invis];

  // Real causal edges with labels
  sleep -> caffeine [label="{label(coef_sleep_to_caffeine)}"];
  age -> caffeine [label="{label(coef_age_to_caffeine)}"];

  sleep -> cognitive_ability [label="{label(coef_sleep_to_cog)}"];
  age -> cognitive_ability [label="{label(coef_age_to_cog)}"];
  caffeine -> cognitive_ability [label="{label(coef_caffeine_to_cog)}"];

  caffeine -> heart_rate [label="{label(coef_caffeine_to_hr)}"];
  cognitive_ability -> heart_rate [label="{label(coef_cog_to_hr)}"];

  // aesthetics
  edge [fontsize=10];
  node [fontsize=11];
}}
"""

st.subheader("Causal Diagram (DAG)")
st.caption("Edge labels show the true coefficients used in the data-generating process. You can modify these on the sidebar.")
st.graphviz_chart(dot)


# Simulate exogenous variables
sleep = np.random.normal(7, 1, sample_size)
sleep = np.clip(sleep, 0, 12)  # Ensure sleep is within a realistic range
age = np.random.normal(45, 10, sample_size)
age = np.clip(age, 15, 75)  # Ensure age is within a realistic range

# Simulate endogenous variables based on structural equations
caffeine = (
    intercept_caffeine +
    coef_sleep_to_caffeine * sleep +
    coef_age_to_caffeine * age +
    np.random.normal(0, noise_level, sample_size)
)
# caffeine = np.clip(caffeine, 0, 200)

cognitive_ability = (
    intercept_cognitive +
    coef_sleep_to_cog * sleep +
    coef_age_to_cog * age +
    coef_caffeine_to_cog * caffeine +
    np.random.normal(0, noise_level, sample_size)
)
# cognitive_ability = np.clip(cognitive_ability, 0, 100)

heart_rate = (
    intercept_hr +
    coef_caffeine_to_hr * caffeine +
    coef_cog_to_hr * cognitive_ability +
    np.random.normal(0, noise_level, sample_size)
)
# heart_rate = np.clip(heart_rate, 50, 150)

# Create dataframe
df = pd.DataFrame({
    "sleep": sleep,
    "age": age,
    "caffeine": caffeine,
    "cognitive_ability": cognitive_ability,
    "heart_rate": heart_rate
})

# Download button
st.sidebar.download_button(
    label="Download Simulated Data",
    data=df.to_csv(index=False).encode("utf-8"),
    file_name="ch19_confounders_data.csv",
    mime="text/csv"
)

# Model controls
st.subheader("Regression Models")
col1, col2 = st.columns(2)

with col1:
    st.markdown("### Model A")
    z_vars_a = st.multiselect("Add control variables (Model A)", ["sleep", "age", "heart_rate"], key="a")

with col2:
    st.markdown("### Model B")
    z_vars_b = st.multiselect("Add control variables (Model B)", ["sleep", "age", "heart_rate"], key="b")

# Run regression
def run_model(z_vars):
    X = df[["caffeine"] + z_vars]
    X = sm.add_constant(X)
    model = sm.OLS(df["cognitive_ability"], X).fit(cov_type='HC1')
    return model

model_a = run_model(z_vars_a)
model_b = run_model(z_vars_b)

def format_regression_table(model):
    params = model.params
    bse = model.bse
    pvalues = model.pvalues
    
    def stars(p):
        if p < 0.001:
            return '***'
        elif p < 0.01:
            return '**'
        elif p < 0.05:
            return '*'
        else:
            return ''
    
    formatted = []
    for var in params.index:
        coef_str = f"{params[var]:.3f}{stars(pvalues[var])}"
        se_str = f"({bse[var]:.3f})"
        formatted.append([coef_str, se_str])
    
    table = pd.DataFrame(formatted, index=params.index, columns=["Coef.", "SE"])
    
    # Add R-squared at the end
    table.loc["R²", "Coef."] = f"{model.rsquared:.3f}"
    table.loc["R²", "SE"] = ""
    
    return table

# Output results
with col1:
    st.markdown("##### Regression Output A")
    st.dataframe(format_regression_table(model_a))

with col2:
    st.markdown("##### Regression Output B")
    st.dataframe(format_regression_table(model_b))

st.markdown("Note: *** significant at 1%, ** significant at 5%, * significant at 10%. Robust standard errors used (HC1).")

# --- Coefficient Plot for Caffeine ---
coef_true = coef_caffeine_to_cog  # From sidebar slider

# Extract estimates and CI for caffeine
def get_caffeine_ci(model):
    est = model.params["caffeine"]
    se = model.bse["caffeine"]
    lower = est - 1.96 * se
    upper = est + 1.96 * se
    return est, lower, upper

est_a, lower_a, upper_a = get_caffeine_ci(model_a)
est_b, lower_b, upper_b = get_caffeine_ci(model_b)

coef_df = pd.DataFrame({
    "Model": ["Model B", "Model A"],
    "Estimate": [est_b, est_a],
    "Lower": [lower_b, lower_a],
    "Upper": [upper_b, upper_a]
})

fig, ax = plt.subplots(figsize=(5, 2))

# Horizontal bars
ax.barh(
    y=coef_df["Model"],
    width=coef_df["Estimate"],
    xerr=[coef_df["Estimate"] - coef_df["Lower"], coef_df["Upper"] - coef_df["Estimate"]],
    capsize=5,
    color=color[0],
    edgecolor="black"
)

# Vertical line for true coefficient
ax.axvline(coef_true, color=color[1], linestyle="--", label="True Coefficient")

ax.set_xlabel("Coefficient Estimate")
ax.set_title("Caffeine Effect Estimates (95% CI)")
ax.spines[['top', 'right']].set_visible(False)
ax.legend()

st.pyplot(fig)


# Preview data
st.markdown("### Simulated Dataset Summary")
st.dataframe(df.describe().T)
