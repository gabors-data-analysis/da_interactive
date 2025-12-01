import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
import seaborn as sns
color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

# Function from utilities to relevant chapter
def create_sample_frame(
    vector: np.array, sample_size: int, n_samples=10000, with_replacement=False, seed=42
):

    rng = np.random.default_rng(seed)
    sample_frame = np.zeros((n_samples, sample_size))
    for i in range(n_samples):
        sample_frame[i] = rng.choice(vector, size=sample_size, replace=with_replacement)

    return sample_frame

st.set_page_config(page_title='Generalizing from Data', layout="wide")
st.title('Generalizing from Sample to Population - Standard Errors')
# Dashboard description
st.markdown('''
This dashboard illustrates the key concepts discussed in Chapter 5 about generalizing from data.
To do so, the user can create a population dataset with a set distribution (with given parameters) and a set number of observations.
From this population, a random sample of a set size is drawn.
The sample is then used to illustrate the concept of generalization.
The variable of interest is the mean of the population.
The user can then explore how we can calculate a confidence interval for the mean of the population based on three different approaches:

- repeated sampling from the population, to uncover the "true" standard error  (with a set number of samples and a set sample size)
- using the bootstrap approach (with a set number of bootstrap rounds and a set sample size)
- and using the standard error formula for the mean.
            
Play around with the settings in the sidebar to see how the results change.
Among other things you may observe that:
- the standard error of the mean is smaller when the sample size is larger
- the standard error of the mean is smaller when the population is more homogeneous
- the confidence intervals are wider when the alpha level is smaller (and thus the confidence level is larger)
- the confidence intervals are wider when the sample size is smaller
- the sampling and bootstrap distributions are more or less normal if enough samples are drawn regardless of the population distribution
- all three approaches yield very similar confidence intervals for the mean of the population.
''')

# User inputs on the sidebar
st.sidebar.header('Settings')
# Population settings
st.sidebar.subheader('Population Settings')
distr_type = st.sidebar.selectbox('Population Distribution', options=['Uniform', 'Normal', 'Lognormal', 'Power-law'], index=1)
population_size = st.sidebar.slider('Population Size', min_value=1000, max_value=10000, value=5000, step=250)
if distr_type == 'Normal':
    st.sidebar.info('Population is generated from a normal distribution with specified mean and standard deviation.')
    population_mean = st.sidebar.slider('Population Mean', min_value=0.0, max_value=100.0, value=0.0, step=1.0)
    population_std = st.sidebar.slider('Population Standard Deviation', min_value=0.1, max_value=10.0, value=1.0, step=0.5)
elif distr_type == 'Uniform':
    st.sidebar.info('Population is generated from a Uniform distribution with specified minimum and maximum values.')
    population_min = st.sidebar.slider('Population Minimum', min_value=0.0, max_value=50.0, value=25.0, step=1.0)
    population_max = st.sidebar.slider('Population Maximum', min_value=50.0, max_value=100.0, value=75.0, step=1.0)
elif distr_type == 'Lognormal':
    st.sidebar.info('Population is generated from a lognormal distribution with specified mean and standard deviation of the underlying normal distribution.')
    population_mean = st.sidebar.slider('Population Mean', min_value=0.0, max_value=4.0, value=2.0, step=0.2)
    population_std = st.sidebar.slider('Population Standard Deviation', min_value=0.1, max_value=0.5, value=0.25, step=0.01)
elif distr_type == 'Power-law':
    st.sidebar.info('Population is generated from a Power-law distribution with specified exponent and minimum value.')
    population_exponent = st.sidebar.slider('Population Exponent', min_value=5.0, max_value=25.0, value=5.0, step=0.5)
    population_min = st.sidebar.slider('Population Minimum', min_value=0.0, max_value=100.0, value=25.0, step=1.0)

# Sampling settings
st.sidebar.subheader('Sampling Settings')
sample_size = st.sidebar.slider('Sample Size', min_value=10, max_value=500, value=30, step=5,
                                help='The number of observations drawn from the population for one sample. Controls the size of the one selected sample the mean of which we are trying to generalize from, as well as the size of the repeated samples in repeated sampling.')
num_samples = st.sidebar.slider('Number of Samples', min_value=10, max_value=1000, value=100, step=10,
                                help='The number of independent samples drawn from the population to estimate the sampling distribution of the sample mean.')
# Bootstrap settings
st.sidebar.subheader('Bootstrap Settings')
num_bootstrap_rounds = st.sidebar.slider('Number of Bootstrap Rounds', min_value=10, max_value=1000, value=100, step=10,
                                         help='The number of bootstrap rounds to perform for estimating the sampling distribution of the sample mean.')
# Other settings
st.sidebar.subheader('Other Settings')
alpha = st.sidebar.slider('Alpha Level for CI', min_value=0.01, max_value=0.1, value=0.05, step=0.01)
rnd_state = st.sidebar.number_input('Random State', min_value=0, max_value=1000, value=42, step=1)
kde_overlay = st.sidebar.checkbox('Show Density Plot Overlay on Histograms', value=False)

np.random.seed(rnd_state)

# If dashboard turns out to be too slow, we can implement a button to run the simulation only when clicked.
# But currently it runs smooth enough without it.
#if st.sidebar.button('Run Simulation'):

# Generate population data
if distr_type == 'Normal':
    population = np.random.normal(loc=population_mean, scale=population_std, size=population_size)
elif distr_type == 'Uniform':
    population = np.random.uniform(low=population_min, high=population_max, size=population_size)
elif distr_type == 'Lognormal':
    population = np.random.lognormal(mean=population_mean, sigma=population_std, size=population_size)
elif distr_type == 'Power-law':
    population = (np.random.pareto(a=population_exponent, size=population_size) + 1) * population_min
population_df = pd.DataFrame({'value': population})
sample_df = population_df.sample(n=sample_size, replace=False, random_state=rnd_state)

st.sidebar.download_button(
    label="Download Population Data",
    data=sample_df.to_csv(index=False).encode('utf-8'),
    file_name='ch05_generalization_population_data.csv',
    mime='text/csv'
)
st.sidebar.download_button(
    label="Download Sample Data",
    data=sample_df.to_csv(index=False).encode('utf-8'),
    file_name='ch05_generalization_sample_data.csv',
    mime='text/csv'
)
st.sidebar.markdown('Code hosted on [Github](https://github.com/gabors-data-analysis/da_interactive/blob/main/streamlit/ch05_generalization_dashboard.py).')

# Visuals on population and sample
col1, col2 = st.columns(2)
with col1:
    st.subheader('Histogram of Population Data with Mean')
    fig, ax = plt.subplots()
    sns.histplot(population_df['value'], bins='auto', color=color[0], edgecolor='white', alpha=0.5, ax=ax, kde=kde_overlay)
    pop_mean = population_df['value'].mean()
    ax.axvline(pop_mean, color=color[3], linestyle='--', linewidth=2)
    ax.text(pop_mean, population_size * 0.01, f'Mean: {pop_mean:.2f}', color='black', fontsize=8)
    ax.set_xlabel('Value')
    ax.set_ylabel('Frequency')
    ax.spines[['top', 'right']].set_visible(False)
    ax.legend().set_visible(False)
    ax.set_xlim(population_df['value'].min(), population_df['value'].max())
    plt.tight_layout()
    st.pyplot(fig)
with col2:
    st.subheader('Histogram of Selected Sample with Mean')
    fig, ax = plt.subplots()
    sns.histplot(sample_df['value'], bins='auto', color=color[0], edgecolor='white', alpha=0.5, ax=ax, kde=kde_overlay)
    sample_mean = sample_df['value'].mean()
    ax.axvline(sample_mean, color=color[3], linestyle='--', linewidth=2)
    ax.text(sample_mean, sample_size * 0.01, f'Mean: {sample_mean:.2f}', color='black', fontsize=8)
    ax.set_xlabel('Value')
    ax.set_ylabel('Frequency')
    ax.spines[['top', 'right']].set_visible(False)
    ax.legend().set_visible(False)
    ax.set_xlim(sample_df['value'].min(), sample_df['value'].max())
    plt.tight_layout()
    st.pyplot(fig)

# Visuals on repeated sampling and bootstrap sampling
col1, col2 = st.columns(2)
with col1:
    st.subheader('Distribution of Sample Means in Repeated Samples')
    rep_sample_means = create_sample_frame(
        population, sample_size=sample_size, n_samples=num_samples, with_replacement=False, seed=rnd_state
    ).mean(axis=1)
    fig, ax = plt.subplots()
    sns.histplot(rep_sample_means, bins='auto', color=color[0], edgecolor='white', alpha=0.5, ax=ax, kde=kde_overlay)
    rep_samp_mean = rep_sample_means.mean()
    ax.axvline(rep_samp_mean, color=color[3], linestyle='--', linewidth=2)
    ax.text(rep_samp_mean, num_samples * 0.01, f'Mean of Repated Sample Means: {rep_samp_mean:.2f}', color='black', fontsize=8)
    ax.set_xlabel('Repeated Sample Means')
    ax.set_ylabel('Frequency')
    ax.spines[['top', 'right']].set_visible(False)
    ax.legend().set_visible(False)
    ax.set_xlim(rep_sample_means.min(), rep_sample_means.max())
    plt.tight_layout()
    st.pyplot(fig)
with col2:
    st.subheader('Distribution of Sample Means in Bootstrap Samples')
    bootstrap_sample_means = create_sample_frame(
        np.array(sample_df['value']), sample_size=sample_size, n_samples=num_bootstrap_rounds, with_replacement=True, seed=rnd_state
    ).mean(axis=1)
    fig, ax = plt.subplots()
    sns.histplot(bootstrap_sample_means, bins='auto', color=color[0], edgecolor='white', alpha=0.5, ax=ax, kde=kde_overlay)
    boot_samp_mean = bootstrap_sample_means.mean()
    ax.axvline(boot_samp_mean, color=color[3], linestyle='--', linewidth=2)
    ax.text(boot_samp_mean, num_bootstrap_rounds * 0.01, f'Mean of Bootstrap Sample Means: {boot_samp_mean:.2f}', color='black', fontsize=8)
    ax.set_xlabel('Bootstrap Sample Means')
    ax.set_ylabel('Frequency')
    ax.spines[['top', 'right']].set_visible(False)
    ax.legend().set_visible(False)
    ax.set_xlim(bootstrap_sample_means.min(), bootstrap_sample_means.max())
    plt.tight_layout()
    st.pyplot(fig)


# SE and CI estimates and visualization
st.subheader('Standard Errors and Confidence Intervals for Sample Means')
col1, col2 = st.columns(2)
with col1:
    se_repeated_sampling = np.std(rep_sample_means)
    se_bootstrap = np.std(bootstrap_sample_means)
    se_formula = np.std(sample_df['value']) / np.sqrt(sample_size)
    st.markdown(f"""The standard errors calculated in three ways are the following:       
- **SE from repeated sampling (standard deviation of repeated sample means)**: {se_repeated_sampling:.2f}
- **SE from bootstrap (standard deviation of bootstrap sample means)**: {se_bootstrap:.2f}
- **SE from formula ($SE(\\bar{{x}}) = \\frac{{Std[x]}}{{\\sqrt{{n}}}}$)**: {se_formula:.2f}""")

    critical_value = norm.ppf(1 - alpha / 2)
    ci_repeated_sampling = (sample_mean - critical_value * se_repeated_sampling, sample_mean + critical_value * se_repeated_sampling)
    ci_bootstrap = (sample_mean - critical_value * se_bootstrap, sample_mean + critical_value * se_bootstrap)
    ci_formula = (sample_mean - critical_value * se_formula, sample_mean + critical_value * se_formula)

    st.markdown(f"""The confidence intervals calculated in three ways are the following:        
- **CI from repeated sampling**: ({ci_repeated_sampling[0]:.2f}, {ci_repeated_sampling[1]:.2f})
- **CI from bootstrap**: ({ci_bootstrap[0]:.2f}, {ci_bootstrap[1]:.2f})
- **CI from formula**: ({ci_formula[0]:.2f}, {ci_formula[1]:.2f})""")

with col2:
    confidence_intervals = [ci_formula, ci_bootstrap, ci_repeated_sampling]
    fig, ax = plt.subplots()
    for i, (lower, upper) in enumerate(confidence_intervals):
        ax.plot([lower, upper], [i, i], color=color[0], marker='o', label='Confidence Interval' if i == 0 else "")
        ax.plot(sample_mean, i, color=color[2], marker='x', label='Sample Mean' if i == 0 else "")
        ax.plot(pop_mean, i, color=color[1], marker='s', label='Population Mean' if i == 0 else "")

    ax.axvline(pop_mean, color=color[1], linestyle='--')
    ax.axvline(sample_mean, color=color[2], linestyle='--')
    ax.set_yticks(range(len(confidence_intervals)))
    ax.set_yticklabels(['CI Formula', 'CI Bootstrap', 'CI Repeated Sampling'])
    ax.set_xlabel("Value")
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), ncol=2, frameon=False)
    ax.spines[['top', 'right']].set_visible(False)
    plt.tight_layout()
    st.pyplot(fig)


