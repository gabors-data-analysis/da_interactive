import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator
from statsmodels.nonparametric.smoothers_lowess import lowess
from scipy.stats import t
import streamlit as st

import warnings
warnings.filterwarnings('ignore')

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.title('Ch. 10 - Multiple Linear Regression')

st.markdown("""
This dashboard helps you explore multiple linear regression analysis, as discussed in Chapter 10, using the `cps-earnings` dataset.

We use data on people of age 24 to 65 (to reflect the typical working age of people with these
kinds of graduate degrees). We excluded the self-employed (their earnings is difficult to measure)
and included those who reported 20 hours or more as their usual weekly time worked. We have
18 241 observations.
            
The dependent variable is log hourly earnings (ln wage).
            
You can navigate between two sections using the sidebar:
- **Visualizing Relationships**: Explore relationships between variables visually.
For categorical variables, see how wages differ across groups. For continuous variables, observe trends using linear regression or LOWESS smoothing.
- **Multiple Regression Analysis**: Fit multiple linear regression models to the data and explore the differences in the results when controlling for different sets of variables and interaction terms.
""")

st.markdown('### Exploration: Visualizing Relationships')

st.markdown("""
This section allows you to visualize the relationships between log hourly earnings and various predictor variables in the `cps-earnings` dataset.
You can select different variables to plot against log wages, choose between LOESS smoothing or linear regression for continuous variables, and decide whether to display 95% confidence intervals.
""")

@st.cache_data
def load_data():
    cps= pd.read_csv("https://osf.io/download/4ay9x/")
    cps.drop(columns=['Unnamed: 0'], inplace=True)
    cps = cps.query("uhours>=20 & earnwke>0 & age>=24 & age<=64 & grade92>=44")
    cps["female"] = (cps.sex == 2).astype(int)
    cps["w"] = cps["earnwke"] / cps["uhours"]
    cps["ln_wage"] = np.log(cps["w"])

    race_mapping = {
        1: "White",
        2: "Black",
        3: "American Indian",
        4: "Asian",
        5: "Pacific Islander"
    }

    marital_mapping = {
        1: "Married",
        2: "Married",
        3: "Married",
        4: "Widowed",
        5: "Divorced",
        6: "Separated",
        7: "Never Married"
    }

    edu_mapping = {
        44: "Master's",
        45: "Professional",
        46: "Doctorate"
    }

    non_other_industries = cps['ind02'].value_counts()[cps['ind02'].value_counts() > 300].index.tolist()

    industry_mapping = {
        'Colleges and universities, including junior colleges (6112, 6113)': 'Higher Education',
        'Other': 'Other',
        'Administration of human resource programs (923)': 'HR Administration',
        'Elementary and secondary schools (6111)': 'K-12 Education',
        'Legal services (5411)': 'Legal',
        'Offices of physicians (6211)': 'Physician Offices',
        'Justice, public order, and safety activities (922, pt. 92115)': 'Justice, Order & Safety',
        'Hospitals (622)': 'Hospital Care',
        'Computer systems design and related services (5415)': 'Information Technology',
        'Insurance carriers and related activities (524)': 'Insurance',
        'Banking and related activities (521, 52211,52219)': 'Banking',
        'Outpatient care centers (6214)': 'Outpatient Care',
        'Management, scientific, and technical consulting services (5416)': 'Mgmt, Science, Tech Services',
        'Architectural, engineering, and related services (5413)': 'Engineering Services'
    }

    cps['Race'] = cps['race'].map(race_mapping).fillna('Other')
    cps['Marital_Status'] = cps['marital'].map(marital_mapping).fillna('Other')
    cps['Education'] = cps['grade92'].map(edu_mapping).fillna('Other')
    cps['Industry'] = cps['ind02'].where(cps['ind02'].isin(non_other_industries), other='Other')
    cps['Industry'] = cps['Industry'].map(industry_mapping).fillna('Other')
    cps.rename(columns={'age': 'Age', 'female': 'Female'}, inplace=True)
        
    cps = cps[['ln_wage', 'Female', 'Age', 'Race', 'Marital_Status', 'Education', 'Industry']]
    cps['Gender'] = cps['Female'].map({1: 'Female', 0: 'Male'})
    cps['Age_squared'] = cps['Age'] ** 2
    cps['Age_cubed'] = cps['Age'] ** 3
    cps['Age_quartic'] = cps['Age'] ** 4
    return cps

if 'data_loaded' not in st.session_state:
    cps = load_data()
    st.session_state['cps'] = cps
    st.session_state['data_loaded'] = True

def plot_wage_function(data, var, use_loess, degree, show_ci, ax, color=color[0:2], n_boot=100):
    if var == 'Age':
        # plot LOESS or linear regression for Age, conditional on gender
        for gender_val, gender_label, col in zip([1, 0], ['Female', 'Male'], color):
            subset = data[data['Female'] == gender_val]

            if use_loess:
                # Compute LOWESS fit
                x = subset['Age']
                y = subset['ln_wage']
                loess_fit = lowess(y, x, frac=0.3, return_sorted=True)
                x_fit, y_fit = loess_fit[:, 0], loess_fit[:, 1]
                ax.plot(x_fit, y_fit, label=gender_label, color=col)
                if show_ci:
                    # Bootstrap for confidence intervals
                    boot_preds = np.zeros((n_boot, len(x_fit)))
                    for i in range(n_boot):
                        boot_idx = np.random.choice(len(subset), len(subset), replace=True)
                        xb = subset['Age'].iloc[boot_idx]
                        yb = subset['ln_wage'].iloc[boot_idx]
                        loess_boot = lowess(yb, xb, frac=0.3, xvals=x_fit)
                        boot_preds[i, :] = loess_boot

                    ci_lower = np.percentile(boot_preds, 2.5, axis=0)
                    ci_upper = np.percentile(boot_preds, 97.5, axis=0)
                    ax.fill_between(x_fit, ci_lower, ci_upper, color=col, alpha=0.2)
            else:
                # Use Seaborn regression with built-in CI
                sns.regplot(
                    x='Age', y='ln_wage', data=subset, ax=ax, label=gender_label, order=degree,
                    color=col, scatter=False, ci=95 if show_ci else False
                )

        ax.legend(fontsize=6)
        # Axis labels and legend formatting
        ax.set_xlabel(var.replace('_', ' '), fontsize=6)
        ax.set_ylabel('ln(earnings per hour, US dollars)', fontsize=6)
        ax.tick_params(axis='both', which='major', labelsize=6)
        ax.grid(False)
        ax.legend(
            loc='upper center', bbox_to_anchor=(0.5, 1.20),
            ncol=2, frameon=False, title='Gender', fontsize=6, title_fontsize=6
        )
        ax.spines[['top', 'right']].set_visible(False)
        plt.tight_layout()

    else:
        sns.barplot(
            x=var, y='ln_wage', hue='Gender', data=data,
            ax=ax[0], palette=color, capsize=0.1 if show_ci else 0,
            errorbar=('ci', 95 if show_ci else False),
            err_kws={'linewidth':0.8}
        )
        ax[0].set_ylim(bottom=3)

        sns.barplot(
            x=var, y='ln_wage', hue='Gender', data=data,
            ax=ax[1], palette=color, capsize=0.1 if show_ci else 0,
            errorbar=('ci', 95 if show_ci else False),
            err_kws={'linewidth':0.8}
        )
        ax[1].set_ylim(0, 0.2)

        # Hide the spines and ticks between the plots
        ax[0].spines['bottom'].set_visible(False)
        ax[1].spines['top'].set_visible(False)
        ax[1].spines['right'].set_visible(False)
        ax[0].spines['right'].set_visible(False)
        ax[0].spines['top'].set_visible(False)
        ax[0].tick_params(bottom=False, labelbottom=False)

        d = 0.01
        kwargs = dict(transform=ax[0].transAxes, color='k', clip_on=False)
        ax[0].plot((-d, +d), (-d, +d), **kwargs)
        kwargs.update(transform=ax[1].transAxes, color='k', clip_on=False)
        ax[1].plot((-d, +d), (1-4*d, 1+4*d), **kwargs)

        ax[1].tick_params(axis='x', labelsize=6)
        ax[0].yaxis.set_major_locator(MultipleLocator(0.2))
        ax[1].yaxis.set_major_locator(MultipleLocator(0.2))
        ax[1].tick_params(axis='y', labelsize=6)
        ax[0].tick_params(axis='y', labelsize=6)
        ax[1].legend().set_visible(False)
        ax[0].legend(
            loc='upper center', bbox_to_anchor=(0.5, 1.20),
            ncol=2, frameon=False, title='Gender', fontsize=6, title_fontsize=6
        )
        ax[1].set_ylabel('')
        ax[1].set_xlabel(var.replace('_', ' '), fontsize=6)
        ax[0].set_ylabel('ln(earnings per hour, US dollars)', fontsize=6)
        plt.xticks(rotation=90)
        plt.tight_layout()
        plt.subplots_adjust(hspace=0.1)
    return ax


st.sidebar.title('Plot Settings')
variable = st.sidebar.selectbox(
    'Select Variable to Plot against ln(wage):',
    options=['Age', 'Race', 'Marital Status', 'Education', 'Industry']
)

if variable == 'Marital Status':
    variable = 'Marital_Status'

if variable == 'Age':
    loess_option = st.sidebar.checkbox('Use LOESS instead of linear regression', value=False)
    if not loess_option:
        degree = st.sidebar.selectbox(
            'Select Degree of Polynomial for Age:', [1, 2, 3, 4], index=1
        )

show_ci_option = st.sidebar.checkbox('Show 95% Confidence Intervals', value=True)

if variable == 'Age':
    fig, ax = plt.subplots(figsize=(5, 3.5))
    plot_wage_function(
        st.session_state['cps'], variable, 
        use_loess=loess_option if variable == 'Age' else False,
        degree=degree if variable == 'Age' and not loess_option else None,
        show_ci=show_ci_option,
        ax=ax
    )
else:
    fig, (ax1, ax2) = plt.subplots(
        2, 1, sharex=True, figsize=(5, 3.5),
        gridspec_kw={'height_ratios': [4, 1]}
    )
    plot_wage_function(
        st.session_state['cps'], variable, 
        use_loess=False,
        degree=None,
        show_ci=show_ci_option,
        ax=(ax1, ax2)
    )

st.pyplot(fig, width='content')