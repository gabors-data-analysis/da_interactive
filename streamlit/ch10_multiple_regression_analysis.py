import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
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

st.markdown('### Multiple Regression Analysis')

st.markdown("""
This section allows you to fit multiple linear regression models to the `cps-earnings` dataset and explore how the inclusion of different control variables and interaction terms affects the estimated
gender wage gap.
            
Please use the sidebar to select control variables and interaction terms for up to two additional models beyond the baseline model that
only includes the female dummy variable.
            
The regression results table displays the estimated coefficients along with their standard errors (in parentheses) and significance levels (indicated by stars). You can choose to display only key results
(intercept, female dummy, and its interactions) or the full table.
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

def get_formula(controls, interactions):
    formula = 'ln_wage ~ Female'
    for c in controls:
        formula +=  f' + {c.replace(" ", "_")}'
        for i in interactions:
            if i in c:
                formula += f' + Female * {c.replace(" ", "_")}'

    return formula

def add_stars(val):
    if val < 0.01:
        return "***"
    elif val < 0.05:
        return "**"
    elif val < 0.1:
        return "*"
    else:
        return ""
    
def get_summary_table(reg_list):
    summary_tables = []
    for reg in reg_list:
        # Build parameter table
        results_df = pd.DataFrame({
            'params': reg.params,
            'stderr': reg.bse,
            'pvalues': reg.pvalues
        })
        results_df['stars'] = results_df['pvalues'].apply(add_stars)

        # Clean variable names
        results_df.index = [
            i.replace('_', ' ')
             .replace('[T.', ' (')
             .replace(']', ')')
             .replace(':', ' x ')
            for i in results_df.index
        ]

        # Build formatted summary column
        results_df['summary'] = (
            results_df['params'].round(3).astype(str)
            + results_df['stars']
            + ' (' + results_df['stderr'].round(3).astype(str) + ')'
        )

        # Add N and R² rows
        extra_rows = pd.DataFrame({
            'summary': ['-------', round(reg.rsquared, 3)]
        }, index=['-------', 'R-squared'])

        # Combine
        results_df = pd.concat([results_df[['summary']], extra_rows])

        summary_tables.append(results_df)

    # Combine all models side-by-side
    combined_table = pd.concat(summary_tables, axis=1)
    combined_table.columns = [f'Model {i+1}' for i in range(len(reg_list))]

    # Reorder so Intercept, Female appear first if present
    ordered_index = ['Intercept', 'Female'] + [
        row for row in combined_table.index
        if row not in ['Intercept', 'Female', '-------', 'R-squared']
    ] + ['-------', 'R-squared']
    return combined_table.reindex(ordered_index).fillna('')

st.sidebar.header('Regression Specification')
st.sidebar.markdown('Note: First model is always the baseline with no controls or interactions. Select controls and interactions for up to 2 additional models.')
st.sidebar.markdown('### Model 2 Specification')
controls_model_2 = st.sidebar.multiselect(
    'Select Control Variables for Model 2:',
    options=['Age', 'Age squared', 'Age cubed', 'Age quartic', 'Race', 'Marital Status', 'Education', 'Industry'],
    default=['Age', 'Age squared', 'Age cubed', 'Age quartic'])
interactions_model_2 = st.sidebar.multiselect(
    'Select Interaction Terms for Model 2:',
    options=[c for c in controls_model_2 if c not in ['Age squared', 'Age cubed', 'Age quartic']],
    default=['Age']
)

st.sidebar.markdown('### Model 3 Specification')
controls_model_3 = st.sidebar.multiselect(
    'Select Control Variables for Model 3:',
    options=['Age', 'Age squared', 'Age cubed', 'Age quartic', 'Race', 'Marital Status', 'Education', 'Industry'],
    default=['Race', 'Marital Status', 'Education'])
interactions_model_3 = st.sidebar.multiselect(
    'Select Interaction Terms for Model 3:',
    options=[c for c in controls_model_3 if c not in ['Age squared', 'Age cubed', 'Age quartic']],
    default=[]
)

reg1 = smf.ols(formula='ln_wage ~ Female', data=st.session_state.cps).fit(cov_type='HC1')
reg2 = smf.ols(formula=get_formula(controls_model_2, interactions_model_2), data=st.session_state.cps).fit(cov_type='HC1')
reg3 = smf.ols(formula=get_formula(controls_model_3, interactions_model_3), data=st.session_state.cps).fit(cov_type='HC1')

summary_table = get_summary_table([reg1, reg2, reg3])
st.markdown('#### Regression Results Summary Table')
show_key = st.checkbox('Only show key results (intercept, female dummy and its interactions):', value=True)
if show_key:
    st.table(data = summary_table.loc[summary_table.index.str.contains('Female|Intercept|R-squared|-------')], border = False)
else:
    st.table(data = summary_table, border = False)

st.markdown("""*Notes: Robust standard errors (HC1) are in parentheses. Significance levels: *** p<0.01, ** p<0.05, * p<0.1.*""")