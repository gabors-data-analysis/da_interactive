import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
import seaborn as sns
import matplotlib.pyplot as plt
from statsmodels.nonparametric.smoothers_lowess import lowess
from scipy.stats import t
import streamlit as st

import warnings
warnings.filterwarnings('ignore')

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.set_page_config(page_title='Ch. 10 - Multiple Linear Regression', layout="wide")

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

pg = st.navigation([
    st.Page("ch10_welcome_page.py", title='Welcome Page'),
    st.Page("ch10_visualizing_relationships.py", title='Visualizing Relationships'),
    st.Page("ch10_multiple_regression_analysis.py", title='Multiple Regression Analysis')])

pg.run()
