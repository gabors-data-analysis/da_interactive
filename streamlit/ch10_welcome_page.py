import streamlit as st

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