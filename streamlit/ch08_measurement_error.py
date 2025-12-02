import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FuncFormatter
import seaborn as sns
import statsmodels.api as sm

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.set_page_config(page_title='Measurement Error', layout="wide")
st.title('The Hidden Impact of Measurement Error on Regression Analysis')

st.markdown(
"""
## Introduction  

This interactive dashboard explores the impact of **measurement error** on regression analysis, 
with a focus on hotel prices in London.  
The app uses the `hotels-europe` dataset, which contains information on hotel prices, distance 
from the city center, and customer ratings.  
For this analysis, we restrict the data to:  

- Hotels located in **London**  
- **Hotel-type accommodations** with at least **3 stars**  
- Hotels located within **10 miles** of the city center  
- Valid customer ratings  
- Weekday prices (November 2017, N = 578)  

By experimenting with different forms of measurement error and transformations, you can observe 
how regression results are distorted and gain intuition for the consequences of error-prone 
measurement in applied econometric work.  

## Concepts  

- **Measurement Error:** Random or systematic noise added to observed variables that can 
  bias regression estimates.  
  - *Classical error*: Random error with mean zero, uncorrelated with the true variable.  
  - *Non-Classical error (Bias)*: Error with non-zero mean, shifting the variable systematically.  
  - *Non-Classical error (Correlated)*: Error that is correlated with another variable (e.g., hotel rating).  

- **Log Transformation:** Allows you to express variables in relative rather than absolute terms, 
  often improving interpretability of elasticities and reducing skewness.   

- **Filtering:** Data can be restricted to exclude implausible values after adding measurement error 
  (e.g., hotels with <1 mile distance or < USD 50 nightly price).  

- **Regression Analysis:** The dashboard runs simple OLS regressions with robust (HC3) standard errors 
  to compare estimates with and without measurement error.  

## Functionalities  

- **Add Measurement Error**  
  - Choose type (Classical, Non-Classical Bias, Non-Classical Correlated).  
  - Adjust parameters such as error mean, error SD, and correlation with rating.  

- **Variable Transformations**  
  - Apply log transformation to distance and/or price.  
  - Plots automatically back-transform tick labels to display values in original units.  

- **Filtering Options**  
  - Exclude hotels with <1 mile distance (after measurement error).  
  - Exclude hotels with <£50 price (after measurement error).  

- **Visualizations**  
  - Histograms showing the distribution of variables with and without error.  
  - Scatterplots with fitted regression lines comparing error-free vs. error-prone data.  

- **Regression Output**  
  - Side-by-side tables of regression coefficients and robust standard errors.  
  - Significance stars (***, **, *) for quick interpretation.  

"""
)


# Load and cach data from OSF
@st.cache_data
def load_data():
    hotels_price = pd.read_csv('data/hotels-europe_price.csv')
    hotels_features = pd.read_csv('data/hotels-europe_features.csv')
    data = pd.merge(hotels_features[
                (hotels_features['city'] == 'London') &
                (hotels_features['accommodation_type'] == 'Hotel') &
                (hotels_features['stars'] >= 3) &
                (hotels_features['distance'] <= 10) &
                (hotels_features['rating'].notna())
            ], hotels_price[
                (hotels_price['year'] == 2017) &
                (hotels_price['month'] == 11) &
                (hotels_price['weekend'] == 0)
            ], on='hotel_id', how='inner')
    data = data[['hotel_id', 'distance', 'price', 'rating']]
    return data

data = load_data()

# Sidebar for settings
st.sidebar.header("Measurement Error Settings")
seed = st.sidebar.number_input("Random Seed", min_value=0, max_value=10000, value=42)
np.random.seed(seed)

def measurement_error_input(label, mean_min, mean_max, mean_def, sd_min, sd_max, sd_def):
    error_type = st.sidebar.selectbox(f"{label} Error Type", ["None", "Classical", "Non-Classical (Bias)", "Non-Classical (Correlated)"],
                                      index=1 if label == "X (Distance)" else 0,
                                      help='''
* Classical: Adds normally distributed error with mean 0 and specified SD.
* Non-Classical (Bias): Adds normally distributed error with specified mean and SD.
* Non-Classical (Correlated): Adds error correlated with rating.
''')
    if error_type == "Classical":
        sd = st.sidebar.slider(f"{label} Error SD", sd_min, sd_max, sd_def)
        return lambda df: np.random.normal(0, sd, size=len(df))
    elif error_type == "Non-Classical (Bias)":
        mean = st.sidebar.slider(f"{label} Error Mean", mean_min, mean_max, mean_def)
        sd = st.sidebar.slider(f"{label} Error SD", sd_min, sd_max, sd_def)
        return lambda df: np.random.normal(mean, sd, size=len(df))
    elif error_type == "Non-Classical (Correlated)":
        base_mean = st.sidebar.slider(f"{label} Base Mean", mean_min, mean_max, mean_def)
        base_sd = st.sidebar.slider(f"{label} Base SD", sd_min, sd_max, sd_def)
        corr = st.sidebar.slider(f"{label} Error Correlation with Rating", -1.0, 1.0, 0.0)
        return lambda df: base_mean + corr * df['rating'].values + np.random.normal(0, base_sd, size=len(df))
    else:
        return lambda df: np.zeros(len(df))

x_error_fn = measurement_error_input("X (Distance)", -5.0, 5.0, 0.0, 0.1, 3.0, 1.0)
y_error_fn = measurement_error_input("Y (Price)", -50.0, 50.0, 0.0, 0.1, 100.0, 10.0)

# Log transformation and model degree
st.sidebar.header("Model Settings")
log_x = st.sidebar.checkbox("Log-transform X (Distance)")
log_y = st.sidebar.checkbox("Log-transform Y (Price)")

# Filter out less than 1 mile
st.sidebar.header("Data Filtering")
filtersmall = st.sidebar.checkbox("Filter out hotels where distance (with measurement error) < 1 mile", value=True)
filtersmall_price = st.sidebar.checkbox("Filter out hotels where price (with measurement error) < 50 USD", value=True)

# Create noisy data
data['distance_err'] = data['distance'] + x_error_fn(data)
data['price_err'] = data['price'] + y_error_fn(data)

# Log transformation
data['ln_distance'] = np.log(data['distance'])
data['ln_price'] = np.log(data['price'])
data['ln_distance_err'] = np.log(data['distance_err'])
data['ln_price_err'] = np.log(data['price_err'])

# Download data
st.sidebar.download_button(
    label="Download Data (without filters)",
    data=data.to_csv(index=False).encode('utf-8'),
    file_name='ch08_measurement_error_data.csv',
    mime='text/csv',
)

st.sidebar.markdown('Code hosted on [Github](https://github.com/gabors-data-analysis/da_interactive/blob/main/streamlit/ch08_measurement_error.py).')

# Filter out hotels with distance < 1 mile
if filtersmall:
    data = data[data['distance_err'] >= 1]

# Filter out hotels with price < 50
if filtersmall_price:
    data = data[data['price_err'] >= 50]

# stop if there are invalid values
if data['distance_err'].min() <= 0:
    st.warning("Invalid values detected in distance (with error). Please apply filtering or reduce the measurement error.")
    st.stop()
if data['price_err'].min() <= 0:
    st.warning("Invalid values detected in price (with error). Please apply filtering or reduce the measurement error.")
    st.stop()

# Select variables for plotting
if log_x:
    x_plot = data['ln_distance']
    x_err_plot = data['ln_distance_err']
else:
    x_plot = data['distance']
    x_err_plot = data['distance_err']

if log_y:
    y_plot = data['ln_price']
    y_err_plot = data['ln_price_err']
else:
    y_plot = data['price']
    y_err_plot = data['price_err']

# Histogram plots with overlays
fig, axs = plt.subplots(1, 2, figsize=(12, 4))

# Distance histogram overlay
sns.histplot(x_plot, ax=axs[0], color=color[0], alpha=0.5, label="Without Error", binwidth=0.5 if not log_x else 0.25)
sns.histplot(x_err_plot, ax=axs[0], color=color[1], alpha=0.5, label="With Error", binwidth=0.5 if not log_x else 0.25)
# Formatting ticks to always be original units
if log_x:
    axs[0].xaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val):.1f}"))
axs[0].set_xlabel("Distance (miles)")
axs[0].set_ylabel("Frequency")
axs[0].spines[['top', 'right']].set_visible(False)
axs[0].legend()
axs[0].set_xlim(min(x_plot.min(), x_err_plot.min()), max(x_plot.max(), x_err_plot.max()))

# Price histogram overlay
sns.histplot(y_plot, ax=axs[1], color=color[0], alpha=0.5, label="Without Error", binwidth=25 if not log_y else 0.25)
sns.histplot(y_err_plot, ax=axs[1], color=color[1], alpha=0.5, label="With Error", binwidth=25 if not log_y else 0.25)
# Formatting ticks to always be original units
if log_y:
    axs[1].xaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val):.0f}"))
axs[1].set_xlabel("Price (USD)")
axs[1].set_ylabel("Frequency")
axs[1].spines[['top', 'right']].set_visible(False)
axs[1].legend()
axs[1].set_xlim(min(y_plot.min(), y_err_plot.min()), max(y_plot.max(), y_err_plot.max()))

# Show the plots
st.subheader("Distribution of Variables with and without Measurement Error")
st.pyplot(fig, clear_figure=False)

# Combined regression plot
fig, ax = plt.subplots(figsize=(5, 3))

# Scatter: without error
ax.scatter(x_plot, y_plot, color=color[0], alpha=0.25, s=10, edgecolor='none')
model_raw = sm.OLS(y_plot, np.vander(x_plot, N=2, increasing=True)).fit(cov_type='HC3')
x_fit = np.linspace(min(x_plot), max(x_plot), 100)
y_fit = model_raw.predict(np.vander(x_fit, N=2, increasing=True))
ax.plot(x_fit, y_fit, color=color[0], linewidth=2, label="Without Error")

# Scatter: with error
ax.scatter(x_err_plot, y_err_plot, color=color[1], alpha=0.25, s=10, edgecolor='none')
model_err = sm.OLS(y_err_plot, np.vander(x_err_plot, N=2, increasing=True)).fit(cov_type='HC3')
x_fit_err = np.linspace(min(x_err_plot), max(x_err_plot), 100)
y_fit_err = model_err.predict(np.vander(x_fit_err, N=2, increasing=True))
ax.plot(x_fit_err, y_fit_err, color=color[1], linewidth=2, label="With Error")

# Formatting ticks to always be original units
if log_x:
    ax.xaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val):.1f}"))
ax.set_xlabel("Distance (miles)")

if log_y:
    ax.yaxis.set_major_formatter(FuncFormatter(lambda val, pos: f"{np.exp(val):.0f}"))
ax.set_ylabel("Price (USD)")

ax.spines[['top', 'right']].set_visible(False)
ax.legend()

st.subheader("Regression with and without Measurement Error")
col1, col2 = st.columns([0.67, 0.33])
with col1:
    st.pyplot(fig, use_container_width=False)

# Construct table with regression results
def stargazer_table(model, label_x):
    coefs = model.params
    ses = model.bse
    pvals = model.pvalues

    def stars(p):
        if p < 0.01:
            return '***'
        elif p < 0.05:
            return '**'
        elif p < 0.1:
            return '*'
        else:
            return ''

    rows = []
    suffixes = ['', ' sq.', ' cub.']
    for i in range(len(coefs)):
        if i == 0:
            name = "Intercept"
        else:
            name = f"{label_x}{suffixes[i-1]}"
        coef_str = f"{coefs[i]:.2f}{stars(pvals[i])}"
        se_str = f"({ses[i]:.2f})"
        rows.append([name, coef_str, se_str])

    r2 = model.rsquared
    rows.append(["R-squared", f"{r2:.2f}", ""])

    df = pd.DataFrame(rows, columns=["Variable", "Coefficient", "Standard Error"])
    return df.set_index("Variable")

# Combine tables with multi-index columns
label_x = "Distance" if not log_x else "ln(Distance)"
table_raw = stargazer_table(model_raw, label_x)
table_err = stargazer_table(model_err, label_x)

merged = pd.concat([table_raw, table_err], axis=1, keys=["Without Measurement Error", "With Measurement Error"])

# Display regression results
st.subheader("Comparison of Regression Outputs")
st.dataframe(merged)
st.markdown('*Note: Robust standard errors (HC3) used. *** significant at 1%, ** significant at 5%, * significant at 10%.*')