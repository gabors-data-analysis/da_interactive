import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

color = ["#3a5e8c", "#10a53d", "#541352", "#ffcf20", "#2f9aa0"]

st.set_page_config(page_title='Ch. 3 - Visualizing Distributions', layout='wide')

# load hotels-europe dataset from OSF and cache it
@st.cache_data
def load_data():
    hotels_price = pd.read_csv('data/hotels-europe_price.csv')
    hotels_features = pd.read_csv('data/hotels-europe_features.csv')
    # Filter to November 2017 weekdays
    hotels_price = hotels_price[(hotels_price['year'] == 2017) & (hotels_price['month'] == 11) & (hotels_price['weekend'] == 0)]
    return hotels_price, hotels_features

# function to apply filters based on user selections
def apply_filters(hotels_price, hotels_features, city, minprice, maxprice):
    filtered_features = hotels_features[hotels_features['city'] == city]
    filtered_price = hotels_price[(hotels_price['price'] >= minprice) & (hotels_price['price'] <= maxprice)]
    workset = pd.merge(filtered_features, filtered_price, on='hotel_id', how='inner')
    return workset

hotels_price, hotels_features = load_data()

st.title('Chapter 3: Visualizing Distributions')
st.markdown("""
Visualizing distributions is a crucial step in every analytical project.
This dashboard focuses on various methods to visualize the distribution of hotel prices, including histograms, boxplots, density plots, and violin plots.
Which options to choose is not always straightforward, and it is also partly a matter of personal taste.
As described in the textbook, how these visualizations look depends on the settings the analyst chooses (except for boxplots - which are unfortunately the least informative).
This dashboard allows you to explore these settings interactively.
You can select a city, year, month, and whether to include only weekends or holidays from the `hotels-europe` dataset.
You can also adjust the number of bins or bin width for histograms and the bandwidth for density plots.
The visualizations will update accordingly, allowing you to see how different settings affect the distribution visualizations.
""")

# User inputs on the sidebar
st.sidebar.header('Settings')
st.sidebar.subheader('Data Selection')
city = st.sidebar.selectbox('Select a city', hotels_features['city'].unique(), index=42)
price_range = st.sidebar.slider('Select price range', min_value=int(hotels_price['price'].min()), max_value=int(hotels_price['price'].max()), value=(50, 500), step=1)

st.sidebar.subheader('Histogram Settings')
bins = st.sidebar.slider('Number of bins', min_value=5, max_value=500, value=50, step=1,
                         help='Number of bins to use for the histogram. The range of the data gets divided into this many equal-width bins. A higher number yields more detailed but potentially noisy histograms, while a lower number might hide some important details.')
bin_width = st.sidebar.slider('Bin width', min_value=1, max_value=100, value=30, step=1,
                              help='Width of each bin for the histogram. The bin width determines how much of the price range each bin covers. A smaller bin width can reveal more detail, while a larger bin width can smooth out fluctuations.')
binning_option = st.sidebar.selectbox('Binning option to use', ['Number of bins', 'Bin width'], index=0)
st.sidebar.subheader('Density Plot Settings')
bandwidth = st.sidebar.slider('Bandwidth', min_value=0.01, max_value=5.0, value=0.1, step=0.01,
                              help='The bandwidth parameter controls the smoothness of the density estimate. A smaller value results in a more detailed plot, while a larger value produces a smoother curve.')

# Getting the filtered dataset based on user inputs
workset = apply_filters(hotels_price, hotels_features, city, price_range[0], price_range[1])

if workset.empty:
    st.error('No data available for the selected filters. Please adjust your selections.')
else:
    st.sidebar.download_button(
        label="Download Filtered Data",
        data=workset.to_csv(index=False).encode('utf-8'),
        file_name='ch03_distribution_visuals_data.csv',
        mime='text/csv'
    )
    col1, col2 = st.columns(2)
    with col1:
        # Histogram (either by number of bins or bin width set by the user)
        fig, ax = plt.subplots()
        if binning_option == 'Number of bins':
            st.subheader(f'Histogram with {bins} Bins')
            sns.histplot(workset['price'], bins=bins, kde=False, ax=ax, color=color[0], edgecolor='white', fill=True, alpha=1)
        else:
            st.subheader(f'Histogram with Bin Width = {bin_width}')
            sns.histplot(workset['price'], binwidth=bin_width, kde=False, ax=ax, color=color[0], edgecolor='white', fill=True, alpha=1)
        ax.set_xlabel('Price')
        ax.set_ylabel('Frequency')
        ax.spines[['top', 'right']].set_visible(False)
        plt.tight_layout()
        st.pyplot(fig)

        # Boxplot (no parameters to set)
        st.subheader('Boxplot')
        fig, ax = plt.subplots()
        sns.boxplot(x=workset['price'], ax=ax, color=color[0], fill=True, linecolor='black')
        ax.set_xlabel('Price')
        ax.spines[['top', 'right']].set_visible(False)
        plt.tight_layout()
        st.pyplot(fig)
    
    with col2:
        # Density Plot (with bandwidth set by the user)
        st.subheader(f'Density Plot with Bandwidth = {bandwidth}')
        fig, ax = plt.subplots()
        sns.kdeplot(workset['price'], bw_method=bandwidth, ax=ax, color=color[0], fill=True, alpha=1)
        ax.set_xlabel('Price')
        ax.set_ylabel('Density')
        ax.spines[['top', 'right']].set_visible(False)
        plt.tight_layout()
        st.pyplot(fig)
        # Violin Plot (KDE overlay with bandwidth set by the user)
        st.subheader(f'Violin Plot with Bandwidth = {bandwidth}')
        fig, ax = plt.subplots()
        sns.violinplot(x=workset['price'], ax=ax, bw_method=bandwidth, color=color[0], fill=True, linecolor='black')
        ax.set_xlabel('Price')
        ax.spines[['top', 'right']].set_visible(False)
        plt.tight_layout()
        st.pyplot(fig)