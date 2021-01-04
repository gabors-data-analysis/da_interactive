---
permalink: /dat_hotels-europe
title: "README: hotels-europe dataset"
toc: false
author_profile: false
redirect_from:
  - /dat_hotels-europe.html
---


**This is version 2, 2020-01-04**


# The concept

## The data
The `hotels-europe` dataset  includes information on price and features of hotels in 46 European cities and for 10 different dates. See details at the end.
 
## the concept
One of the datasets we use is about hotels in 40 European cities. The dataset includes information on daily price, location, star rating and average customer review. We will use it in many ways, to talk about pricing (prediction)  or finding underpriced hotels, as well as the relationship between price and rating. The platform will allow students to look at various relationships with regression results, plots, and do it separately for cities, or a group of cities. They will be able to compare cites, as well. In the textbook we do it for a single city, Vienna, and so they can reproduce the analysis for Rotterdam or Rome and compare, and discuss.  

## code
For those who are interested will see/download the code (in R) as well. 

# Details of tasks it shall do

The app shall allow filtering when it makes sense
	i. filter sample (allow two filters, city and date)
	ii. filter further (tick to keep/drop star ratings, keep/drop not city actual, keep/drop type of accomodation)

It should have the following options
1. Describe a variable. Pick a variable from dropdown menu and it will show stg like datasummary_skim()
	i. histogram 
	ii. descriptive stats
	iii. let you define extreme values (below and above) and show stats without

2. Compare a variable over two subsets - histograms / kernel density overlaid
	i. cities (select any two)	
	ii. star ratings (select any two)

3. Correlation 
	i. pick `x` and `y` variables [default: price and distance]
	ii. allow ln transformation (if transformed, use a ln friendly scale, 1,2,5,10, 20, 50, 100)
	iii. include extreme values (if yes, ask for a definition, and filter accordingly) Y/N
	iv. show scatterplot + smooth (dropdown choice of linear, quadratic or lowess)

4. Multivariate regression
	i. pick `x_1, x_2, .... x_N` and `y` variables. Run a regression of y on all x variables.
	ii. allow ln transformation (for any variable)
	iii. Show results in a table
	iv. Show a graph with results, with beta coefficients (ie standardized Xi) + 95% CI

5. Prediction
	i. pick `x_1, x_2, .... x_N` and `y` variables. Run a regression of y on all x variables, and calculate the residual.
	ii. print R2, and a 5-fold 80-20 CV RMSE.  
	iii. show a `y_hat - y` plot scatterplot and a linear fit. color points above and below the line, and enlarge top 5 above (and 5 below), add annoation (much higher (lower) than predicted).  



# Workflows -- analytical tasks

We will offer potential **analytical tasks**:
1. Show descriptive statistics for a city
2. Compare descriptive statistics for two cities

## Describe data for a city

1. Sample design
	1. Pick a city
	2. Pick a date
	3. filter extreme values (we'll specify)
2. Pick three variables (default: price, distance, stars)
	1. Show a histogram
	2. Show basic descriptive stats table: (mean, sd, median, min, max, p5, p95) - maybe below graph

## Compare two cities
1. Sample design
	1. Pick two cities
	2. Pick a date
	3. filter extreme values (we'll specify)
2. Pick three variables (default: price, distance, stars)
	1. Show two kernel density plots overlaid
	2. Show basic descriptive stat comparison table: (mean, sd, median, min, max, p5, p95)

## Correlation 
1. Sample design
	1. Pick a city
	2. Pick a date
	3. filter extreme values (we'll specify)
2. Pick two continuous (numeric) variables (default: price, distance)
	1. Show scatterplot and trendline
	2. Allow modify functional form for `x` and `y` (natural log)
	3. Allow modify functional form for trendline (linear, lowess, quadratic, cubic )
3. Show measures of correlation
	1. correlation coefficient	

Can wait/Later to add
1. if transformed, use a ln friendly scale, 1,2,5,10, 20, 50, 100
2. trendline spline with user setting, say 2 knots.
3. binscatter

## Conditional mean (regression)


# Background

### related
example 1: https://gallery.shinyapps.io/slr_diag/
example 2: https://datavizm20.classes.andrewheiss.com/lesson/

needed
[learnR for markdown](https://desiree.rbind.io/post/2020/learnr-iframes/)

# Tidy data table

There are two data tables


## `hotel_features`
* This is a cross section data, with id= hotel_id
* The dataset has N=______ observations.    

| variable           	| description                                    	| type    	|
|--------------------	|------------------------------------------------	|---------	|
| hotel_id           	| Hotel ID                                       	| numeric 	|
| accommodation_type 	| Type of accomodation                           	| factor  	|
| country            	| Country                                        	| string  	|
| city               	| City based on search                           	| string  	|
| city_actual        	| City actual of hotel                           	| string  	|
| neighbourhood      	| Neighburhood                                   	| string  	|
| center1label       	| Centre 1 - name of location for distance       	| string  	|
| distance           	| Distance - from main city center               	| numeric 	|
| center2label       	| Centre 2 - name of location for distance_alter 	| string  	|
| distance_alter     	| Distance - alternative - from Centre 2         	| numeric 	|
| stars              	| Number of stars                                	| numeric 	|
| rating             	| User rating average                            	| numeric 	|
| rating_count       	| Number of user ratings                         	| numeric 	|
| ratingta           	| User rating average (tripadvisor)              	| numeric 	|
| ratingta_count     	| Number of user ratings (tripadvisor)           	| numeric 	|


## `hotel_prices`
* this is a panel data with id= hotel_id and the date (year, month, weekend)
* The dataset has N=______ observations.    



| variable    	| description                           	| type    	|
|-------------	|---------------------------------------	|---------	|
| hotel_id    	| Hotel ID                              	| numeric 	|
| year        	| Year (YYYY)                           	| numeric 	|
| month       	| Month (MM)                            	| numeric 	|
| weekend     	| Flag, if day is a weekend             	| binary  	|
| holiday     	| Flag, if day is a public holiday      	| binary  	|
| nnights     	| Number of nights (1 or 4)             	| factor  	|
| price       	| Pricee in EUR                         	| numeric 	|
| scarce_room 	| Flag, if room was noted as scarce     	| binary  	|
| offer       	| Flag, if there was an offer available 	| binary  	|
| offer_cat   	| Type of offer                         	| factor  	|


## work data

`hotels-europe` is created by joining the two tidy tables and cleaning it. 

* ID variable: hotel_id and date
* The dataset has N=______ observations.    


| variable name 		| info    	 	            						| type   	|  
|--------------------	|------------------------------------------------	|---------	|
| hotel_id           	| Hotel ID                                       	| numeric 	|
| accommodation_type 	| Type of accomodation                           	| factor  	|
| country            	| Country                                        	| string  	|
| city               	| City based on search                           	| string  	|
| city_actual        	| City actual of hotel                           	| string  	|
| neighbourhood      	| Neighburhood                                   	| string  	|
| center1label       	| Centre 1 - name of location for distance       	| string  	|
| distance           	| Distance - from main city center               	| numeric 	|
| center2label       	| Centre 2 - name of location for distance_alter 	| string  	|
| distance_alter     	| Distance - alternative - from Centre 2         	| numeric 	|
| stars              	| Number of stars                                	| numeric 	|
| rating             	| User rating average                            	| numeric 	|
| rating_count       	| Number of user ratings                         	| numeric 	|
| ratingta           	| User rating average (tripadvisor)              	| numeric 	|
| ratingta_count     	| Number of user ratings (tripadvisor)           	| numeric 	|
| hotel_id           	| Hotel ID                                       	| numeric 	|
| year               	| Year (YYYY)                                    	| numeric 	|
| month              	| Month (MM)                                     	| numeric 	|
| weekend            	| Flag, if day is a weekend                      	| binary  	|
| holiday            	| Flag, if day is a public holiday               	| binary  	|
| nnights            	| Number of nights                               	| factor  	|
| price              	| Pricee in EUR                                  	| numeric 	|
| scarce_room        	| Flag, if room was noted as scarce              	| binary  	|
| offer              	| Flag, if there was an offer available          	| binary  	|
| offer_cat          	| Type of offer                                  	| factor  	|