Time series for swimming pool data


## Describe

Selection: 
* coverage - Pick a period (from: date (D-M-Y) to date(D-M-Y)) 
* pool - which swimming pool, one option is `all pools`
* aggregation- Decide level of aggregation: day, week, month

Show graph of tickets sold for period+aggregation

Have two panels, allowing for comparison, ie both panel, users may do selections

Show some descriptive statistics
* mean
* min, max
* p5, p95

## Heatmap

Selection: 
* coverage - Pick a period (from: date (D-M-Y) to date(D-M-Y)) 
* pool - which swimming pool, one option is `all pools`

Show heatmap of average tickets sold for y=days, x=months

### Prediction
* tick: linear trend, quadratic trend, monthly dummies, days of week dummies, USA holidays
* add interactions, like june*weekend (need to think how)
* add serial correlation (set q, number of lags)
* show cross-validated RMSE (as in book)
* Estimate it on 2010-2015, show graph -on 2016, actual vs predicted

