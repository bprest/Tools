rm(list=ls()) 
setwd('/Users/Brianprest/Documents/GitHub/Tools')
library(data.table)
library(prophet)

# df <- read.csv('https://github.com/facebook/prophet/blob/master/examples/example_wp_log_peyton_manning.csv')
# dt <- fread('https://github.com/facebook/prophet/blob/master/examples/example_wp_log_peyton_manning.csv')
dt <- fread('example_wp_log_peyton_manning.csv')

dt[, date := as.Date(ds)]
dt[, ey := exp(y)]

dt[, mean(ey), by=weekdays(date)]

plot(y ~ date, data=dt, type='l')
dt

# df <- read.csv('example_wp_log_peyton_manning.csv')

m = prophet(dt)

m

future = make_future_dataframe(m, periods=365) # set up frame with historical data + one year into the future
dt[, range(date)]

forecast <- predict(m, df=future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)

dyplot.prophet(m, forecast)


# https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html
# Change weekly pattern to differ by on/off season

is_nfl_season <- function(ds) {
    dates <- as.Date(ds)
    month <- as.numeric(format(dates, '%m'))
    return(month > 8 | month < 2)
}
dt[, on_season := is_nfl_season(ds)]
dt[, off_season := !is_nfl_season(ds)]

m2 = copy(m)
# Then we disable the built-in weekly seasonality, and replace it with two weekly seasonalities that have these columns specified as a condition. This means that the seasonality will only be applied to dates where the condition_name column is True. We must also add the column to the future dataframe for which we are making predictions.
m <- prophet(weekly.seasonality=FALSE)
m <- add_seasonality(m, name='weekly_on_season', period=7, fourier.order=3)#, condition.name='on_season')
m$seasonalities$weekly_on_season$condition_name = 'on_season'
m <- add_seasonality(m, name='weekly_off_season', period=7, fourier.order=3)#, condition.name='off_season')
m$seasonalities$weekly_off_season$condition_name = 'off_season'
m$seasonalities
m <- fit.prophet(m, dt)

future$on_season <- is_nfl_season(future$ds)
future$off_season <- !is_nfl_season(future$ds)
forecast <- predict(m, future)
prophet_plot_components(m, forecast)


str(m)

# Add regressor
nfl_sunday <- function(ds) {
    dates <- as.Date(ds)
    month <- as.numeric(format(dates, '%m'))
    as.numeric((weekdays(dates) == "Sunday") & (month > 8 | month < 2))
}
dt[, nfl_sunday := nfl_sunday(ds)]

m <- prophet()
m <- add_regressor(m, 'nfl_sunday')
m <- fit.prophet(m, dt)

future$nfl_sunday <- nfl_sunday(future$ds)

forecast <- predict(m, future)
prophet_plot_components(m, forecast)

range(future$nfl_sunday)

str(m)
