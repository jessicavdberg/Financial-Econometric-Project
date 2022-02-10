# FM_project
 
```r
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(scales)
install.packages("ggpmisc")
library(ggpmisc)
library(forecast)
library(stats)
library(tseries)

## Forecasting the three most traded currencies 

p1 <- use_data[,c(1,14,41)] %>% 
      gather(Currency, Value, -date) %>% 
      ggplot() +
      geom_line(aes( x = date, y = Value, color = Currency, linetype = Currency), size = 1) + 
      scale_x_date(labels = date_format("%Y"), date_breaks ="3 year" ) + 
      scale_color_manual(values = c("darkred", "steelblue")) + 
      ylab("USD to EUR and GBP") + 
      stat_smooth(aes(x = date, y = Value, color = Currency, fill = Currency, method = "loess")) +
      fmxdat::theme_fmx() +
      labs(title = "The Euro and The Pound Sterling", subtitle = "") + 
      theme(legend.position = "bottom") 
p1

p2 <- use_data[,c(1,20)] %>% 
    ggplot() +
    geom_line(aes(x = date, y = Japan, color="steelblue", linetype="dotdash")) + 
    stat_smooth(aes(x = date, y = Japan),  color = "#FC4E07", fill = "#FC4E07", size=1, linetype = 11, method = "loess") +
    fmxdat::theme_fmx() +
    labs(y = "USD to JPY", x ="date", title = "The Japanses Yen ", subtitle = "") +
    theme(legend.position = "none") 

p2

# Forecasting 

forecastArima <- function(x, n.ahead = 30) {
    myTs <- ts(x$Japan, start = 1, frequency = 256)
       fit.arima <- arima(myTs, order = c(0, 0, 1))
      fore <- forecast(fit.arima, h = n.ahead)
       plot(fore)
       upper <- fore$upper[, "95%"]
       lower <- fore$lower[, "95%"]
       trend <- as.numeric(fore$fitted)
       pred <- as.numeric(fore$mean)
       output <- data.frame(actual = c(x$Japan, rep(NA, n.ahead)), trend = c(trend, rep(NA, n.ahead)), pred = c(rep(NA, nrow(x)), pred), lower = c(rep(NA, nrow(x)), lower), upper = c(rep(NA, nrow(x)), upper), date = c(x$date,max(x$date) +(1:n.ahead))) 
       return(output)
}

plotForecastResult <- function(x, title = NULL) {
       x <- x[order(x$date), ]
       max.val <- max(c(x$actual, x$upper), na.rm = T)
       min.val <- min(c(x$actual, x$lower), na.rm = T)
       plot(x$date, x$actual, type = "l", col = "darkorchid2", main = title, 
                      xlab = "Date", ylab = "Currency Rate", xlim = range(x$date), 
                      ylim = c(min.val, max.val))
       grid()
       lines(x$date, x$trend, col = "cyan2")
       lines(x$date, x$pred, col = "red")
       lines(x$date, x$lower, col = "black")
       lines(x$date, x$upper, col = "black")
       legend("bottomleft", col = c("darkorchid2", "cyan2", "red",  "black"), lty = 1, c("Actual", "Trend", "Forecast", "Lower/Upper Bound"))
       }

result.arima <- forecastArima(data, n.ahead = 90)


plotForecastResult(result.arima, title = "Forecasting the Japanses Yen with ARIMA")
```