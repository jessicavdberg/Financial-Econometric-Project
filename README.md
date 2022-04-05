# Finanical Econometric Project

Using principal component analysis, I identify the core driving factors of emerging and developing market currency rates. I analyse 33 currencies from 1994-03-30 to 2021-10-29. I find that the first principal component is related to commodities, especially oil. The second principal component relates to political instability and the absence of violence/terrorism. The third principal component relates to some Asian factor.  These factors can be used in further studies to make predictions about currency rates and determine whether they are under/overvalued.
 
In the Essay folder, you will find the pdf of the paper that is submitted for grading, called "Essay.pdf". Here, I will explain some of the code that I used. 
 
I loaded the following packages to preform my analysis. 
 
```r
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(scales)
library(ggpmisc)
library(forecast)
library(stats)
library(tseries)
library(tidyverse)
library(devtools)
library(dplyr)
library(readxl)
library("FactoMineR")
library("factoextra")
library("corrplot")

pacman::p_load("rugarch","tbl2xts", "lubridate", "PerformanceAnalytics", "ggthemes", "robustbase")

currency <- readRDS("C:/Users/jesic/OneDrive/Desktop/DATA/currencies.rds")

```
The dataset was manipulated in a few ways in order to make it usable for the empirical analysis. First, the weekend rows were removed from the dataset since this information is NA. Secondly, the first 1107 rows were also removed, as for many countries this information was not available. 

A couple of other manipulations were done in order to make the data tidy. Lastly, the data was spread as it is an requirement for PCA analysis. 

```r
currency_spread <- currency %>% spread(Name, Price)

# Removing weekend from dateset 
no_weekend <- currency_spread[which(weekdays(as.Date(currency_spread$date, format = "%m/%d/%Y"))
         %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

# getting rid of NA data
final <- no_weekend[-c(1:1107),]

# Making data tidy 
try1 <- final %>% gather(Curry, Value, -date) %>% mutate(Curr1 = gsub("_Cncy", "", Curry)) %>% mutate(Currency=gsub("_Inv","",Curr1))

data <- try1[ , c("date", "Currency", "Value", "Curry", "Curr1")]
data <- data[, -c(4,5)]

use_data <- data %>% spread(Currency, Value)
colnames(use_data)
```
 
I only use emerging and developing market currency rates, therefore the frontier and standalone market countries are removed from the dataset. I then start to preform my prinicpal comoponent analysis, stating with analyzing teh scree plot and the first principal componenet.  

```r
# Only emerging and developed countries - not frontiers
use <- use_data[,-c(2,4,6,15,23,29,40,42)]

use <- use %>% select(-date)

# By hand 
covmat <- cov(use)

res.pca <- PCA(use, graph = FALSE)

eig.val <- get_eigenvalue(res.pca)

# Scree plot 
scree <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
scree 
````

```r
w <- res.pca[["ind"]][["dist"]]

date <- use_data$date

plot_data <- data.frame(date, w)

pc1 <- plot_data %>% ggplot() + 
    geom_line(aes(x = plot_data$date, y = plot_data$w), color ="steelblue") + 
    fmxdat::theme_fmx() +
    labs(y = "", x ="date", title = "First Principal Component", subtitle = "") +
    theme(legend.position = "none") 

pc1

```

The next piece of code contains numerous descriptive graphs that were used to interpret the PCA results and make sound economic conclusion. However, it was decided not to add the graphs in the paper. 
```r
var <- get_pca_var(res.pca)


# Coordinates
obs1 <- head(var$coord)
# Cos2: quality on the factore map
obs2 <- head(var$cos2)
# Contributions to the principal components
obs3 <- head(var$contrib)


graph1 <- fviz_pca_var(res.pca, col.var = "black")

graph2 <- corrplot(var$cos2, is.corr=FALSE)

graph <- fviz_cos2(res.pca, choice = "var", axes = 1:2)


# Contributions of variables to PC1
graph4 <- fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
graph5 <- fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Total contribution by Pc1 and PC2
graph6 <- fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)


graph7 <- fviz_pca_var(res.pca, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

```


Furthermore, I forecast the currency rates of the three most traded currencies.
First, I plot the currencies, manipulating the data and making it tidy. I use the "stat_smooth" function to easily recognize patterns. 

```r
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
 ```
 
 ```r
p2 <- use_data[,c(1,20)] %>% 
    ggplot() +
    geom_line(aes(x = date, y = Japan, color="steelblue", linetype="dotdash")) + 
    stat_smooth(aes(x = date, y = Japan),  color = "#FC4E07", fill = "#FC4E07", size=1, linetype = 11, method = "loess") +
    fmxdat::theme_fmx() +
    labs(y = "USD to JPY", x ="date", title = "The Japanses Yen ", subtitle = "") +
    theme(legend.position = "none") 

p2
```
After plotting the data, I use ARIMA function to forecast the currency rates. 

```r

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
