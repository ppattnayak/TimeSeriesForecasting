#load the necessary libaries
library(ggplot2)
library(XLConnect)
library(tidyverse)
library(forecast)
library(dplyr)
library(plyr)
library(Amelia)
library(caTools)
library(tseries)
library(scales)
library(gridExtra)
library(lmtest)

#read the csv file from U.S. Bureau of Economic Analysis.
#U.S. Bureau of Economic Analysis, Motor Vehicle Retail Sales: Domestic Autos [DAUTONSA], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DAUTONSA, December 18, 2017.
#https://fred.stlouisfed.org/series/DAUTONSA?utm_source=series_page&utm_medium=related_content&utm_term=other_formats&utm_campaign=other_format
#Source: U.S. Bureau of Economic Analysis   Release: Supplemental Estimates, Motor Vehicles  
#Units:  Thousands of Units, Not Seasonally Adjusted

#Frequency:  Monthly

#Autos are all passenger cars, including station wagons. Domestic sales are all United States (U.S.) sales of vehicles assembled in the U.S., Canada, and Mexico.



monthly_sales <- read.csv("G:\\RProject\\Time Series Stock Returns\\Auto Sales\\DAUTONSA.csv",header = TRUE, stringsAsFactors = FALSE) #Replace with the data file location
head(monthly_sales)
#DATE DAUTONSA csales cs_monthly 
#1 2011-01-01    258.6  258.6 
#2 2011-02-01    338.6  338.6 
#3 2011-03-01    445.3  445.3 
#4 2011-04-01    405.4  405.4
#5 2011-05-01    362.2  362.2
#6 2011-06-01    349.0  349.0

#we will be using the monthly domestic auto sales(in thousands) for building our time series

#Before we proceed, let's check if we have any missing data
#Amelia library gives us a missmap function that shows the missing details in a visual map
missmap(monthly_sales)

#Since no missing data is found, we can go ahead with visually plotting the daily count to see if we can identify any trends,
#seasonality, cycle or outlier from the data.

#Before we plot the count data, we need to convert the dteday field from character to date type
monthly_sales$DATE <- as.Date(monthly_sales$DATE, "%m/%d/%Y")

#Plot the daily bicycle count
uc_ts_plot <- ggplot(monthly_sales, aes(DATE,DAUTONSA)) + geom_line(na.rm=TRUE) + xlab("Month") + ylab("Auto Sales in Thousands") + scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + stat_smooth(colour = "green")
uc_ts_plot

#There seems to be an outlier that we could see from the plot. We need to remove it before we proceed with stationarizing the series.
monthly_ts <- ts(monthly_sales[,c('DAUTONSA')])
monthly_sales$csales <- tsclean(monthly_ts)

#Plot the cleaned daily bicycle count
c_ts_plot <- ggplot(monthly_sales, aes(DATE,csales)) + geom_line(na.rm=TRUE) + xlab("Month") + ylab("Auto Sales in Thousands") + scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("1 year")) + stat_smooth(colour="green")
c_ts_plot

#Compare cleaned and uncleaned plots
grid.arrange(uc_ts_plot,c_ts_plot,ncol=1)

#If data points are still volatile, then we can apply smoothing smoothing
#By applying smoothing, we can have a better idea about the series and it's components
#It also makes the series more predictable
#in this case, we can use quarterly/biannualy moving average. If the data points are on a daily basis, 
#any level of seasonality(daily, weekly, monthly or yearly) can be incorporated. 
#Our data does not require any smoothing

#monthly_sales$cs_Q <- ma(monthly_sales$csales, order =3)
#ggplot(monthly_sales, aes(DATE,cs_Q)) + geom_line(na.rm=TRUE) + xlab("Month") + ylab("Cleaned Daily Bike Count") + scale_x_date(labels = date_format(format= "%b-%Y"),breaks = date_breaks("3 months"))

#We use this smoothed data for our analysis

my_ts <- ts(na.omit(monthly_sales$csales), frequency = 12)
#my_ts <- ts(daily_count_ts, frequency = 365)

plot(my_ts)
#Seasonality can be removed from the data by which can be removed by taking log transformation
plot(my_ts)
#Now that the series is smoothed, we need to remove trend by using appropriate order of difference and make the series stationary
#We do this by looking at acf, Dickey-Fuller Test and standard deviation

Acf(my_ts)
#ACF plot shows positive correlation at higher lags. this indicates that we need differencing to make the series stationary.

adf.test(my_ts)
#P value is 0.2037 indicating the null hypothesis 'series is non-stationary' is true i.e the series is not stationary

#Let's try a order 1 difference
#We will fit ARIMA(0,d,0)90,D,0)12 models and verify acf residuals to find which 'd' or 'D' order of differencing is appropriate

#Applying only one order of difference i.e ARIMA(0,1,0)(0,0,0)
dfit1 <- arima(my_ts,order = c(0,1,0))
plot(residuals(dfit1))

par(mfrow=c(2,1))
Acf(residuals(dfit1))
Pacf(residuals(dfit1))
par(mfrow=c(1,1))

#The differenced series still shows some strong autocorrelation at the seasonal period 12.
#Because the seasonal pattern is strong and stable, 
#we know that we will want to use an order of seasonal differencing in the model.

#Before that let's try only with one seasonal difference i.e ARIMA(0,0,0)(0,1,0)
dfit2 <- arima(my_ts, order =c(0,0,0), seasonal = list(order = c(0,1,0), period = 12))

plot(residuals(dfit2))
#Residuals does not look like white noise
par(mfrow=c(2,1))
Acf(residuals(dfit2))
Pacf(residuals(dfit2))
par(mfrow=c(1,1))

#The seasonally differenced series shows a very strong pattern of positive autocorrelation and is similar to a seasonal random walk model.
#This indicates an AR signature or another order of difference.

#Let's go ahead and apply both seasonal and non-seasonal differencing i,e ARIMA(0,1,0)(0,1,0)12
dfit3 <- arima(my_ts, order =c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))

plot(residuals(dfit3))

#Residuals seems to return to the mean and we don't see any pattern in the residuals.
par(mfrow=c(2,1))
Acf(residuals(dfit3))
Pacf(residuals(dfit3))
par(mfrow=c(1,1))

#ACF at lag 1 is -ve and slightly smaller than -0.4. We know that if the lag 1 acf falls below -0.5, 
#then the series is over differenced. Positive spikes in acf have become negative, another sign of possible over differencing.
#Therefore, this model might be suffering from slight over differencing. This overdifferencing can be compensated by adding a MR term.

#To select the appropriate order of differencing, we have to consider the error statistics, the standard deviation in specific.
#SD is same as RMSE
summary(dfit1)
summary(dfit2)
summary(dfit3)

#Out of the above dfit3 model i.e ARIMA(0,1,0)(0,1,0)12 has the lowest standard deviation and AIC.
#Therefore, it is the correct order of differencing.

#therefore, d=1 and D=1
#Now, we need to identify AR/MA and SAR/SMA values and fit the model.

#ACF is negative at lag 1 and shows sharp cut-off immediately after lag 1, we can add a MA to the model to compensate for the overdifferencing.
#Since, we do not see any correlation at lag s,2s,3s etc i.e 12,24,36 etc, we do not need to add SAR/SMA to our model
dfit4 <- arima(my_ts, order =c(0,1,1), seasonal = list(order = c(0,1,0), period = 12))
plot(residuals(dfit4))


par(mfrow=c(2,1))
Acf(residuals(dfit4))
Pacf(residuals(dfit4))
par(mfrow=c(1,1))
summary(dfit4)

#Slight amount of correlation remains at lag 8, but the overall plots seem good. Let's check the model parameter's significance.
coeftest(dfit4)
#P value is negligible and the test shows that ma1 coefficient is statistically significant.

#Next, we run auto.Arima to find the model with lowest AIC and compare it to our model.
dfit5 <- auto.arima(my_ts, seasonal = TRUE)
plot(residuals(dfit5))


par(mfrow=c(2,1))
Acf(residuals(dfit5))
Pacf(residuals(dfit5))
par(mfrow=c(1,1))
summary(dfit5)
coeftest(dfit5)
#Auto arima gives us ARIMA(1,1,2)(1,0,0)[12]. All coefficients are significant.

#Clearly this model performs worse than the model we built earlier as ARIMA(0,1,1)(0,1,0)[12] has a AIC of 675.86
#As opposed to a higher AIC of 799.46 generated by auto-arima.
#RMSE for both models are about the same.

#By rule of parsimony and/or minimum AIC, we can reject ARIMA(1,1,2)(1,0,0)[12] and accept ARIMA(0,1,1)(0,1,0)[12] as our model.
#Looking back at the auto generated model, we can infer that an additional AR component, which underdifferences the series,
#is being compensated by addition of a MA component.
#As AR and MA components tend to cancel each other's effect, it's always a good idea to try a model with one fewer AR term and one fewer MA term.



#To see how our model will perform in future, we can use n-fold holdout method.
hold <- window(ts(my_ts), start = 72)

#fit the model to predict for observation(months) 72 through 83.
fit_predicted <- arima(ts(my_ts[-c(72:83)]), order =c(0,1,1), seasonal = list(order = c(0,1,0), period = 12))

#Use the above model to forecast values for last 10 months
forecast_pred <- forecast(fit_predicted,h=24)

#Plot the predicted values and the validate against the actual data
plot(forecast_pred, main =" ")
lines(ts(my_ts), col = 'dark red')

#Blue line is the predicted data and the confidence bands are in dark grey(80%) and light grey(95%). 
#Model's prediction is pretty good and we can see predicted sales closely follow the actual data. This is an indication of a good model.


#Next step in our model is to forecast values i.e the monthly sales data.
f_values <- forecast(dfit4, h=24)
#Plot the predictions
plot(f_values,showgap = FALSE)



#Confidence band for long term predictions keep on diverging. This is an indication that long term or distant futures
#predicted values are less certain. The reason for such uncertainty is that the model regresses future predictions on 
#previously predicted values. Therefore, the Confidence bands increase in width.

#Therefore, long-term forecasts using ARIMA/SARIMA should be avioded. For long-term prediction, 
# it would be good to also draw on other sources of information during the model selection process and/or
#use an auxiliary model and take into account othe factors such as economic conditions, average income etc.
