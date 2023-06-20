---
title: "Forecasting the Purchasing Power of Peso in Davao City from June 2023 - December 2024"
author: "Vladimer Kobayashi"
date: "6/8/2021"
output:
bookdown::pdf_document2: default
bookdown::html_document2: default
editor_options: 
chunk_output_type: console
---
##libraries included in the code
library(ggplot2)  ##for plotting the data
library(stlplus)  ##for decomposing the data
library(forecast) ##for ARIMA

  
##inputting the data
purch_pow.ts <- ts(c(1.02,	1.03,	1.03,	1.02,	1.02,	1.01,	1.00,	0.99,	0.97,	0.97,	0.98,	0.98,
                  0.98,	0.98,	0.98,	0.98,	0.98,	0.97,	0.98,	0.97,	0.97,	0.97,	0.97,	0.96,
                  0.96,	0.96,	0.96,	0.96,	0.96,	0.96,	0.97,	0.97,	0.96,	0.95,	0.96,	0.95,
                  0.95,	0.94,	0.93,	0.93,	0.93,	0.93,	0.93,	0.93,	0.93,	0.93,	0.92,	0.92,
                  0.90,	0.90,	0.89,	0.88,	0.87,	0.87,	0.85,	0.85,	0.84,	0.84,	0.83,	0.83,
                  0.82,	0.81,	0.81,	0.81,	0.81), start = 2018, frequency = 12)

##time series plot
(autoplot(purch_pow.ts) + ggtitle("Purchasing Power of Peso in Davao City") 
                        + xlab("Year") + ylab("Purchasing Power in Peso"))

##stl function for decomposition
##to observe seasonal, trend pattern
purch_pow.ts_stl = stlplus(purch_pow.ts, s.window = "period") 
plot(purch_pow.ts_stl)
acf(purch_pow.ts)
pacf(purch_pow.ts)

##KPSS test to determine the order of differencing
##KPSS test shows that after one differencing, the test statistics is less than than
##1 percent level of significance (0.739)
diff1 = diff(purch_pow.ts, differences = 1)
diff1 %>% ur.kpss() %>% summary()

##In this case, the order of differencing is 1, based on the ndiffs function
ndiffs(purch_pow.ts) 

##ACF and PACF plot of the differenced data
##Plots are to determine the right ARIMA model
acf(diff1) ##q = 1
pacf(diff1)##p = 1

##Comapring of AICc from Arima() and auto.arima() functions
fit1 <- Arima(y=purch_pow.ts, order = c(0,1,1))
fit1 ##AICc = -439.42

fit2 <- Arima(y=purch_pow.ts, order = c(1,1,0))
fit2 ##AICc = -439.9

fit3 <- auto.arima(y=purch_pow.ts, seasonal = FALSE)
fit3 ##AICc = -451.9

fit4 <- auto.arima(y=purch_pow.ts, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)
fit4 ##AICc = -451.9

## Given the AICc of the four (4) models, the model with the lowest AICc is from fit3 and fit4 which is from auto.arima
## Hence, we will use the data from the auto.arima [fit4] for forecasting.

##Checking of residuals
checkresiduals(fit1)
checkresiduals(fit2)
checkresiduals(fit3)
checkresiduals(fit4)

##Checking for accuracy who has the lowest RMSE to determine which model fits the best
accuracy(fit1) ##RMSE = 0.00750
accuracy(fit2) ##RMSE = 0.00747
accuracy(fit3) ##RMSE = 0.00680
accuracy(fit4) ##RMSE = 0.00680

##Forecasting and plotting of the forecast
forecast1 <- forecast(fit4, h=19)
(autoplot(forecast1) + ggtitle("Purchasing Power of Peso in Davao City") 
+ xlab("Year") + ylab("Purchasing Power in Peso"))

##Forecasting and plotting of the forecast using the four ARIMA models
(autoplot(purch_pow.ts) +  
    autolayer(forecast1, series = "ARIMA(0,1,1)", alpha = 0.5) +
    autolayer(forecast2, series = "ARIMA(1,1,0)", alpha = 0.5) +
    autolayer(forecast3, series = "auto.arima w/o seasonality", alpha = 0.5) +
    autolayer(forecast4, series = "auto.arima w/o seasonality, stepwise, approximation", alpha = 0.5) +
    guides(colour = guide_legend("Model")) +
    ggtitle("Purchasing Power of Peso in Davao City") +
    xlab("Year") + ylab("Purchasing Power in Peso"))
