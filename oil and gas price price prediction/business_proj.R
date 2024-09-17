library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(Metrics)
# reading the dataset
df <- read.csv('weekly_fuel_prices_from_2005_to_20231113.csv')


#plotting different type of oil as a time series
plot(df[,'EURO.SUPER_95'])
plot(df[['EURO.SUPER_95']], type = 'l',xlab = 'weeks', ylab = 'Price of the EUR_SUPER_95')  # 'l' for a line plot
plot(df[,'AUTOMOTIVE_GAS_OIL'])
plot(df[,'AUTOMOTIVE_GAS_OIL'], type='l',xlab = 'weeks', ylab = 'Price of the AUTOMOTIVE_GAS_OIL')
plot(df[,'LPG'])
plot(df[,'LPG'], type='l',xlab = 'weeks', ylab = 'Price of the LPG')
plot(df[,'HEATING_GAS_OIL'])
plot(df[,'HEATING_GAS_OIL'], type='l',xlab = 'weeks', ylab = 'Price of the HEATING_GAS_OIL')
plot(df[,'RESIDUAL_FUEL_OIL'])
plot(df[,'RESIDUAL_FUEL_OIL'], type='l',xlab = 'weeks', ylab = 'Price of the RESIDUAL_FUEL_OIL')
plot(df[,'HEAVY_FUEL_OIL'])
plot(df[,'HEAVY_FUEL_OIL'], type='l',xlab = 'weeks', ylab = 'Price of the HEAVY_FUEL_OIL')

#reading the Euro_super_95, heavy_oil, auto_gas as a time series, with freq=52 weeks
super <- ts(df[,'EURO.SUPER_95'], frequency = 52)
heavy_oil <- ts(df[,'HEAVY_FUEL_OIL'], frequency = 52)
auto_gas <- ts(df[,'AUTOMOTIVE_GAS_OIL'], frequency = 52)
LPG <- ts(df[,'LPG'], frequency = 52)
##create a variable 'time'
tt<- 1:NROW(super)



data <- list(super, heavy_oil, auto_gas, LPG)

# Creating a boxplot
boxplot(data,
        main = "Boxplot of oil's products",
        xlab = "names of products",
        ylab = "Price",
        col = c("red", "green", "blue", "orange"),  # Set colors for the boxes
        names = c("EUR_SUPER_95", "HEAVY_FUEL_OIL", "AUTOMOTIVE_GAS_OIL", "LPG"),  # Labels for each box
        border = "black"  # Set color for the borders of the boxes
)




##acf and pacf of variable "super: Eur_super_95"
# result shows obvious autocorrelation
acf(super)
pacf(super)

decomposed_super <- decompose(super)

# Plot the decomposed components
plot(decomposed_super)







linear_model <- lm(super~ tt)
summary(linear_model)
rmse_linear_model <- rmse(predict(linear_model), super)
 
# RMSE value: 152.79

##plot of the linear_model 
plot(tt, super, xlab="Time", ylab="Price of the EUR_SUPER_95", type='l')
abline(linear_model, col=3)


##check the residuals? are they autocorrelated? Test of DW
dwtest(linear_model)

# DW_linear_model = 0.017499


#The Durbin-Watson test is used to detect autocorrelation 
#(serial correlation) in the residuals of a regression analysis. 
#In our case, the test result indicates a very low Durbin-Watson (DW) 
#statistic and an extremely low p-value, suggesting strong evidence #
#against the null hypothesis of no autocorrelation.

#Here's how to interpret the results:

#DW Statistic: The DW statistic ranges between 0 and 4. 
#A value around 2 suggests no autocorrelation. 
#Values significantly below 2 indicate positive autocorrelation, 
#while values significantly above 2 suggest negative autocorrelation. 
#our value of 0.017499 strongly indicates positive autocorrelation.

#P-Value: The very low p-value (typically less than 0.05) 
#suggests that the DW statistic is significantly different from 2, 
#providing strong evidence to reject the null hypothesis. 
#In our case, the p-value is much smaller than the 
#conventional significance level of 0.05, indicating 
#a highly significant result.


##check the residuals
res_linear_model<- residuals(linear_model)
plot(res_linear_model,xlab="Time", ylab="residuals", type='l' )


#the corrologram function shows that the model needs improvement which was obvious
acf(res_linear_model)
# it shows our residual highly correlated which means we should look for
# better model.

#fit a linear model with tslm
m1<- tslm(super~ trend+ season)
summary(m1)

fit<- fitted(m1)
rmse_tslinear_model_super <- rmse(fit, super)

# rmse_tslinear_model: 152.31

plot(super)
lines(fitted(m1), col=2)

fore <- forecast(m1)
plot(fore)
##forecasts from regression model for eur_super_95 The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res_tslm<- residuals(m1) 
plot(res_tslm) 
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res_tslm)

dw<- dwtest(m1, alt="two.sided")
dw

#DW = 0.016606

# it shows our residual highly correlated which means we should look for
# better model.




#Exponential smoothing
#install.packages("sm")
library(sm)
x <- tt
y <- df[,'EURO.SUPER_95']
sm.regression(tt, df[,'EURO.SUPER_95'],   h = 52, add = T)

plot(x, y, type='l')
sm.regression(x, y,   h = 10, add = T, col=2)

fc <- ses(super, h=12, alpha=0.95)
# Accuracy of one-step-ahead training errors


autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("EUR_SUPER_95") + xlab("Year")
# Calculate RMSE
#rmse_sm_super :summary(fc)
#rmse_sm_super = 20.66, alpha=0.95

fc1 <- holt(super, h=6)
fc2 <- holt(super, damped=TRUE, phi = 0.9, h=12)
autoplot(super) +
  autolayer(fc1, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("EUR_SUPER_95") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc1)
#RMSE= 20.38

summary(fc2)
#RMSE = 19.69

# Fitting model using arima model  
fit_arima_super <- auto.arima(super) 

# Extracting ARIMA parameters
arima_order <- fit_arima_super$arma
p <- arima_order[1]  # Autoregressive order (p)
q <- arima_order[2]  # Moving average order (q)
d <- fit_arima$drift  # Differencing order (d)

# Displaying the selected ARIMA parameters
cat("Selected ARIMA parameters: (p =", p, ", d =", d, ", q =", q, ")\n")

# Next 52 weeks forecasted values  
forecastedValues_super <- forecast(fit_arima_super, 12) 


plot(forecastedValues_super, main = "Forecasting price of the EUR_SUPER_95  ARIMA(2,1,1)(1,0,0)[52] ", 
     col.main = "darkgreen")  
lines(fitted(fit_arima_super), col = "blue")

pacf(fit_arima_super$residuals)
summary(fit_arima_super)
#ARIMA(2,1,1)(1,0,0)[52] with drift
# based on the summary RMSE= 17.82


##################################### heavy_oil section

##acf of variable "heavy_oil"
# result shows obvious autocorrolation
acf(heavy_oil)
pacf(heavy_oil)

decomposed_heavy_oil <- decompose(heavy_oil)

# Plot the decomposed components
plot(decomposed_heavy_oil)



linear_model_heavy_oil <- lm(heavy_oil~ tt)
summary(linear_model_heavy_oil)
rmse_linear_model_heavy_oil <- rmse(predict(linear_model_heavy_oil), heavy_oil)
# rmse=11.58

##plot of the linear_model 
plot(tt, heavy_oil, xlab="Time", ylab="Price of the heavy_oil", type='l')
abline(linear_model_heavy_oil, col=3)


##check the residuals? are they autocorrelated? Test of DW
dwtest(linear_model_heavy_oil)
#data:  linear_model_heavy_oil
#DW = 0.015114



#The Durbin-Watson test is used to detect autocorrelation 
#(serial correlation) in the residuals of a regression analysis. 
#In our case, the test result indicates a very low Durbin-Watson (DW) 
#statistic and an extremely low p-value, suggesting strong evidence #
#against the null hypothesis of no autocorrelation.

#Here's how to interpret the results:

#DW Statistic: The DW statistic ranges between 0 and 4. 
#A value around 2 suggests no autocorrelation. 
#Values significantly below 2 indicate positive autocorrelation, 
#while values significantly above 2 suggest negative autocorrelation. 
#our value of 0.017499 strongly indicates positive autocorrelation.

#P-Value: The very low p-value (typically less than 0.05) 
#suggests that the DW statistic is significantly different from 2, 
#providing strong evidence to reject the null hypothesis. 
#In our case, the p-value is much smaller than the 
#conventional significance level of 0.05, indicating 
#a highly significant result.


##check the residuals
res_linear_model_heavy_oil<- residuals(linear_model_heavy_oil)
plot(res_linear_model_heavy_oil,xlab="Time", ylab="residuals", type='l' )


#the corrologram function shows that the model needs improvement which was obvious
acf(res_linear_model_heavy_oil)
# it shows our residual highly correlated which means we should look for
# better model.

#fit a linear model with tslm
m1_heavy_oil<- tslm(heavy_oil~ trend+ season)
summary(m1_heavy_oil)
fit_heavy_oil<- fitted(m1_heavy_oil)

rmse_tslinear_model_heavy_oil <- rmse(fit_heavy_oil, heavy_oil)
#rmse_tslm_heavy_oil = 117.35
plot(heavy_oil)
lines(fitted(m1_heavy_oil), col=2)

fore <- forecast(m1_heavy_oil)
plot(fore)
##forecasts from regression model for eur_super_95 The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res_tslm_heavy_oil<- residuals(m1_heavy_oil) 
plot(res_tslm_heavy_oil) 
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res_tslm_heavy_oil)

dw<- dwtest(m1_heavy_oil, alt="two.sided")
dw
#data:  m1_heavy_oil
#DW_tslm_heavy_oil = 0.01439

# it shows our residual highly correlated which means we should look for
# better model.




#Exponential smoothing
#install.packages("sm")
library(sm)
x_heavy_oil <- tt
y_heavy_oil <- df[,'HEAVY_FUEL_OIL']
sm.regression(tt, df[,'HEAVY_FUEL_OIL'],   h = 52, add = T)

plot(x_heavy_oil, y_heavy_oil, type='l')
sm.regression(x_heavy_oil, y_heavy_oil,   h = 10, add = T, col=2, alpha=0.95)

fc_heavy_oil <- ses(heavy_oil, h=12)
summary(fc_heavy_oil)
# rmse_fc_heavy_oil = 14.46
# Accuracy of one-step-ahead training errors


autoplot(fc_heavy_oil) +
  autolayer(fitted(fc_heavy_oil), series="Fitted") +
  ylab("HEAVY_FUEL_OIL") + xlab("year")


fc1_heavy_oil <- holt(heavy_oil, h=12)
fc2_heavy_oil <- holt(heavy_oil, damped=TRUE, phi = 0.9, h=12)
autoplot(heavy_oil) +
  autolayer(fc1_heavy_oil, series="Holt's method", PI=FALSE) +
  autolayer(fc2_heavy_oil, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("year") +
  ylab("HEAVY_FUEL_OIL") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc1_heavy_oil)
#rmse_fc1_heavy_oil= 14.45

summary(fc2_heavy_oil)
#rmse_fc2_heavy_oil = 14.37

# Fitting model using arima model  
fit_arima_heavy_oil <- auto.arima(heavy_oil) 

# Extracting ARIMA parameters
arima_order_heavy_oil <- fit_arima_heavy_oil$arma
p <- arima_order_heavy_oil[1]  # Autoregressive order (p)
q <- arima_order_heavy_oil[2]  # Moving average order (q)
d <- fit_arima_heavy_oil$drift  # Differencing order (d)

# Displaying the selected ARIMA parameters
cat("Selected ARIMA parameters: (p =", p, ", d =", d, ", q =", q, ")\n")

# Next 52 weeks forecasted values  
forecastedValues_heavy_oil <- forecast(fit_arima_heavy_oil, 12) 


plot(forecastedValues_heavy_oil, main = "Forecasting price of the Heavy oil ARIMA(1,1,1)", 
     col.main = "darkgreen")  
lines(fitted(fit_arima_heavy_oil), col = "blue")

pacf(fit_arima_heavy_oil$residuals)
summary(fit_arima_heavy_oil)
# based on the summary RMSE= 14.34
#Series: heavy_oil 
#ARIMA(1,1,1) 





##################################### auto_gas section

plot(df[,'AUTOMOTIVE_GAS_OIL'])
plot(df[,'AUTOMOTIVE_GAS_OIL'], type='l',xlab = 'weeks', ylab = 'Price of the AUTOMOTIVE_GAS_OIL')

##acf of variable "auto_gas"
# result shows obvious autocorrolation
acf(auto_gas)
pacf(auto_gas)

decomposed_auto_gas <- decompose(auto_gas)

# Plot the decomposed components
plot(decomposed_auto_gas)



linear_model_auto_gas <- lm(auto_gas~ tt)
summary(linear_model_auto_gas)
rmse_linear_model_auto_gas <- rmse(predict(linear_model_auto_gas), auto_gas)
#rmse_linear_model_auto_gas = 173.57
##plot of the linear_model 
plot(tt, auto_gas, xlab="Time", ylab="Price of the auto_gas", type='l')
abline(linear_model_auto_gas, col=3)


##check the residuals? are they autocorrelated? Test of DW
dwtest(linear_model_auto_gas)
#DW = 0.01586
#The Durbin-Watson test is used to detect autocorrelation 
#(serial correlation) in the residuals of a regression analysis. 
#In our case, the test result indicates a very low Durbin-Watson (DW) 
#statistic and an extremely low p-value, suggesting strong evidence #
#against the null hypothesis of no autocorrelation.

#Here's how to interpret the results:

#DW Statistic: The DW statistic ranges between 0 and 4. 
#A value around 2 suggests no autocorrelation. 
#Values significantly below 2 indicate positive autocorrelation, 
#while values significantly above 2 suggest negative autocorrelation. 
#our value of 0.017499 strongly indicates positive autocorrelation.

#P-Value: The very low p-value (typically less than 0.05) 
#suggests that the DW statistic is significantly different from 2, 
#providing strong evidence to reject the null hypothesis. 
#In our case, the p-value is much smaller than the 
#conventional significance level of 0.05, indicating 
#a highly significant result.


##check the residuals
res_linear_model_auto_gas<- residuals(linear_model_auto_gas)
plot(res_linear_model_auto_gas,xlab="Time", ylab="residuals", type='l' )


#the corrologram function shows that the model needs improvement which was obvious
acf(res_linear_model_auto_gas)
# it shows our residual highly correlated which means we should look for
# better model.

#fit a linear model with tslm
m1_auto_gas<- tslm(auto_gas~ trend+ season)
summary(m1_auto_gas)
fit_auto_gas<- fitted(m1_auto_gas)

plot(auto_gas)
lines(fitted(m1_auto_gas), col=2)

fore <- forecast(m1_auto_gas)
plot(fore)
##forecasts from regression model for auto_gas The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res_tslm_auto_gas<- residuals(m1_auto_gas) 
plot(res_tslm_auto_gas) 
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res_tslm_auto_gas)

dw<- dwtest(m1_auto_gas, alt="two.sided")
dw
# it shows our residual highly correlated which means we should look for
# better model.




#Exponential smoothing
#install.packages("sm")
library(sm)
x_auto_gas <- tt
y_auto_gas <- df[,'AUTOMOTIVE_GAS_OIL']
sm.regression(tt, df[,'AUTOMOTIVE_GAS_OIL'],   h = 52, add = T)

plot(x_auto_gas, y_auto_gas, type='l')
sm.regression(x_auto_gas, y_auto_gas,   h = 10, add = T, col=2)

fc_auto_gas <- ses(auto_gas, h=12)
# Accuracy of one-step-ahead training errors
summary(fc_auto_gas)
#RMSE = 21.87

autoplot(fc_auto_gas) +
  autolayer(fitted(fc_auto_gas), series="Fitted") +
  ylab("auto_gas") + xlab("year")


fc1_auto_gas <- holt(auto_gas, h=12)
fc2_auto_gas <- holt(auto_gas, damped=TRUE, phi = 0.9, h=12)
autoplot(auto_gas) +
  autolayer(fc1_auto_gas, series="Holt's method", PI=FALSE) +
  autolayer(fc2_auto_gas, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("year") +
  ylab("auto_gas") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc1_auto_gas)
#RMSE = 21.87
summary(fc2_auto_gas)
#RMSE = 21.73

# Calculate residuals for each forecast
residuals_fc1_auto_gas <- residuals(fc1_auto_gas)
residuals_fc2_auto_gas <- residuals(fc2_auto_gas)
acf(residuals_fc1_auto_gas)
acf(residuals_fc2_auto_gas)
# Fitting model using arima model  
fit_arima_auto_gas <- auto.arima(auto_gas) 

# Extracting ARIMA parameters
arima_order_auto_gas <- fit_arima_auto_gas$arma
p <- arima_order_auto_gas[1]  # Autoregressive order (p)
q <- arima_order_auto_gas[2]  # Moving average order (q)
d <- fit_arima_auto_gas$drift  # Differencing order (d)

# Displaying the selected ARIMA parameters
cat("Selected ARIMA parameters: (p =", p, ", d =", d, ", q =", q, ")\n")

# Next 52 weeks forecasted values  
forecastedValues_auto_gas <- forecast(fit_arima_auto_gas, 12) 


plot(forecastedValues_auto_gas, main = "Forecasting price of the auto gas ARIMA(0,1,2)", 
     col.main = "darkgreen")  
lines(fitted(fit_arima_auto_gas), col = "blue")

pacf(fit_arima_auto_gas$residuals)
summary(fit_arima_auto_gas)
# based on the summary RMSE= 19.61




##################################### LPG section

plot(df[,'LPG'])
plot(df[,'LPG'], type='l',xlab = 'weeks', ylab = 'Price of the LPG')

##acf of variable "auto_gas"
# result shows obvious autocorrolation
acf(LPG)
pacf(LPG)

decomposed_LPG <- decompose(LPG)

# Plot the decomposed components
plot(decomposed_LPG)



linear_model_LPG <- lm(LPG~ tt)
summary(linear_model_LPG)
rmse_linear_model_LPG <- rmse(predict(linear_model_LPG), LPG)
#rmse_linear_model_LPG = 85.48

##plot of the linear_model 
plot(tt, LPG, xlab="weeks", ylab="Price of the LPG", type='l')
abline(linear_model_LPG, col=3)


##check the residuals? are they autocorrelated? Test of DW
dwtest(linear_model_LPG)
# DW = 0.0073603

#The Durbin-Watson test is used to detect autocorrelation 
#(serial correlation) in the residuals of a regression analysis. 
#In our case, the test result indicates a very low Durbin-Watson (DW) 
#statistic and an extremely low p-value, suggesting strong evidence #
#against the null hypothesis of no autocorrelation.

#Here's how to interpret the results:

#DW Statistic: The DW statistic ranges between 0 and 4. 
#A value around 2 suggests no autocorrelation. 
#Values significantly below 2 indicate positive autocorrelation, 
#while values significantly above 2 suggest negative autocorrelation. 
#our value of 0.017499 strongly indicates positive autocorrelation.

#P-Value: The very low p-value (typically less than 0.05) 
#suggests that the DW statistic is significantly different from 2, 
#providing strong evidence to reject the null hypothesis. 
#In our case, the p-value is much smaller than the 
#conventional significance level of 0.05, indicating 
#a highly significant result.


##check the residuals
res_linear_model_LPG<- residuals(linear_model_LPG)
plot(res_linear_model_LPG,xlab="Time", ylab="residuals", type='l' )


#the corrologram function shows that the model needs improvement which was obvious
acf(res_linear_model_LPG)
# it shows our residual highly correlated which means we should look for
# better model.

#fit a linear model with tslm
m1_LPG<- tslm(LPG~ trend+ season)
summary(m1_LPG)
fit_LPG<- fitted(m1_LPG)

plot(LPG)
lines(fitted(m1_LPG), col=2)
rmse_tslinear_LPG <- rmse(fit_LPG, LPG)
#rmse_tslinear_LPG = 85.43
fore <- forecast(m1_LPG)
plot(fore)
##forecasts from regression model for auto_gas The dark shaded region shows 80% prediction intervals and the light shaded 95% prediction intervals (range of values the random variable could take with relatively high probability). 

#analysis of residuals
res_tslm_LPG<- residuals(m1_LPG) 
plot(res_tslm_LPG) 
#the form of residuals seems to indicate the presence of negative autocorrelation
Acf(res_tslm_LPG)

dw<- dwtest(m1_LPG, alt="two.sided")
dw
#DW = 0.0071435


# it shows our residual highly correlated which means we should look for
# better model.




#Exponential smoothing
#install.packages("sm")
library(sm)
x_LPG <- tt
y_LPG <- df[,'LPG']
sm.regression(tt, df[,'LPG'],   h = 6, add = T)

plot(x_LPG, y_LPG, type='l')
sm.regression(x_LPG, y_LPG,   h =5 , add = T, col=2)

fc_LPG <- ses(LPG, h=5)
# Accuracy of one-step-ahead training errors
summary(fc_LPG)
#RMSE_fc_LPG = 7.33
autoplot(fc_LPG) +
  autolayer(fitted(fc_LPG), series="Fitted") +
  ylab("LPG") + xlab("year")


fc1_LPG <- holt(LPG, h=12)
fc2_LPG <- holt(LPG, damped=TRUE, phi = 0.9, h=12)
autoplot(LPG) +
  autolayer(fc1_LPG, series="Holt's method", PI=FALSE) +
  autolayer(fc2_LPG, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("year") +
  ylab("LPG") +
  guides(colour=guide_legend(title="Forecast"))
summary(fc1_LPG)
#RMSE = 6.86
summary(fc2_LPG)
#RMSE = 6.63
# Calculate residuals for each forecast
residuals_fc1_LPG <- residuals(fc1_LPG)
residuals_fc2_LPG <- residuals(fc2_LPG)
acf(residuals_fc1_LPG)
acf(residuals_fc2_LPG)
# Fitting model using arima model  
fit_arima_LPG <- auto.arima(LPG) 

# Extracting ARIMA parameters
arima_order_LPG <- fit_arima_LPG$arma
p <- arima_order_LPG[1]  # Autoregressive order (p)
q <- arima_order_LPG[2]  # Moving average order (q)
d <- fit_arima_LPG$drift  # Differencing order (d)

# Displaying the selected ARIMA parameters
cat("Selected ARIMA parameters: (p =", p, ", d =", d, ", q =", q, ")\n")

# Next 52 weeks forecasted values  
forecastedValues_LPG <- forecast(fit_arima_LPG, 12) 


plot(forecastedValues_LPG, main = "Forecasting price of the LPG ARIMA(1,1,2) ", 
     col.main = "darkgreen")  
lines(fitted(fit_arima_LPG), col = "blue")

pacf(fit_arima_LPG$residuals)
summary(fit_arima_LPG)
# based on the summary RMSE= 6.43

























































