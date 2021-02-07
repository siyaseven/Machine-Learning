setwd("C:/Users/tanjo/Downloads/Siya_Machine_Learning/Machine-Learning/Final Project_JS_Groupo")
#Here find the list of packages used for this project

library(timeSeries)
library(tseries) #for the Adf.test
""" 
The objective
0. Data mining (cleaning was done in Python)
1. Load the data returns of stocks
2. Explore the data 
3. AR model and Predict the stock returns
4. MA model and Predict the stock returns
5. Back Testing of the models
6. Try to compute the coefficinets of the models manual
""" 

#1. Uploading the data 
data = read.csv("dataR.csv", header = TRUE, sep = ",")
data =data[-1]#deleting the first column (which is Pandas indexing)
data = data[-1,]# removing the first row
head(data)# confirming that the data is in order fo
dim(data)

#2. Explore the data 
# Descriptive statistics (Measures of central location)
# Measures of spread (Variance, deviation)

da = data[1:2]
summary(da[2]) # Descriptive Statistics
sqrt(var(da[2])) #Standard deviation
boxplot(da[2]) #Box plot 

# 3. AR model

# Convert our data to time series object
da = as.timeSeries(da)
str(da)
# Check the trend in our time series object
plot(da)

# Check seasonality in our time series object

# Check Stationarity
#Dickey Fuller test (Unit root test)
#H0: p=1 (Non Stationary)
#H1: |p|<1 Stationary
#We reject H0 and conclude that our data is stationary at alpha = 0.025
# with lag order = 16
adf.test(da)

# Model fitting 
AR_model = ar(da)
summary(AR_model) # It is insane# gives unreliable results
AR_model
ARMAacf(AR_model$ar,36,TRUE)

# Check ACF and PACF

#4. MA model and Predict the stock returns

MA_model =  arma(da,order = c(2,2))
summary(MA_model)
 A = c(1:5)
M =  c(1:5)

for (i in 1:5){
    MA_model =  arma(da,order = c(i,i))
    summary(MA_model)
}
# Convert our data to time series object
# Check the trend in our time series object
# Check Stationarity 
# Check ACF and PACF 
# Check seasonality in our time series object
# Model fitting 

