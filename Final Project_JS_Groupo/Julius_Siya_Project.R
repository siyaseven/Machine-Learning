setwd("C:/Users/tanjo/Downloads/Siya_Machine_Learning/Machine-Learning/Final Project_JS_Groupo")
#Here find the list of packages used for this project
library(timeSeries)
library(fAssets)
library(fPortfolio)
library(rmgarch)
library(parma)
library(forecast)
library(rugarch)
library(fExtremes)
library(evd)
library(rmgarch)
library(ff)
library(vars)
library(doParallel)
library(tseries)

#Uploading the data 
data0 = read.csv("dataR.csv", header = TRUE, sep = ",")
data0 =data0[-1]#deleting the first column (which is Pandas indexing)
data0 = data0[-1,]# removing the first row
head(data0)# confirming that the data is in order fo
dim(data0)

#head(data0)
#data0$CAC40=data0$CAC40*100
#data0$BVSP=data0$BVSP*100
#data0$DAX=data0$DAX*100
#data0$HSI=data0$HSI*100
#data0$SP500=data0$SP500*100

head(data0)
colnames(data0)[3]
for (i in 2:6){
  data0[,i]=data0[,i]*100
}
head(data0)

ret = as.timeSeries(data0)# converting the returns into timeseries object
head(ret)
plot(ret, main="Historical Returns",col="blue")
ret_ts=ts(ret, start=c(1998), end=c(2016), frequency=313)#ploting timeseries returns
plot(ret_ts, main="Historical Returns",col="blue")#Displaying plot
head(ret_ts)

#Descriptive statistics of the returns of the stocks and risk 
assetsRiskReturnPlot(ret_ts)# Returns plot vs Risk
par(mfrow = c(2, 3))#creating a space for plots
assetsHistPlot(ret[, 1:5])#the histogram plots of the returns
assetsCorTestPlot(ret_ts[, 1:5])# the correlations test between the returns of
#each variable.
assetsBasicStatsPlot(ret_ts,title = "", description = "") #returns Statistics 

#Dickey Fuller test (Unit root test)
#H0: p=1 (Non Stationary)
#H1: |p|<1 Stationary
#We reject H0 and conclude that our data is stationary at alpha = 0.05
# with lag order = 16
for (i in 1:5){
  x=ret[,i]
  x=as.timeSeries(x)
  z=adf.test(x)
  print (z)
}
adf.test(as.timeSeries(ret[,2]))
################################################################################

spec1 = ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = TRUE),variance.model = list(model = "sGARCH"), distribution.model = "sstd")
# d = c()
# n1=c()
# for (i in 1:5){
#   x=ret[,i]
#   x=as.timeSeries(x)
#   f=ugarchfit(spec1, data = x)
#   rsf=residuals(f)
#   sr=(rsf-mean(rsf))/sd(rsf)
#   d=c(d,sr)
#   n1=c(n1,colnames(ret[,i]))
# }
#names(d)=n1
#converting the data into time series
cac40re=as.timeSeries(ret[,1])
bvspre=as.timeSeries(ret[,2])
daxre=as.timeSeries(ret[,3])
hsire=as.timeSeries(ret[,4])
sp500re=as.timeSeries(ret[,5])

#fitting ugarch model for each variable so that we can obtain residuals 
fitcac40 = ugarchfit(spec1, data = cac40re)
fitbvsp = ugarchfit(spec1, data = bvspre)
fitdax = ugarchfit(spec1, data = daxre)
fithsi = ugarchfit(spec1, data = hsire)
fitsp500 = ugarchfit(spec1, data = sp500re)
#standardising the residuals
rescac40=(residuals(fitcac40)-mean(residuals(fitcac40)))/sd(residuals(fitcac40))
resbvsp=(residuals(fitbvsp)-mean(residuals(fitbvsp)))/sd(residuals(fitbvsp))
resdax=(residuals(fitdax)-mean(residuals(fitdax)))/sd(residuals(fitdax))
reshsi=(residuals(fithsi)-mean(residuals(fithsi)))/sd(residuals(fithsi))
ressp500=(residuals(fitsp500)-mean(residuals(fitsp500)))/sd(residuals(fitsp500))

#creating the datasest consist of the standardise residuals
datares=cbind(rescac40,resbvsp,resdax,reshsi,ressp500)
datares=as.timeSeries(datares)
modelvar = VAR(datares, p=16)#modelling residuals data of our variables
modelvar

#arch-test, normality test and serial correlation test 
#H0:There is no ARCH effects
#H1:There is ARCH effects
#We rejects the null hypothesis under alpha=0.05
archtest = arch.test(modelvar)
archtest
summary(archtest)

#Normality test
#H0:Data is normal distributed
#H1:The data is not normal distributed
#We rejects the null hypothesis under alpha=0.05
normalitytest <- normality.test(modelvar)
normalitytest
summary(normalitytest)

# serial correlation test
#H0:There is no Serial correlation
#H1:There exist Serial correlation
#We rejects the null hypothesis under alpha=0.05
serialtest <- serial.test(modelvar)
serialtest

##############################################################################
#Now We run the Multivariate GARCH models 
##############################################################################
#Portfolio Optimization using DCC-GARCH and GO-GARCH
##############################################################################
#1.DCC GARCH
##############################################################################
#specification of DCC-GARCH Model with multivariate t distribution
#NB 5 is the number of timeseries variables.
#We consider GARCH(1,1) model
uspec.n = multispec(replicate(5, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
spec.dcct = dccspec(uspec.n, dccOrder = c(1, 1), distribution ='mvt')
#Here we specify the DCC model and the risk metric we are using
#We basical defining the type of model we are using to optimize our portfolio
spec = parmaspec(scenario = ret, forecast = colMeans(ret), risk='CVaR',
      target = mean(colMeans(ret)),targetType = 'equality', riskType = 'minrisk',
     LB = rep(0,5), UB = rep(1,5), budget = 1,model="DCC",spec=spec.dccn)
#Here we fit the DCC model to our data 
dcc_fit=dccfit(spec.dcct, data=ret, solver="solnp", fit.control=list(eval.se=TRUE))
dcc_fit 
coef(dcc_fit)#Getting the coefficients off our models
res_dcc=residuals(dcc_fit)
# DCC_GARCH Plots
likelihood(dcc_fit)
#Conditional correlation matrix for DCC-GARCH
condcor_dcc=rcor(dcc_fit)

y=colnames(condcor_dcc)# colnames of Correlaltions matrix of DCC-GARCH model
#plotting estimated time evolutions for the pairwise conditional correlations for
#DCC-GARCH
par(mfrow = c(2, 2))
for (i in 1:5){
  for (j in 1:5 ){
    if (j<i){
      plot(condcor_dcc[i,j,], type = "l",col="blue",main=c(y[i],y[j]))
    }
  }
  
}
#Here we solve the for the weights of our the parameters giving the model we 
#specified for DCC-GARCH model.
dcc_sol_lp = parmasolve(spec, type = "LP")#solving weights using linear programming
dcc_sol_nlp = parmasolve(spec, type = 'NLP')#using nonlinear convex programming
#printing the weights of our portfolio
weights(dcc_sol_lp)# weights corresponding to linear programming
weights(dcc_sol_nlp)#weights corresponding to non-linear programming

#printing the Conditional Value at Risk corresponding to DCC-Model 
#sovled by linear and Non-linear programing techniques
parmarisk(dcc_sol_lp)# the portfolio CVaR in %
parmarisk(dcc_sol_nlp)# the portfolio CVaR in %

#printing the expected returns corresponding to DCC-Model 
#sovled by linear and Non-linear programing techniques
parmareward(dcc_sol_lp)# the portfolio expected return
parmareward(dcc_sol_nlp)# the portfolio expected return

#calculating Sharp ratio for DCC

dcc_sr_lp =parmareward(dcc_sol_lp)/parmarisk(dcc_sol_lp)
dcc_sr_nlp =parmareward(dcc_sol_nlp)/parmarisk(dcc_sol_nlp)
dcc_sr_lp
dcc_sr_nlp

##########################################################
### Go Garch
##########################################################

#specification of GO-GARCH Model with multivariate t distribution

# the procedure is the same a the one of DCC-GARCH, the differences is on the 
# specification of the models
spec.ggg = gogarchspec(mean.model = list(model = "AR", lag = 1),distribution.model = "manig",ica = "fastica")
#Here we fit the Go-GARCH model
gog_fit=gogarchfit(spec.ggg, data=ret, solver='hybrid', dfun='tanh', maxiter1=40000, epsilon=1e-08, rseed=100)
gog_fit
res_gog=residuals(gog_fit)

condcor_gog=rcor(gog_fit)#Conditional correlation
#Ploting estimated evolutions of the pairwise condional correllation for Go-GARCH 
par(mfrow = c(3, 2))
for (i in 1:5){
  for (j in 1:5 ){
    if (j<i){
      plot(condcor_gog[i,j,], type = "l",col="blue",main=c(y[i],y[j]))
    }
  }
  
}

#Here we specify the GO-GARCH model and the risk metric we are using
#We basical defining the type of model we are using to optimize our portfolio
specgo = parmaspec(scenario = ret, forecast = colMeans(ret), risk='CVaR', target = mean(colMeans(ret)),targetType = 'equality', riskType = 'minrisk', LB = rep(0,5), UB = rep(1,5), budget = 1,model="gogarch",spec=spec.ggn)
gog_sol_lp = parmasolve(spec, type = "LP")# using Linear programing
gog_sol_nlp = parmasolve(spec, type = "NLP")#using nonlinear convex programming
#printing the weights of our portfolio
weights(gog_sol_lp)#weights corresponding to linear programming
weights(gog_sol_nlp)#weights corresponding to non-linear programming

#printing the Conditional Value at Risk corresponding to GO-GARCH-Model 
#sovled by linear and Non-linear programing techniques
parmarisk(gog_sol_lp)# the portfolio CVaR in %
parmarisk(gog_sol_nlp)# the portfolio CVaR in %


#printing the expected returns corresponding to GO-GARCH-Model 
#sovled by linear and Non-linear programing techniques
parmareward(gog_sol_lp)
parmareward(gog_sol_nlp)

#calculating Sharp ratio

gog_sr_lp =parmareward(gog_sol_lp)/parmarisk(gog_sol_lp)
gog_sr_nlp =parmareward(gog_sol_nlp)/parmarisk(gog_sol_nlp)
gog_sr_lp
gog_sr_nlp

#Model Diagnostics
#Diebold-Mariano Test, 
#We compare the forecast accuracy of two forecast methods 
# model comparison: dcc against go garch
dm.test(residuals(dcc_fit),residuals(gog_fit),alternative="two.sided", h=1, power=2)
dm.test(residuals(dcc_fit),residuals(gog_fit),alternative="greater", h=1, power=2)
dm.test(residuals(dcc_fit),residuals(gog_fit),alternative="less", h=1, power=2)
  