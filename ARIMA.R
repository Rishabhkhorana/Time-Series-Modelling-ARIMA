#### this Script is created by Rishabh khorana###

######22768807###########################



rm(list=ls(all=TRUE))  
graphics.off();

library(tseries)
library(forecast)
library(tseries)
library(forecast)
library(sandwich)      # compute the variance for Heteroskedastic and/or Autocorrelated Residuals
library(lmtest)        # confidence intervals with heteroskedastic variance
library(huxtable)


setwd('D:/Masters Of Business analytics/BUSN5002(BUSINESS ANAYLTICS)/STUDY MATERIAL/Final_exam')

inflation_data = read.csv("inflation_finished_goods.csv",header= TRUE)


#### converting this data into time series data ####
inflation_data.ts =ts(inflation_data,start=c(1960,2) , end=c(2008,2), frequency=4)
inflation.ts = inflation_data.ts[,c('Finished')]
inflation.diff = diff(inflation.ts, lag = 2)


#### creating the plots for the time series data  ####

par(mfcol = c(2,1))
plot(inflation.ts)
abline(0,0 ,col ="red")
abline(h=mean(inflation.ts),col = 'blue')

plot(inflation.diff)
abline(0,0)


### question a ####
#### creating a plot for autocorrelation factor  and Partial autocorrelation factor we get###
par(mfcol = c(2,1))
acf_model = Acf(inflation.ts,  type = c("correlation"), ci = 0.95, main= "ACF inflation spread", xlab= "Number of Quaters", lag.max = 40)
pacf_model =  Acf(inflation.ts,  type = c("partial"), ci = 0.95, main= "PACF inflation spread",xlab= "Number of Quaters", lag.max = 40)

acf_model
pacf_model



##### question b ####

#### creating the model for the above data#####
AR3= arima(inflation.ts, order=c(3,0,0), include.mean = TRUE, method = "ML") # AR(3,0,0)
AR3
coeftest(AR3)
confint(AR3)
print(Box.test(residuals(AR3),lag=8,type="Ljung-Box" ,fitdf=4)$p.value, digits=3)
print(Box.test(residuals(AR3),lag=12,type="Ljung-Box" ,fitdf=4)$p.value, digits=3)

#### checking the model is stationary for the arma to work#### for that the value of the|Z| > 1
abs(polyroot(c(1,-coeftest(AR3)[1:3]))) # all are above 1, stationary condition is satisfied
par(mfcol=c(1,1)) 
plot(AR3) 
### since all the values of coefficient are in the circle and then model is stationary.



### conditions for the arma model that Wt the white noise should be iid

#####Conducting a Ljung box test to find the independence between the variables ####
# H0: the correlation of the residuals is equal to zero
## Ha: THe correlation between he residuals is not equal to zero.
Box.test(residuals(AR3),lag=4,type="Ljung-Box" ,fitdf=0)
 

### testing the model with value of the lags###
for (i in c(5,7,9)){
  print(Box.test(residuals(AR3),lag=i,type="Ljung-Box" ,fitdf=0)$p.value, digits=3)
}


### we have to care of df and  the 1st parameter > df
for (i in c(5,7,9)){
  print(Box.test(residuals(AR3),lag=i,type="Ljung-Box" ,fitdf=4)$p.value, digits=3)
}

for (i in c(8,10,12)){
  print(Box.test(residuals(AR3),lag=i,type="Ljung-Box" ,fitdf=4)$p.value, digits=3)
}
### hence we can conclude that there is no auto correlation between the residuals ###



### question b and c
#### comparing the model to the other model### as the 13 lag is significant ###
AR13= arima(inflation.ts, order=c(13,0,0), include.mean = TRUE, method = "ML")
print(Box.test(residuals(AR13),lag=30,type="Ljung-Box" ,fitdf=14)$p.value, digits=3)


ARMA31= arima(inflation.ts, order=c(3,0,1), include.mean = TRUE, method = "ML") # AR(3,0,0)
ARMA31
coeftest(ARMA31)
confint(ARMA31)
print(Box.test(residuals(ARMA31),lag=8,type="Ljung-Box" ,fitdf=5)$p.value, digits=3)
print(Box.test(residuals(ARMA31),lag=12,type="Ljung-Box" ,fitdf=5)$p.value, digits=3)






ARMA11  = arima(inflation.ts, order=c(1,0,1), include.mean = TRUE, method = "ML") 
coeftest(ARMA11)
print(Box.test(residuals(ARMA11),lag=8,type="Ljung-Box" ,fitdf=3)$p.value, digits=3)
print(Box.test(residuals(ARMA11),lag=12,type="Ljung-Box" ,fitdf=3)$p.value, digits=3)
## question d
AIC(ARMA11, ARMA31, AR3)
BIC(ARMA11, ARMA31, AR3)
### WE HAVE CREAETD TWO MODELS HERE AR([1,3]) AND ARMA(2,[1,3])

rest.AR3   = c(NA,0,NA,NA)
rest.ARMA23 = c(NA,NA,NA,0,NA,NA)
AR3.r   = arima(inflation.ts, order=c(3,0,0), include.mean = TRUE, method = "ML",fixed = rest.AR3)   # AR([1,3])
ARMA11  = arima(inflation.ts, order=c(1,0,1), include.mean = TRUE, method = "ML")                    # ARMA(1,1)
ARMA21  = arima(inflation.ts, order=c(2,0,1), include.mean = TRUE, method = "ML")                    # ARMA(2,1)
ARMA23.r= arima(inflation.ts, order=c(2,0,3), include.mean = TRUE, method = "ML",fixed = rest.ARMA23)# ARMA(2,[1,3])
auto    = auto.arima(inflation.ts, trace=TRUE, max.p =8, max.q=8)                                    

AIC(AR3, AR13 , AR3.r, ARMA11, ARMA21, ARMA23.r, auto)
BIC(AR3, AR13 , AR3.r, ARMA11, ARMA21, ARMA23.r, auto)


#Print the fitted models 
huxreg("AR(3)"    = coeftest(AR3)   , "AR(13)"    = coeftest(AR13)   , "AR[1,3]"  = coeftest(AR3.r), 
       "ARMA(1,1)"= coeftest(ARMA11), "ARMA(2,1)"= coeftest(ARMA21), "ARMA(2,[1,3])"= coeftest(ARMA23.r), "auto" =  coeftest(auto) ,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       statistics = c(N = "nobs", "logLik", "AIC", BIC="BIC"))





list.arma = list(AR3, AR13, AR3.r, ARMA11, ARMA21, ARMA23.r, auto)
res.arma  = sapply(list.arma, residuals)
res.arma  = matrix(res.arma,ncol=7)
df.fit    = c(4,14,3,3,4,5,5) 

# Computing the Ljung-Box tests and saving their p-values
box.pvalues = matrix(NA,nrow=3, ncol=7)
for(j in 1:7){
  dfh = df.fit[j]
  m=0
  for (i in c(14,15,16)){
    m= m+1
    box.pvalues[m,j] = Box.test(res.arma[,j],lag=i,type="Ljung-Box", fitdf=dfh)$p.value
  }
}
colnames(box.pvalues) = c("AR(3)", " AR(13)","AR[1,3]", "ARMA(1,1)","ARMA(2,1)" ,"ARMA(2,[1,3])", "auto" ) 
rownames(box.pvalues) = c("p-value Q(14)", "p-value Q(15)", "p-value Q(16)")
print(box.pvalues, digits = 3)


### out of all the other models we can see that AR(3) and AR[1,3] are the best fitting models where as the other models  the auto  model is has a higher aic value



# Studying the forecast properties
par(mfcol=c(3,1)) 
plot(forecast(auto, 20))
plot(forecast(AR3.r, 20))
plot(forecast(AR3,20))



# Comparing forecast performances: forecast vs true realization
spread.1 = window(inflation.ts, end  =c(2006,1))
spread.2 = window(inflation.ts, start=c(2006,2), end=c(2008,2))

auto.s    = arima(spread.1, order=c(0,0,2), include.mean = TRUE, method = "ML")                     # ARMA(1,0,3)
AR3.r.s   = arima(spread.1, order=c(3,0,0), include.mean = TRUE, method = "ML",fixed = rest.AR3)   # AR([1,3])
AR3.s= arima(spread.1, order=c(3,0,0), include.mean = TRUE, method = "ML")
par(mfcol=c(3,1)) 
plot(forecast(auto.s, 20))   #### predicting  5 years ahead in the future
lines(spread.2)
plot(forecast(AR3.r.s, 20))
lines(spread.2)
plot(forecast(AR3.s, 20))
lines(spread.2)

