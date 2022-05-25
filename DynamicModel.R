# make a model that fits on the last 3 days of data

# make copy of extra
samp = extra
# add in a column for smoothtemp - defined above
samp$smoothtemp = smoothtemp

# turn samp into a dataframe and attach indCons to it
samp = data.frame(samp,indCons[,1:100])

n =nrow(samp)


# fit using 3 days data and predict the next day 
# 3*48 is first 3 days 
# remove dow as it doesn't work in this short term prediction model 
fit_gam = gam(I1002 ~  s(tod) + s(smoothtemp), data = samp[1:(3*48),],family=gaussian(link = "log"))
# predict for next day, take the exp as we make the log predictions
predz = exp(predict(fit_gam,newdata = samp[(3*48+1):(4*48),]))
# find the residuals from the prediction (true - prediction)
residz = samp[(3*48+1):(4*48),]$I1002 - predz

# see how good the fit is
plot(residz,type="l")
abline(h=0, col="red")

####################
# fit an ARIMA model to the half hourly consumption, using the previous 3 days data to predict the next day 
####################
set.seed(1)
# plot of the first 4 days of data for I1002
plot(samp[1:(4*48),]$I1002, type="l")
# ARIMA(p=3*48,d=0,q=48)
arima_model  = arima(samp[1:(7*48) ,"I1002"], order = c((2*48),0, 48))




## THIS NO WORKY

for(week in 7:51){
  fit_gam = gam(I1002 ~ dow + s(tod) + s(smoothtemp) + s(I1002lag48) + s(I1002lag336), data = samp[1:((week-1)*336),],family=gaussian(link = "log"))
  predz = c(predz,predict(fit_gam,newdata = samp[((week-1)*336+1):(week*336),]))
}

plot(samp$I1002,type="l")
lines((5*336+1):(51*336),exp(predz),type="l",col=2)
plot((5*336+1):(51*336),exp(predz),type="l",col=2)







