###### common model 2
set.seed(1)

n = nrow(extra)
# number of customers in samp2
s = 20

# randomly sample s customers
indx = sample(1:ncol(indCons), size = s)

# put all customers in data frame with survey data
samp2 = data.frame(time = rep(extra$time,s))
samp2$toy = rep(extra$toy,s)
samp2$dow = factor(rep(extra$dow,s))
samp2$tod = rep(extra$tod,s)
samp2$smoothtemp = rep(smoothtemp,s)
samp2$ID = rep(survey$ID[indx], each=n)
samp2$sc = factor(rep(survey$SOCIALCLASS[indx], each=n))
samp2$own = factor(rep(survey$OWNERSHIP[indx], each=n))
samp2$hw = factor(rep(survey$HEAT.WATER[indx], each=n))
samp2$wg = factor(rep(survey$HOME.APPLIANCE..White.goods.[indx], each=n))
samp2$cons = as.vector(as.matrix(indCons[indx]))

# add mean of last week
meanWeek = matrix(NA,nrow = n, ncol=s)
for(i in 337:n){
  meanWeek[i,] = colMeans(indCons[(i-336):i,indx])
}
samp2$meanWeek = as.vector(meanWeek)

# train on first 10 weeks, test on 11th week
week = 10
samp_train = samp2[1:(week*336),]
samp_test = samp2[(week*336+1):((week+1)*336),]

for(i in 2:s){
  samp_train = rbind(samp_train,samp2[1:(week*336) + (i-1)*n,])
  samp_test = rbind(samp_test,samp2[(week*336+1):((week+1)*336) + (i-1)*n,])
}

######### GAM
# fit log-normal gam on train set
fit_com = gam(cons ~ dow + sc + own + hw + wg + s(tod) +
                s(smoothtemp) + s(meanWeek), data = samp_train,family=gaussian(link = "log"))

summary(fit_com)

predz = rep(0,336)
realz = rep(0,336)
for(i in 1:s){
  predz = predz + exp(predict(fit_com,newdata = samp_test[1:336 + (i-1)*336,]))
  realz = realz + samp_test$cons[1:336 + (i-1)*336]
}
plot(realz,type="l", ylim = c(min(c(predz,realz)),max(c(predz,realz))))
lines(predz,col=2)


######### GAMLSS

# fit log-normal gamlss on train set
fit_com = gam(list(cons ~ dow + sc + own + hw + wg + s(tod) 
                   + s(smoothtemp) + s(meanWeek), ~ dow + s(tod)), data = samp_train,
              family=gaulss(link = list("log","logb")))

summary(fit_com)

predz = rep(0,336)
realz = rep(0,336)
for(i in 1:s){
  predz = predz + exp(predict(fit_com,newdata = samp_test[1:336 + (i-1)*336,]))
  realz = realz + samp_test$cons[1:336 + (i-1)*336]
}
plot(realz,type="l", ylim = c(min(c(predz[,1],realz)),max(c(predz[,1],realz))))
lines(predz[,1],col=2)

logpred1 = predict(fit_com,newdata = samp_test[1:336,])
pred1.mean = exp(logpred1[,1])
real1 = samp_test$cons[1:336]

plot(real1,type="l", ylim = c(min(c(pred1[,1],real1)),max(c(pred1[,1],real1))))
lines(pred1.mean,col=2)
