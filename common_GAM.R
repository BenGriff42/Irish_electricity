###### common model 2
set.seed(1)

n = nrow(extra)
# number of customers in samp2
s = 100

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

############################

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

# sum of predictions over all customers for next week
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



#### big run over weeks

week = 6

# take data for the first number of weeks specified by "week" for the s customers in the sample inm the train dataset
samp_train = samp2[rep(1:(week*336), times = s) + n*rep(0:(s-1),each = week*336),]
samp_test = samp2[rep((week*336+1):((week+1)*336),times = s) + n*rep(0:(s-1),each = 336),]

fit_com = gam(list(cons ~ dow + sc + own + hw + wg + s(tod) 
                   + s(smoothtemp) + s(meanWeek), ~ dow + s(tod)), data = samp_train,
              family=gaulss(link = list("log","logb")))

predz = exp(predict(fit_com,newdata = samp_test)[,1])
#predM = matrix(predz, ncol=s, nrow = 336,byrow = F)
write.table(matrix(predz, ncol=s, nrow = 336,byrow = F), file = paste0("week",week,".txt"))
rm("extra","indCons","Irish", "meanWeek", "predM", "survey", "smoothtemp")


for(week in 7:51){
  samp_train = samp2[rep(1:(week*336), times = s) + n*rep(0:(s-1),each = week*336),]
  samp_test = samp2[rep((week*336+1):((week+1)*336),times = s) + n*rep(0:(s-1),each = 336),]
  
  fit_com = gam(list(cons ~ dow + sc + own + hw + wg + s(tod) 
                     + s(smoothtemp) + s(meanWeek), ~ dow + s(tod)), data = samp_train,
                family=gaulss(link = list("log","logb")))
  
  predz = exp(predict(fit_com,newdata = samp_test)[,1])
  
  #predM = rbind(predM,matrix(predz, ncol=s, nrow = 336,byrow = F))
  write.table(matrix(predz, ncol=s, nrow = 336,byrow = F), file = paste0("week",week,".txt"))
  
  print(week)
}

# plots
plot(samp2$cons[(25*336+1):(28*336)],type="l")
lines(predM[,1],col=2)

predz = rep(0,336)
realz = rep(0,336)
for(i in 1:s){
  predz = predz + predM[,i]
  realz = realz + samp_test$cons[1:336 + (i-1)*336]
}
plot(realz,type="l", ylim = c(min(c(predz,realz)),max(c(predz,realz))))
lines(predz,col=2)

plot((realz - predz),type="l")




















