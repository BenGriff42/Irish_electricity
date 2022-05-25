# start with 7 am (tod: 16)

extra7 <- extra %>% filter(tod == 16, )
indCons7 = indCons[1,]

for(i in 2:nrow(extra7)){
  indCons7[i,] = indCons[16 + (i-1)*48,]
}

plot(rowMeans(indCons7),type="l")

indCons7mean = rowMeans(indCons7)

extra7$demandMean = indCons7mean

fitMean = gam(demandMean ~ s(toy) + dow + s(temp), data = extra7)
summary(fitMean)


# make copy of extra
dat = extra
# add in a column for smoothtemp - defined above
dat$smoothtemp = smoothtemp

# turn dat into a dataframe and attach indCons to it
dat = data.frame(dat,indCons)

n =nrow(dat)
# this adds a new column for each household
# the column added is the lag48 value of the energy usage
for (i in 1:ncol(indCons)){
  vec.name = paste0(colnames(dat)[i+8], "lag48")
  colname = colnames(dat)[i+8]
  coltoadd <- dat[1:(n-48), colname]
  dat[,vec.name] <- NA
  dat[49:n, vec.name] <- coltoadd
}

# this adds a new column for each household
# the column added is the lag336 value of the energy usage
for (i in 1:ncol(indCons)){
  vec.name = paste0(colnames(dat)[i+8], "lag336")
  colname = colnames(dat)[i+8]
  coltoadd <- dat[1:(n-336), colname]
  dat[,vec.name] <- NA
  dat[337:n, vec.name] <- coltoadd
}

# for one household (I1002) fit a GAM
fit1 = gam(I1002 ~ dow + s(tod) + s(smoothtemp) + s(I1002lag48) + s(I1002lag336), data = dat)
plot(fit1)
summary(fit1)

# for one household (I1002) fit a GAMLSS
fit2 = gam(list(I1002 ~ dow + s(tod) + s(smoothtemp) + s(I1002lag48) + s(I1002lag336), ~ dow + s(tod)), data = dat, family = "gaulss")
plot(fit2)
summary(fit2)



######

# make a common gam over sum of energy usage



# make copy of extra
com_dat = extra
# add in a column for smoothtemp - defined above
com_dat$smoothtemp = smoothtemp

com_dat$totDem = rowSums(indCons)


# this adds a new column
# the column added is the lag48 value of the energy usage
vec.name = "totDemlag48"
coltoadd <- com_dat[1:(n-48), "totDem"]
com_dat[,vec.name] <- NA
com_dat[49:n, vec.name] <- coltoadd


# this adds a new column 
# the column added is the lag336 value of the energy usage
vec.name = "totDemlag336"
coltoadd <- com_dat[1:(n-336), "totDem"]
com_dat[,vec.name] <- NA
com_dat[337:n, vec.name] <- coltoadd

plot(com_dat$totDem,type="l")


# fit week by week and predict next week
fit_gam = gam(totDem ~ dow + s(tod) + s(smoothtemp) + s(totDemlag48) + s(totDemlag336), data = com_dat[1:(5*336),],family=gaussian(link = "log"))
predz = predict(fit_gam,newdata = com_dat[(5*336+1):(6*336),])

for(week in 7:51){
  fit_gam = gam(totDem ~ dow + s(tod) + s(smoothtemp) + s(totDemlag48) + s(totDemlag336), data = com_dat[1:((week-1)*336),],family=gaussian(link = "log"))
  predz = c(predz,predict(fit_gam,newdata = com_dat[((week-1)*336+1):(week*336),]))
  print(week)
}

plot(com_dat$totDem,type="l")
lines((5*336+1):(51*336),exp(predz),type="l",col=2)

week = 20
plot(com_dat$totDem[(week*336+1):((week+1)*336)],type="l")
lines(1:336,exp(predz)[((week-5)*336+1):((week-4)*336)],type="l",col=2)

# residuals
plot((com_dat$totDem[(5*336+1):(51*336)] - exp(predz)),type="l")
















