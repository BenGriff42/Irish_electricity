####### 24/05


# make copy of extra
samp = extra
# add in a column for smoothtemp - defined above
samp$smoothtemp = smoothtemp

# turn samp into a dataframe and attach indCons to it
samp = data.frame(samp,indCons[,1:100])

n =nrow(samp)
# this adds a new column for each household
# the column added is the lag48 value of the energy usage
for (i in 1:100){
  vec.name = paste0(colnames(samp)[i+8], "lag48")
  colname = colnames(samp)[i+8]
  coltoadd <- samp[1:(n-48), colname]
  samp[,vec.name] <- NA
  samp[49:n, vec.name] <- coltoadd
}

# this adds a new column for each household
# the column added is the lag336 value of the energy usage
for (i in 1:100){
  vec.name = paste0(colnames(samp)[i+8], "lag336")
  colname = colnames(samp)[i+8]
  coltoadd <- samp[1:(n-336), colname]
  samp[,vec.name] <- NA
  samp[337:n, vec.name] <- coltoadd
}

# fit week by week and predict
fit_gam = gam(I1002 ~ dow + s(tod) + s(smoothtemp) + s(I1002lag48) + s(I1002lag336), 
              data = samp[1:(5*336),],family=gaussian(link = "log"))
predz = predict(fit_gam,newdata = samp[(5*336+1):(6*336),])

for(week in 7:51){
  fit_gam = gam(I1002 ~ dow + s(tod) + s(smoothtemp) + s(I1002lag48) + s(I1002lag336), 
                data = samp[1:((week-1)*336),],family=gaussian(link = "log"))
  predz = c(predz,predict(fit_gam,newdata = samp[((week-1)*336+1):(week*336),]))
}

plot(samp$I1002,type="l")
lines((5*336+1):(51*336),exp(predz),type="l",col=2)
plot((5*336+1):(51*336),exp(predz),type="l",col=2)

