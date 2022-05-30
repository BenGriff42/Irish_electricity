#predictions = read.table("IndGAM_log_allPredz_parallel.txt")

library(dplyr)

predictions3dp = predictions %>%
  mutate_if(is.numeric, round, digits=3)

predictions3dp = read.table("IndGAM_log_allPredz_parallel_3dp.txt")

 # remove NA rows for df
predictions3dp = predictions3dp[-((nrow(predictions3dp)-336):nrow(predictions3dp)),]
# save as txt file
#write.table(predictions3dp, file ="IndGAM_log_allPredz_parallel_3dp.txt")

plot(predictions3dp[1:48, 2], type="l")
lines(indCons[1:48, 2], col=2)

ncust = ncol(predictions3dp)
entire_avg = rowSums(predictions3dp)/ncust

plot(entire_avg[1:48], type="l", ylim=c(0, 3))
for (i in 2:(nrow(predictions3dp)/48)){
  lines(entire_avg[((i-1)*48 +1):(i*48)])
}

# EXPLORE WEIRD THINGS IN PLOTS

# some really massive spikes
large= as.vector(which(entire_avg>2))
large# = [2295 2296 5833 5834 ]
error_areas = predictions3dp[large,]
bad_predz = error_areas[sapply(error_areas, function(x) any(x>5.5))]
bad_predz_names = substring(colnames(bad_predz), 1, nchar(colnames(bad_predz))-3) # remove 'log'
# this shows that I4593 and I5198 have erraneous predictions

odd = indCons[, bad_predz_names] # this is the true data fr the 2 customers with crazy large predictions
plot(odd[,1], type='l', ylim=c(0,10)) # i think this has really large values (relatively), but not the largest
plot(odd[,2], type='l', ylim=c(0,10)) # long low demand periods

# remove these two weird things 
drop = colnames(bad_predz)
predictions3dp_modified = predictions3dp[, !(names(predictions3dp) %in% drop)]


## In 2 side by side plots, have the predicted value for all bar 2 customers vs the true values
# it is plotting the mean value of all the customers demand for each day
par(mfrow = c(1,2))

ncust_mod = ncol(predictions3dp_modified)
entire_avg_mod = rowSums(predictions3dp_modified)/ncust_mod

plot(entire_avg_mod[1:48], type="l", ylim=c(0, 1.5), col = (rgb(red = 1, green = 0, blue = 1,alpha=0.1)))
for (i in 2:(nrow(predictions3dp_modified)/48)){
  lines(entire_avg_mod[((i-1)*48 +1):(i*48)], col = (rgb(red = 1, green = 0, blue = 1,alpha=0.1)))
}

ncust = ncol(indCons)
entire_avg_cons = rowSums(indCons)/ncust

plot(entire_avg_cons[1:48], type="l", ylim=c(0, 1.5), col = (rgb(red = 1, green = 0, blue = 1,alpha=0.1)))
for (i in 2:(nrow(indCons)/48)){
  lines(entire_avg_cons[((i-1)*48 +1):(i*48)], col = (rgb(red = 1, green = 0, blue = 1,alpha=0.1)))
}



#### CALCULATE THE MSE OF THE PREDICTIONS
# want to check the out of sample (test data) model fit goodness

# remove the 2 odd households
indCons_modified = indCons[, !(names(indCons) %in% bad_predz_names)]
# remove first 5 weeks as we did not predict for these
indCons_modified = indCons_modified[-c(1:(5*336)) , ]

matrix_SE = (indCons_modified - predictions3dp_modified)^2

halfhourly_MSE = rowMeans(matrix_SE) # 15119 values
plot(halfhourly_MSE, type="l")
which(halfhourly_MSE>50) # 15 timepoints, 3 lots of consecutive ones, where bad MSE


wks = floor((length(halfhourly_MSE)/336)) # or ceiling

weekly_MSE = rep(0,wks)
for (i in 1:wks){
  weekly_MSE[i] = mean(halfhourly_MSE[((i-1)*336 +1):(i*336)])
}

plot(weekly_MSE, type="l")

which(weekly_MSE>1)
# weeks 12 and 34 have weird MSE

# can we find customers with high MSEs?
cust_MSE = colMeans(matrix_SE)
plot(cust_MSE, type="l", ylim=c(0,2))
which(cust_MSE>2)

# find the IDs of these customers
bad_MSE = cust_MSE[sapply(cust_MSE, function(x) any(x>2))]
bad_MSE_names = names(bad_MSE)
# we tested 1.5 as the limit, and still got 7 bad, so >2 is an appropraite threshold

par(mfrow=c(3,2))
for (i in 1:length(bad_MSE_names)){
plot(indCons[, bad_MSE_names[i]], type="l")
plot(predictions3dp_modified[, paste0(bad_MSE_names[i],"log")], type="l")
}
# this shows that they just have a few dodgy predictions which shoot their MSE up

# 7 bad MSE customers
# let's bin them off to then see how well our model works on a weekly basis with these 
# anomalies removed

####### MSE PLOTS WITH THE BAD GUYS REMOVED
#############################################
# remove these 7 bad
indCons_modified2 = indCons_modified[, !(names(indCons_modified) %in% bad_MSE_names)]
predictions3dp_modified2 = 
  predictions3dp_modified[, !(names(predictions3dp_modified) %in% paste0(bad_MSE_names,"log"))]

matrix_SE_mod = (indCons_modified2 - predictions3dp_modified2)^2

halfhourly_MSE_mod = rowMeans(matrix_SE_mod) # 15119 values
plot(halfhourly_MSE_mod, type="l")


weekly_MSE_mod = rep(0,wks)
for (i in 1:wks){
  weekly_MSE_mod[i] = mean(halfhourly_MSE_mod[((i-1)*336 +1):(i*336)])
}

plot(6:50,weekly_MSE_mod, type="l", xlab="week")











