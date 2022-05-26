predictions = read.table("IndGAM_log_allPredz_parallel.txt")

library(dplyr)

predictions3dp = predictions %>%
  mutate_if(is.numeric, round, digits=3)


# remove NA rows for df

predictions3dp = predictions3dp[-((nrow(predictions3dp)-336):nrow(predictions3dp)),]
# save as txt file
write.table(predictions3dp, file ="IndGAM_log_allPredz_parallel_3dp.txt")

plot(predictions3dp[1:48, 2], type="l")
lines(indCons[1:48, 2], col=2)

ncust = ncol(predictions3dp)
entire_avg = rowSums(predictions3dp)/ncust

plot(entire_avg[1:48], type="l", ylim=c(0, 3))
for (i in 2:(nrow(predictions3dp)/48)){
  lines(entire_avg[((i-1)*48 +1):(i*48)])
}

# EXPLOREE WEIRD THINGS IN PLOTS

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



