library(dplyr)

# read in file from HPC that is a table containing all the predictions for the common GAM
commonpreds = read.table("/home/uj21900/Documents/SM2SC2GroupCW/Irish_electricity/commonGAM_log_predz.txt")


# Checks for bad prediction values

large = as.vector(which(entire_avg>2))
large # no time point with an extremely large average prediction - good :)
max = max(commonpreds)
max # seems reasonable

# PLOTS FOR PREDICTIONS
ncust = ncol(commonpreds)
entire_avg = rowSums(commonpreds)/ncust

plot(entire_avg[1:48], type="l", ylim=c(0, 1.5), 
     col = (rgb(red = 0, green = 0, blue = 1,alpha=0.1)),
     main="Mean predicted consumption per day\n (common GAM)", 
     xlab = "time (half hour intervals of day)", ylab="mean prediction")
for (i in 2:(nrow(commonpreds)/48)){
  lines(entire_avg[((i-1)*48 +1):(i*48)], col = (rgb(red = 0, green = 0, blue = 1,alpha=0.1)))
}


# LOAD IN DATA FOR OBSERVED VALUES
load("Irish.RData")
indCons = Irish$indCons
rm("Irish")

# remove first 5 weeks as we didn't predict for these
indCons_modified = indCons[-c(1:(5*336)) , ]
# only predicted for 35 weeks, i.e. weeks 5-40
indCons40 = indCons_modified[1:(35*336),]

entire_avg_cons = rowSums(indCons40)/ncust

# PLOTS FOR TRUE VALUES
plot(entire_avg_cons[1:48], type="l", ylim=c(0, 1.5), 
     col = (rgb(red = 0, green = 0, blue = 1,alpha=0.1)), 
     main="Mean observed consumption per day\n (weeks 5-40)", 
     xlab = "time (half hour intervals of day)", ylab="mean consumption")
for (i in 2:(nrow(indCons)/48)){
  lines(entire_avg_cons[((i-1)*48 +1):(i*48)], col = (rgb(red = 0, green = 0, blue = 1,alpha=0.1)))
}


#### CALCULATE THE MSE OF THE PREDICTIONS
# want to check the out of sample (test data) model fit goodness

# find standard errors on predictions for weeks 5-40
matrix_SE = (indCons40 - commonpreds)^2

halfhourly_MSE = rowMeans(matrix_SE) # 11760 values
plot(halfhourly_MSE, type="l")
which(halfhourly_MSE>50) # this and the plot show no large values - MSE looks good

wks = floor((length(halfhourly_MSE)/336)) # or ceiling

weekly_MSE = rep(0,wks)
for (i in 1:wks){
  weekly_MSE[i] = mean(halfhourly_MSE[((i-1)*336 +1):(i*336)])
}

plot(6:40, weekly_MSE, type="l", xlab="week",
     ylab = "weekly averaged MSE",
     main = "Average MSE over all customers per week, where\n predictions are made using the common GAM")
# this plot looks good


# FIND CUSTOMERS WITH A HIGH MSE
cust_MSE = colMeans(matrix_SE)
plot(cust_MSE, type="l", ylim=c(0,3), 
     main="MSE per customer (for common GAM)", ylab="Customer MSE")
colNum_badcust = which(cust_MSE>2)# I4745 I4769
# find the IDs of these customers
bad_MSE = cust_MSE[sapply(cust_MSE, function(x) any(x>2))]
bad_MSE_names = names(bad_MSE)
# decided that these MSE were not too bad, so have left in the data