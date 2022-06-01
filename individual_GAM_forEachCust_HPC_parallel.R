###################################
### READ IN DATA
###################################

library(dplyr)
library(mgcv)
library(doParallel)

load("Irish.RData")

indCons = Irish$indCons
extra = Irish$extra

rm("Irish")


###################################

###################################

# smoothtemp takes in the previous timepoints smoothtemp and the current timepoints
# temp to calculate smoothtemp
smoothtemp = vector()
smoothtemp[1] = extra$temp[1]
alpha = 0.9

for(i in 2:nrow(extra)){
  smoothtemp[i] = alpha*smoothtemp[i-1] + (1-alpha)*extra$temp[i]
}

# make copy of extra
samp = extra
# remove extra - won't use it again 
rm("extra")

# add in a column for smoothtemp - defined above
samp$smoothtemp = smoothtemp

# turn samp into a dataframe and attach indCons to it
ncust = ncol(indCons) # change to ncol(indCons) for HPC
samp = data.frame(samp,indCons[,1:ncust])

cust_names = colnames(indCons)

n =nrow(samp)


########################################################################
### USING LOG(X + 0.001) AS THE RESPONSE IN THE MODEL
########################################################################

logcust_names = paste0(cust_names, "log")

## ADD IN A LOG COLUMN FOR EACH CUSTOMER
# this adds a new column for each household which is log(usage + 0.001)
for (i in 1:ncust){
  vec.name = paste0(colnames(samp)[i+8], "log")
  colname = colnames(samp)[i+8]
  coltoadd <- log( samp[1:n, colname] + 0.001 )
  samp[1:n,vec.name] <- coltoadd
}

# make a new dataframe for the log(x+0.001) data
logsamp = samp[1:n,c((1:8), (ncust+9):((2*ncust)+8)) ]

rm("samp")

# this adds a new column for each household
# the column added is the lag48 value of the log(energy usage+0.001)
for (i in 1:ncust){
  vec.name = paste0(colnames(logsamp)[i+8], "lag48")
  colname = colnames(logsamp)[i+8]
  coltoadd <- logsamp[1:(n-48), colname]
  logsamp[,vec.name] <- NA
  logsamp[49:n, vec.name] <- coltoadd
}

# this adds a new column for each household
# the column added is the lag336 value of the log(energy usage + 0.001)
for (i in 1:ncust){
  vec.name = paste0(colnames(logsamp)[i+8], "lag336")
  colname = colnames(logsamp)[i+8]
  coltoadd <- logsamp[1:(n-336), colname]
  logsamp[,vec.name] <- NA
  logsamp[337:n, vec.name] <- coltoadd
}

######################################
### USE A FUNCTION 
######################################

# write a function for each customer for the inner for loop
# train and test logsamp
each_cust_function <- function(cust){
logsamp_train = logsamp[1:((week-1)*336) , c(1:6,8,c(8,8+ncust,8+2*ncust)+cust)]
names(logsamp_train)[8] <- "logdem"
names(logsamp_train)[9] <- "logdemlag48"
names(logsamp_train)[10] <- "logdemlag336"
logsamp_test = logsamp[((week-1)*336+1):(week*336) , c(1:6,8,c(8,8+ncust,8+2*ncust)+cust)]
names(logsamp_test)[8] <- "logdem"
names(logsamp_test)[9] <- "logdemlag48"
names(logsamp_test)[10] <- "logdemlag336"

# specify customer column from logsamp to fit using cust_names[cust]
fit_gam = bam(logdem ~ 
                dow + s(tod) + s(smoothtemp) + s(logdemlag48)
              + s(logdemlag336), data = logsamp_train,
              family=gaussian(),
              discrete=TRUE)

#predz[,cust] = exp(predict(fit_gam,newdata = logsamp_test))
#return(exp(predict(fit_gam,newdata = logsamp_test)))
return(data.frame(name=logcust_names[cust], t(as.matrix(exp(predict(fit_gam,newdata = logsamp_test))))))
}


################################
### MAKE PARALLEL
################################

num_cores <- detectCores()
registerDoParallel(num_cores)

# outer for loop over each week
predz = indCons[1:336, 1:ncust]*0
rownames(predz) <- NULL

all_predz_df = logsamp[1:2, 9:(ncust+8)]*0  # empty dataframe to fill for weeks 6-51 predictions
rownames(all_predz_df) <- NULL

# fit week by week and predict
for(week in 6:50){
  predz = foreach(cust = 1:ncust, .combine = "rbind") %dopar% {
    each_cust_function(cust)
  }
  
  predz.order = t(predz[which(predz[,1] ==logcust_names[1:ncust]),-1])
  rownames(predz.order) = NULL
  colnames(predz.order) = logcust_names[1:ncust]
  
  predz.df = predz.order
  
  all_predz_df = rbind(all_predz_df, predz.df)
  
  print(week)
}

print("exit loop")
stopImplicitCluster()

# OUT OF FOR LOOP
all_predz_df = all_predz_df[-(1:2),] # remove first 2 rows of zeros

# round to  3d.p.
all_predz_df = all_predz_df %>%
  mutate_if(is.numeric, round, digits=3)

write.table(all_predz_df, file ="IndGAM_log_allPredz_parallel_week40.txt")
