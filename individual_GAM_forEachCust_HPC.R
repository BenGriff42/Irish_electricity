###################################
### READ IN DATA
###################################

library(dplyr)
library(mgcv)

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
# this adds a new column for each household
# the column added is the lag48 value of the energy usage
for (i in 1:ncust){
  vec.name = paste0(colnames(samp)[i+8], "lag48")
  colname = colnames(samp)[i+8]
  coltoadd <- samp[1:(n-48), colname]
  samp[,vec.name] <- NA
  samp[49:n, vec.name] <- coltoadd
}

# this adds a new column for each household
# the column added is the lag336 value of the energy usage
for (i in 1:ncust){
  vec.name = paste0(colnames(samp)[i+8], "lag336")
  colname = colnames(samp)[i+8]
  coltoadd <- samp[1:(n-336), colname]
  samp[,vec.name] <- NA
  samp[337:n, vec.name] <- coltoadd
}




# outer for loop over each week
predz = matrix(NA,ncol=ncust,nrow=336)
all_predz_df = indCons[1:2, ]*0 # empty dataframe to fill for weeks 6-51 predictions
rownames(all_predz_df) <- NULL


# remove indCons
rm("indCons")

# fit week by week and predict
for(week in 6:51){
  
  # fit for each customer in ncust
  for(cust in 1:ncust){
    # train and test samp
    samp_train = samp[1:((week-1)*336),c(1:6,8,c(8,8+ncust,8+2*ncust)+cust)]
    names(samp_train)[8] <- "dem"
    names(samp_train)[9] <- "demlag48"
    names(samp_train)[10] <- "demlag336"
    samp_test = samp[((week-1)*336+1):(week*336),c(1:6,8,c(8,8+ncust,8+2*ncust)+cust)]
    names(samp_test)[8] <- "dem"
    names(samp_test)[9] <- "demlag48"
    names(samp_test)[10] <- "demlag336"
    
    # specify customer column from samp to fit using cust_names[cust]
    fit_gam = gam(dem ~ 
                    dow + s(tod) + s(smoothtemp) + s(demlag48)
                  + s(demlag336), data = samp_train,
                  family=gaussian(link = "log"))
    
    predz[,cust] = exp(predict(fit_gam,newdata = samp_test))
  }
  
  predz.df = as.data.frame(predz)
  names(predz.df)[1:ncust] = cust_names[1:ncust]
  
  all_predz_df = rbind(all_predz_df, predz.df)
  #write.table(predz.df, file = paste0("week",week,"Ind.txt"))
  
  print(week)
}

print("exit loop")

# OUT OF FOR LOOP
all_predz_df = all_predz_df[-(1:2),] # remove first 2 rows of zeros
write.table(all_predz_df, file ="IndGAM_allPredz.txt")

