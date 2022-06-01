###################################
### READ IN DATA
###################################

library(dplyr)
library(mgcv)
library(doParallel)

load("Irish.RData")

indCons = Irish$indCons
extra = Irish$extra
survey = Irish$survey
###################################
n = nrow(extra) # number of observation points
s = ncol(indCons) # number of customers

# smoothtemp takes in the previous timepoints smoothtemp and the current timepoints
# temp to calculate smoothtemp
smoothtemp = vector()
smoothtemp[1] = extra$temp[1]
alpha = 0.9

for(i in 2:nrow(extra)){
  smoothtemp[i] = alpha*smoothtemp[i-1] + (1-alpha)*extra$temp[i]
}


# put all customers in data frame with survey data
samp2 = data.frame(time = rep(extra$time,s))
samp2$toy = rep(extra$toy,s)
samp2$dow = factor(rep(extra$dow,s))
samp2$tod = rep(extra$tod,s)
samp2$smoothtemp = rep(smoothtemp,s)
samp2$ID = rep(survey$ID, each=n)
samp2$sc = factor(rep(survey$SOCIALCLASS, each=n))
samp2$own = factor(rep(survey$OWNERSHIP, each=n))
samp2$hw = factor(rep(survey$HEAT.WATER, each=n))
samp2$wg = factor(rep(survey$HOME.APPLIANCE..White.goods., each=n))
samp2$logcons = as.vector(as.matrix(log(indCons +0.001))) # using log(x+0.001) in our model 

# add mean of last week
meanWeek = matrix(NA,nrow = n, ncol=s)
for(i in 337:n){
  meanWeek[i,] = colMeans(indCons[(i-336):i,])
}
samp2$meanWeek = as.vector(meanWeek)


#### big run over weeks
week = 6

# take data for the first number of weeks specified by "week" for the s customers in the sample inm the train dataset
samp_train = samp2[rep(1:(week*336), times = s) + n*rep(0:(s-1),each = week*336),]
samp_test = samp2[rep((week*336+1):((week+1)*336),times = s) + n*rep(0:(s-1),each = 336),]

num_cores <- detectCores()

fit_com = bam(logcons ~ dow + sc + own + hw + wg + s(tod) 
                   + s(smoothtemp) + s(meanWeek),
              data = samp_train,
              family=gaussian(), 
              discrete=TRUE,
              nthreads = num_cores)

predz = exp(predict(fit_com,newdata = samp_test))

predz = round(predz, 3) # round to 3dp 

#predz = predz %>%
 # mutate_if(is.numeric, round, digits=3) # round to 3dp  -changed as crashed HPC script 30/05

predM = matrix(predz, ncol=s, nrow = 336,byrow = F)
#write.table(matrix(predz, ncol=s, nrow = 336,byrow = F), file = paste0("week",week,".txt"))
rm("extra","indCons","Irish", "survey")




for(week in 7:40){
  samp_train = samp2[rep(1:(week*336), times = s) + n*rep(0:(s-1),each = week*336),]
  samp_test = samp2[rep((week*336+1):((week+1)*336),times = s) + n*rep(0:(s-1),each = 336),]

  fit_com = bam(logcons ~ dow + sc + own + hw + wg + s(tod)
                + s(smoothtemp) + s(meanWeek),
                data = samp_train,
                family=gaussian(),
                discrete=TRUE,
                nthreads = num_cores)

  predz = exp(predict(fit_com,newdata = samp_test))
  
  predz = round(predz,3)
 # predz = predz %>%
   # mutate_if(is.numeric, round, digits=3) # round to 3dp

  predM = rbind(predM, matrix(predz, ncol=s, nrow = 336,byrow = F))
  #write.table(matrix(predz, ncol=s, nrow = 336,byrow = F), file = paste0("week",week,".txt"))

  print(week)
}

print("exit loop")

# OUT OF FOR LOOP
write.table(predM, file ="commonGAM_log_predz.txt")
