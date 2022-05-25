library(dplyr)
library(mgcv)
setwd("~/Documents/Group_project_2")
load("Irish.RData")

indCons = Irish$indCons
survey = Irish$survey
extra = Irish$extra

##########

# smoothtemp takes in the previous timepoints smoothtemp and the current timepoints
# temp to calculate smoothtemp
smoothtemp = vector()
smoothtemp[1] = extra$temp[1]
alpha = 0.9

for(i in 2:nrow(extra)){
  smoothtemp[i] = alpha*smoothtemp[i-1] + (1-alpha)*extra$temp[i]
}

plot(extra$temp,pch=".")
lines(smoothtemp)