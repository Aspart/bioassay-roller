library(plyr)
library(gtools)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(drc)

myrnorm = function(mean, sd, n) {
  rnorm(n, mean, sd)
}

######################################
# lets model some dose-response data #
sample.4LP = function(A,B,C,D,e, rp = 1) {
  # concentration params
  z = 150 / (2^(0:9)) # doses
  # samples from each dose
  rep = 3
  # samples
  y = D + (D-A)/(1+(z/C)^B)
  data = sapply(y, myrnorm, sd=e, n=rep)
  colnames(data) = z
  melt.df = melt(data)[,2:3]
  colnames(melt.df) = c("dose", "responce")
  melt.df
}

dose.mean = function(melt.df) {
  # calculate mean for each concentration point
  splitted <- ddply(melt.df, .(dose, variable), summarise, mean = round(mean(value), 2))
  mean = dcast(splitted, dose ~ variable, value.var="mean")
  mean
}

dose.sd = function(melt.df) {
  # calculate mean and sd for each concentration point
  splitted <- ddply(melt.df, .(dose, variable), summarise, sd = round(sd(value), 2))
  sd = dcast(splitted, dose ~ variable, value.var="sd")
  sd
} 

# model params for 4LP from 'Two Approaches to Potency Bioassay Analysis' Harry Yang, Ph.D
B = 1.36 # slope parameter
A = 73454 # lower asymptotes
D = 415000 # upper asymptotes
C = 7.98 # EC50
e = 6000 # TODO: reference SD = 2 * test SD

data = sample.4LP(A,B,C,D,e)

model = drm(responce~dose, data = data, fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")))
plot(model)