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

sample.4LP = function(B, A, D, C, e, rp = 1, rep=3) {
  # concentration params
  #z = 150 / (2^(0:9)) # doses
  z = c(100, 5, 2, 1, 0.5 , 0.25 , 0.1 , 0.05, 0.01, 0.001)
  # samples
  y = D + (A-D)/(1+(rp*z/C)^B)
  data = sapply(y, myrnorm, sd=e, n=rep)
  colnames(data) = z
  melt.df = melt(data)[,2:3]
  colnames(melt.df) = c("dose", "responce")
  melt.df
}

dose.mean = function(melt.df) {
  # calculate mean for each concentration point
  cn = colnames(melt.df)
  splitted <- ddply(melt.df, c(cn[1], cn[2]), summarise, mean = round(mean(cn[2]), 2))
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
# B - slope parameter
# D - lower asymptotes
# A - upper asymptotes
# C - EC50
# error TODO: reference SD = 2 * test SD

A = 43535
B = 1.024
C = 0.61946
D = 22671
e = 967.19

data = sample.4LP(B, A, D, C, e)

model = drm(responce~dose, data = data, fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")))
plot(model)
summary(model)