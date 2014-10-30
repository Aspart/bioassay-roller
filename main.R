library(plyr)
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
sample.4LP = function(A,B,C,D,e, rp = c(0.5, 0.75, 1.25, 1.5)) {
  # concentration params
  doses = 150 / (10^(0:9))
  # samples from each dose
  rep = 3
  # samples
  sample.rp = c(1, rp)
  # and concentracion point labels
  dose.labels = paste('c', c(1:10), sep='')
  dose.labels = as.vector(sapply(dose.labels, function (x) rep(x,rep)))
  # concentration
  y = D + ((A-D)/(1+(C*doses%*%t(sample.rp)/C)^B))
  data = apply(y, 2, sapply, myrnorm, sd=e, n=rep)
  colnames(data) = sample.rp
  data = data.frame(dose = dose.labels, data)
  data
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
A = 73454
B = 1.36
Cref = 7.98
D = 415000
e = 6000 # TODO: reference SD = 2 * test SD

data = sample.4LP(A,B,Cref,D,e)
melt.df <- melt(data, id.var = "dose")
data.mean = dose.mean(melt.df)
data.sd = dose.sd(melt.df)
