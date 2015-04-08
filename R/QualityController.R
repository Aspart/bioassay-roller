# Mean, SD, RSD for each point of raw data ---------
raw.QC = function(df) {
  point.mean = unlist(lapply(split(df, f=df[,'dose']), function(x) mean(x$response)))
  point.sd = unlist(lapply(split(df, f=df[,'dose']), function(x) sd(x$response)))
  point.rsd = round(point.sd/point.mean*100,2)
  rbind(dose = colnames(point.mean), mean = point.mean, sd = point.sd, 'RSD, %' = point.rsd)
}

# R coefficient -------------------------------------
r.squared = function(model) {
  num = sum(residuals(model)^2)
  denum = sum((model$origData$response - mean(model$origData$response))^2)
  1 - num/denum
}

# QC section -----------------------------------
model.QC = function(model) {
  coef = coef(model)
  c('A/D' = as.vector(coef[3]/coef[2]), R = r.squared(model))
}
