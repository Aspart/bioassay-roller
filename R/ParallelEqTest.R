
# Equivalence test----------------------------------------------------------------
# H0: Reference and test are not parallel
# HA: Reference and test are parallel
# Have to obtain param.fold from historic data (Upper, Lower and Slope relative fold)
eq.parallel.test = function(ref.model, test.model, param.fold=c(2,2,1.3), ci.level=0.95) {
  ref.coef = summary(ref.model)$coefficients
  test.coef = summary(test.model)$coefficients
  dif = test.coef[,1]-ref.coef[,1]
  dif.se = apply(cbind(test.coef[,2], ref.coef[,2]), 1, function(x) {
    sqrt(x[1]/sqrt(test.model$sumList$lenData)+x[2]/sqrt(ref.model$sumList$lenData))
  })

  critial.fold = c(1/3, 1.5, 2)
  ci = apply(rbind(mean = dif, sd = dif.se), 2, function(x) {
    c(lower = as.vector(x[1]-qnorm((1+ci.level)/2)*x[2]), upper = as.vector(x[1]+qnorm((1+ci.level)/2)*x[2]))
  })
  tmp = rbind(mean = dif, sd = dif.se, ci)
  # TODO: fix this shit!
}



eq.test.hist.fold = function(file.list, level=0.95) {
  df.list = lapply(file.list, read.ylab.data)
  melt.df.list = lapply(df.list, melt.assay.data)
  models.list = lapply(melt.df.list, drm.single.model)
  coef.df = do.call(rbind, lapply(models.list, coef))
  coef.int = apply(coef.df, 2, nconf.int, level=level)
  coef.int
}

nconf.int = function(x, level=0.95) {
  error <- qnorm((1+level)/2)*sd(x)/sqrt(length(x))
  c(Lower = mean(x)-error, Upper = mean(x)+error)
}
