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
