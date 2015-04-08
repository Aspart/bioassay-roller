# Remove outliers from df provided --------
outlier.removal = function(df) {
  res = lapply(split(df, df$dose), function(x) {
    outlier_tf = outlier(x$response,logical=TRUE)
    find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
    x = x[-find_outlier,]
  })
  do.call(rbind, res)
}
