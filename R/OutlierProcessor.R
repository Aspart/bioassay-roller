# Remove outliers from df provided --------
outlier.removal = function(df) {
  res = lapply(split(df, df$dose), function(x) {
    outliers = ESD.test(x$response, max=3)
    x = x[!outliers,]
  })
  new.df = do.call(rbind, res)
  rownames(new.df) = NULL
  new.df
}


ESD.test = function(x, max, alpha=0.05) {
  n <- length(x)
  result = rep(F, n)
  subx <- data.frame(row=as.integer(rownames(as.data.frame(x))), x)
  outliers = data.frame()
  for(i in 1:max) {
    r <- abs(subx$x - mean(subx$x))/sd(subx$x)
    p <- 1 - alpha/(2*(n-i+1))
    t <- qt(p, (n-i-1))
    l <- t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
    outliers <- as.data.frame(rbind(outliers, list(row=subx[which.max(r),'row'], r=max(r), l=l)))
    if(max(r)>l) {
      result[subx[which.max(r), 'row']] <- T
    }
    subx <- as.data.frame(subx[-which.max(r),])
  }
  result
}
