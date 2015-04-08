# Roxygen comment example ----------------------------
#' Build model for data set with single sample
#'
#' @param df - input data.frame in long format
#' @return The object of class \code{drc}
drm.single.model = function(df) {
  result <- tryCatch({
    mod.normal = LL.4(names=c("Slope, B","Lower Limit, D","Upper Limit, A", "ED50, C"))
    drm(response~dose, data = df, fct = mod.normal, na.action = na.omit)
  }, error=function(e) {
    print(e)
    NULL
  })
  if(is.null(result)) {
    result = lm(response~dose, data=df)
  }
  result
}

#' Build model for data set with two or more samples
#'
#' @param df - input data.frame in long format with two or more samples
#' @return The object of class \code{drc}

drm.comparable.model = function(df) {
  mod.normal = LL.4(names=c("Slope, B","Lower Limit, D","Upper Limit, A", "ED50, C"))
  drm(response~dose, curveid = sample, data = df, fct = mod.normal, na.action = na.omit)
}

# Relative potency ED50/ED50 -----------------------
RP = function(ref.model, test.model) {
  result = list()
  if(class(ref.model)=="drc" && class(test.model)=="drc") {
    merged.melt.df = rbind(cbind(sample = 'ref', ref.model$origData), cbind(sample = 'test', test.model$origData))
    merged.model = drm.comparable.model(merged.melt.df)
    si = SI(merged.model, c(50,50), display=F)
    result = c(si[,1:2], 'RSD, %' = round(si[,2]/si[,1]*100, 2))
  }
  result
}
