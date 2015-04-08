VarianseAnalyser <- function(df, keys) {
  byDose = split(df, list(df$sample, df$dose))
  lapply(lapply(byDose, function(x) aov(as.formula(paste0("response", "~", paste(keys, collapse="+"))), x)), summary)
}
