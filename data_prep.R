library(plyr)
library(gtools)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
library(drc)

load.raw = function(file) {
  data = read.table(
    file=file,
    sep=';',
    header=F,
    row.names = NULL)
  data = data[data$V1 != '', ]
  data = data[data$V1 != 'A' & data$V1 !='H' & data$V1 != '<>', ]
  data = subset(data, select=-c(V2, V13))
  list.df = split(subset(data[!is.na(data$V3),], select=-c(V1)), (0:(nrow(data[!is.na(data$V3),])-1) %/% 6))  # modulo division
  names(list.df) = as.factor(as.vector(data[seq(1, nrow(data), 7),1]))
  list.df = lapply(list.df, function(df) {rownames(df) <- NULL; df})
  list.df
}

df.to.comparable = function(df, doses) {
  test = df[c(F,T),]
  colnames(test) = doses
  rownames(test) = NULL
  ref = df[c(T,F),ncol(df):1]
  colnames(ref) = doses
  rownames(ref) = NULL
  result = list(ref,test)
  names(result) = c('ref', 'test')
  result
}

melt.comparable.df = function(df) {
  test = melt(df$test, variable.name='dose', value.name='responce')
  test$sample = 'test'
  ref = melt(df$ref, variable.name='dose', value.name='responce')
  ref$sample = 'ref'
  result = rbind(test,ref)
  result$dose = as.numeric(as.character(result$dose))
  result
}

drm.model = function(df) {
  drm(responce~dose, sample, data = df, fct = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50")))
}

doses = round(150 / (2^(0:9)), 2)
df.list = load.raw('/Users/roman/Projects/Biocad/bioassay-raw/bev_281014/raw.csv')
df.list = lapply(df.list, df.to.comparable, doses)
df.list = lapply(df.list, melt.comparable.df)
result = lapply(df.list, drm.model)
lapply(result, SI, c(50,50), ci = "delta")
