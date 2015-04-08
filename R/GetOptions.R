GetOptions <- function(input) {
  args = GetArguments(input)
  .GetOptions(args)
}

.GetOptions <- function(args) {
  df = LoadAssayDataFromFolder(args$input, args$type)
  samples = as.character(unique(df$sample))
  factors = colnames(df)
  toJSON(list(samples=samples, factors=factors))
}
