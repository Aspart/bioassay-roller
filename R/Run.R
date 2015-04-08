Run <- function(input) {
  args = GetArguments(input)
  .Run(args)
}

.Run = function(args) {
  df = LoadAssayDataFromFolder(args$input, args$type)
  df.samples = split(df, df$sample)
  ref.df = df.samples[[args$ref]]
  test.df = df.samples[[args$test]]
  # Unpool data by factor levels -------------
  ref.df.list = unpool(ref.df, args$unpool)
  test.df.list = unpool(test.df, args$unpool)
  # Process analysis -------------------------
  result = mapply(assayProcessor, ref.df.list, test.df.list, SIMPLIFY=F)
  rt = lapply(names(result), function(name) {
    WriteResultsToFile(result[[name]], args$out, name)
  })
}
