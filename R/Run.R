Run = function(args) {
  df = LoadAssayDataFromFolder(args$input, args$type)
  ref.df = split(df, df$sample)[[args$ref]]
  ref.df[ref.df$response<0, 'response'] = NA
  ref.df[ref.df$dose==0, 'dose'] = NA
  test.df = split(df, df$sample)[[args$test]]
  test.df[test.df$response<0, 'response'] = NA
  test.df[test.df$dose==0, 'dose'] = NA
  # Unpool data by factor levels -------------
  ref.df = unpool(ref.df, args$unpool)
  test.df = unpool(test.df, args$unpool)
  # Process analysis -------------------------
  result = mapply(assayProcessor, ref.df, test.df, SIMPLIFY=F)

  rt = lapply(names(result), function(name) {
    result.to.file(result[[name]], args$out, name)
  })
  rt
}
