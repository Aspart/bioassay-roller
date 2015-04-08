# Main analysis function -----------------------------
assayProcessor = function(ref.df, test.df) {
	# raw data QC
  ref.raw.QC = raw.QC(ref.df)
  test.raw.QC = raw.QC(test.df)
  # Melt it, fit model and process QC
  ref.model = drm.single.model(ref.df)
  test.model = drm.single.model(test.df)
  # Model quality control (R, A/D)
  ref.QC = model.QC(ref.model)
  test.QC = model.QC(test.model)
  # Paralellism test
  parallel = PT(ref.model, test.model)
  # Measure relative potency and selectivity index
  rp = RP(ref.model, test.model)

  ref.raw = do.call(rbind.fill, lapply(split(ref.df, ref.df$dose), function(x) x$response))
  test.raw = do.call(rbind.fill, lapply(split(test.df, test.df$dose), function(x) x$response))

  list('Ref, RFU'=ref.raw.QC,
       'Test, RFU'=test.raw.QC,
       'Ref RAW'=ref.raw,
       'Test RAW'=test.raw,
       'Ref, model QC'=ref.QC,
       'Test, model QC'=test.QC,
       'Parallel test'=parallel,
       'RP'=rp,
       'Ref model'=ref.model,
       'Test model'=test.model,
       'Ref, df'=ref.df,
       'Test, df'=test.df)
}
