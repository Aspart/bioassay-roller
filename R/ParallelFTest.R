# F test -------------------------------------------------
# H0: Reference and test are parallel
# HA: Reference and test are not parallel
# Reject H0 if F value is higher than a critical value
# Fail often with good precision
f.parallel.test = function(ref.model, test.model, level=0.95) {
  melt.df = rbind(cbind(sample='ref', ref.model$origData), cbind(sample='test', test.model$origData))
  initial = (coef(ref.model)+coef(test.model))/2
  initial = c(initial, initial[4])
  model = f.test.constrained.fit(response~dose, melt.df, sample, initial)
  coef = coef(model)
  mod.shared = LL.4(fixed = c(coef[1], coef[2], coef[3], NA), names=c("Slope, B","Lower Limit, D","Upper Limit, A", "ED50, C"))
  constrained.ref.model = drm(response~dose, data = ref.model$origData, fct = mod.shared, na.action = na.omit)
  constrained.test.model = drm(response~dose, data = test.model$origData, fct = mod.shared, na.action = na.omit)

  testFit = modelFit(test.model)
  refFit = modelFit(ref.model)
  unconstrained.RSS = testFit$RSS[2] + refFit$RSS[2]
  unconstrained.DF = testFit$ModelDf[2] + refFit$ModelDf[2]

  testFit = modelFit(constrained.test.model)
  refFit = modelFit(constrained.ref.model)
  constrained.RSS = testFit$RSS[2] + refFit$RSS[2]
  constrained.DF = testFit$ModelDf[2] + refFit$ModelDf[2]

  f=((constrained.RSS-unconstrained.RSS)/(constrained.DF-unconstrained.DF))/(unconstrained.RSS/unconstrained.DF)
  p = pf(f, constrained.DF-unconstrained.DF, constrained.DF)
  if(p>1-level) {
    c(result='FAIL', 'p-value' = p)
  } else {
    c(result='PASS', 'p-value' = p)
  }
}

# A = minimum asymptote
# B = hill slope
# C = inflection point
# D = maximum asymptote
model4PL = function(x, B, A, D, C) ((D-A)/(1+((x/C)^B))) + A

# for two curves only!
shared4PL = function(par, data.list) {
  if(length(data.list)!=2) stop('exactly 2 curveid have to be present in data')
  c(model4PL(data.list[[1]][,2], par[1], par[2], par[3], par[4])-data.list[[1]][,1],
    model4PL(data.list[[2]][,2], par[1], par[2], par[3], par[5])-data.list[[2]][,1]
  )
}

# length(par) != 5 -> error
f.test.constrained.fit = function(formula, data, curveid, initial) {
  arguments = as.list(match.call())
  levels = eval(arguments$curveid, data)
  data.list = split(data, levels)
  data.list = lapply(data.list, function(x) model.frame(formula, x))
  model = nls.lm(par = initial, fn = shared4PL, data.list = data.list)
  model
}
