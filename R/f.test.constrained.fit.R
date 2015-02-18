# A = minimum asymptote
# B = hill slope
# C = inflection point
# D = maximum asymptote
gen.LL.4 = function(x, B, A, D, C) ((D-A)/(1+((x/C)^B))) + A

# for two curves only!
SRSS.LL.4 = function(par, data.list) {
  if(length(data.list)!=2) stop('exactly 2 curveid have to be present in data')
  c(gen.LL.4(data.list[[1]][,2], par[1], par[2], par[3], par[4])-data.list[[1]][,1],
    gen.LL.4(data.list[[2]][,2], par[1], par[2], par[3], par[5])-data.list[[2]][,1]
  )
}

# length(par) != 5 -> error
f.test.constrained.fit = function(formula, data, curveid, initial) {
  if(length(initial)!=5) stop('par should be length of 5')
  arguments = as.list(match.call())
  levels = eval(arguments$curveid, data)
  data.list = split(data, levels)
  data.list = lapply(data.list, function(x) model.frame(formula, x, na.action = na.omit))
  model = nls.lm(par = initial, fn = SRSS.LL.4, data.list = data.list)
  model
}
