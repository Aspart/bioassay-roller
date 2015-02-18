# Roxygen comment example ----------------------------
#' Build model for data set with single sample
#'
#' @param df - input data.frame in long format
#' @return The object of class \code{drc}
drm.single.model = function(df) {
  mod.normal = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50"))
  drm(response~dose, data = df, fct = mod.normal, na.action = na.omit)
}

#' Build model for data set with two or more samples
#'
#' @param df - input data.frame in long format with two or more samples
#' @return The object of class \code{drc}

drm.comparable.model = function(df) {
  mod.normal = LL.4(names=c("Slope","Lower Limit","Upper Limit", "ED50"))
  drm(response~dose, curveid = sample, data = df, fct = mod.normal, na.action = na.omit)
}

# Check curves for parallelism --------------------------
PT = function(ref.model, test.model) {
  f.test = f.parallel.test(ref.model, test.model)
  #eq.test = eq.parallel.test(ref.model, test.model)
  # Paralellism tests results
  test.result = list('F-Test' = f.test)
}

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
  mod.shared = LL.4(fixed = c(coef[1], coef[2], coef[3], NA), names=c("Slope","Lower Limit","Upper Limit", "ED50"))
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

# Equivalence test----------------------------------------------------------------
# H0: Reference and test are not parallel
# HA: Reference and test are parallel
# Have to obtain param.fold from historic data (Upper, Lower and Slope relative fold)
eq.parallel.test = function(ref.model, test.model, param.fold=c(2,2,1.3), ci.level=0.95) {
  ref.coef = summary(ref.model)$coefficients
  test.coef = summary(test.model)$coefficients
  dif = test.coef[,1]-ref.coef[,1]
  dif.se = apply(cbind(test.coef[,2], ref.coef[,2]), 1, function(x) {
    sqrt(x[1]/sqrt(test.model$sumList$lenData)+x[2]/sqrt(ref.model$sumList$lenData))
  })

  critial.fold = c(1/3, 1.5, 2)
  ci = apply(rbind(mean = dif, sd = dif.se), 2, function(x) {
    c(lower = as.vector(x[1]-qnorm((1+ci.level)/2)*x[2]), upper = as.vector(x[1]+qnorm((1+ci.level)/2)*x[2]))
  })
  tmp = rbind(mean = dif, sd = dif.se, ci)
  # TODO: fix this shit!
}

# Mean, SD, RSD for each point of raw data ---------
raw.QC = function(df) {
  point.mean = unlist(lapply(split(df, f=df[,'dose']), function(x) mean(x$response, na.rm = T)))
  point.sd = unlist(lapply(split(df, f=df[,'dose']), function(x) sd(x$response, na.rm = T)))
  point.rsd = round(point.sd/point.mean*100,2)
  rbind(dose = colnames(point.mean), mean = point.mean, sd = point.sd, 'RSD, %' = point.rsd)
}

# Relative potency ED50/ED50 -----------------------
RP = function(ref.model, test.model) {
  merged.melt.df = rbind(cbind(sample = 'ref', ref.model$origData), cbind(sample = 'test', test.model$origData))
  merged.model = drm.comparable.model(merged.melt.df)
  si = SI(merged.model, c(50,50), display=F)
  c(si[,1:2], 'RSD, %' = round(si[,2]/si[,1]*100, 2))
}

# R coefficient -------------------------------------
r.squared = function(model) {
  num = sum(residuals(model)^2, na.rm = T)
  denum = sum((model$origData$response - mean(model$origData$response, na.rm = T))^2, na.rm = T)
  sqrt(1 - num/denum)
}

# QC section -----------------------------------
model.QC = function(model) {
  coef = coef(model)
  c('A/D' = as.vector(coef[3]/coef[2]), R = r.squared(model))
}

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

  ref.raw = do.call(rbind, lapply(split(ref.df, ref.df$dose), function(x) x$response))
  test.raw = do.call(rbind, lapply(split(test.df, test.df$dose), function(x) x$response))

  list('Ref, RFU' = ref.raw.QC, 'Test, RFU' = test.raw.QC, 'Ref RAW'=ref.raw, 'Test RAW'=test.raw, 'Ref, model QC' = ref.QC, 'Test, model QC' = test.QC, 'Parallel test'=parallel, 'RP' = rp, 'Ref model' = ref.model, 'Test model' = test.model)
}

# Parse command line into separate arguments -----------------
parse.cmd = function(input) {
	## Default setting when no arguments passed
	if(length(input) < 1) {
		input = c("--help")
	}

	## Help section
	if("--help" %in% input) {
		cat("
				The Bioassay Analysis Script (Alternative to PLA)

				Arguments:
				--ref=file    - reference file
				--test=file   - test file
        --out=dir     - output directory
				--aov=val1,val2,...     - list of values to process AoV
				--unpool=val1,val2,...  - list of values to unpool analysis by
        --rb   - if specified, outliers would be removed from data
				--help              - print this text

				Example:
				./bat.R --ref=ref.assay --test=test.assay --aov=Day,Plate --unpool=Plate --rb \n\n")

		q(save="no")
	}

	## Parse arguments (we expect the form --arg=value)
	parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
	argsDF = as.data.frame(do.call("rbind", parseArgs(input)))
	argsL = as.list(as.character(argsDF$V2))
	names(argsL) = argsDF$V1

  if("--rb" %in% input) {
    argsL$rb = T
  } else {
    argsL$rb = F
  }
	if(is.null(argsL$out)) {
	  writeLines('No output directory provided, exit now')
	  q(save="no")
	}
	if(is.null(argsL$ref)) {
	  writeLines('No ref file provided, exit now')
		q(save="no")
	}
	if(is.null(argsL$test)) {
	  writeLines('No test file provided, exit now')
		q(save="no")
	}
	if(is.null(argsL$aov)) {
	  writeLines('No values to process AOV, will be scipped')
	} else {
		argsL$aov=unlist(strsplit(argsL$aov, ","))
	}
	if(is.null(argsL$unpool)) {
	  writeLines('No values to unpool, pooled analysis will be processed')
	} else {
		argsL$unpool=unlist(strsplit(argsL$unpool, ","))
	}
	argsL
}

# Process unpooling by keys -----------
unpool = function(ds, keys) {
	for(key in keys) {
		if(class(ds) == 'list') {
			ds = unlist(lapply(ds, unpool, key), recursive = F)
		} else {
			ds = split(ds, ds[,key])
		}
	}
	if(class(ds)=='list') {
    ds
	} else {
    list('1' = ds)
	}
}

# Remove outliers from df provided --------
outlier.removal = function(df) {
   res = lapply(split(df, df$dose), function(x) {
    outlier_tf = outlier(x$response,logical=TRUE)
    find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
    x = x[-find_outlier,]
   })
   do.call(rbind, res)
}

# Write results to output file -------------
result.to.file = function(result, dir, name) {
  file = file.path(dir, paste(name, '.txt', sep=""))
  sink(file)
  writeLines('Reference RFU')
  write.table(format(rbind(t(result$'Ref RAW'), result$'Ref, RFU'), digits=3), sep = "\t", quote = F, col.names=NA)
  writeLines('Test RFU')
  write.table(format(rbind(t(result$'Test RAW'), result$'Test, RFU'), digits=3), sep = "\t", quote = F, col.names=NA)
  writeLines('Reference model QC')
  write.table(format(result$`Ref, model QC`, digits=3), col.names = F, sep = "\t", quote = F)
  writeLines('Test model QC')
  write.table(format(result$`Test, model QC`, digits=3), col.names = F, sep = "\t", quote = F)
  writeLines('Parallelism F-Test')
  write.table(format(result$`Parallel test`$`F-Test`, digits=3), col.names = F, sep = "\t", quote = F)
  writeLines('RP')
  write.table(format(result$RP, digits=3), col.names = F, sep = "\t", quote = F)
  writeLines('Reference model coefficients')
  write.table(format(coef(result$'Ref model'), digits=3), col.names = F, sep = "\t", quote = F)
  writeLines('Test model coefficients')
  write.table(format(coef(result$'Test model'), digits=3), col.names = F, sep = "\t", quote = F)
  sink()
  plot(model.point.ggplot(result$'Ref model', result$'Test model'))
  ggsave(file.path(dir, paste(name, '_model.png', sep="")))
  plot(model.boxplot.ggplot(result$'Ref model', result$'Test model'))
  ggsave(file.path(dir, paste(name, '_boxes.png', sep="")))
  T
}

exec = function(args) {
  ref.df = read.table(args$ref, sep="\t", header=T, check.names = F)
  ref.df[ref.df$response<0, 'response'] = NA
  test.df = read.table(args$test, sep="\t", header=T, check.names = F)
  test.df[test.df$response<0, 'response'] = NA
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
