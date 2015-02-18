model.point.ggplot = function(ref.model, test.model) {
  LP.4 = function(x, B, C, A, D) D + (A-D)/(1+(x/C)^B)
  LP.4mod <- function(x, ...) LP.4(10^x,... )  # to achieve propper plot
  unique.dose = unique(ref.model$origData$dose)
  points = rbind(ref.model$origData, test.model$origData)
  pl = ggplot() + labs(title='Model', x='Log Dose, mug/ml', y='Response, RFU') +
    scale_x_log10(breaks=unique.dose) +
  	stat_function(fun = LP.4mod,
  								data = data.frame(x = unique.dose, Sample = 'reference'),
  								args = list(B = coef(ref.model)[1],  D = coef(ref.model)[2], A = coef(ref.model)[3], C = coef(ref.model)[4]),
  								colour = 'red') +
  	stat_function(fun = LP.4mod,
  								data = data.frame(x = unique.dose, Sample = 'test'),
  								args = list(B = coef(test.model)[1], D = coef(test.model)[2], A = coef(test.model)[3], C = coef(test.model)[4]),
  								colour = "deepskyblue3",
  								linetype='dashed') +
  	geom_point(data = points, aes(x=dose, y=response, color=sample, ymax=max(response)*1.05), position = position_dodge(width = 0.2))
  pl
}

model.boxplot.ggplot = function(ref.model, test.model) {
	points = rbind(ref.model$origData, test.model$origData)
	pl = ggplot(data=points, mapping=aes(x=factor(dose), y=response, color=factor(sample))) +
		labs(title='Model', x='Log Dose, mug/ml', y='Response, RFU') +
		geom_boxplot(position = position_dodge(width = 0.2))
  pl
}
