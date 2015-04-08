model.multiple.point.ggplot = function(ref.model, ref.df, test.model, test.df) {
  LP.4 = function(x, B, C, A, D) D + (A-D)/(1+(x/C)^B)
  LP.4mod <- function(x, ...) LP.4(10^x,... )  # to achieve propper plot
  linear = function(x, A, B) x*A+B
  linear.mod <- function(x, ...) linear(10^x, ...)

  unique.dose = sort(unique(ref.df$dose))

  ref.name = as.character(unique(ref.df$sample))
  test.name = as.character(unique(test.df$sample))
  points = rbind(ref.df, test.df)

  cols <- c("red", "deepskyblue3")
  names(cols) <- c(ref.name, test.name)

  ref.stat.function <- if(class(ref.model)=="drc") {
    stat_function(fun = LP.4mod,
                  data = data.frame(x=unique.dose, sample=ref.name),
                  args = list(B=coef(ref.model)[1],  D=coef(ref.model)[2], A=coef(ref.model)[3], C=coef(ref.model)[4]),
                  mapping = aes(colour=sample))
  } else if(class(ref.model)=="lm") {
    stat_function(fun = linear.mod,
                  data = data.frame(x=unique.dose, sample=ref.name),
                  args = list(A=coef(ref.model)[2], B=coef(ref.model)[1]),
                  mapping = aes(colour=sample))
  }

  test.stat.function <- if(class(test.model)=="drc") {
    stat_function(fun = LP.4mod,
                  data = data.frame(x=unique.dose, sample=test.name),
                  args = list(B=coef(test.model)[1],  D=coef(test.model)[2], A=coef(test.model)[3], C=coef(test.model)[4]),
                  mapping = aes(colour=sample))
  } else if(class(test.model)=="lm") {
    stat_function(fun = linear.mod,
                  data = data.frame(x=unique.dose, sample=test.name),
                  args = list(A=coef(test.model)[2], B=coef(test.model)[1]),
                  mapping = aes(colour=sample))
  }

  pl = ggplot() +
    scale_x_log10(breaks=unique.dose, labels=unique.dose) +
    ref.stat.function +
    test.stat.function +
    geom_point(data=ref.df, aes(x=dose, y=response, ymax=max(response)*1.05, colour=sample), position=position_dodge(width=0.2)) +
    geom_point(data=test.df, aes(x=dose, y=response, ymax=max(response)*1.05, colour=sample), position=position_dodge(width=0.2)) +
    labs(title='Model', x='Log Dose, mug/ml', y='Response, RFU', fill="Sample") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_color_manual("Sample", values=cols)
  pl
}

model.single.point.ggplot = function(ref.model, ref.df, test.model, test.df) {
  LP.4 = function(x, B, C, A, D) D + (A-D)/(1+(x/C)^B)
  LP.4mod <- function(x, ...) LP.4(10^x,... )  # to achieve propper plot
  linear = function(x, A, B) x*A+B
  linear.mod <- function(x, ...) linear(10^x, ...)

  unique.dose = sort(unique(ref.df$dose))

  ref.name = as.character(unique(ref.df$sample))
  test.name = as.character(unique(test.df$sample))
  points = rbind(ref.df, test.df)
  ref.df = ddply(ref.df, sample~dose, summarise, response=mean(response))
  test.df = ddply(test.df, sample~dose, summarise, response=mean(response))

  cols <- c("red", "deepskyblue3")
  names(cols) <- c(ref.name, test.name)

  ref.stat.function <- if(class(ref.model)=="drc") {
    stat_function(fun = LP.4mod,
                  data = data.frame(x=unique.dose, sample=ref.name),
                  args = list(B=coef(ref.model)[1],  D=coef(ref.model)[2], A=coef(ref.model)[3], C=coef(ref.model)[4]),
                  mapping = aes(colour=sample))
  } else if(class(ref.model)=="lm") {
    stat_function(fun = linear.mod,
                  data = data.frame(x=unique.dose, sample=ref.name),
                  args = list(A=coef(ref.model)[2], B=coef(ref.model)[1]),
                  mapping = aes(colour=sample))
  }

  test.stat.function <- if(class(test.model)=="drc") {
    stat_function(fun = LP.4mod,
                  data = data.frame(x=unique.dose, sample=test.name),
                  args = list(B=coef(test.model)[1],  D=coef(test.model)[2], A=coef(test.model)[3], C=coef(test.model)[4]),
                  mapping = aes(colour=sample))
  } else if(class(test.model)=="lm") {
    stat_function(fun = linear.mod,
                  data = data.frame(x=unique.dose, sample=test.name),
                  args = list(A=coef(test.model)[2], B=coef(test.model)[1]),
                  mapping = aes(colour=sample))
  }

  pl = ggplot() +
    scale_x_log10(breaks=unique.dose, labels=unique.dose) +
    ref.stat.function +
    test.stat.function +
    geom_point(data=ref.df, aes(x=dose, y=response, ymax=max(response)*1.05, colour=sample), position=position_dodge(width=0.2)) +
    geom_point(data=test.df, aes(x=dose, y=response, ymax=max(response)*1.05, colour=sample), position=position_dodge(width=0.2)) +
    labs(title='Model', x='Log Dose, mug/ml', y='Response, RFU', fill="Sample") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_color_manual("Sample", values=cols)
  pl
}

model.boxplot.ggplot = function(ref.model, ref.df, test.model, test.df) {
  points = rbind(ref.df, test.df)

	pl = ggplot(data=points, mapping=aes(x=factor(dose), y=response, colour=sample)) +
		labs(title='Model', x='Log Dose, mug/ml', y='Response, RFU') +
		geom_boxplot(position = position_dodge(width = 0.2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pl
}
