ParseCommandLine = function(input) {
  ## Default setting when no arguments passed
  if(length(input) < 1) {
    input = c("--help")
  }

  ## Help section
  if("--help" %in% input) {
    cat("
        The Bioassay Analysis Script (Alternative to PLA)

        Arguments:
        --input=<DIR>   - input directory
        --type=<TYPE>   - capital letter, type of input data files. Could be one of (M)agellan/(F)luoroscan
        --ref=<val>     - reference name
        --test=<val>    - test name
        --out=<DIR>     - output directory
        --aov=val1,val2,...     - list of values to process AoV
        --unpool=val1,val2,...  - list of values to unpool analysis by
        --rb   - if specified, outliers will be removed from data
        --help              - print this text

        Example:
        --ref=ref.assay --test=test.assay --aov=Day,Plate --unpool=Plate --rb \n\n")

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
  if(is.null(argsL$input)) {
    writeLines('No input directory provided, exit now')
    q(save="no")
  }
  if(is.null(argsL$type)) {
    writeLines('No type provided, exit now')
    q(save="no")
  } else if(!(argsL$type %in% c('F', 'M'))) {
    writeLines('Type does not match (M)agellan/(F)luoroscan, exit now')
    q(save="no")
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
