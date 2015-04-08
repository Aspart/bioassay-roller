ParseCommandLine = function(input) {
  ## Default setting when no arguments passed
  if(length(input) < 1) {
    input = c("--help")
  }
  spec = matrix(c(
    'input', 'i', 1, "character", "Input directory",
    'type', 'f', 1, "character", "Input file type, capital letter. Could be one of (M)agellan/(F)luoroscan",
    'ref', 'r', 1, "character", "Reference name",
    'test', 't', 1, "character", "Test name",
    'out', 'o', 1, "character", "Output directory",
    'aov', 'a', 2, "character", "List of factors to process AoV",
    'unpool', 'u', 2, "character", "List of factors to unpool analysis",
    'robust', 'rb', 0, "logical", "Remove outliers from raw data automaticaly",
    'help', 'h', 0, "logical", "Display this message")
    , byrow=T, ncol=5)

  opt = getopt(spec, opt=input)

  if(!is.null(opt$help)) {
    cat(getopt(spec, usage=TRUE))
    q(status=1)
  }

  if(!is.null(opt$aov)) {
    opt$aov = unlist(strsplit(opt$aov, ","))
  }
  if(!is.null(opt$unpool)) {
    opt$unpool = unlist(strsplit(opt$unpool, ","))
  }
  opt
}
