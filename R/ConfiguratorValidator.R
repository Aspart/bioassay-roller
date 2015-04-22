ConfigurationValidator <- function(args) {
  #reverce compatibility
  if(!is.null(args$inputDir)) {
    args$input = args$inputDir
  }
  if(!is.null(args$inputFormat)) {
    args$type = args$inputFormat
  }
  if(is.null(args$input)) {
    stop('Input is empty')
  }
  if(is.null(args$type)) {
    stop('File type not specified')
  }
  if(is.null(args$ref)) {
    stop('Reference sample name not specified')
  }
  if(is.null(args$test)) {
    stop('Test sample name not specified')
  }
  if(is.null(args$out)) {
    args$out=file.path(args$input, "results")
    if (!file.exists(args$out)) {
      dir.create(args$out)
    }
    warning('Output directory not specified, using default')
  }
  if(!is.null(args$aov)) {
    args$aov=unlist(strsplit(args$aov, ","))
  }
  if(is.null(args$unpool)) {
    args$unpool=unlist(strsplit(args$unpool, ","))
  }
  args
}
