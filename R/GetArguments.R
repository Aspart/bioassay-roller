GetArguments <- function(input) {
  if((file.exists(input) || jsonlite::validate(input))) {
    args <- jsonlite::fromJSON(input)
  } else {
    args <- ParseCommandLine(input)
  }
  args = ConfigurationValidator(args)
  args
}
