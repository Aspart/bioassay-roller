.bind.fill <- function(...) {
  x <- list(...)
  x.max = max(unlist(lapply(x, length)))
  x.filled = lapply(x, function(z) c(z, rep(NA, x.max-length(z))))
  x.filled
}

cbind.fill <- function(...) {
  do.call(cbind, .bind.fill(...))
}

rbind.fill <- function(...) {
  do.call(rbind, .bind.fill(...))
}

