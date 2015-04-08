# Process unpooling by keys -----------
unpool = function(ds, keys) {
  for(key in keys) {
    if(class(ds) == 'list') {
      ds = unlist(lapply(ds, unpool, key), recursive = F)
    } else {
      ds = split(ds, ds[[key]])
    }
  }
  if(class(ds)=='list') {
    ds
  } else {
    list('1' = ds)
  }
}
