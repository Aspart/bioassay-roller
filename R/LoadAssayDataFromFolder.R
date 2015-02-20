LoadAssayDataFromFolder = function(path, type) {
  switch(type,
         'F' = LoadFluoroscanDataFromFolder(path),
         'M' = LoadMagellanDataFromFolder(path)
         )
}

LoadFluoroscanDataFromFolder = function(path) {
  do.call(rbind, lapply(lapply(list.files(path), function(x) file.path(path, x)), LoadFluoroscanData))
}

LoadMagellanDataFromFolder = function(path) {
  do.call(rbind, lapply(lapply(list.files(path), function(x) file.path(path, x)), LoadMagellanData))
}
