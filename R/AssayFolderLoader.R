# Load files from folder and provide them as single data.frame
# LoadAssayDataFromFolder - switcher for file types in folder

LoadAssayDataFromFolder = function(path, type) {
  if(type=='F') {
    LoadFluoroscanDataFromFolder(path)
  } else if(type=='M') {
    LoadMagellanDataFromFolder(path)
  } else {
    stop(paste("Unsupported file type", type))
  }
}

LoadFluoroscanDataFromFolder = function(path) {
  do.call(rbind, lapply(lapply(list.files(path), function(x) file.path(path, x)), function(x) if(file.info(x)["isdir"]==F) LoadFluoroscanData(x)))
}

LoadMagellanDataFromFolder = function(path) {
  do.call(rbind, lapply(lapply(list.files(path), function(x) file.path(path, x)), LoadMagellanData))
}
