LoadFluoroscanData = function(file) {
  input = read.table(file, sep='\t', fill = TRUE)
  response = as.numeric(as.vector(as.matrix(input[1:6,])))
  dose = as.numeric(as.vector(as.matrix(input[17:22,])))
  sample = as.vector(as.matrix(input[10:15,]))
  fct = input[(24:nrow(input))[(!is.na(input[24:nrow(input), 1]) & !(input[24:nrow(input),1]==""))],1:2]
  tmp = data.frame(dose=dose, response=response, sample=sample)
  factors = setNames(data.frame(t(fct)[2,, drop=F], stringsAsFactors = F), fct$V1)
  row.names(factors) = NULL
  result = data.frame(tmp, factors)
}

LoadMagellanData = function(file) {

}
