# Check curves for parallelism --------------------------
PT = function(ref.model, test.model) {
  test.result = list()
  if(class(ref.model)=="drc" && class(test.model)=="drc") {
    f.test = f.parallel.test(ref.model, test.model)
    #eq.test = eq.parallel.test(ref.model, test.model)
    # Paralellism tests results
    test.result$'F-Test' = f.test
  }
  test.result
}

