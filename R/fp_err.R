
fp_is_err_obj = function(x) {
  is(x,"fp_err_obj")
}

fp_has_err_obj = function(li) {
  any(sapply(li, fp_is_err_obj))
}

fp_err_obj = function(msg, show=TRUE) {
  x = list(msg=msg)
  class(x) = c("fp_err_obj","list")
  if (show) cat(msg)
  x
} 
