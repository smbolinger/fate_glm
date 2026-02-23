suppress_warnings <- function(.expr, .f, ...) {
  eval.parent(substitute(
    withCallingHandlers( .expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <- 
        if(is.character(.f)) grepl(.f, cm) else rlang::as_function(.f)(cm,...)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}
# 
# withCallingHandlers({
#   x <- 0
#   warning("Unrecognized record 123")
#   x <- x + 1
#   warning("another warning")
#   x + 1
# }, warning = function(w) {
#   if (startsWith(conditionMessage(w), "Unrecognized record"))
#     invokeRestart("muffleWarning")
# })

if (FALSE){
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, startsWith, "o")
  sqrt(-1)
  # Warning message:
  # In sqrt(-1) : NaNs produced
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, ~nchar(.)>10)
  # Warning message:
  # ooops
  suppress_warnings({sqrt(-1); warning("ooops", call. = FALSE)}, "NaN")
  # Warning message:
  # ooops
}

## From: https://stackoverflow.com/questions/16517795/selective-suppresswarnings-that-filters-by-regular-expression/16521046#16521046