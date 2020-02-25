all.equal.tbl_df <- function(target, current, ...) {  #nolint
  all.equal(as.data.frame(target), as.data.frame(current), ...)
}
