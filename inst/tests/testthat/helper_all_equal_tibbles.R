all.equal.tbl_df <- function(target, current, ...) {
  all.equal(as.data.frame(target), as.data.frame(current), ...)
}
