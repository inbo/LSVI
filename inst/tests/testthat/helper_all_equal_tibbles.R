all.equal.tbl_df <-  #nolint: object_name_linter,
  function(target, current, ...) {
    all.equal(as.data.frame(target), as.data.frame(current), ...)
  }
