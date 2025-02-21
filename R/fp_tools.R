
select_cols = function(df, cols) {
  restore.point("select_cols")
  use = cols %in% names(df)
  cols = cols[use]
  df = df[, cols]
  if (!is.null(names(cols))) {
    names(df) = names(cols)
  }
  df
}
