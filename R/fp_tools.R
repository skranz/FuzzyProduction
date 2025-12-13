
add_col_left = function(df, ...) {
  args = list(...)
  restore.point("add_col_left")

  len = NROW(df)
  args = lapply(args, function(x) rep(x, length=len))
  bind_cols(as_tibble(args), df)
}


na_val = function(x, val=0) {
  x[is.na(x)] = val
  x
}

first_nn = function (...)
{
  args = list(...)
  for (val in args) {
    if (!is.null(val))
      return(val)
  }
  return(NULL)
}

file_remove_existing = function(file) {
  if (file.exists(file)) file.remove(file)
}

rename_cols = function (x, old_cols, new_cols)
{
  restore_point("rename_cols")
  inds = match(old_cols, names(x))
  old_cols = old_cols[!is.na(inds)]
  new_cols = new_cols[!is.na(inds)]
  inds = inds[!is.na(inds)]
  names(x)[inds] = new_cols
  x
}


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
