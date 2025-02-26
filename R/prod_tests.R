
example = function() {
  library(repboxAI)
  run_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/cell_base/tab_html_hx_pdf/r0"
  prod_df = rai_load_prod_df(run_dir=run_dir)

  library(restorepoint)
  library(dplyr)
  #df = readRDS("C:/libraries/FuzzyProduction/prod_df.Rds")
  tests = tests_cell_base()
  prod_run_tests(tests, prod_df, keys="tabid")
  prod_run_tests(tests, prod_df, return_details=TRUE, keys="cellid")
}

prod_run_tests = function(prod_tests,prod_df, prod=NULL, return_details=FALSE, keys=first.non.null(prod_tests$keys,prod$keys)) {
  restore.point("prod_run_tests")

  res_li = lapply(prod_tests$fun, prod_run_test_fun, prod_df=prod_df, prod=prod, return_details = return_details, keys=keys)
  if (length(res_li)==0) return(NULL)
  if (!return_details) {
    res = as.data.frame(do.call(c,res_li))
  } else if (return_details & is.null(keys)) {
    res = do.call(bind_cols, res_li)
  } else {
    res = res_li[[1]]
    for (i in setdiff(seq_len(length(res_li)),1) ) {
      res = full_join(res, res_li[[i]], by=keys)
    }
  }
  res
}

prod_run_test_fun = function(test_fun, prod_df, prod=prod, return_details=FALSE, keys=prod$keys) {
  restore.point("prod_run_test_fun")
  df = test_fun(prod_df, prod)
  if (is(test_fun, "flag_test_fun")) {
    test_prefix = "flag_"
  } else {
    stop("unknown test function of class ", class(test_fun)[1])
  }
  cols = names(df)

  key_cols = intersect(keys, cols)
  test_cols = cols[startsWith(cols, test_prefix)]
  details_df = df[,c(key_cols, test_cols)]

  if (is(test_fun, "flag_test_fun")) {
    i = 1
    for (i in seq_along(test_cols)) {
      test = stri_sub(test_cols[i],nchar(test_prefix)+1)
      rel_col = paste0("relevant_", test)

      if (!rel_col %in% cols) next

      details_df[[ test_cols[i] ]] = ifelse(is.true(df[[rel_col]]),details_df[[ test_cols[i] ]], NA_integer_)
    }
    for (i in seq_along(test_cols)) {
      details_df[[ test_cols[i] ]] = add_class(details_df[[ test_cols[i] ]], "flag_test")
    }
  }
  details_df = rename.cols(details_df, test_cols, stri_sub(test_cols, nchar(test_prefix)+1))
  if (return_details) return(details_df)
  return(prod_agg_test_details(details_df, keys=keys))
}

prod_agg_test_details = function(details, keys=NULL) {
  restore.point("prod_add_test_details")
  cols = setdiff(names(details), keys)

  agg = lapply(details[cols], function(vals) {
    num_rel = sum(!is.na(vals))
    ifelse(num_rel == 0,0, sum(vals, na.rm=TRUE) / num_rel)
  })
  agg
}


flag_test_funs = function(...) {
  funs = list(...)
  funs = lapply(funs, add_class, new_class = "flag_test_fun")
  funs
}

prod_tests_define = function(..., descr=NULL, keys=NULL) {
  funs = unlist(list(...))
  restore.point("prod_tests_define")
  x = list(funs=funs, descr=descr, keys=keys)
  x
}
