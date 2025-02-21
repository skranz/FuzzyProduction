example = function() {

}

prod_to_schema = function(prod, obj_arr=c("obj","arr")[1]) {
  if (obj_arr=="arr") {
    schema_arr(schema_obj(prod$fields))
  } else {
    schema_obj(prod$fields)
  }
}

prod_empty_df = function(prod) {
  as_tibble(lapply(prod$fields, schema_empty_instance))
}

prod_na_df = function(prod, len=1) {
  as_tibble(lapply(prod$fields, schema_na_instance, len))
}



prods_define = function(...) {
  prods = list(...)
  restore.point("fp_prods")
  names(prods) = sapply(prods, function(prod) prod$pid)

  for (i in seq_along(prods)) {
    prod = prods[[i]]

    if (!is.null(prod$parent)) {
      pprod = prods[[prod$parent]]
      keys = setdiff(pprod$keys, names(prod$fields))
      prod$fields = c(pprod$fields[keys], prod$fields)
    } else if (!is.null(prod$widens)) {
      pprod = prods[[prod$widens]]
      add_fields = setdiff(pprod$vars, prod$vars)
      prod$fields = c(pprod$fields[add_fields], prod$fields)
      prod$vars = names(prod$fields)
    }
    is_key = sapply(prod$fields, function(field) isTRUE(field$is_key))
    prod$keys = prod$vars[is_key]
    prods[[i]] = prod
  }
  prods
}

prod_define = function(pid, fields, widens=NULL, parent=NULL, descr=NULL) {
  fields = lapply(fields, function(field) {
    field$is_key = first.non.null(field$is_key, FALSE)
    field$parse = first.non.null(field$parse, TRUE)
    field
  })
  list(pid=pid, fields=fields, vars=names(fields), widens = widens, parent = parent, descr=descr)
}


#' Somewhat flexible function to convert results data frames into proper
#' product data frames
df_to_prod_df = function(df, prod,  df_to_prod_cols=NULL,prod_to_df_cols=NULL, prod_df=NULL) {
  restore.point("df_to_prod_df")
  if (is(df, "try-error")) return(prod_empty_df(prod))
  len = NROW(df)
  if (len==0) return(prod_empty_df(prod))

  if (is.null(df_to_prod_cols) & !is.null(prod_to_df_cols)) {
    df_to_prod_cols = invert_names_values(prod_to_df_cols)
  } else if (is.null(df_to_prod_cols)) {
    df_to_prod_cols = intersect(names(df), prod$vars)
  }

  # Ensures correct order
  #df_to_prod_cols = df_to_prod_cols[intersect(prod$vars, names(df_to_prod_cols))]

  # Not all prod cols are in df
  mod_df = select_cols(df, df_to_prod_cols)
  if (NCOL(mod_df)<length(prod$vars)) {
    if (is.null(prod_df)) prod_df = prod_na_df(prod, len)
    prod_df[names(mod_df)] = mod_df
  } else {
    prod_df = mod_df
  }
  # Ensures correct order and possibly removes extra variables
  if (!identical(names(prod_df), prod$vars))
    prod_df = prod_df[, prod$vars]
  prod_df
}

