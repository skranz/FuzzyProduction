example = function() {

}

prod_to_rclasses = function(prod) {
  schema_fields_to_rclasses(prod$fields)
}

prod_to_json_schema = function(prod,obj_arr=c("obj","arr")[1], add_description=TRUE, allow_null_def=TRUE) {
  restore.point("prod_to_json_schema")
  schema = prod_to_schema(prod,obj_arr)
  DataSchema::to_json_schema(schema,add_description = TRUE, allow_null_def=allow_null_def)
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



prods_define = function(..., .lists=NULL) {
  prods = list(...)
  restore.point("fp_prods")
  if (!is.null(.lists)) {
    prods = c(prods, do.call(c, .lists))
  }


  names(prods) = sapply(prods, function(prod) prod$prod_id)

  for (i in seq_along(prods)) {
    prod = prods[[i]]
    #if (i == 4) stop()

    if (!is.null(prod$parent)) {
      pprod = prods[[prod$parent]]
      prod$fields = c(pprod$fields[ prod[["from_parent"]] ], prod$fields)
      prod$vars = names(prod$fields)
      prod$parent_keys = pprod$keys
    } else if (!is.null(prod$widens)) {
      #if (length(prod$widens)>1)
      #  restore.point("kshjkdhsfksjflksdlj")
      vars = unique(unlist(lapply(prod$widens, function(w)
        prods[[w]]$vars
      )))
      for (w in rev(prod$widens)) {
        pprod = prods[[w]]
        add_fields = setdiff(pprod$vars, prod$vars)
        prod$fields = c(pprod$fields[add_fields], prod$fields)
        prod$keys = union(prod$keys, pprod$keys)
        prod$vars = names(prod$fields)
      }
      vars = union(vars, names(prod$fields))
      prod$fields = prod$fields[vars]
      prod$vars = vars
    }
    #is_key = sapply(prod$fields, function(field) isTRUE(field$is_key))
    #prod$keys = prod$vars[is_key]
    prods[[i]] = prod
  }
  prods
}

prod_define = function(prod_id, fields=NULL, widens=NULL, parent=NULL, from_parent=NULL, descr=NULL,keys=NULL, ...) {
  restore.point("prod_define")
  #fields = lapply(fields, function(field) {
    #field$is_key = first.non.null(field$is_key, FALSE)
  #  field
  #})
  if (is.null(fields)) fields = list()
  prod = list(prod_id=prod_id, fields=fields,keys=keys, vars=names(fields), widens = widens, parent = parent, from_parent=from_parent, descr=descr)
  class(prod) = c("fp_prod","list")
  prod
}


#' Somewhat flexible function to convert results data frames into proper
#' product data frames
df_to_prod_df = function(df, prod,  df_to_prod_cols=NULL,prod_to_df_cols=NULL, prod_df=NULL, convert_class=TRUE) {
  restore.point("df_to_prod_df")
  if (isTRUE(is.character(prod))) stop("prod must be the actual product (an R list) not just the prod_id.")

  if (is(df, "try-error")) return(prod_empty_df(prod))
  len = NROW(df)
  if (len==0) return(prod_empty_df(prod))

  if (is.null(df_to_prod_cols) & !is.null(prod_to_df_cols)) {
    df_to_prod_cols = invert_names_values(prod_to_df_cols)
  } else if (is.null(df_to_prod_cols)) {
    df_to_prod_cols = intersect(names(df), prod$vars)
  }


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

  if (convert_class) {
    prod_df = prod_set_df_col_class(prod_df, prod)
  }
  prod_df
}

prod_set_df_col_class = function(df, prod) {
  schema = prod_to_schema(prod)
  schema_set_df_col_class(df, schema)

}
