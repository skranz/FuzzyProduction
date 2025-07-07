
fp_load_all_prod_df = function(fp_dir, prod_id, add_ids=TRUE, as_df = TRUE, add_ver_dir=FALSE) {
  restore.point("fp_load_all_prod_df")
  ver_dirs = fp_all_ver_dirs(fp_dir, prod_id)
  df_li = lapply(ver_dirs, fp_load_prod_df, add_ids=add_ids, add_ver_dir=add_ver_dir)
  if (!as_df) return(df_li)
  df = bind_rows(df_li)
  df
}


fp_load_prod_df = function(ver_dir = ver_dir, prod_df=NULL, add_ids=FALSE, extra_cols=NULL, add_ver_dir=FALSE) {
  restore.point("fp_load_prod_df")
  if (!is.null(prod_df)) return(prod_df)
  prod_df = readRDS(file.path(ver_dir, "prod_df.Rds"))
  if (add_ids) {
    id_df = fp_ver_dir_to_ids(ver_dir)
    prod_df = add_col_left(prod_df, proc_id=id_df$proc_id, ver_id=id_df$ver_id)
  }
  if (!is.null(extra_cols)) {
    prod_df = add_col_left(prod_df, extra_cols)
  }
  if (add_ver_dir & !is.null(prod_df)) {
    prod_df$ver_dir = rep(ver_dir, NROW(prod_df))
  }

  prod_df

}

