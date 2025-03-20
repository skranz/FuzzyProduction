fp_pick_and_load_prod_df = function(fp_dir, prod_id, proc_id=NULL, version_pick_fun=NULL,add_ids=TRUE, extra_cols=NULL, add_fp_dir=TRUE) {
  if (length(fp_dir)>1) {
    restore.point("fp_pick_and_load_prod_df_before_loop")
    li = lapply(fp_dir, fp_pick_and_load_prod_df,prod_id=prod_id, proc_id=proc_id, version_pick_fun = version_pick_fun, add_ids=add_ids, extra_cols=NULL, add_fp_dir=add_fp_dir)
    return(bind_rows(li))
  }
  restore.point("fp_pick_and_load_prod_df")
  if (!is.null(version_pick_fun)) {
    ver_dirs = fp_all_ver_dirs(fp_dir, prod_id, proc_id)
    ver_dir = version_pick_fun(ver_dirs)
  } else {
    ver_dir =  fp_newest_ver_dir(fp_dir, prod_id, proc_id)
  }
  if (NROW(ver_dir)==0) return(NULL)
  df = fp_load_prod_df(ver_dir, add_ids=add_ids, extra_cols=extra_cols)
  if (add_fp_dir) {
    df = add_col_left(df, fp_dir=fp_dir)
  }
  df
}


fp_load_newest_prod_df = function(fp_dir, prod_id, proc_id, add_ids=TRUE) {
  ver_dir = fp_newest_ver_dir(fp_dir, prod_id, proc_id)
  fp_load_prod_df(ver_dir,add_ids=add_ids)
}


fp_load_all_prod_df = function(fp_dir, prod_id, add_ids=TRUE, as_df = TRUE) {
  restore.point("fp_load_all_prod_df")
  ver_dirs = fp_all_ver_dirs(fp_dir, prod_id)
  df_li = lapply(ver_dirs, fp_load_prod_df, add_ids=add_ids)
  if (!as_df) return(df_li)
  df = bind_rows(df_li)
  df
}


fp_load_prod_df = function(ver_dir = ver_dir, prod_df=NULL, add_ids=FALSE, extra_cols=NULL) {
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
  
  prod_df
  
}

