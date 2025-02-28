#' Initialize a production run
pru_init = function(fp_dir,prod_id, proc_id=proc_info$proc_id, to_v0=TRUE, ver_dir=NULL,proc_info=NULL, ...) {
  pru = list(fp_dir=fp_dir,prod_id=prod_id, proc_id=proc_id, to_v0=to_v0, proc_info=proc_info, ...)
  pru$proc_dir = proc_dir = file.path(fp_dir,pru$prod_id, pru$proc_id)

  if (to_v0) {
    pru$ver_ind = 0
  } else {
    ver_dirs = list.dirs(proc_dir, full.names=FALSE, recursive=TRUE)
    if (length(ver_dirs)==0) {
      pru$ver_ind = 1
    } else {
      ver_nums = as.integer(substr(ver_dirs, 2))
      max_ver = max(ver_nums)
      pru$ver_ind = max_ver+1
    }
  }
  pru$ver_id = paste0(prod_id, "-", proc_id, "--v", pru$ver_ind)
  pru$ver_dir = paste0(pru$proc_dir, "/v", pru$ver_ind)
  pru
}

pru_save = function(pru, prod_df, save_fields=NULL, save_pru=TRUE) {
  restore.point("pru_save")
  if (!dir.exists(pru$ver_dir)) dir.create(pru$ver_dir, recursive = TRUE)

  ver_dir = pru$ver_dir
  if (save_pru) {
    saveRDS(pru,file.path(ver_dir,"pru.Rds"))
  }
  err_file = file.path(ver_dir, "has_err.txt")
  if (file.exists(err_file)) file.remove(err_file)

  for (field in save_fields) {
    if (!is.null(pru[[field]])) {
      saveRDS(pru[[field]],  file.path(ver_dir, paste0(field,".Rds") ))
    }
  }

  saveRDS(prod_df, file.path(ver_dir, "prod_df.Rds"))
  pru$prod_df = prod_df
  invisible(pru)
}

pru_backport_save = function(pru,bp_prod, prod_df=pru$prod_df) {
  restore.point("pru_backport_save")
  if (is.null(bp_prod)) stop("Need bp_prod.")
  bp_prod_df = df_to_prod_df(prod_df, bp_prod)
  pru$proc_info$prod_id = bp_prod$prod_id
  dirs = pru_make_ver_dir(pru)
  pru[names(dirs)] = dirs
  bp = pru_save(pru,bp_prod_df)
  invisible(bp)
}
