example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  fp_dir = file.path(project_dir,"fp")

  fp_all_proc_id(fp_dir,"cell_base")
  prod_id = "cell_base"
  proc_id = "tab_html_hx_pdf"
  fp_newest_ver_dir(fp_dir, prod_id, proc_id)

  fp_all_outage_ver_dirs(project_dir)
  fp_all_error_ver_dirs(project_dir)

  df = fp_all_ver_info(fp_dir)

  df = fp_load_all_prod_df(fp_dir, "cell_base")
}

fp_newest_ver_dir = function(fp_dir, prod_id, proc_id=NULL) {
  restore.point("fp_newest_ver_dir")
  info = fp_all_ver_info(fp_dir, prod_id=prod_id, proc_id=proc_id)
  if (NROW(info)==0) return(NULL)
  info %>%
    group_by(prod_id, proc_id) %>%
    arrange(desc(mtime)) %>%
    slice(1) %>%
    pull(ver_dir)
}

fp_ver_dir_to_fp_dir = function(ver_dir) {
  dirname(dirname(dirname(ver_dir)))
}

fp_ver_dir_to_prod_dir = function(ver_dir) {
  dirname(dirname(ver_dir))
}



fp_ver_dir_to_ids = function(ver_dir) {
  dname = dirname(ver_dir)
  proc_id = basename(dname)
  prod_id = basename(dirname(dname))
  ver_id = paste0(proc_id,"--", basename(ver_dir))
  ver_ind = as.integer(stri_sub(basename(ver_dir),2))
  data.frame(prod_id=prod_id, proc_id=proc_id, ver_id=ver_id, ver_ind=ver_ind, ver_dir=ver_dir)
}

fp_ver_dir_to_prod_id = function(ver_dir) {
  dname = dirname(ver_dir)
  prod_id = basename(dirname(dname))
  prod_id
}


fp_ver_dir_to_proc_id = function(ver_dir) {
  dname = dirname(ver_dir)
  proc_id = basename(dname)
  proc_id
}


fp_all_prod_id = function(fp_dir) {
  list.dirs(fp_dir,full.names = FALSE,recursive = FALSE)
}


fp_all_proc_id = function(fp_dir, prod_id=NULL, ends_with=NULL) {
  if (is.null(prod_id)) {
    prod_id_dirs = list.dirs(fp_dir,full.names = TRUE,recursive = FALSE)
  } else {
    prod_id_dirs = file.path(fp_dir, prod_id)
  }
  proc_ids = list.dirs(prod_id_dirs,full.names = FALSE,recursive = FALSE)
  if (!is.null(ends_with)) {
    proc_ids = proc_ids[endsWith(proc_ids, ends_with())]
  }
  proc_ids
}


fp_all_proc_dir = function(fp_dir, prod_id, only_success=TRUE) {
  fp_dir = file.path(fp_dir,"fp","prod_vers",prod_id)
  if (only_success) {
    files = list.files(fp_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
    ver_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(ver_dirs)
}

fp_all_outage_ver_dirs = function(fp_dir, prod_id=NULL,proc_id=NULL, search_file = if (need_backup) "outage_pru.Rds" else "has_outage.txt", need_backup=TRUE) {
  restore.point("fp_all_outage_ver_dirs")
  fp_all_ver_dirs(fp_dir, prod_id, proc_id, search_file)
}



fp_all_error_ver_dirs = function(fp_dir, prod_id=NULL,proc_id=NULL, search_file = if (need_backup) "error_pru.Rds" else "has_error.txt",  need_backup=FALSE) {
  fp_all_ver_dirs(fp_dir, prod_id, proc_id, search_file)
}

fp_ver_dir_ok = function(ver_dir, search_file = "prod_df.Rds") {
  file.exists(file.path(ver_dir, search_file))
}

fp_all_ok_ver_dirs = function(fp_dir, prod_id=NULL,proc_id=NULL, search_file = "prod_df.Rds") {
  fp_all_ver_dirs(fp_dir, prod_id, proc_id, search_file)
}



fp_all_ver_dirs = function(fp_dir, prod_id=NULL,proc_id=NULL, search_file = "prod_df.Rds", strict_fp_dir=FALSE) {
  restore.point("fp_all_ver_dirs")
  parent_dir = fp_dir
  if (!is.null(prod_id) & strict_fp_dir) parent_dir = file.path(fp_dir, prod_id)
  if (!is.null(prod_id) & !is.null(proc_id) & strict_fp_dir) parent_dir = file.path(fp_dir, proc_id)

  if (!is.null(search_file)) {
    files = list.files(parent_dir, glob2rx(search_file),full.names = TRUE, recursive=TRUE)
    ver_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  if (!is.null(prod_id) & !strict_fp_dir) {
    prod_ids = fp_ver_dir_to_prod_id(ver_dirs)
    ver_dirs = ver_dirs[prod_ids %in% prod_id]
  }
  if (!is.null(proc_id) & !strict_fp_dir) {
    proc_ids = fp_ver_dir_to_proc_id(ver_dirs)
    ver_dirs = ver_dirs[proc_ids %in% proc_id]
  }
  unique(ver_dirs)
}


fp_all_ver_info = function(fp_dir, prod_id=NULL,proc_id=NULL, search_file = "prod_df.Rds", strict_fp_dir=FALSE) {
  restore.point("fp_all_ver_info")
  if (!is.null(prod_id) & strict_fp_dir) fp_dir = file.path(fp_dir, prod_id)
  if (!is.null(prod_id) & !is.null(proc_id) & strict_fp_dir) fp_dir = file.path(fp_dir, proc_id)

  if (!is.null(search_file)) {
    files = list.files(fp_dir, glob2rx(search_file),full.names = TRUE, recursive=TRUE)
    if (NROW(files)==0) return(NULL)

    ver_dirs = dirname(files)
    if (!is.null(prod_id) & !strict_fp_dir) {
      prod_ids = fp_ver_dir_to_prod_id(ver_dirs)
      ver_dirs = ver_dirs[prod_ids %in% prod_id]
      files = files[prod_ids %in% prod_id]
    }
    if (!is.null(proc_id) & !strict_fp_dir) {
      proc_ids = fp_ver_dir_to_proc_id(ver_dirs)
      ver_dirs = ver_dirs[proc_ids %in% proc_id]
      files = files[proc_ids %in% proc_id]
    }
    if (NROW(ver_dirs)==0) return(NULL)

    df = fp_ver_dir_to_ids(ver_dirs) %>%
      mutate(
        mtime = file.mtime(files),
        prod_df_file = files,
        prod_df_mb = file.size(files) / 1e6
      )
    return(df)

  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
}


fp_rerun_error_ver = function(ver_dir, pru_file = file.path(ver_dir, "error_pru.Rds")) {
  if (!file.exists(pru_file)) {
    cat("\n", pru_file, " does not exist. Cannot re-run.\n")
  }
  pru = readRDS(pru_file)
  pru_rerun(pru)
}

fp_rerun_outage_ver = function(ver_dir,pru_file = file.path(ver_dir, "outage_pru.Rds")) {
  fp_rerun_error_ver(ver_dir, pru_file)
}

fp_rerun_all_outage_ver = function(fp_dir, prod_id=NULL,proc_id=NULL, ver_dirs = fp_outage_ver_dirs(fp_dir, prod_id, proc_id,need_backup = TRUE)) {
  restore.point("fp_rerun_all_outage_ver")
  if (length(ver_dirs)==0) return("\nNo outage version found.")
  for (ver_dir in ver_dirs) {
    fp_rerun_outage_ver(ver_dir)
  }
}

fp_rerun_all_error_ver = function(fp_dir, prod_id=NULL,proc_id=NULL, ver_dirs = fp_error_ver_dirs(fp_dir, prod_id, proc_id,need_backup = TRUE)) {
  restore.point("fp_rerun_all_outage_ver")
  if (length(ver_dirs)==0) return("\nNo outage version found.")
  for (ver_dir in ver_dirs) {
    fp_rerun_error_ver(ver_dir)
  }
}

