example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_4"
  fp_dir = file.path(project_dir,"fp")

  fp_all_proc_id(fp_dir,"cell_base")
  prod_id = "cell_base"
  proc_id = "tab_html_hx_pdf"
  fp_newest_ver_dir(fp_dir, prod_id, proc_id)


  df = fp_all_ver_info(fp_dir)

  df = fp_load_all_ver_df(fp_dir, "cell_base")
}

fp_load_newest_ver_df = function(fp_dir, prod_id, proc_id, add_ids=TRUE) {
  ver_dir = fp_newest_ver_dir(fp_dir, prod_id, proc_id)
  fp_load_ver_df(ver_dir,add_ids=add_ids)
}

fp_newest_ver_dir = function(fp_dir, prod_id, proc_id=NULL) {
  info = fp_all_ver_info(fp_dir, prod_id=prod_id, proc_id=proc_id)
  info %>%
    group_by(prod_id, proc_id) %>%
    arrange(desc(mtime)) %>%
    slice(1) %>%
    pull(ver_dir)
}

fp_load_all_ver_df = function(fp_dir, prod_id, add_ids=TRUE, as_df = TRUE) {
  restore.point("fp_load_all_ver_df")
  ver_dirs = fp_all_ver_dirs(fp_dir, prod_id, only_success = TRUE)
  df_li = lapply(ver_dirs, fp_load_ver_df, add_ids=add_ids)
  if (!as_df) return(df_li)
  df = bind_rows(df_li)
  df
}


fp_ver_dir_to_ids = function(ver_dir) {
  dname = dirname(ver_dir)
  proc_id = basename(dname)
  prod_id = basename(dirname(dname))
  ver_id = paste0(proc_id,"--", basename(ver_dir))
  data.frame(prod_id=prod_id, proc_id=proc_id, ver_id=ver_id, ver_dir=ver_dir)
}

fp_load_ver_df = function(ver_dir = ver_dir, ver_df=NULL, add_ids=FALSE) {
  restore.point("fp_load_ver_df")
  if (!is.null(ver_df)) return(ver_df)
  ver_df = readRDS(file.path(ver_dir, "ver_df.Rds"))
  if (add_ids) {
    id_df = fp_ver_dir_to_ids(ver_dir)
    ver_df = add_col_left(ver_df, proc_id=id_df$proc_id, ver_id=id_df$ver_id)
  }
  ver_df

}

fp_all_prod_id = function(fp_dir) {
  list.dirs(fp_dir,full.names = FALSE,recursive = FALSE)
}


fp_all_proc_id = function(fp_dir, prod_id=NULL) {
  fp_dir = file.path(fp_dir,"fp","prod_vers")
  if (is.null(prod_id)) {
    prod_id_dirs = list.dirs(fp_dir,full.names = TRUE,recursive = FALSE)
  } else {
    prod_id_dirs = file.path(fp_dir, prod_id)
  }
  list.dirs(prod_id_dirs,full.names = FALSE,recursive = FALSE)
}


fp_all_proc_dir = function(fp_dir, prod_id, only_success=TRUE) {
  fp_dir = file.path(fp_dir,"fp","prod_vers",prod_id)
  if (only_success) {
    files = list.files(fp_dir, glob2rx("ver_df.Rds"),full.names = TRUE, recursive=TRUE)
    ver_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(ver_dirs)
}


fp_all_ver_dirs = function(fp_dir, prod_id=NULL,proc_id=NULL, only_success=TRUE) {
  fp_dir = file.path(fp_dir,"fp","prod_vers")
  if (!is.null(prod_id)) parent.dir = file.path(fp_dir, prod_id)
  if (!is.null(prod_id) & !is.null(proc_id)) parent.dir = file.path(fp_dir, proc_id)

  if (only_success) {
    files = list.files(fp_dir, glob2rx("ver_df.Rds"),full.names = TRUE, recursive=TRUE)
    ver_dirs = dirname(files)
  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
  unique(ver_dirs)
}


fp_all_ver_info = function(fp_dir, prod_id=NULL,proc_id=NULL, only_success=TRUE) {
  restore.point("fp_all_ver_info")
  if (!is.null(prod_id)) fp_dir = file.path(fp_dir, prod_id)
  if (!is.null(prod_id) & !is.null(proc_id)) fp_dir = file.path(fp_dir, proc_id)

  if (only_success) {
    files = list.files(fp_dir, glob2rx("ver_df.Rds"),full.names = TRUE, recursive=TRUE)
    ver_dirs = dirname(files)
    df = fp_ver_dir_to_ids(ver_dirs) %>%
      mutate(
        mtime = file.mtime(files),
        ver_df_file = files,
        ver_df_mb = file.size(files) / 1e6
      )
    return(df)

  } else {
    stop("Not yet implemented with only_success = FALSE")
  }
}



