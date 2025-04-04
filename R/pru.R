#' Initialize a production run
pru_init = function(fp_dir,prod_id, proc_id=proc_info$proc_id, to_v0=TRUE, ver_dir=NULL,proc_info=NULL, ..., pru=NULL) {
  if (is.null(pru)) {
    pru = list(fp_dir=fp_dir,prod_id=prod_id, proc_id=proc_id, to_v0=to_v0, proc_info=proc_info, ...)
  } else {
    copy.into.env(pru)
  }

  restore.point("pru_init")
  pru = pru_init_dirs(pru, ver_dir=ver_dir)

  if (length(pru$proc_dir)==0) stop("proc_dir not correctly specified.")
  pru
}

pru_init_dirs = function(pru, ver_dir=pru[["ver_dir"]], to_v0 = pru[["to_v0"]]) {
  restore.point("pru_init_dirs")
  fp_dir = pru$fp_dir
  proc_id = pru$proc_id
  prod_id = pru$prod_id

  if (is.null(ver_dir)) {
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
  } else {
    pru$proc_dir = proc_dir = dirname(ver_dir)
    pru$proc_id = basename(pru$proc_dir)
    pru$ver_dir = ver_dir
    pru$ver_id = basename(ver_dir)
    pru$ver_ind = as.integer(stri_sub(pru$ver_id, 2))
  }
  pru
}

# Save a temporary pru object for debugging purposes
temp_pru_save = function(pru) {
  if (!dir.exists(pru$ver_dir)) dir.create(pru$ver_dir, recursive = TRUE)
  saveRDS(pru, file.path(pru$ver_dir, "temp_pru.Rds"))
}

pru_save = function(pru, prod_df, save_fields=c("proc_info","issues"), save_pru=TRUE) {
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
  pru$prod_id = bp_prod$prod_id
  if (!is.null(pru$proc_info)) {
    pru$proc_info$prod_id = pru$prod_id
  }
  # proc_id and ver_ind stays same
  pru$proc_dir = file.path(pru$fp_dir,pru$prod_id, pru$proc_id)
  if (length(pru$proc_dir)==0) stop("pru$proc_dir not well defined")
  pru$ver_dir = paste0(pru$proc_dir, "/v", pru$ver_ind)
  bp = pru_save(pru,bp_prod_df)
  invisible(bp)
}


pru_next_stage = function(pru, stage_fn) {
  restore.point("pru_next_stage")
  pru$cur_stage = stage_fn
  pru$items = NULL
  pru$outage_inds = pru$item_status = pru$item_cat = pru$num_outage = pru$num_error =  pru$num_ok  = pru$all_ok = NULL
  do.call(stage_fn, list(pru))
}


pru_make_items = function(pru, run_fun, num_items=NROW(df), fill_outage=TRUE, backup_outage=TRUE, verbose = TRUE, df=NULL, cont_fine_statuss = c("ok","no_status")) {
  restore.point("pru_make_items")
  items = pru[["items"]]
  if (is.null(items)) items = vector("list", num_items)

  if(fill_outage & length(pru$outage_inds)>0) {
    broad_statuss = sapply(items, fp_broad_status)
    inds = which(broad_statuss != "ok")
  } else {
    inds = seq_along(items)
  }
  start_time = as.numeric(Sys.time())
  if (verbose) cat(paste0("\nRun ",NROW(inds)," AI calls "))
  ind = 1
  for (ind in inds) {
    item = run_fun(ind, pru)
    items[[ind]] = item
    status = fp_fine_status(item)
    if (verbose) {
      if (status %in% c("ok")) {
        cat(".")
      } else if (status == "no_status") {
        cat("?")
      } else {
        cat(paste0("\n  item ",ind, ": ", status,"\n"))
      }
    }
    if (!status %in% cont_fine_statuss) {
      break
    }
  }
  pru$items = items
  restore.point("pru_run_all_rai_post")
  if (verbose) cat(paste0(" ", round(as.numeric(Sys.time())-start_time), " sec.\n"))
  pru
}


pru_add_issue = function(pru, type,details="") {
  issue = tibble(type=type, details=details)
  pru$issues = c(pru$issues, list(issue))
  pru
}

pru_rerun = function(pru) {
  restore.point("pru_rerun")
  if (!is.null(pru$cur_stage)) {
    return(pru_next_stage(pru, pru$cur_stage))
  }
  if (is.null(pru[["fun_call"]])) {
    cat("\nNo stage nor fun_call object stored in pru. Cannot re-run it.")
    pru
  }
  run_preserved_call(pru$fun_call)
}
