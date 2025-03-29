# Functions that help to pick and load particular versions of a product

example = function() {
  parent_dir =  "~/repbox/projects_share"

  project_dirs = repboxExplore::get_project_dirs(parent_dir)
  res_df = fp_pick_prod_ver(parent_dir, c("readme_var", "readme_overview"))
  res_df = fp_pick_prod_ver(project_dirs, "readme_var")

  pref = fp_pref(proc_regex = c("_mocr$"))
  ver_df = fp_pick_prod_ver(project_dirs, "tab_html", pref = pref)
  ver_df

  df = fp_pick_and_load_prod_df(project_dirs,"readme_overview")
}

#' Picks a single product version
#'
#' Allows for fp_dir to be a large directory with multiple
fp_pick_prod_ver = function(parent_dir, prod_id, proc_id=NULL, pref=fp_pref(), return_score_df = FALSE) {
  restore.point("fp_pick_prod_ver")
  pref = fp_normalize_pref(pref)
  #prod_dir = file.path(fp_dir, prod_id)

  df = fp_all_ver_info(parent_dir, prod_id,proc_id = proc_id)
  if (NROW(df)==0) {
    return(fp_err_obj(paste0("\nNo version of product ", prod_id, " found in ", fp_dir,"\n")))
    return(NULL)
  }
  df$prod_dir = fp_ver_dir_to_prod_dir(df$ver_dir)

  match_score = function(ver_val, pref_val) {
    if (length(pref_val)==0) {
      return(rep(0, length(ver_val)))
    }
    -na_val(match(ver_val, pref_val), length(ver_val)+1)
  }

  if (!is.null(pref$proc_regex)) {
    for (regex in pref$proc_regex) {
      new_ids =  df$proc_id[stri_detect_regex(df$proc_id, regex)]
      pref$proc_id = union(pref$proc_id,new_ids)
    }
  }

  df$score_proc_id = match_score(df$proc_id, pref$proc_id)
  df$score_ver_ind = match_score(df$ver_ind, pref$ver_ind)
  df$score_younger = as.numeric(df$mtime) / 1e6

  arrange_cols = paste0("score_",pref$order_by)
  df_sorted <- df %>%
    group_by(prod_dir) %>%
    arrange(across(all_of(arrange_cols), desc))
  if (return_score_df) return(ungrouo(df_sorted))

  res_df = df_sorted %>%
    slice(1) %>%
    ungroup()
  res_df
}



fp_pick_and_load_prod_df = function(parent_dir, prod_id,proc_id=NULL, pref=fp_pref(),add_ids=TRUE, extra_cols=NULL, add_fp_dir=TRUE) {
  restore.point("fp_pick_and_load_prod_df")
  ver_df =fp_pick_prod_ver(parent_dir, prod_id, proc_id, pref)
  if (NROW(ver_df)==0) return(NULL)
  df_li = lapply(seq_len(NROW(ver_df)), function(i) {
    ver_dir = ver_df$ver_dir[[i]]
    df = fp_load_prod_df(ver_dir, add_ids=add_ids, extra_cols=extra_cols)
    if (add_fp_dir) {
      fp_dir = fp_ver_dir_to_fp_dir(ver_dir)
      df = add_col_left(df, fp_dir=fp_dir)
    }
    df
  })
  bind_rows(df_li)
}


fp_load_newest_prod_df = function(fp_dir, prod_id, proc_id, add_ids=TRUE) {
  ver_dir = fp_newest_ver_dir(fp_dir, prod_id, proc_id)
  fp_load_prod_df(ver_dir,add_ids=add_ids)
}


#' create a product version preference
#'
#' TO DO: expand function in future
fp_pref = function(order_by = c("proc_id","ver_ind","younger"), proc_id = NULL, ver_ind=NULL, proc_regex=NULL) {
  pref = list(order_by=order_by, proc_id=proc_id, ver_ind=ver_ind, proc_regex=proc_regex)
  class(pref) = c("fp_pref","list")
  pref
}

fp_normalize_pref = function(pref) {
  if (is.character(pref)) {
    pref = fp_pref(proc_regex = pref)
  }
  pref
}

fp_has_prod = function(fp_dir,prod_id, proc_id=NULL) {
  dirs = fp_all_ok_ver_dirs(fp_dir, prod_id,proc_id)
  length(dirs) > 0
}


# Pick input versions and create / update pru$input_infos
pru_pick_inputs = function(pru, prod_ids, pref=fp_pref(), load=TRUE) {
  restore.point("pru_pick_input_version")
  fp_dir = pru$fp_dir

  input_infos = lapply(prod_ids, function(prod_id) {
    fp_pick_prod_ver(fp_dir, prod_id, pref)
  })
  names(input_infos) = prod_ids
  if (is.null(pru$input_infos)) {
    pru$input_infos = input_infos
  } else {
    pru$input_infos[names(input_infos)] = input_infos
  }
  if (load) pru = pru_load_inputs(pru, prod_ids)
  pru
}

pru_has_input_err = function(pru) {
  if (fp_has_err_obj(pru$input$infos)) return(TRUE)
  #if (fp_has_err_obj(pru$inputs)) return(TRUE)
  return(FALSE)
}

pru_get_input = function(pru, prod_id)  {
  pru$inputs[[prod_id]]
}

pru_load_inputs = function(pru, prod_ids=names(pru$input_infos), overwrite=FALSE) {
  if (is.null(pru$inputs)) {
    pru$inputs = list()
  }
  if (!overwrite) {
    prod_ids = setdiff(prod_ids, names(pru$inputs))
  }
  for (prod_id in prod_ids) {
    info = pru$input_infos[[prod_id]]
    if (is.null(info)) stop(paste0("No input version for ", prod_id, " had been picked. First call pru_pick_input"))
    pru$inputs[[prod_id]] = readRDS(info$prod_df_file)
  }
  pru
}

