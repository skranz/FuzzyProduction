# Functions that help to pick input versions


fp_has_input = function(fp_dir,prod_id, proc_id=NULL) {
  dirs = fp_all_ok_ver_dirs(fp_dir, prod_id,proc_id)
  length(dirs) > 0
}


# Pick input versions and create / update pru$input_infos
pru_pick_inputs = function(pru, prod_ids, pref=fp_input_pref(), load=TRUE) {
  restore.point("pru_pick_input_version")
  fp_dir = pru$fp_dir

  input_infos = lapply(prod_ids, function(prod_id) {
    fp_pick_input(fp_dir, prod_id, pref)
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

example = function() {
  project_dir = "~/repbox/projects_share/aejapp_1_2_7"
  fp_dir = file.path(project_dir, "rai", "prod_runs")
  fp_pick_input(fp_dir, "tab_tino")
}

# create an input preference: to do will be extended
fp_input_pref = function(order_by = c("proc_id","ver_ind","younger"), proc_id = NULL, ver_ind=NULL) {
  list(order_by=order_by, proc_id=proc_id, ver_ind=ver_ind)

}

fp_pick_input = function(fp_dir, prod_id, pref=fp_input_pref()) {
  restore.point("fp_pick_input")
  prod_dir = file.path(fp_dir, prod_id)

  df = fp_all_ver_info(fp_dir, prod_id,proc_id = pref$pref_proc)
  if (NROW(df)==0) {
    return(fp_err_obj(paste0("\nNo version of product ", prod_id, " found in ", fp_dir,"\n")))
    return(NULL)
  }

  match_score = function(ver_val, pref_val) {
    if (length(pref_val)==0) {
      return(rep(0, length(ver_val)))
    }
    -na_val(match(ver_val, pref_val), length(ver_val)+1)
  }

  df$score_proc_id = match_score(df$proc_id, pref$proc_id)
  df$score_ver_ind = match_score(df$ver_ind, pref$ver_ind)
  df$score_younger = as.numeric(df$mtime) / 1e6

  arrange_cols = paste0("score_",pref$order_by)
  df_sorted <- df %>%
    arrange(across(all_of(arrange_cols), desc))
  df_sorted[1,]
}

