# Helper function to deal with unreliable production stages
#
# 3 broad_status:
#
# "ok"
# "outage" e.g. due to rate limit. attempt again in a latter run
# "error"

pru_set_status = function(pru, items=pru[[items]],forbid_any_error=TRUE, forbid_any_outage=TRUE, save_pru_if_outage = TRUE, save_pru_if_error = TRUE) {
  restore.point("pru_set_status")
  fine_status = fp_fine_status(items)
  if (fine_status == "no_status" & isTRUE(is.list(items))) {
    all_broad_status = sapply(items, fp_broad_status)
  } else {
    all_broad_status = fp_fine_status_to_broad_status(fine_status)
  }
  pru$forbid_any_outage = forbid_any_outage
  pru$forbid_any_error = forbid_any_error


  n_all = length(all_broad_status)
  pru$num_error = sum(all_broad_status=="error")
  pru$num_outage = sum(all_broad_status=="outage")
  pru$num_ok = sum(all_broad_status=="ok")

  broad_status = "ok"
  if (n_all == 0) {
    broad_status = "ok" # empty is ok
  } else if ( (forbid_any_error & pru$num_error > 0) | pru$num_error == n_all) {
    broad_status = "error"
  } else if ((forbid_any_outage & pru$num_outage > 0)) {
    broad_status = "outage"
  } else if (pru$num_error + pru$num_outage == n_all) {
    broad_status = "outage"
  } else {
    broad_status = "ok"
  }
  pru$broad_status = broad_status
  file_remove_existing(file.path(pru$ver_dir,"has_outage.txt"))
  file_remove_existing(file.path(pru$ver_dir,"has_error.txt"))

  if (pru$num_outage > 0) {
    if (!dir.exists(pru$ver_dir)) dir.create(pru$ver_dir, recursive = TRUE)
    writeLines("outage", file.path(pru$ver_dir,"has_outage.txt"))
    if (save_pru_if_outage) {
      saveRDS(pru,file.path(pru$ver_dir,"outage_pru.Rds") )
      cat("Could not compute complete version ", pru$ver_dir, " due temporary service inavailability. Can be continued in another run.\n")
    }
  }
  if (broad_status == "error") {
    if (!dir.exists(pru$ver_dir)) dir.create(pru$ver_dir, recursive = TRUE)
    writeLines("error", file.path(pru$ver_dir,"has_error.txt"))
    if (save_pru_if_outage) {
      saveRDS(pru,file.path(pru$ver_dir,"error_pru.Rds") )
      cat("Could not compute complete version ", pru$ver_dir, " service invailabilities. Can be resumed in another run.\n")
    }
  }
  pru$broad_status = broad_status
  pru
}

pru_is_ok = function(pru) {
  !isTRUE(pru$broad_status != "ok")
}


fp_fine_status = function(x) {
  if (is.null(x) | length(x)==0) return("empty")
  first.non.null(x[["fine_status"]], attr(x, "fine_status"), "no_status")
}

fp_broad_status = function(x) {
  restore.point("fp_broad_status")
  broad_status = first.non.null(x[["broad_status"]], attr(x, "broad_status"))
  if (!is.null(broad_status)) return(broad_status)
  fp_fine_status_to_broad_status(fp_fine_status(x))
}


fp_fine_status_to_broad_status = function(fine_status) {
  case_when(
    fine_status %in% c("rate_limit","unavailable", "permission") ~ "outage",
    fine_status %in% c("error", "input_limit", "timeout") ~ "error",
    fine_status == "empty" ~ "empty",
    TRUE ~ "ok"
  )
}
