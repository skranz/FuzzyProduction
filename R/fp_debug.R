fp_backup_last_run_pru = function(pru=NULL, backup_loc=NULL) {
  pru$backup_loc = backup_loc
  options(fp_last_run_pru = pru)
}

fp_get_backup_last_run_pru = function() {
  pru = getOption("fp_last_run_pru")
  if (!is.null(pru$backup_loc)) {
    cat("\npru backup location: ", pru$backup_loc, "\n")
  }
  pru
}
