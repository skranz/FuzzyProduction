# Helper function to deal with unreliable production stages
#
# Mainly relevant if a stage makes multiple AI calls
# even if some calls fail e.g. due to rate limits
# we want to store successful results and continue
# incomplete call another time.

pru_next_stage = function(pru, stage_fn) {
  restore.point("pru_next_stage")
  pru$stage_fns = unique(c(pru$stage_fns, stage_fn))
  pru$cur_stage = stage_fn

  if (is.null(pru$stage_results)) {
    pru$stage_results = list()
  }
  if (is.null(pru$stage_results[[stage_fn]])) {
    pru$stage_results[[stage_fn]] = list()
  }
  do.call(stage_fn, list(pru))

}


pru_run_stage_items = function(pru, run_fun, num_items=pru$num_items, fill_incomplete=isTRUE(pru$is_incomplete), backup_incomplete=TRUE, verbose = TRUE) {
  restore.point("pru_run_all_rai")
  if (fill_incomplete & !is.null(pru$ai_li)) {
    rows = pru$incomplete_rows
  } else {
    rows = seq_len(pru$nrow)
  }
  start_time = as.numeric(Sys.time())
  if (verbose) cat(paste0("\nMake ",NROW(rows)," AI calls "))
  if (is.null(pru[["li"]])) {
    pru$ai_li = vector("list", pru$nrow)
  }
  for (row in rows) {
    pru$ai_li[[row]] = rai = run_fun(row, pru)
    pru$ai_li[[row]]
  }
  pru$ai_li = lapply(rows, function(row) {
    rai = run_fun(row, pru)
    if (!isTRUE(rai$status_code==200)) {
      if (verbose) cat("x")
    } else {
      if (verbose) cat(".")
    }
    rai
  })
  restore.point("pru_run_all_rai_post")
  if (verbose) cat(paste0(round(as.numeric(Sys.time())-start_time), " sec.\n"))
  pru$status_codes = sapply(pru$ai_li, function(rai) {
    if (is.null(rai)) return(-1)
    rai$status_code
  })
  pru$incomplete_rows = which(!is.true(pru$status_codes == 200))
  pru$complete_rows = which(is.true(pru$status_codes == 200))

  pru$num_incomplete = length(pru$incomplete_rows)
  pru$num_complete =  length(pru$complete_rows)

  incomplete_file = file.path(pru$run_dir, "incomplete_pru.Rds")
  if (backup_incomplete & pru$num_incomplete>0) {
    if (!dir.exists(pru$run_dir)) dir.create(pru$run_dir, recursive = TRUE)
    problem_codes = pru$status_codes[pru$status_codes != 200]
    saveRDS(pru,incomplete_file )

    cat(paste0("\n ", length(problem_codes), " of ", length(status_codes), "  AI calls returned problems (status code: ", paste(unique(status_codes), collapse=", "), "). Incomplete pru object saved to ", pru$run_dir))

  } else if (pru$num_incomplete == 0 & file.exists(incomplete_file)) {
    try(file.remove(incomplete_file))
  }
  pru
}
