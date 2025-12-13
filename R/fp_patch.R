#' Get the proc_id for a revised version
#'
#' Increments the revision number in a process ID.
#' E.g., 'proc-id' -> 'r1_proc-id', 'r1_proc-id' -> 'r2_proc-id'.
#'
#' @param base_proc_id The process ID of the base version.
#' @return A character string with the new process ID for the revised version.
#' @export
fp_get_revised_proc_id = function(base_proc_id) {
  restore.point("fp_get_revised_proc_id")
  if (stringi::stri_startswith_fixed(base_proc_id, "r") &&
      stringi::stri_detect_regex(base_proc_id, "^r[0-9]+_")) {

    rev_num_str = stringi::stri_extract_first_regex(base_proc_id, "[0-9]+")
    rev_num = as.integer(rev_num_str) + 1

    rest = stringi::stri_sub(base_proc_id, from = nchar(rev_num_str) + 3)

    return(paste0("r", rev_num, "_", rest))
  } else {
    return(paste0("r1_", base_proc_id))
  }
}

#' Create a revised product version by applying a patch via group replacement
#'
#' This function creates a new, revised product version by applying a patch.
#' It works by replacing entire groups of rows from the base data frame with
#' corresponding groups from the patch data frame, based on a key column.
#' This method is suitable when a patch can change the number of rows within a group.
#'
#' @param base_ver_dir The version directory of the original product.
#' @param patch_ver_dir The version directory of the product containing corrections.
#' @param revised_ver_dir The directory where the new, revised version will be saved.
#' @param key_col The column name to group by for patching (e.g., "regid").
#' @return The path to the directory of the newly created revised version, invisibly.
#' @export
fp_create_revised_version = function(base_ver_dir, patch_ver_dir, revised_ver_dir, key_col) {
  restore.point("fp_create_revised_version")

  # 1. Load data
  base_df = fp_load_prod_df(base_ver_dir)
  patch_df = fp_load_prod_df(patch_ver_dir)

  if (!key_col %in% names(base_df) || !key_col %in% names(patch_df)) {
    stop(paste0("Key column '", key_col, "' not found in both data frames."))
  }

  # 2. Handle patch counter (.times_patched)
  if (!".times_patched" %in% names(base_df)) {
    base_df$.times_patched = 0L
  }

  base_counts = base_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_col))) %>%
    dplyr::summarize(.old_patched_count = max(.data$.times_patched, 0L, na.rm = TRUE), .groups = "drop")

  patch_df_with_counts = patch_df %>%
    dplyr::left_join(base_counts, by = key_col) %>%
    dplyr::mutate(
      .times_patched = tidyr::replace_na(.data$.old_patched_count, 0L) + 1L
    ) %>%
    dplyr::select(-.data$.old_patched_count)

  # 3. Create revised data frame by replacing patched parts
  keys_to_patch = unique(patch_df[[key_col]])
  base_df_unpatched_part = base_df %>%
    dplyr::filter(!.data[[key_col]] %in% keys_to_patch)

  revised_df = dplyr::bind_rows(base_df_unpatched_part, patch_df_with_counts)

  # 4. Save revised version
  if (!dir.exists(revised_ver_dir)) {
    dir.create(revised_ver_dir, recursive = TRUE)
  }

  fp_save_prod_df(revised_df, revised_ver_dir)

  # Save metadata for provenance
  revision_info = list(
    type = "revised_version_by_group_replace",
    base_ver_dir = base_ver_dir,
    patch_ver_dir = patch_ver_dir,
    key_col = key_col,
    revision_time = Sys.time()
  )
  saveRDS(revision_info, file.path(revised_ver_dir, "revision_info.Rds"))

  cat("\nSuccessfully created revised version in:", revised_ver_dir, "\n")
  return(invisible(revised_ver_dir))
}
