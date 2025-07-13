# Like fp_html_report in fp_report.R will make HTML report of fp_products

# But this report focuses on showing differences between versions of the same product.

# Key idea: only one table per product

# For each original row in some product
# Show base line results that are most common across versions
# show below divergent rows for some versions, only show divergence for columns
# that differ.

# How to build table:

# First determine for each column the communalty: the largest set ()


#' Generate an HTML report showing differences between product versions
#'
#' This function takes a set of version directories, groups them by product,
#' and for each product, generates a "diff" table comparing the different
#' versions. The final output is a single HTML file containing these diff tables.
#'
#' @param ver_dirs A character vector of version directories, typically from `fp_all_ok_ver_dirs`.
#' @param outfile The path for the output HTML file. If NULL, a default is created.
#' @param prods Optionally a list of product definitions (from `prods_define`) to determine the order and keys for products in the report.
#' @param max_rows The maximum number of rows to display for each diff table.
#' @param max_cols The maximum number of columns to display for each diff table.
#' @param title The title of the HTML report.
#' @return The path to the created `outfile`, invisibly.
fp_html_diff_report = function(ver_dirs, outfile=NULL, prods = NULL, max_rows=100, max_cols=5, title = "Fuzzy Production Difference Report") {
  restore.point("fp_html_diff_report")

  if (length(ver_dirs) == 0) {
    cat("No version directories provided to fp_html_diff_report.\n")
    return(invisible(NULL))
  }

  if (is.null(outfile)) {
    fp_dir = fp_ver_dir_to_fp_dir(ver_dirs[1])
    outfile = file.path(fp_dir, "fp_diff_report.html")
  }

  # 1. Get info for all ver_dirs and find unique products
  info_df = fp_ver_dir_to_ids(ver_dirs)
  all_prod_ids = unique(info_df$prod_id)

  # 2. Determine order of products for the report
  if (!is.null(prods)) {
    # Order by prods list, but only include products present in ver_dirs
    report_prod_ids = intersect(names(prods), all_prod_ids)
  } else {
    # Default to alphabetical order
    report_prod_ids = sort(all_prod_ids)
  }

  # 3. Start building HTML content
  css = fp_report_css()

  html_head = paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'><title>", .html_escape(title), "</title>",
    "<style>", css, "</style></head><body><h1>", .html_escape(title), "</h1>"
  )

  html_body_parts = c()

  # 4. Loop through products and generate diff tables
  for (prod_id in report_prod_ids) {
    html_body_parts = c(html_body_parts, paste0("<h2>Product: ", .html_escape(prod_id), "</h2>"))

    # Get ver_dirs for this product
    prod_info = dplyr::filter(info_df, .data$prod_id == !!prod_id)

    # Load all prod_df into a named list
    df_list = list()
    for (i in 1:NROW(prod_info)) {
      row = prod_info[i, ]
      ver_dir = row$ver_dir
      df = tryCatch({
        fp_load_prod_df(ver_dir)
      }, error = function(e) {
        warning("Could not load prod_df from ", ver_dir, ": ", e$message)
        return(NULL)
      })
      if (!is.null(df) && NROW(df) > 0) {
        # Name of the list element is the version identifier
        df_list[[row$ver_id]] = df
      }
    }

    if (length(df_list) < 2) {
      html_body_parts = c(html_body_parts, "<p><em>Not enough versions with data to compare for this product.</em></p>")
      next
    }

    # Determine key for diffing
    key = NULL
    if (!is.null(prods) && !is.null(prods[[prod_id]]$keys)) {
      key = prods[[prod_id]]$keys
    }

    # Generate diff table
    diff_df = tryCatch({
      fp_diff_tables(df_list, key = key)
    }, error = function(e) {
      warning("Could not generate diff table for ", prod_id, ": ", e$message)
      # Create a simple data frame with the error message
      data.frame(error = paste("Failed to generate diff table:", e$message))
    })

    # Truncate if necessary
    orig_rows = NROW(diff_df)
    orig_cols = NCOL(diff_df)

    df_display = diff_df
    truncated = FALSE
    trunc_msg = c()

    if (orig_rows > max_rows) {
      df_display = df_display[1:max_rows, , drop = FALSE]
      trunc_msg = c(trunc_msg, paste0("rows truncated to ", max_rows, " (from ", orig_rows, ")"))
      truncated = TRUE
    }
    if (orig_cols > max_cols) {
      df_display = df_display[, 1:max_cols, drop = FALSE]
      trunc_msg = c(trunc_msg, paste0("columns truncated to ", max_cols, " (from ", orig_cols, ")"))
      truncated = TRUE
    }

    # Generate table HTML
    table_html = .df_to_html_table(df_display)
    html_body_parts = c(html_body_parts, "<div class='table-container'>", table_html, "</div>")

    # Add truncation info message if needed
    if (truncated) {
        msg = paste0("<p class='trunc-info'>Note: ", paste(trunc_msg, collapse=", "), ".</p>")
        html_body_parts = c(html_body_parts, msg)
    }
  }

  html_foot = "</body></html>"

  # 5. Combine all parts and write to file
  final_html = paste(c(html_head, html_body_parts, html_foot), collapse="\n")
  writeLines(final_html, outfile)

  cat("HTML diff report written to:", outfile, "\n")
  invisible(outfile)
}

example = function() {
    # toy data: two versions of the small table in your screenshot
  mocr = tibble::tribble(
    ~tabid, ~otabid, ~tabtitle,
      1,     "001",  "Table 1—Baseline Characteristics",
      2,     "002",  "Table 2—Balancing Test at Baseline",
      3,     "003",  "Table 3—OLS Estimates on Profit at Different Time Periods (ANCOVA)"
  )

  pdf_txt = tibble::tribble(
    ~tabid, ~otabid, ~tabtitle,
      1,     "001",  "Table 1—Baseline Characteristics",
      2,     "002",  "Table 2 — Balancing Test at Baseline",   # note the extra spaces
      3,     "003",  "Table 3 — OLS Estimates on Profit at Different Time Periods (ANCOVA)"
  )

  library(tidyr)
  diff_df = fp_diff_tables(list(mocr = mocr, pdf_txt = pdf_txt))

}


# ---------- helper: detect a key ----------
fp_detect_df_li_key = function(df_list) {
  library(purrr)

  # step 1: common columns
  common_cols = reduce(map(df_list, names), intersect)
  if (length(common_cols) == 0) stop("No common columns across versions.")

  # step 2: score columns
  score_col = function(col) {
    map_dbl(df_list, function(df) {
      x = df[[col]]
      uniqueness = n_distinct(x, na.rm = TRUE) / nrow(df)
      completeness = 1 - mean(is.na(x))
      # Jaccard stability: overlap of value sets with first version
      stab = length(intersect(df_list[[1]][[col]], x)) /
             length(union   (df_list[[1]][[col]], x))
      mean(c(uniqueness, completeness, stab))
    }) %>% mean()
  }
  scores = setNames(map_dbl(common_cols, score_col), common_cols)
  ranked = names(sort(scores, decreasing = TRUE))

  # greedy search
  best = NULL
  for (k in seq_along(ranked)) {
    cand = ranked[seq_len(k)]
    unique_ok = map_lgl(df_list, ~ n_distinct(select(.x, all_of(cand))) == nrow(.x))
    if (mean(unique_ok) >= 0.8) { best = cand; break }
  }
  if (is.null(best)) best = "row_index"
  best
}

# create diff tables as data frame
# create diff tables as data frame
fp_diff_tables = function(df_list, key = NULL) {
  restore.point("fp_diff_tables")
  library(purrr)

  if (is.null(key)) key = fp_detect_df_li_key(df_list)

  # add row_index if needed
  df_list = imap(df_list, function(df, nm) {
    if ("row_index" %in% key) df = mutate(df, row_index = row_number())
    df$.version = nm
    df
  })

  df = do.call(bind_rows, df_list)
  org_cols = setdiff(names(df),".version")
  key_cols = key
  val_cols = setdiff(names(df), union(key_cols, ".version"))

  # Serialize columns to create a comparable key/value string.
  # This now handles list-columns by converting them to canonical JSON.
  serialize_for_compare = function(df_part) {
    if (length(names(df_part)) == 0) return(rep("", NROW(df_part)))
    s_cols <- lapply(df_part, function(col) {
      if (is.list(col)) {
        sapply(col, function(x) {
          if (is.null(x) || (is.list(x) && length(x) == 0) || (is.data.frame(x) && nrow(x) == 0)) return("[]")
          jsonlite::toJSON(x, auto_unbox = TRUE, sort = TRUE)
        })
      } else {
        as.character(col)
      }
    })
    do.call(stringi::stri_join, c(s_cols, list(sep = "|")))
  }

  df$.value = serialize_for_compare(df[, val_cols, drop = FALSE])
  df$.key = serialize_for_compare(df[, key_cols, drop = FALSE])
  df$.order = 1:NROW(df)

  df = df %>%
    group_by(.key) %>%
    mutate(.key_order = min(.order)) %>%
    ungroup()

  # Convert all columns to character for display, pretty-printing list-cols
  df_char = df
  for (col in org_cols) {
      if(is.list(df_char[[col]])) {
          df_char[[col]] = sapply(df_char[[col]], function(x) {
            if (is.null(x)) return(NA_character_)
            if ((is.list(x) && length(x) == 0) || (is.data.frame(x) && nrow(x) == 0)) return("[]")
            jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)
          })
      }
  }
  df = as.data.frame(lapply(df_char, as.character))


  diff_str = function(x) {
    x[x==x[1] & seq_along(x)>1]=""
    x
  }

  key_val_cols = c(key_cols, val_cols)

  diff_df = df %>%
    group_by(.key_order, .value) %>%
    mutate(
      .group_n = n(),
    ) %>%
    group_by(.key_order) %>%
    arrange(desc(.group_n), .value) %>%
    mutate(.in_order = 1:n()) %>%
    mutate(.differs = .value != first(.value)) %>%
    filter( (1:n())==1 | .differs) %>%
    mutate(across(all_of(c(key_cols,val_cols)), diff_str)) %>%
    ungroup() %>%
    arrange(.key_order, .in_order)

  diff_df = diff_df[, union(".version",org_cols)]

  diff_df
}

paste_cols = function(df, cols = names(df), sep="|") {
  if (length(cols) == 0) return(rep("", NROW(df)))
  do.call(stri_join,c(df[cols],list(sep="|")))
}
