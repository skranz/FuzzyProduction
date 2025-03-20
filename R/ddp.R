# Directly derived products
# Example: cell_list from tab_html
#
# Each run of tab_html can be deterministically transformed into a cell_list
# We use the same version and runid for directly derived products
#
# Below are helper functions




#' Sometimes we make a deterministic transformation step, like
#' tab_html to cell_list
#' We then may want to simply keep the version and run names of
#' the original run and just change the product.
ddp_get_ver_dir = function(ver_dir, ddp_prod_id, ddp_proc_id = NULL) {
  restore.point("ddp_get_ver_dir")
  ver_id = basename(ver_dir)
  if (is.null(ddp_proc_id)) {
    ddp_proc_id = basename(dirname(ver_dir))
  }
  fp_dir = fp_ver_dir_to_fp_dir(ver_dir)
  ddp_ver_dir = file.path(fp_dir,ddp_prod_id, ddp_proc_id, ver_id)
}

ddp_is_up_to_date = function(ver_dir, ddp_prod_id) {
  restore.point("ddp_is_up_to_date")
  ddp_ver_dir = ddp_get_ver_dir(ver_dir, ddp_prod_id)
  if (!dir.exists(ddp_ver_dir)) return(FALSE)
  ddp_file = file.path(ddp_ver_dir,"prod_df.Rds")
  if (!file.exists(ddp_file)) return(FALSE)

  file = file.path(ver_dir, "prod_df.Rds")
  if (isTRUE(file.mtime(ddp_file) >= file.mtime(file))) return(TRUE)
  return(FALSE)
}

#' Sometimes we make a deterministic transformation step, like
#' tab_html to cell_list
#' We then may want to simply keep the version and run names of
#' the original run and just change the product.
ddp_init_pru = function(pru, ddp_prod_id, prods = repbox_prods(), ver_dir=pru$ver_dir, prod_id = first_nn(pru$prod_id,fp_ver_dir_to_prod_id(ver_dir)), ddp_proc_id=NULL) {
  restore.point("ddp_init_pru")
  if (is.null(ver_dir)) stop("You must provide at least ver_dir (original ver_dir). Or the complete original pru / rais object.")
  input_info = data.frame(prod_id=prod_id, ver_dir = ver_dir, found=TRUE, num_cand = 1)
  ddp_prod = prods[ddp_prod_id]

  fp_dir = dirname(dirname(dirname(ver_dir)))
  ddp_ver_dir = ddp_get_ver_dir(ver_dir, ddp_prod_id, ddp_proc_id)
  ddp_pru = pru_init(fp_dir = fp_dir, prod_id=ddp_prod_id, input_info=input_info, ver_dir=ddp_ver_dir)
  ddp_pru
}


#' Derives all ddp instances
#'
#' Will be typically called from a wrapper function like \code{hx_all_tab_html_to_cell_list}
#'
ddp_derive_all_instances = function(parent_dir,from_prod_id, to_prod_id, convert_fun,  overwrite=FALSE) {
  restore.point("ddp_derive_all_instances")
  from_files = list.files(parent_dir, glob2rx("prod_df.Rds"),full.names = TRUE, recursive=TRUE)
  file = from_files[1]
  for (file in from_files) {
    ver_dir = dirname(file)
    if (!overwrite) {
      if (ddp_is_up_to_date(ver_dir, to_prod_id)) next
    }
    convert_fun(ver_dir = ver_dir)
  }
}

