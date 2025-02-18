example = function() {
  prods = fp_prods(
    fp_prod("tab_base",
      fields = fp_fields(
        fp_field("artid", key=TRUE),
        fp_field("tabid", "Table 1", key=TRUE),
        fp_field("tabnum", 1L)
      )
    ),
    fp_prod("tab_title_note",
      widens = "tab_base", # means same keys, extends fields
      fields = fp_fields(
        fp_field("tabtitle", "Table 1: Regression analysis"),
        fp_field("tabnote", "")
      )
    ),
    fp_prod("tab_html",
      widens = "tab_title_note",
      fields = fp_fields(
        fp_field("tabhtml", type="html_table", descr="Normalized HTML table with cellid, row and col definitions.")
      )
    )
  )

}


fp_prods = function(...) {
  prods = list(...)
  names(prods) = sapply(prods, function(prod) prod$pid)

  for (i in seq_along(prods)) {
    prod = prods[[i]]

    if (!is.null(prod$parent)) {
      pprod = prods[[prod$parent]]
      keys = setdiff(pprod$keys, names(prod$fields))
      prod$fields = c(pprod$fields[keys], prod$fields)
    } else if (!is.null(prod$widens)) {
      pprod = prods[[prod$widens]]
      add_fields = setdiff(pprod$vars, prod$vars)
      prod$fields = c(pprod$fields[add_fields], prod$fields)
    }
    is_key = sapply(prod$fields, function(field) field$key)
    prod$keys = prod$vars[is_key]
    prods[[i]] = prod
  }
  prods
}


fp_fields = function(...) {
  fields = list(...)
  names(fields) = sapply(fields, function(field) field$var)
  fields
}


fp_field = function(var, example="",type="", key=FALSE, descr="") {
  list(var=var, class=class(example)[1],type=type, key=key, descr=descr, example=example)
}

fp_prod = function(pid, fields, widens=NULL, parent=NULL) {
  list(pid=pid, fields=fields, vars=names(fields), widens = widens, parent = parent)
}
