example = function() {
  prods = fp_prods(
    fp_prod("tab_base",
      list(
        artid = schema_str(is_key=TRUE),
        tabid = schema_str(is_key=TRUE),
        tabnum = schema_int()
      )
    )
  )

  prods = fp_prods(
    fp_prod("tab_base",
      fields = fp_fields(
        artid = schema_str(key=TRUE),
        tabid = schema_str(key=TRUE),
        tabnum = schema_int()
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

fp_prod_to_schema = function(prod) {
  schema_object(prod$fields)
}

fp_prods = function(...) {
  prods = list(...)
  restore.point("fp_prods")
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
    is_key = sapply(prod$fields, function(field) isTRUE(field$is_key))
    prod$keys = prod$vars[is_key]
    prods[[i]] = prod
  }
  prods
}

fp_prod = function(pid, fields, widens=NULL, parent=NULL) {
  fields = lapply(fields, function(field) {
    field$is_key = first.non.null(field$is_key, FALSE)
    field$parse = first.non.null(field$parse, TRUE)
    field
  })
  list(pid=pid, fields=fields, vars=names(fields), widens = widens, parent = parent)
}
