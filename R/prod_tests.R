
example = function() {
  run_dir = "~/repbox/projects_share/aejapp_1_2_4/rai/prod_runs/cell_base/tab_html_hx_pdf/r0"
  df = hx_load_prod_df(run_dir=run_dir)
  
}

prod_tests_run = function(prod_df, prod_tests, prod) {
  
}

tests_cell_base = function() {
  pid = "cell_base"
  
  prod_tests_define(
    flag_test_funs(
      test_cell_base_no_two_num,
      test_cell_base_expect_bracket_below
    ),
    descr = list(
      
    )
  )
  
  
}

test_cell_base_no_two_num = function(df,...) {
  df %>% mutate(
    has_deci_other = is.true(stri_detect_fixed(other_num_str, ".")),
    relevant_no_two_num = has_num,
    test_no_two_num = !is.na(other_num_str),
    relevant_no_two_deci = has_deci,
    test_no_two_deci =  (!is.na(other_num_str)) & (has_deci & has_deci_other)
  )
}

test_cell_base_expect_bracket_below = function(df,...) {
  df = df %>%
    group_by(col) %>%
    arrange(row) %>%
    mutate(has_below_bracket = lead(is.true(bracket != ""))) %>%
    group_by(row) %>%
    mutate(
      any_below_bracket = any(has_deci & has_below_bracket)
    ) %>% 
    ungroup() %>%
    mutate(
      relevant_expect_bracket_below = has_deci & any_below_bracket,
      test_expect_bracket_below = has_deci & any_below_bracket & !(has_below_bracket)
    )
}
