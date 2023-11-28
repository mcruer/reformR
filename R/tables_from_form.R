tables_from_form <- function (form, template) {

  table_info <- template %>%
    gplyr::pull_cell(table_info)


  column_info <- template %>%
    gplyr::pull_cell (column_info)

  unnamed_tables <- purrr::pmap(
    table_info %>%
      dplyr::select(-table.name),
    ~ pull_unnamed_table(form, ..1, ..2, ..3, ..4, ..5)
  ) %>%
    purrr::set_names(table_info$table.name %>% unique)

  apply_column_names <- function (table, table_name, column_info){
    column_info <- column_info %>%
      dplyr::filter(table.name == table_name)

    table %>%
      dplyr::select(column_info$col.start) %>%
      purrr::set_names(column_info$column.name)
  }

  unnamed_tables %>%
    purrr::imap(apply_column_names, column_info)
}
