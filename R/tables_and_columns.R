tables_and_columns <- function (raw_template) {

  table.lengths <- tags (raw_template) %>%
    dplyr::filter(!is.na(table.name)|is.table.end) %>%
    dplyr::arrange(col.start, row.start) %>%
    dplyr::group_by(sheet_name, col.start) %>%
    dplyr::mutate(table.end.row = dplyr::lead(row.start)) %>%
    dplyr::filter(!is.na(table.name)) %>%
    dplyr::ungroup () %>%
    dplyr::select (sheet_name, table.name, table.end.row)

  table_info <- tags (raw_template) %>%
    dplyr::filter(!is.na(column.name)) %>%
    #CProg::filter_out_na(column.name) %>%
    dplyr::arrange(row.start, col.start) %>%
    tidyr::fill(table.name, .direction = "down") %>%
    dplyr::select(sheet_name, row.start, col.start, table.name, column.name) %>%
    dplyr::left_join(table.lengths) %>%
    dplyr::group_by(sheet_name, table.name) %>%
    #mutate(col.max = max(col.start)) %>% peek
    dplyr::summarise(
      row.start = mean(row.start),
      col.end = max(col.start),
      col.start = min(col.start),
      row.end = mean(table.end.row),
    ) %>%
    dplyr::relocate(row.start, row.end, col.start, col.end, .after = table.name)

  column_info <- tags(raw_template) %>%
    dplyr::filter(!is.na(column.name)) %>%
    #CProg::filter_out_na(column.name) %>%
    dplyr::arrange(row.start, col.start) %>%
    tidyr::fill(table.name, .direction = "down") %>%
    dplyr::group_by(table.name) %>%
    dplyr::mutate(col.start = col.start %>% magrittr::subtract(min(col.start) - 1)) %>%
    #CProg::quickm(col.start, ~ .x %>% magrittr::subtract(min(.x) - 1)) %>%
    dplyr::select(col.start, table.name, column.name) %>%
    dplyr::ungroup()

  list("table_info" = table_info, "column_info" = column_info)
}
