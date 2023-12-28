tags <- function (raw_template) {
  raw_template %>%
    dplyr::rename(row.start = index) %>%
    tidyr::pivot_longer(cols = -c(sheet_name, row.start)) %>%
    dplyr::rename(col.start = name) %>%
    dplyr::mutate(col.start = stringr::str_sub(col.start, start = 2) %>%
                    as.numeric) %>%
    dplyr::rename (name = value) %>%
    dplyr::filter(stringr::str_detect(name, tag)) %>%
    dplyr::mutate(
      var.name = extract_var_name(name),
      table.name = extract_table_name(name),
      column.name = extract_col_name(name),
      is.table.end = stringr::str_detect(name, stringr::str_c(tag, "tblend"))
    ) %>%
    dplyr::select (-name) %>%
    dplyr::relocate(sheet_name, .before = 1)
}


