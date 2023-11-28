extract_var_name <- function(string) {
  string %>%
    stringr::str_extract(stringr::str_c(tag, "var\\..*"))%>%
    stringr::str_remove(stringr::str_c(tag, "var."))
  #if_else(.=="", NA_character_, .)
}

extract_table_name <- function (string) {
  string %>%
    stringr::str_remove(stringr::str_c("^", tag, "tbl\\.")) %>%
    stringr::str_remove(stringr::str_c(tag, ".*$")) %>%
    dplyr::if_else(.=="", NA_character_, .)
}

extract_col_name <- function(string) {
  string %>%
    stringr::str_extract(stringr::str_c(tag, "col\\..*"))%>%
    stringr::str_remove(stringr::str_c(tag, "col."))
  #if_else(.=="", NA_character_, .)
}
