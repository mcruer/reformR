variable_info <- function (raw_template) {
  tags (raw_template) %>%
    dplyr::filter(!is.na(var.name))
}

