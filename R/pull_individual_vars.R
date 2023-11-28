pull_individual_vars <- function(form, template) {
  template %>%
    gplyr::pull_cell(variable_info) %>%
    dplyr::select (1:4) %>%
    dplyr::mutate(value = list(sheet_name, row.start, col.start) %>%
                    purrr::pmap_chr(~ {
                      form %>%
                        dplyr::filter(sheet_name == ..1) %>%
                        dplyr::slice(..2) %>%
                        dplyr::pull(stringr::str_c("x", ..3))
                    })) %>%
    dplyr::ungroup() %>%
    dplyr::select(var.name, value) %>%
    tidyr::pivot_wider(names_from = var.name, values_from = value)

}
