utils::globalVariables(c(".", "col.end", "col.start", "column.name", "is.table.end",
                         "name", "row.end", "index", "row.start", "sheet_name",
                         "str_remove", "table.end.row", "table.name", "value", "var.name",
                         "variables_from_form", "raw_df"))


#' Extract Relevant Information from Forms Using A Tmemplate
#'
#' The `reform` function uses a structured template to extract specific data elements
#' from a raw data form. It streamlines the process of data extraction, allowing users
#' to rely on pre-defined templates to pull individual variables and table datas from
#' structured forms.
#'
#' @param form A data frame, typically generated by `read_excel_all`,
#'   representing the raw data from the actual form you want to extract data from.
#' @param template A list containing structured templates generated by
#'   `generate_template`. This template provides metadata and location data to guide
#'   the extraction process.
#'
#' @return A named list containing:
#'   * `variables_form_form`: Data for individual variables extracted from the form.
#'   * Other elements represent table data extracted from the form.
#'
#' @seealso [generate_template()]
#'
#' @examples
#' \dontrun{
#'   raw_form <- read_excel("path_to_form.xlsx")
#'   template_data <- generate_template(read_excel_all("path_to_template.xlsx"))
#'   extracted_data <- reform(raw_form, template_data)
#'   print(extracted_data$variables_form_form)
#' }
#'
#' @export
reform <- function (form, template){
  list (
    list ("variables_from_form" = pull_individual_vars(form, template)),
    tables_from_form (form, template)
  ) %>%
    purrr::flatten() %>%
    purrr::map(list) %>%
    tibble::as_tibble() %>%
    tidyr::unnest(variables_from_form, keep_empty = TRUE)
}

