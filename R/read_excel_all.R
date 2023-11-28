
#' @importFrom magrittr %>%
NULL

#' Read All Sheets from an Excel File
#'
#' This function reads all sheets from a given Excel file and returns a named list of data frames
#' where each sheet corresponds to an entry in the list.
#'
#' @param path A character string specifying the path to the Excel file.
#' @param sheets A vector with sheet names or positions (numeric). If NULL (the default), the entire workbook is read.
#' @param sheets_regex A regex string used to select the sheets desired.
#' @param range A cell range to read from the sheets. If NULL (the default), the entire sheet is read.
#'
#' @return A named list of data frames. Each data frame corresponds to a sheet in the Excel file.
#' @export
#'
#' @examples
#' \dontrun{
#'   all_sheets <- read_excel_all("path_to_file.xlsx")
#'   head(all_sheets$Sheet1)
#' }
#'
#' @importFrom purrr map set_names
#' @importFrom readxl excel_sheets read_excel
read_excel_all <- function(path, sheets = NULL, sheets_regex = ".", range = NULL) {

  if (is.null(sheets)) {
    sheets <- tibble::tibble (sheets = readxl::excel_sheets(path)) %>%
      gplyr::filter_in(sheets, sheets_regex) %>%
      dplyr::pull(sheets)
  }

  tibble::tibble (sheet_name = sheets) %>%
    dplyr::mutate(raw_df = purrr::map(
      sheets,
      ~ readxl::read_excel(
        path,
        sheet = .x,
        col_names = FALSE,
        col_types = "text",
        trim_ws = FALSE,
        range = range,
      )
    )) %>%
    tidyr::unnest(raw_df) %>%
    dplyr::group_by(sheet_name) %>%
    CProg::add_index() %>%
    dplyr::ungroup() %>%
    janitor::clean_names()

}
# read_excel_all <- function(path, range=NULL) {
#   sheets <- readxl::excel_sheets(path)
#   purrr::map(
#     sheets,
#     ~ readxl::read_excel(
#       path,
#       sheet = .x,
#       col_names = FALSE,
#       col_types = "text",
#       trim_ws = no,
#       range = range
#     ) %>%
#       CProg::rename_x() %>%
#       CProg::add_index() %>%
#       dplyr::mutate(sheet_name = .x, .before = 1)
#   ) %>%
#     dplyr::bind_rows()
# }

