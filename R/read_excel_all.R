
#' @importFrom magrittr %>%
NULL


# read_excel_all <- function(path, sheets = NULL, sheets_regex = ".", range = NULL) {
#
#   if (is.null(sheets)) {
#     sheets <- tibble::tibble (sheets = readxl::excel_sheets(path)) %>%
#       gplyr::filter_in(sheets, sheets_regex) %>%
#       dplyr::pull(sheets)
#   }
#
#   tibble::tibble (sheet_name = sheets) %>%
#     dplyr::mutate(raw_df = purrr::map(
#       sheets,
#       ~ readxl::read_excel(
#         path,
#         sheet = .x,
#         col_names = FALSE,
#         col_types = "text",
#         trim_ws = FALSE,
#         range = range,
#       )
#     )) %>%
#     tidyr::unnest(raw_df) %>%
#     dplyr::group_by(sheet_name) %>%
#     gplyr::add_index() %>%
#     dplyr::ungroup() %>%
#     janitor::clean_names()
#
# }

#' Read All Sheets from an Excel File
#'
#' This function reads all sheets from a given Excel file and returns a named list of data frames
#' where each sheet corresponds to an entry in the list.
#'
#' @param path A character string specifying the path to the Excel file.
#' @param sheets A vector with sheet names or positions (numeric). If NULL (the default), the entire workbook is read.
#' @param sheets_regex A regex string used to select the sheets desired.
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
read_excel_all <- function(path, sheets = NULL, sheets_regex = ".") {

  if (is.null(sheets)) {
    sheets <- tibble::tibble (sheets = openxlsx::getSheetNames (path)) %>%
      gplyr::filter_in(sheets, sheets_regex) %>%
      dplyr::pull(sheets)
  }

  tibble::tibble (sheet_name = sheets) %>%
    dplyr::mutate(raw_df = purrr::map(
      sheets,
      ~ openxlsx::read.xlsx(
        path,
        sheet = .x,
        startRow = 1,
        colNames = FALSE,
        rowNames = FALSE,
        detectDates = FALSE,
        skipEmptyRows = FALSE,
        skipEmptyCols = FALSE,
        rows = NULL,
        cols = NULL,
        check.names = FALSE,
        sep.names = ".",
        namedRegion = NULL,
        na.strings = "",
        fillMergedCells = FALSE
      ) %>%
        as_tibble() %>%
        gplyr::to_character()
    )) %>%
    tidyr::unnest(raw_df) %>%
    dplyr::group_by(sheet_name) %>%
    gplyr::add_index() %>%
    dplyr::ungroup() %>%
    janitor::clean_names()

}
