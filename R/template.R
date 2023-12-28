#' Transform Form Data Based on Template Metadata
#'
#' Uses a provided data frame, which is a read version of an Excel form, and transforms it
#' based on template metadata tags. The function prepares the data for further extraction
#' by renaming columns, adding index, pivoting, modifying, reclassifying, and filtering based
#' on specific conditions.
#'
#' @param df A data frame, typically a result from `read_excel()`, representing the raw Excel form.
#'
#' @details
#' The `form_template` function is part of a larger process of automating data extraction from Excel forms.
#' These forms have been populated with specific tags in a template version to guide the data extraction.
#' Tags like "*((var." indicate single variables to be extracted. The function processes the raw data to only
#' keep the necessary meta-data tags and restructures the data in a usable format.
#'
#' The process involves:
#' 1. Renaming columns to a consistent format using `rename_x()`.
#' 2. Adding an index column to the data for tracking purposes using `add_index()`.
#' 3. Pivoting the data to make it longer and more usable with `pivot_longer()`.
#' 4. Modifying certain columns with `quickm()`.
#' 5. Reclassifying columns to character data type with `reclass_c()`.
#' 6. Filtering rows based on certain conditions, primarily to keep rows that have relevant tags using `filter_in()`.
#'
#'
#' @return
#' A modified tibble prepared for data extraction based on the template's metadata tags.
#'
#' @examples
#' \dontrun{
#' library(readxl)
#'
#' # Assume 'sample_form.xlsx' is a representative Excel form file.
#' raw_data <- read_excel("sample_form.xlsx")
#'
#' transformed_data <- template(raw_data)
#' }
#'
template <- function (df) {
  df %>%
    gplyr::rename_x() %>%
    gplyr::add_index(col_name = "row") %>%
    tidyr::pivot_longer(cols = -row) %>%
    dplyr::rename(col = name) %>%
    gplyr::quickm(col, str_remove, "x") %>%
    gplyr::to_number(col) %>%
    dplyr::rename (name = value) %>%
    gplyr::filter_in(name, tag, na.rm = TRUE)
}


