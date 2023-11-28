#' Download Example Files to Working Directory
#'
#' Copies the example files 'Form.xlsx' and 'Template.xlsx' from the
#' package's `extdata` folder to the user's current working directory.
#'
#' The `download_example` function provides users with easy access to the
#' example datasets packaged with `reformR`. These datasets can
#' be used for demonstration purposes or to test the functionality of
#' other functions within the package.
#'
#' @details
#' The example files are stored in the `extdata` directory of the
#' `reformR` package. This function uses the `fs` package to handle
#' file operations.
#'
#' @examples
#' \dontrun{
#' # To copy the example files to your working directory
#' download_example()
#'
#' # Check if the files are available in the working directory
#' list.files(pattern = "xlsx")
#' }
#'
#' @export
download_example <- function() {
  # Get the paths to the files within the package
  form_path <- system.file("extdata", "Form.xlsx", package = "reformR")
  template_path <- system.file("extdata", "Template.xlsx", package = "reformR")

  # Check if files exist
  if (!file.exists(form_path) || !file.exists(template_path)) {
    stop("Example files not found in the package.")
  }

  # Use the fs package to copy files to the working directory
  fs::file_copy(form_path, "./Form.xlsx", overwrite = TRUE)
  fs::file_copy(template_path, "./Template.xlsx", overwrite = TRUE)

  message("Files 'Form.xlsx' and 'Template.xlsx' have been copied to the current working directory.")
}
