% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reform.R
\name{reform}
\alias{reform}
\title{Extract Relevant Information from Forms Using A Tmemplate}
\usage{
reform(form, template)
}
\arguments{
\item{form}{A data frame, typically generated by \code{read_excel_all},
representing the raw data from the actual form you want to extract data from.}

\item{template}{A list containing structured templates generated by
\code{generate_template}. This template provides metadata and location data to guide
the extraction process.}
}
\value{
A named list containing:
\itemize{
\item \code{variables_form_form}: Data for individual variables extracted from the form.
\item Other elements represent table data extracted from the form.
}
}
\description{
The \code{reform} function uses a structured template to extract specific data elements
from a raw data form. It streamlines the process of data extraction, allowing users
to rely on pre-defined templates to pull individual variables and table datas from
structured forms.
}
\examples{
\dontrun{
  raw_form <- read_excel("path_to_form.xlsx")
  template_data <- generate_template(read_excel_all("path_to_template.xlsx"))
  extracted_data <- reform(raw_form, template_data)
  print(extracted_data$variables_form_form)
}

}
\seealso{
\code{\link[=generate_template]{generate_template()}}
}
