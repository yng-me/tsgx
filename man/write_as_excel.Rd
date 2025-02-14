% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write-as-excel.R
\name{write_as_excel}
\alias{write_as_excel}
\title{Write data into an Excel workbook}
\usage{
write_as_excel(
  .data,
  wb = NULL,
  sheet = set_sheet_name(wb),
  append_to_existing_sheet = F,
  title = NULL,
  subtitle = NULL,
  description = NULL,
  footnote = NULL,
  source_note = NULL,
  start_col = 2,
  start_row = 2,
  options = get_config("facade"),
  names_separator = ">",
  save_as_excel = FALSE,
  cols_with_decimal_format = NULL,
  filename = NULL,
  format_precision = 2,
  overwrite = TRUE,
  ...
)
}
\arguments{
\item{.data}{A data frame, data frame extension (e.g. a tibble), a lazy data frame (e.g. from dbplyr or dtplyr), or Arrow data format.}

\item{wb}{Workbook instance created using openxlsx package.}

\item{sheet}{Tab/sheet name.}

\item{append_to_existing_sheet}{Whether to create new worksheet or append data to existing worksheet.}

\item{title}{Table title.}

\item{subtitle}{Table subtitle}

\item{description}{Table description.}

\item{footnote}{Table footnote.}

\item{source_note}{Table footnote.}

\item{start_col}{Column position to start write the data.}

\item{start_row}{Start row in Excel to write the data}

\item{options}{Formatting options.}

\item{names_separator}{Column separator that defines the table hierarchy.}

\item{save_as_excel}{Whether to save/export the table as Excel.}

\item{cols_with_decimal_format}{Columns to apply decimal formatting}

\item{filename}{Name of file to specify with .xlsx extension.}

\item{format_precision}{Number of decimal place/s to apply for numeric data}

\item{overwrite}{Whether to overwrite the existing file.}

\item{...}{\code{openxlsx} workbook object and sheet name.}
}
\value{
A formatted workbook object.
}
\description{
Write data into an Excel workbook
}
\examples{

library(openxlsx)
library(dplyr)

starwars_species_sex <- starwars |>
    generate_crosstab(species, sex)

wb <- createWorkbook()

write_as_excel(starwars_species_sex, wb = wb, sheet = "Table 1")
}
