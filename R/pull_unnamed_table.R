pull_unnamed_table <- function(form, sheet_name, row.start, row.end, col.start, col.end) {
  out <- form %>%
    dplyr::filter(sheet_name == {{sheet_name}}) %>%
    dplyr::select(-index, -sheet_name)

  if (is.na(row.end)) {
    return(out[row.start:nrow(out), col.start:col.end])
  } else{
    return(out[row.start:row.end, col.start:col.end])
  }
}

# pull_unnamed_table(form, "Sheet1", 11, 14, 12, 15)
# pull_unnamed_table(form, "Sheet1", 6, NA, 9, 10)
