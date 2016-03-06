# Misc --------------------------------------------------------------------

# return empty dataframe indicator
IsDataFrameEmpty <- function (x) {
  ifelse(nrow(x) == 0, TRUE, FALSE)
}
