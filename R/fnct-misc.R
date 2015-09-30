# Misc --------------------------------------------------------------------

# return empty dataframe indicator
IsDataFrameEmpty <- function(x) {
  ifelse(nrow(x) == 0, TRUE, FALSE)
}

# transform to log10 scale and truncate values to range [0.5, 2]
require(scales) # trans_new() is in the scales library
log10truncate_trans <-
  function() trans_new("log10truncate",
                       transform = function (x) {
                         ifelse(x <= 2 & x >= 0.5,
                                log10(x),
                                ifelse(x > 2,
                                       log10(2),  # trunc to log10(2) if x>2
                                       log10(0.5) # trunc to log10(.5) if x<0.5
                                )
                         )
                       },
                       inverse = function (x) 10^x
  )
