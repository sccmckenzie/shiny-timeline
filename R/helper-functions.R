library(lubridate)
library(dplyr)
library(purrr)
library(ggplot2)

# how will app handle events that bleed over to adjacent weeks?
t <- seq(as_datetime("1993-03-07", tz = "US/Central"), as_datetime("1993-03-13 23:00:00", tz = "US/Central"), by = dhours(1))


