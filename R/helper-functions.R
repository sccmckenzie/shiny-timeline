library(lubridate)
library(dplyr)
library(purrr)
library(ggplot2)

palette <- c("#79b008", "#cf3601", "#e79b00", "#e6c021", "#b0084b", "#0084be")

# how will app handle events that bleed over to adjacent weeks?
times <- seq(as_datetime("1993-03-07",tz = "US/Central"), as_datetime("1993-03-13 23:00:00", tz = "US/Central"), by = dhours(1))

event.generator <- function(.t, ltr) {
  tibble(event = ltr,
         instance = c("start", "end"),
         time = sample(.t, size = 2, replace = FALSE) %>% sort())
}

set.seed(17)
map_dfr(.x = LETTERS[1:5], .f = event.generator, .t = times) %>% 
  ggplot() +
  geom_point(aes(event, time, color = event)) +
  geom_line(aes(event, time, color = event)) +
  scale_color_manual(values = palette) +
  coord_flip()
