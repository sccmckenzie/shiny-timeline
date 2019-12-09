library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

times <- seq(as_datetime("1993-02-14",tz = "US/Central"), as_datetime("1993-03-14 23:00:00", tz = "US/Central"), by = dhours(1))

event.generator <- function(.t, x, y) {
  tibble(event = x,
         category = y,
         instance = c("start", "end"),
         time = sample(.t, size = 2, replace = FALSE) %>% sort())
}

event.generator1 <- function(x, y) map_dfr(.x = x, .f = event.generator, .t = times, y = y)
set.seed(1)
events <- map2_dfr(.x = list(c(1:4), c(5:8), c(9:12)), .y = c("A", "B", "C"), .f = event.generator1) %>% 
  pivot_wider(names_from = instance, values_from = time)

year_ww <- function(.t) {
  str_c(year(.t), ".", str_pad(epiweek(.t), width = 2, pad = "0"))
}

wk_boundary <- function(.wk, tz = "US/Central") {
  if (!str_detect(.wk, "\\d{4}\\.\\d{1,2}")) {
    stop("please enter week with format YYYY-WW")
  }
  
  jan1 <- make_date(year = str_sub(.wk, 1L, 4L), 1L, 1L)
  jan1.wd <- wday(jan1)
  if (jan1.wd <= 4) {
    day1 <- jan1 - (jan1.wd - 1)
  } else {
    day1 <- jan1 + (7 - jan1.wd + 1)
  }
  
  make_datetime(year(day1), month(day1), day(day1), tz = tz) + weeks(as.numeric(str_sub(.wk, 6L, 7L)) - 1)
}

## !!!!!!! This needs to be fixed!!!!!
weeks_crossed <- function(t1, t2) {
  d <- unique(c(seq(t1, t2, by = dweeks(1)), t2))
  
  map_chr(d, year_ww)
}

hrs_during_week <- function(t1, t2, wk) {
  bnd_start <- wk_boundary(wk)
  
  bnd_end <- bnd_start + weeks(1)
  
  if (bnd_start %within% interval(t1, t2)) { # if event begins in past week
    if (bnd_end %within% interval(t1, t2)) { # and ends in future week
      a <- int_length(interval(bnd_start, bnd_end)) 
    } else {  # and ends in current week
      a <- int_length(interval(bnd_start, t2))
    }
  } else { # if event begins in current week
    if (bnd_end %within% interval(t1, t2)) { # and ends in future week
      a <- int_length(interval(t1, bnd_end))
    } else { # and ends in current week
      a <- int_length(interval(t1, t2))
    }
  }
  
  (a / 3600) %>% round(1)
}

events %>% 
  mutate(wk = map2(.x = start, .y = end, .f = weeks_crossed)) %>% 
  unnest(wk) %>% 
  group_by(event) %>% 
  mutate(hrs_total = (end - start) %>% as.duration() %>% as.double() %>% round(1) / 3600,
         hrs_down = pmap_dbl(.l = list(t1 = start, t2 = end, wk = wk), .f = hrs_during_week)) %>% 
  filter(as.numeric(wk) > 1993.09)

