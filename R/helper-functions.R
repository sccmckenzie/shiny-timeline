library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

year_ww <- function(.t) {
  str_c(year(floor_date(as_date(.t), unit = "weeks")), ".", str_pad(epiweek(.t), width = 2, pad = "0"))
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

weeks_crossed <- function(t1, t2) {
  d <- unique(c(seq(t1, t2, by = dweeks(1)), t2))
  
  unique(map_chr(d, year_ww))
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

ww_choices <- function(.date) {
  #a <- weeks_crossed(as_datetime(.date, tz = "US/Central"), now())
  a <- weeks_crossed(as_datetime(.date, tz = "US/Central"), as_datetime("1993-04-01", tz = "US/Central"))
  b <- as.numeric(a)
  a[length(a)] <- str_c(a[length(a)], " (Current WW)")
  setNames(b, a)
}

pull.data <- function(input_date) {
  times <- seq(as_datetime("1992-12-01",tz = "US/Central"), as_datetime("1993-04-01 23:00:00", tz = "US/Central"), by = dhours(1))
  
  event.generator <- function(.t, x, y) {
    tibble(event = x,
           category = y,
           instance = c("start", "end"),
           time = sample(.t, size = 2, replace = FALSE) %>% sort())
  }
  
  event.generator1 <- function(x, y) map_dfr(.x = x, .f = event.generator, .t = times, y = y)
  
  p <- list()
  
  set.seed(1)
  map2_dfr(.x = list(c(1:8), c(8:16), c(17:24)), .y = c("A", "B", "C"), .f = event.generator1) %>% 
    pivot_wider(names_from = instance, values_from = time) %>% 
    mutate(wk = map2(.x = start, .y = end, .f = weeks_crossed)) %>% 
    unnest(wk) %>% 
    group_by(event) %>% 
    mutate(hrs_total = (end - start) %>% as.duration() %>% as.double() %>% round(1) / 3600,
           hrs_wk = pmap_dbl(.l = list(t1 = start, t2 = end, wk = wk), .f = hrs_during_week)) %>% 
    ungroup() %>% 
    filter(as.numeric(wk) >= as.numeric(year_ww(input_date))) %>% # in practice this input will be fed into data pull much earlier
    print() 
}

p1 <- function(.data, input_ww1, input_ww2) {
  .data %>% 
    # filter(as.numeric(wk) <= input_ww2,
    #        as.numeric(wk) >= input_ww1) %>% 
    group_by(wk, category) %>% 
    summarise(hrs = sum(hrs_wk)) %>% 
    ungroup() %>% 
    complete(wk, category) %>% 
    ggplot(aes(wk, hrs)) +
    geom_col(aes(fill = category), position = "dodge")
}

