make_events <-  function(d, n, spacing, words) {
  t1 <- rnorm(n, mean = as_datetime(d, tz = "US/Central"), sd = dweeks(spacing)) %>% as_datetime(tz = "US/Central")
  t2 <- t1 + duration(rnorm(n, mean = dweeks(spacing / 2), sd = dweeks(spacing/ 4)))
  t3 <- t2 + sample(c(ddays(1), NA, prob = c(.2, .8)), n, replace = TRUE)
  
  tibble(nickname = factor(sample(words, n)), t1, t2, t3)
}