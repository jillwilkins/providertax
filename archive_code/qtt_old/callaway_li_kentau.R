library(dplyr)

never_treated <- hospdata_matched_base %>%
  filter(qtt_gname == 0) %>%          # or gname == 0, whatever your never-treated flag is
  select(mcrnum, year, op_margin) %>% # swap outcome as needed
  arrange(mcrnum, year)

years <- sort(unique(never_treated$year))

kendall_by_period <- lapply(years[-(1:2)], function(t) {
  lvl    <- never_treated %>% filter(year == t - 2) %>% select(mcrnum, Y_lag2 = op_margin)
  cur    <- never_treated %>% filter(year == t)     %>% select(mcrnum, Y_t    = op_margin)
  prev   <- never_treated %>% filter(year == t - 1) %>% select(mcrnum, Y_tm1  = op_margin)

  merged <- lvl %>%
    inner_join(prev, by = "mcrnum") %>%
    inner_join(cur,  by = "mcrnum") %>%
    mutate(dY = Y_t - Y_tm1) %>%
    filter(!is.na(Y_lag2), !is.na(dY))

  if (nrow(merged) < 10) return(NULL)  # too few units to trust a tau estimate

  test <- cor.test(merged$Y_lag2, merged$dY, method = "kendall")
  data.frame(year = t, n = nrow(merged), tau = unname(test$estimate), p_value = test$p.value)
})

kendall_results <- bind_rows(kendall_by_period)
print(kendall_results)

library(ggplot2)

# 1. Visual check -- plot tau over time, flag your structural-break years
ggplot(kendall_results, aes(x = year, y = tau)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = c(2008, 2014, 2020), linetype = "dashed", color = "grey50") +
  labs(title = "Kendall's Tau Stability Check (Copula Stability Assumption)",
       subtitle = "Pre-treatment dependence between Y_{t-2} and \u0394Y_t, never-treated hospitals",
       x = "Year", y = "Kendall's \u03c4") +
  theme_minimal()

# 2. Formal trend test -- Fisher z-transform tau, regress on year
kendall_results <- kendall_results %>%
  mutate(z = atanh(tau))   # Fisher's z-transformation stabilizes variance for correlation-type stats

trend_test <- lm(z ~ year, data = kendall_results)
summary(trend_test)   # a significant slope on 'year' = evidence of drift, i.e. against stability