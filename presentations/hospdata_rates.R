# PRELIMINARY INFO 


hospdata_stack_rate <- hospdata_stack %>%
  left_join(
    taxrates_clean %>%
      mutate(state = state.abb[match(state, state.name)]) %>%
      group_by(state, year) %>%
      slice(1) %>%
      ungroup() %>%
      select(state, year, tax_rate, tax_base, tax_rate2, tax_base2),
    by = c("state", "year")
  )

head(hospdata_stack$state)
head(taxrates_clean$state)

# Which states have rate info?
hospdata_stack_rate %>%
  filter(!is.na(tax_rate)) %>%
  distinct(state) %>%
  arrange(state)

hospdata_stack_rate %>%
  filter(!is.na(tax_rate)) %>%
  group_by(state) %>%
  summarise(
    mean_rate = mean(tax_rate, na.rm = TRUE),
    min_rate  = min(tax_rate, na.rm = TRUE),
    max_rate  = max(tax_rate, na.rm = TRUE),
    n_years   = n_distinct(year)
  ) %>%
  arrange(desc(mean_rate))

library(fixest)

# Basic OLS with state and year fixed effects
ols_npr <- feols(npr_bed ~ tax_rate | state + year + mcrnum + exp_status, 
                 data = hospdata_stack_rate %>% filter(!is.na(tax_rate), tax_rate < 11),
                 cluster = ~state)

ols_op <- feols(op_bed ~ tax_rate | state + year + mcrnum + exp_status,
                data = hospdata_stack_rate %>% filter(!is.na(tax_rate), tax_rate < 11),
                cluster = ~state)

ols_mcaid <- feols(mcaid_prop_discharges ~ tax_rate | state + year + mcrnum + exp_status,
                   data = hospdata_stack_rate %>% filter(!is.na(tax_rate), tax_rate < 11),
                   cluster = ~state)

# View all results together
etable(ols_npr, ols_op, ols_mcaid)

scatter_rate_npr <- hospdata_stack_rate %>%
  filter(!is.na(tax_rate), tax_rate < 11) %>%
  ggplot(aes(x = tax_rate, y = npr_bed)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Tax Rate", y = "Net Patient Revenue Per Bed")

ggsave("scatter_rate_npr.png", scatter_rate_npr, width = 8, height = 6, dpi = 300)

