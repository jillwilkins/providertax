# ==============================================================================
# ADD STATE UNINSURED RATE DATA
# ==============================================================================


# Step 1: Recreate Garthwaite et al. (2015) CPS-based uninsured rate by state-year
library(haven)

data_cps_og <- read_dta("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/replication/garth2015/cps_00013.dta")

data_cps <- data_cps_og %>%
  filter(age >= 21 & age < 65) %>%
  mutate(
    year         = year - 1,
    any_ins      = as.integer(hcovany == 2),
    uninsured    = as.integer(hcovany == 1),
    any_public   = as.integer(hcovpub == 2),
    unemp        = as.integer(empstat >= 20 & empstat <= 22),
    foreign_born = as.integer(citizen == 3)
  ) %>%
  group_by(statefip, year) %>%
  summarise(
    population     = sum(wtsupp, na.rm = TRUE),
    insured_rate   = weighted.mean(any_ins,      wtsupp, na.rm = TRUE),
    uninsured_rate = 1 - insured_rate,
    any_public     = weighted.mean(any_public,   wtsupp, na.rm = TRUE),
    unemp          = weighted.mean(unemp,        wtsupp, na.rm = TRUE),
    foreign_born   = weighted.mean(foreign_born, wtsupp, na.rm = TRUE),
    mean_age       = weighted.mean(age,          wtsupp, na.rm = TRUE),
    .groups = "drop"
  )


# add state abbreviations
library(tigris)

data_cps <- data_cps %>%
  mutate(
    state_name = fips_codes$state_name[match(sprintf("%02d", as.numeric(statefip)), 
                                             fips_codes$state_code)],
    state      = state.abb[match(state_name, state.name)]
  )

# join to state-level data
state_data <- state_data %>%
  left_join(
    data_cps %>% select(state, year, uninsured_rate), 
    by = c("state", "year")
  )

View(state_data)

feols(uncomp_bed ~ uninsured_rate | state + year,
      cluster = ~state,
      data = state_data %>% filter(year <= 2013))

plot_data <- state_data %>%
  filter(year == 2004, !is.na(uncomp_bed), !is.na(uninsured_rate))









# OLD ATTEMPT not getting me the same values as Garthwaite et al. (2015) - need to check more carefully
# IPUMS from CPS 
install.packages('ipumsr')
library('ipumsr')

ddicps <- read_ipums_ddi(paste0(data_input_path, "cps_00001.xml"))
data_cps_og <- read_ipums_micro(ddicps)

# Garthwaite, Gross, and Notowidigdo (2015), "Insurance Expansion and Hospital 
# Uncompensated Care: Evidence from Voluntary Medicaid Expansions"
# Replicating their CPS share-insured construction (create-share-insured.do)

data_cps <- data_cps_og %>%
  


# rename variables, convert the fips code, create uninsured variable, and aggregate to the state-year level
# limit age to match Garthwaite et al. (2015)
data_cps <- data_cps_og %>%
  filter(AGE >= 21 & AGE <= 64) %>%
  rename(survey_year = YEAR) %>%
  mutate(
    year      = survey_year - 1,
    uninsured = as.integer(HCOVANY == 1),
    state     = c(state.abb, "DC")[match(sprintf("%02d", as.numeric(STATEFIP)),
                                         c(sprintf("%02d", c(1:56)[-c(3,7,14,43,52)]), "11"))]
  ) %>%
  group_by(state, survey_year, year) %>%
  summarise(
    population     = sum(ASECWT, na.rm = TRUE),
    n_uninsured    = sum(ASECWT * uninsured, na.rm = TRUE),
    uninsured_rate = n_uninsured / population,
    .groups = "drop"
  )

# to avoid medicaid expansion, cut off at 2013 
data_cps <- data_cps %>% filter(year <= 2013) 

#save cps ipums data
write.csv(
  data_cps,
  paste0(data_output_path, "ipums_data_cps.csv"),
  row.names = FALSE
)


state_data <- state_data %>%
  left_join(
    data_cps %>% select(state, year, uninsured_rate, n_uninsured, population),
    by = c("state", "year")
  )

View(data_cps)
View(state_data)
colnames(state_data)

feols(uncomp_bed ~ uninsured_rate | state + year,
      cluster = ~state,
      data = state_data %>% filter(year <= 2013))




unins <- ggplot(plot_data, aes(x = uninsured_rate, y = uncomp_bed)) +
  geom_point(color = "#185FA5", size = 2.5, alpha = 0.8) +
  geom_text(aes(label = state), hjust = -0.2, vjust = 0.4, size = 3,
            color = "gray30", check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "#D85A30",
              fill = "#F0997B", alpha = 0.2, linewidth = 0.8) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.08))) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Uninsured rate vs. uncompensated care per bed (2004)",
    x = "Uninsured rate",
    y = "Uncompensated care per bed",
    caption = "Source: CPS ASEC, AHA. Each point is a state."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9)
  )

ggsave("uninsured_vs_uncomp_bed.png", unins, width = 10, height = 8)



# BEDS ANALYSIS 

library(ggplot2)
library(dplyr)

hospdata_analysis %>%
  group_by(year) %>%
  summarise(avg_discharges = mean(tot_discharges, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_discharges)) +
  geom_line(color = "#185FA5", linewidth = 0.8) +
  geom_point(color = "#185FA5", size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average discharges per hospital over time",
    x = "Year",
    y = "Average discharges",
    caption = "Source: AHA"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "plain"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50", size = 9)
  )

ggsave("total_discharges_over_time.png", width = 6, height = 4)
