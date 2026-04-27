# ============================================================
# Staggered Treatment Adoption Visualization using panelView
# Treatment variable: firsttax
# ============================================================
 
# Install panelView if not already installed
# install.packages("panelView")
 
library(panelView)
 
# ------------------------------------------------------------
# Basic treatment status plot
# Shows which units received treatment and when
# ------------------------------------------------------------
# Filter data to 2004-2022 first
state_data_filtered <- state_data %>%
  filter(year >= 2004 & year <= 2022)

state_data_panelview <- state_data_filtered %>%
  select(state, year, ever_treat, post_treat,
         median_income_pre, eligibility, exp_status, firsttax)

# Get the 2021 rows for those states and copy them as 2022
rows_to_add <- state_data_panelview %>%
  filter(state %in% c("CT", "VT", "RI"), year == 2021) %>%
  mutate(year = 2022)

# Bind them in
state_data_panelview <- bind_rows(state_data_panelview, rows_to_add)

# Verify
state_data_panelview %>%
  filter(state %in% c("CT", "VT", "RI"), year %in% c(2021, 2022)) %>%
  select(state, year, post_treat) %>%
  arrange(state, year)

p <- panelview(firsttax ~ post_treat,
          data = state_data_panelview, 
          index = c("state", "year"),
          color = c("#B0C4DE", "#0a1a82"), 
          by.timing = TRUE,
          xlab = "Year", 
          ylab = "State")

panelview <- panelview(
    data        = state_data_panelview,
    D           = "post_treat",
    index       = c("state", "year"),
    type        = "treat",
    main        = "",
    xlab        = "Year",
    ylab        = "State",
    legend.labs = c("Control", "Treated", "Missing"),
    color       = c("#BFCDE0", "#1B3A6B"),
    axis.lab.gap = c(1, 0),
    by.timing   = TRUE,
    display.all = TRUE        # <-- forces never-treated units to show
  )

panelview <- panelview + labs(fill = NULL) + 
  scale_fill_manual(
    values = c("#BFCDE0", "#1B3A6B" ),
    labels = c("No Provider Tax", "Provider Tax")  # change these to whatever you want
  )

ggsave("panelview_treatment_status.png", panelview, width = 10, height = 8, dpi = 300)


