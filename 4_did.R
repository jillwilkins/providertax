# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025 (11/18/25)
## Date Edited:   12/9/2025
## Goal:          Preliminary: Event Study Analysis        
## 

library(fixest)
library(did)
library(ggplot2)

View(hospdata_clean)

# Identify mcrnums in multiple states
multi_state_mcrnums <- hospdata_clean %>%
  group_by(mcrnum) %>%
  summarize(n_states = n_distinct(state)) %>%
  filter(n_states > 1) %>%
  pull(mcrnum)  # just get the vector of mcrnums

# Exclude them from the dataset
hospdata_clean <- hospdata_clean %>%
  filter(!mcrnum %in% multi_state_mcrnums)

hospdata_clean <- hospdata_clean %>%
  mutate(private_discharges = tot_discharges - mcaid_discharges - mcare_discharges)

# share commercial discharges 
att_payermix5 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_aha %>% filter(!is.na(private_prop_discharges), private_prop_discharges > 0, year >= 2004, SERV == 10),
                control_group = "notyettreated",  
                xformla = ~ prebeds + ct_pre_income,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix5, type = "group")
ggdid(att_payermix5)

# event study; aggreegate treatment effects
overall_att_payermix5 <- aggte(att_payermix5, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_payermix5)

# plot for overall
overall_att_payermix5 <- ggdid(overall_att_payermix5)
overall_payermix5_pretty <- overall_att_payermix5 +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Commercial Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_payermix5_pretty)
ggsave("events_dec/es_privateshare.png", plot = overall_payermix5_pretty, width = 10, height = 8, dpi = 300)

# number of commercial discharges
att_private_number <- att_gt(yname = "private_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_aha %>% filter(!is.na(private_prop_discharges), private_prop_discharges > 0, year >= 2004, SERV == 10),
                control_group = "notyettreated",  
                xformla = ~ prebeds + ct_pre_income,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_private_number, type = "group")
ggdid(att_private_number) 

# event study; aggreegate treatment effects
overall_att_private_num <- aggte(att_private_number, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_private_num)  

# plot for overall
overall_private_num <- ggdid(overall_att_private_num)
overall_private_num <- overall_private_num +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Number of Commercial Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_private_num)
ggsave("events_dec/es_private_num.png", plot = overall_private_num, width = 10, height = 8, dpi = 300)

# number of mediciad discharges 
att_mcaid_number <- att_gt(yname = "mcaid_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_aha %>% filter(!is.na(mcaid_prop_discharges), mcaid_prop_discharges > 0, year >= 2004, SERV == 10),
                control_group = "notyettreated",  
                xformla = ~ prebeds + ct_pre_income,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_mcaid_number, type = "group")
ggdid(att_mcaid_number)

# event study; aggreegate treatment effects
overall_att_mcaid_num <- aggte(att_mcaid_number, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_mcaid_num)

# plot for overall
overall_mcaid_num <- ggdid(overall_att_mcaid_num)
overall_mcaid_num <- overall_mcaid_num +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Number of Medicaid Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_mcaid_num)
ggsave("events_dec/es_mcaid_num.png", plot = overall_mcaid_num, width = 10, height = 8, dpi = 300) 


# share of medicaid discharges 
att_mcaid1 <- att_gt(yname = "mcaid_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_aha %>% filter(!is.na(mcaid_prop_discharges), mcaid_prop_discharges > 0, year >= 2004, SERV == 10),
                control_group = "notyettreated",  
                xformla = ~ prebeds + ct_pre_income,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_mcaid1, type = "group")
ggdid(att_mcaid1)

# event study; aggreegate treatment effects
overall_att_mcaid1 <- aggte(att_mcaid1, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_mcaid1)

# plot for overall
overall_mcaid1 <- ggdid(overall_att_mcaid1)
overall_mcaid1_pretty <- overall_mcaid1 +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Medicaid Share of Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_mcaid1_pretty)
ggsave("events_dec/es_mcaid1.png", plot = overall_mcaid1_pretty, width = 10, height = 8, dpi = 300) 


# share uncompensated care charges 
att_ucc_prop <- att_gt(yname = "ucc_prop",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_aha %>% filter(!is.na(ucc_prop), ucc_prop > 0, year >= 2004, SERV == 10),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds + ct_pre_income,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
summary(att_ucc_prop, type = "group")
ggdid(att_ucc_prop)

overall_att_ucc_prop <- aggte(att_ucc_prop, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_ucc_prop)
overall_ucc_prop <- ggdid(overall_att_ucc_prop)
overall_ucc_pretty <- overall_ucc_prop +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Share of Uncompensated Care Charges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_ucc_pretty)
ggsave("events_nov5/overall_ucc_clean.png", plot = overall_ucc_pretty, width = 10, height = 8, dpi = 300, bg = "transparent")

# dollar of uncompensated care charges
summary(hospdata_clean$tot_uncomp_care_charges)
att_ucc_charge <- att_gt(yname = "tot_uncomp_care_charges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_clean %>% filter(!is.na(tot_uncomp_care_charges), tot_uncomp_care_charges > 0, tot_uncomp_care_charges < 50000000, year >= 2004),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds + ct_pre_income,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
summary(att_ucc_charge, type = "group")
ggdid(att_ucc_charge)

overall_att_ucc_charge <- aggte(att_ucc_charge, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_ucc_charge)

overall_ucc_charge <- ggdid(overall_att_ucc_charge)
overall_ucc_charge <- overall_ucc_charge +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Total Uncompensated Care Charges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_ucc_charge)


 
# define 95th percentile cutoff for net patient revenue
npr_95th <- quantile(hospdata_clean$net_pat_rev, 0.95, na.rm = TRUE)
npr_5th <- quantile(hospdata_clean$net_pat_rev, 0.05, na.rm = TRUE)

# net patient revenue 
att_npr <- att_gt(yname = "net_pat_rev",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_aha %>% filter(beds > 30, SERV == 10, !is.na(net_pat_rev), net_pat_rev > npr_5th, net_pat_rev < npr_95th, year >= 2004),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds + ct_pre_income,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
summary(att_npr, type = "group")
ggdid(att_npr)

overall_att_npr <- aggte(att_npr, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_npr)
overall_npr <- ggdid(overall_att_npr)
overall_npr_pretty <- overall_npr +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Net Patient Revenue"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10 )),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") +
  scale_y_continuous(labels = scales::label_number(scipen = 999))
print(overall_npr_pretty)
ggsave("events_dec/overall_npr_clean.png", plot = overall_npr_pretty, width = 10, height = 8, dpi = 300, bg = "transparent")


# log npr 
hospdata_clean <- hospdata_clean %>%
  mutate(log_net_pat_rev = log(net_pat_rev))

att_npr_log <- att_gt(
  yname = "log_net_pat_rev",  # create a new variable in your data
  tname = "year",
  idname = "mcrnum",
  gname = "treatment_num",
  data = hospdata_clean %>%
           filter(!is.na(net_pat_rev), net_pat_rev > 0, net_pat_rev < 1500000000, year >= 2004) %>%
           mutate(log_net_pat_rev = log(net_pat_rev)),
  control_group = "notyettreated",
  xformla = ~ ct_pre_income,
  est_method = "dr",
  clustervars = "state",
  base_period = "universal",
  allow_unbalanced = TRUE
)

summary(att_npr_log, type = "group")
ggdid(att_npr_log)

overall_att_npr_log <- aggte(att_npr_log, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_npr_log)
overall_npr_log <- ggdid(overall_att_npr_log)

# Step 4: back-transform to dollar scale using exp()
overall_att_dollars <- overall_att_npr_log
overall_att_dollars$att <- exp(overall_att_npr_log$att) - 1  # interpretable as % change
overall_att_dollars$se.aggte <- overall_att_npr_log$se.aggte * exp(overall_att_npr_log$att)  # approximate

# Step 5: plot in dollar terms or percent changes
overall_npr_log_pretty <- ggdid(overall_att_npr_log) +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Approximate % Change in Net Patient Revenue"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(size = 13, margin = margin(t = 10 )),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1))  # shows % changes

print(overall_npr_pretty)

# total discharges 
summary(hospdata_clean$tot_discharges)
att_dis <- att_gt(yname = "mcare_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_clean %>% filter(!is.na(tot_discharges), tot_discharges > 10, year >= 2004),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds + ct_pre_income,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
summary(att_dis, type = "group")
ggdid(att_dis)

overall_att_dis <- aggte(att_dis, type = "dynamic", na.rm = TRUE, min_e = -6, max_e = 6)
summary(overall_att_dis)

overall_dis <- ggdid(overall_att_dis)
overall_dis_pretty <- overall_dis +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Total Discharges"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,    
      margin = margin(b = 10)
    ),
    axis.title.x = element_text(size = 13, margin = margin(t = 10 )),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_dis_pretty)
