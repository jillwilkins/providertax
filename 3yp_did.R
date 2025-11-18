# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025 (11/18/25)
## Date Edited:   11/18/2025
## Goal:          Preliminary: Event Study Analysis that was used in my third year paper presentation        
## 

# this was the event study ran for commercial discharges 
att_payermix5 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter(!is.na(private_prop_discharges), private_prop_discharges > 0),
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
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
ggsave("events_nov5/es_payermix5.png", plot = overall_payermix5_pretty, width = 10, height = 8, dpi = 300)


# this was the event study ran for medicaid proportion of discharges 
# mcaid prop discharges clustered at the state leve, all hospitals, beds covariates. 
att_mcaid1 <- att_gt(yname = "mcaid_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean,
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
ggsave("events_nov5/es_mcaid1.png", plot = overall_mcaid1_pretty, width = 10, height = 8, dpi = 300) 


# this was the event study for uncompensated care charges 
att_ucc_prop <- att_gt(yname = "ucc_prop",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_clean,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds,                # covariates (use ~1 if none)
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
