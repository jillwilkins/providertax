# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025
## Date Edited:   10/23/2025
## Goal:          Event Study Analysis         
## 

library(fixest)
library(did)

# Event study for Payer Mix 
payer_did <- hospdata %>% 
    filter(!is.na(private_prop_discharges), 
    private_prop_discharges >= quantile(private_prop_discharges, 0.01, na.rm = TRUE),
    private_prop_discharges <= quantile(private_prop_discharges, 0.98, na.rm = TRUE))
    
att_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = payer_did,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ beds,               # covariates (use ~1 if none)
                est_method = "dr",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )

summary(att_payermix)
ggdid(att_payermix)
overall_att_payermix <- aggte(att_payermix, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix)
overall_payermix <- ggdid(overall_att_payermix)
overall_payermix_pretty <- overall_payermix +
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 

ggsave("sumplots/events/overall_payermix_pretty.png", plot = overall_payermix_pretty, width = 10, height = 8, dpi = 300) 


# Event Study for Medicaid Discharges 
dis_did <- hospdata %>% 
    filter(!is.na(mcaid_prop_discharges), 
    mcaid_prop_discharges >= quantile(mcaid_prop_discharges, 0.01, na.rm = TRUE),
    mcaid_prop_discharges <= quantile(mcaid_prop_discharges, 0.99, na.rm = TRUE))

att_mcaid_prop_dis <- att_gt(yname = "mcaid_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = dis_did,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ beds,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_mcaid_prop_dis)
ggdid(att_mcaid_prop_dis)

overall_att_mcaid_prop_dis <- aggte(att_mcaid_prop_dis, type = "dynamic", na.rm = TRUE)
summary(overall_att_mcaid_prop_dis)
 
# overall event study plot & title 
overall_mcaid_prop_dis <- ggdid(overall_att_mcaid_prop_dis)

# medicaid discharges proportion pretty 
overall_med_dis_pretty <- overall_mcaid_prop_dis +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Share of Medicaid Discharges"
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 

ggsave("sumplots/events/overall_med_dis_pretty2.png", plot = overall_med_dis_pretty, width = 10, height = 8, dpi = 300)

# Event Study for ucc_prop
att_ucc_prop <- att_gt(yname = "ucc_prop",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ beds,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_ucc_prop)
ggdid(att_ucc_prop)

overall_att_ucc_prop <- aggte(att_ucc_prop, type = "dynamic", na.rm = TRUE)
summary(overall_att_ucc_prop)

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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_ucc_pretty)
ggsave("sumplots/events/overall_ucc_pretty2.png", plot = overall_ucc_pretty, width = 10, height = 8, dpi = 300)
