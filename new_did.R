# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025
## Date Edited:   10/23/2025
## Goal:          Event Study Analysis         
## 

library(fixest)
library(did)

# Payer Mix 
payer_did <- hospdata %>% 
    filter(!is.na(private_prop_discharges), 
    private_prop_discharges >= quantile(private_prop_discharges, 0.01, na.rm = TRUE),
    private_prop_discharges <= quantile(private_prop_discharges, 0.98, na.rm = TRUE))

    
att_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "ipw",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix, type = "group")
ggdid(att_payermix)

# event study; aggreegate treatment effects
overall_att_payermix <- aggte(att_payermix, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
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
print(overall_payermix_pretty)
ggsave("sumplots/events/overall_payermix_pretty.png", plot = overall_payermix_pretty, width = 10, height = 8, dpi = 300) 

# commercial payer mix by urban/rural status
#urban 
att_urb_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata %>% filter(rural == 0),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "ipw",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_urb_payermix, type = "group")

# event study; aggreegate treatment effects
overall_att_urb_payermix <- aggte(att_urb_payermix, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_urb_payermix)

# aggregate plot 
overall_urb_payermix <- ggdid(overall_att_urb_payermix)
overall_urb_payermix_pretty <- overall_urb_payermix +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Commercial Share of Discharges (Urban Hospitals)"
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
print(overall_urb_payermix_pretty)
ggsave("sumplots/events/overall_urb_payermix_prettywbeds.png", plot = overall_urb_payermix_pretty, width = 10, height = 8, dpi = 300) 

# rural 
att_rural_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata %>% filter(rural == 1),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "ipw",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_rural_payermix, type = "group")

# event study; aggreegate treatment effects
overall_att_rural_payermix <- aggte(att_rural_payermix, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_rural_payermix)

# aggregate plot 
overall_rural_payermix <- ggdid(overall_att_rural_payermix)
overall_rural_payermix_pretty <- overall_rural_payermix +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Commercial Share of Discharges (Rural Hospitals)"
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
print(overall_rural_payermix_pretty)
ggsave("sumplots/events/overall_rural_payermix_prettywbeds.png", plot = overall_rural_payermix_pretty, width = 10, height = 8, dpi = 300) 

# Event Study for Medicaid Discharges 
dis_did <- hospdata %>% 
    filter(!is.na(mcaid_prop_discharges), 
    mcaid_prop_discharges >= quantile(mcaid_prop_discharges, 0.01, na.rm = TRUE),
    mcaid_prop_discharges <= quantile(mcaid_prop_discharges, 0.99, na.rm = TRUE))

att_mcaid_prop_dis <- att_gt(yname = "mcaid_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata %>% filter(year != 2013),
                control_group = "notyettreated",          # or "nevertreated"
                xformla = ~ 1,                        # covariates (use ~1 if none)
                est_method = "ipw",                        # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_mcaid_prop_dis, type = "group")
ggdid(att_mcaid_prop_dis)

overall_att_mcaid_prop_dis <- aggte(att_mcaid_prop_dis, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
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
print(overall_med_dis_pretty)
ggsave("sumplots/events/overall_mcaid_dis_pretty.png", plot = overall_med_dis_pretty, width = 10, height = 8, dpi = 300)

# Event Study for ucc_prop
att_ucc_prop <- att_gt(yname = "ucc_prop",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,                # covariates (use ~1 if none)
                est_method = "ipw",            # doubly-robust (optional)
                clustervars = "state",
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_ucc_pretty)
ggsave("sumplots/events/overall_ucc_pretty.png", plot = overall_ucc_pretty, width = 10, height = 8, dpi = 300)


# alchhos 
att_alchhos <- att_gt(yname = "alchhos",
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
summary(att_alchhos, type = "group")
ggdid(att_alchhos)

overall_att_alchhos <- aggte(att_alchhos, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_alchhos)
overall_alchhos <- ggdid(overall_att_alchhos)
overall_alchhos_pretty <- overall_alchhos +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Share of Alcohol and Drug Services"
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
print(overall_alchhos_pretty)
ggsave("sumplots/events/overall_alchhos_pretty.png", plot = overall_alchhos_pretty, width = 10, height = 8, dpi = 300)

# psyemhos
att_psyemhos <- att_gt(yname = "psyemhos",
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
summary(att_psyemhos, type = "group")
ggdid(att_psyemhos)

overall_att_psyemhos <- aggte(att_psyemhos, type = "dynamic", na.rm = TRUE, min_e = -5, max_e = 5)
summary(overall_att_psyemhos)
overall_psyemhos <- ggdid(overall_att_psyemhos)
overall_psyemhos_pretty <- overall_psyemhos +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Share of Psychiatric Emergency Services"
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
print(overall_psyemhos_pretty)
ggsave("sumplots/events/overall_psyemhos_pretty.png", plot = overall_psyemhos_pretty, width = 10, height = 8, dpi = 300)
