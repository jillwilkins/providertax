# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/9/2025
## Date Edited:   10/23/2025
## Goal:          Event Study Analysis         
## 

library(fixest)
library(did)
library(ggplot2)

# Payer Mix 
payer_messy <- hospdata %>% 
    filter(is.na(private_prop_discharges) |
    private_prop_discharges <= quantile(private_prop_discharges, 0.01, na.rm = TRUE) |
    private_prop_discharges >= quantile(private_prop_discharges, 0.98, na.rm = TRUE))
View(payer_messy)

hospdata <- hospdata %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(private_prop_discharges))) %>%
  filter(!all(is.na(mcaid_discharges))) %>%
  filter(!all(is.na(state))) %>%
  ungroup()

# pre treatment level beds 
hospdata <- hospdata %>%
  group_by(mcrnum) %>%
  mutate(prebeds = mean(beds[yes_tax == 0], na.rm = TRUE)) %>%
  ungroup()
View(hospdata %>% filter(!is.na(prebeds)))



payer_did %>%
  group_by(mcrnum) %>%
  summarise(unique_treats = n_distinct(treatment_num2)) %>%
  filter(unique_treats > 1)
payer_did <- payer_did %>%
  group_by(mcrnum) %>%
  mutate(treatment_num2 = first(treatment_num2)) %>%
  ungroup()

    
att_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata ,
                control_group = "notyettreated",  
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix, type = "group")
ggdid(att_payermix)

# event study; aggreegate treatment effects
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
print(overall_payermix_pretty)
 ggsave("sumplots/events/overall_payermix_pretty2.png", plot = overall_payermix_pretty, width = 10, height = 8, dpi = 300) 

# commercial payer mix by urban/rural status
#urban 
att_urb_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num2",                  # the column we created
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
                gname = "treatment_num2",                  # the column we created
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
                gname = "gname",                  # the column we created
                data = hospdata_analysis %>% filter (year >= 2004, year <= 2014),
                control_group = "notyettreated",          # or "nevertreated"
                xformla = ~ beds,                        # covariates (use ~1 if none)
                est_method = "ipw",                        # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_mcaid_prop_dis, type = "group")
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

print(overall_med_dis_pretty)
ggsave("sumplots/events/overall_mcaid_dis_pretty.png", plot = overall_med_dis_pretty, width = 10, height = 8, dpi = 300)

# Event Study for ucc_prop
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
                gname = "treatment_num2",                  # the column we created
                data = hospdata,
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ beds,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_psyemhos, type = "group")
ggdid(att_psyemhos)

overall_att_psyemhos <- aggte(att_psyemhos, type = "dynamic", na.rm = TRUE, min_e = -13, max_e = 13)
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


# total beds 
att_beds <- att_gt(yname = "beds",
                tname = "year",
                idname = "mcrnum",
                gname = "gname",                  # the column we created
                data = hospdata_analysis %>% filter (year >= 2004, year <= 2014),
                control_group = "notyettreated",  # or "notyettreated"                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_beds, type = "group")
ggdid(att_beds)

overall_att_beds <- aggte(att_beds, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_beds)
overall_beds <- ggdid(overall_att_beds)
overall_beds_pretty <- overall_beds +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Share of Total Beds"
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
print(overall_beds_pretty)
ggsave("sumplots/events/overall_beds_pretty.png", plot = overall_beds_pretty, width = 10, height = 8, dpi = 300)

# share of obbd beds 
hospdata_clean <- hospdata_clean %>%
  mutate(obbd_share = obbd / beds)
summary(hospdata_clean$obbd_share)

att_obbd <- att_gt(yname = "obbd",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata_clean %>% filter(beds > 30, obbd > 0),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ prebeds + ct_pre_income,                # covariates (use ~1 if none)
                est_method = "dr",            # doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_obbd, type = "group")
ggdid(att_obbd)

overall_att_obbd <- aggte(att_obbd, type = "dynamic", na.rm = TRUE, min_e = -13, max_e = 13)
summary(overall_att_obbd)
overall_obbd <- ggdid(overall_att_obbd)
overall_obbd_pretty <- overall_obbd +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Total OBBD"
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
print(overall_obbd_pretty)
ggsave("overall_obbd.png", plot = overall_obbd_pretty, width = 10, height = 8, dpi = 300)

colnames(hospdata)
# cost to charge ratio by urban/rural status

att_urb_ccr <- att_gt(yname = "cost_to_charge",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata %>% filter(rural == 0, cost_to_charge <= 1.5, cost_to_charge > 0),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "ipw",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_urb_ccr, type = "group")

# event study; aggreegate treatment effects
overall_att_urb_ccr <- aggte(att_urb_ccr, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_urb_ccr)

# aggregate plot 
overall_urb_ccr <- ggdid(overall_att_urb_ccr)
overall_urb_ccr_pretty <- overall_urb_ccr +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Cost to Charge Ratio (Urban Hospitals)"
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
print(overall_urb_ccr_pretty)
ggsave("sumplots/events/overall_urb_ccr_pretty.png", plot = overall_urb_ccr_pretty, width = 10, height = 8, dpi = 300) 

# rural 
att_rural_ccr <- att_gt(yname = "cost_to_charge",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  # the column we created
                data = hospdata %>% filter(rural == 1, cost_to_charge < 1.5, cost_to_charge > 0),
                control_group = "notyettreated",  # or "notyettreated"
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "ipw",# doubly-robust (optional)
                clustervars = "state",
                allow_unbalanced = TRUE
                )
summary(att_rural_ccr, type = "group")

# event study; aggreegate treatment effects
overall_att_rural_ccr <- aggte(att_rural_ccr, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_rural_ccr)

# aggregate plot 
overall_rural_ccr <- ggdid(overall_att_rural_ccr)
overall_rural_ccr_pretty <- overall_rural_ccr +
  ggtitle(" ") +
  labs(
    x = "Years Relative to Treatment",
    y = "Cost to Charge Ratio (Rural Hospitals)"
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
print(overall_rural_ccr_pretty)
ggsave("sumplots/events/overall_rural_ccr_pretty.png", plot = overall_rural_ccr_pretty, width = 10, height = 8, dpi = 300) 


# Keep treated cohorts 2004-2012
# Use adopters 2013+ as "not yet treated" controls
hospdata_analysis <- hospdata_analysis %>%
  mutate(
    analysis_group = case_when(
      gname == 0 ~ "Never Treated",           # Never adopters
      gname <= 2012 ~ "Early Adopter",        # Your treatment group
      gname >= 2013 ~ "Late Adopter"          # Control until they adopt
    )
  )

# For C&S, this means:
# - States adopting 2013+ serve as controls for 2004-2012 adopters
# - Then become treated themselves
# - C&S handles this automatically with staggered timing

result_test <- att_gt(
  yname = "ucc_prop",
  tname = "year",
  idname = "mcrnum",
  gname = "gname",  # Includes all cohorts
  data = hospdata_analysis,
  control_group = "notyettreated",  # Uses 2013+ as controls for early cohorts
  est_method = "dr"
)

hospdata_analysis <- hospdata_analysis %>%
  mutate(log_op = log(tot_operating_exp), 
         log_npr = log(net_pat_rev), 
         dis_per_bed = mcaid_discharges / beds, 
         log_mcc = log(mcaid_charges),
         hosplarge = ifelse(beds > 500, 1, 0))


result_test <- att_gt(yname = "cost_per_discharge",
                tname = "year",
                idname = "mcrnum",
                gname = "gname",                  
                data = hospdata_analysis %>% filter(state != "HI", year <= 2020),  
                control_group = "notyettreated",  
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state", 
                base_period = "universal",
                allow_unbalanced = TRUE
                )
warnings()

summary(result_test, type = "group")
ggdid(result_test)

agg_simple <- aggte(result_test, type = "simple")
summary(agg_simple)

agg_dynamic <- aggte(result_test, type = "dynamic", min_e = -6, max_e = 6)
summary(agg_dynamic)
ggdid(agg_dynamic)

agg_group <- aggte(result_test, type = "group")
summary(agg_group)
ggdid(agg_group)



# View results
summary(agg_simple)
summary(agg_dynamic)
ggdid(agg_dynamic)  

# Method 1: Filter data BEFORE running att_gt
hospdata_early <- hospdata_analysis %>%
  filter(gname <= 2012 | gname == 0)  # Early adopters + never treated

summary(hospdata_analysis$mcaid_discharges)

result_pre <- att_gt(
  yname = "ucc_prop",
  tname = "year",
  idname = "mcrnum",
  gname = "gname",
  data = hospdata_analysis,
  control_group = "notyettreated"
)

agg_simple_early <- aggte(result_pre, type = "simple")
summary(agg_simple_early)

agg_dynamic_early <- aggte(result_pre, type = "dynamic")
summary(agg_dynamic_early)
ggdid(agg_dynamic_early)

agg_group_early <- aggte(result_pre, type = "group")
summary(agg_group_early)

result_all <- att_gt(
  yname = "mcaid_prop_discharges",
  tname = "year",
  idname = "mcrnum",
  gname = "gname",
  data = hospdata_analysis,
  control_group = "notyettreated"
)

agg_simple_late <- aggte(result_all, type = "simple")
summary(agg_simple_late)

agg_dynamic_late <- aggte(result_all, type = "dynamic")
summary(agg_dynamic_late)
ggdid(agg_dynamic_late)

agg_group_late <- aggte(result_late, type = "group")
summary(agg_group_late)

