# GOAL: Run payer mix event study and save each version to compare with and without clusters

# cleaning 
# drop years are 2020
hospdata_clean <- hospdata %>%
  filter(year <= 2020)

# drop hospitals that have beds = NA 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%                       # group by hospital
  filter(!all(is.na(beds))) %>%            # keep hospitals with at least one non-NA HOSPBD
  ungroup()

# drop hospitals that have more than 5 NA beds 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(sum(is.na(beds)) <= 3) %>%  # keep hospitals with 5 or fewer NA beds
  ungroup()

# drop if hospitals have beds = 0 ever 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!any(beds == 0, na.rm = TRUE)) %>%  # drop hospitals with any zero beds
  ungroup()

# drop if hospitals ever have beds less than 10
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!any(beds < 30, na.rm = TRUE)) %>%  # drop hospitals with any beds < 30
  ungroup()

# drop hospitals that have NA for an outcome in all years 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(private_prop_discharges))) %>%   # keep hospitals with at least one non-NA outcome
  ungroup()

hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(mcaid_prop_discharges))) %>%   # keep hospitals with at least one non-NA outcome
  ungroup()

# drop if state is empty for all years
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(state))) %>%   # keep hospitals with at least one non-NA state
  ungroup()

# define pre treatment level covariates 
# pre treatment level beds 
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  mutate(prebeds = mean(beds[yes_tax == 0], na.rm = TRUE)) %>%
  ungroup()
summary(hospdata_clean$prebeds)
# go to add income data

hospdata_clean <- hospdata_clean %>%
  group_by(county) %>%
  mutate(ct_pre_income = mean(median_income[year < firsttax], na.rm = TRUE)) %>%  
  ungroup()
summary(hospdata_clean$ct_pre_income)

# view hospdata_clean pre income na 
hospdata_clean %>%
  filter(is.na(ct_pre_income)) %>%
  count(state, sort = TRUE)

# pass 1: clustered at the state leve, all hospitals, beds and income covariate. 
att_payermix <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter(private_prop_discharges <= 1),
                control_group = "notyettreated",  
                xformla = ~ prebeds + ct_pre_income,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix, type = "group")
ggdid(att_payermix)

# event study; aggreegate treatment effects
overall_att_payermix <- aggte(att_payermix, type = "dynamic", na.rm = TRUE, min_e = -5, max_e = 5)
summary(overall_att_payermix)

# plot for overall
overall_payermix <- ggdid(overall_att_payermix)
overall_payermix_pretty <- overall_payermix +
  ggtitle("Payermix- Clustered at State; Beds and Income Covariates") +
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
 ggsave("events_nov4/es_payermix_inc.png", plot = overall_payermix_pretty, width = 10, height = 8, dpi = 300) 

# pass 2: clustered at the hospital level, no covariates
att_payermix2 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata,
                control_group = "notyettreated",  
                xformla = ~ 1,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "mcrnum",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix2, type = "group")
ggdid(att_payermix2)

# event study; aggreegate treatment effects
overall_att_payermix2 <- aggte(att_payermix2, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix2)

# plot for overall
overall_payermix2 <- ggdid(overall_att_payermix2)
overall_payermix2_pretty <- overall_payermix2 +
  ggtitle("Payermix- Clustered at Hospital; No Covariates") +
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
print(overall_payermix2_pretty)
 ggsave("events_nov4/es_payermix2.png", plot = overall_payermix2_pretty, width = 10, height = 8, dpi = 300) 

# pass 3: clustered at the state leve, all hospitals, beds covariates. 
att_payermix3 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "-1",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix3, type = "group")
ggdid(att_payermix3)

# event study; aggreegate treatment effects
overall_att_payermix3 <- aggte(att_payermix3, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix3)

# plot for overall
overall_payermix3 <- ggdid(overall_att_payermix3)
overall_payermix3_pretty <- overall_payermix3 +
  ggtitle("Payermix- Clustered at State; All Hospitals, Pre Beds Covariate") +
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
print(overall_payermix3_pretty)
 ggsave("events_nov5/es_payermix3.png", plot = overall_payermix3_pretty, width = 10, height = 8, dpi = 300) 

# pass 4: clustered at the hospital level, beds covariates
att_payermix4 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "mcrnum",
                base_period = "-1",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix4, type = "group")
ggdid(att_payermix4)

# event study; aggreegate treatment effects
overall_att_payermix4 <- aggte(att_payermix4, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix4)

# plot for overall
overall_payermix4 <- ggdid(overall_att_payermix4)
overall_payermix4_pretty <- overall_payermix4 +
  ggtitle("Payermix- Clustered at Hospital; All Hospitals, Pre Beds Covariates") +
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
print(overall_payermix4_pretty)
 ggsave("events_nov5/es_payermix4.png", plot = overall_payermix4_pretty, width = 10, height = 8, dpi = 300) 

 # pass 6: rural covariates WONT RUN
att_payermix6 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata,
                control_group = "notyettreated",  
                xformla = ~ rural,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "mcrnum",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix6, type = "group")
ggdid(att_payermix6)

# event study; aggreegate treatment effects
overall_att_payermix2 <- aggte(att_payermix2, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix2)

# plot for overall
overall_payermix2 <- ggdid(overall_att_payermix2)
overall_payermix2_pretty <- overall_payermix2 +
  ggtitle("Payermix- Clustered at Hospital; Rural Covariates") +
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
print(overall_payermix2_pretty)
 ggsave("events_nov4/es_payermix2.png", plot = overall_payermix2_pretty, width = 10, height = 8, dpi = 300) 



# mcaid prop discharges clustered at the state leve, all hospitals, beds covariates. 
att_mcaid <- att_gt(yname = "mcaid_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "mcrnum",
                base_period = "-1",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_mcaid, type = "group")
ggdid(att_mcaid)

# event study; aggreegate treatment effects
overall_att_mcaid <- aggte(att_mcaid, type = "dynamic", na.rm = TRUE)
summary(overall_att_mcaid)

# plot for overall
overall_mcaid <- ggdid(overall_att_mcaid)
overall_mcaid_pretty <- overall_mcaid +
  ggtitle("Medicaid Share- Clustered at Hospital; All Hospitals, Pre Beds Covariate") +
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_mcaid_pretty)
ggsave("events_nov5/es_mcaid3.png", plot = overall_mcaid_pretty, width = 10, height = 8, dpi = 300) 

# with cluster at hospital level 
ggsave("events_nov5/es_mcaid4.png", plot = overall_mcaid_pretty, width = 10, height = 8, dpi = 300)

###############################################
# cleaning the data more 
unique(hospdata$year)
num_na_beds_2023 <- hospdata %>%
  filter(year == 2012, is.na(beds)) %>%  # keep rows for 2023 with NA beds
  summarise(num_hospitals = n_distinct(mcrnum))  # count unique hospitals

num_na_beds_2023

num_hospitals <- hospdata_clean %>%
  distinct(mcrnum) %>%
  count()
num_hospitals


################################################
states_pre2014 <- hospdata_clean %>%
  filter(firsttax < 2014) %>%   # keep only treatments before 2014
  distinct(state, firsttax) %>%  # unique state-year pairs
  arrange(firsttax)
View(states_pre2014)
# now at this point i will re run the analysis 
# CLEAN data: clustered at the state leve, all hospitals, beds covariates. 
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

# ATT GT for pre 2014 analysis
# Aggregate ATT by relative year
agg_pre2014 <- aggte(att_payermix5, type = "dynamic")

# Keep only relative years corresponding to calendar years < 2014
agg_pre2014_subset <- aggte(att_payermix5, type = "dynamic") 


 

ggsave("events_nov5/es_payermix5_2014.png", plot = overall_payermix5_pretty, width = 10, height = 8, dpi = 300)

# CLEAN data: clustered at the hosp level, all hospitals, beds covariates. 
att_payermix6 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "mcrnum",
                base_period = "-1",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix6, type = "group")
ggdid(att_payermix6)

# event study; aggreegate treatment effects
overall_att_payermix6 <- aggte(att_payermix6, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix6)

# plot for overall
overall_att_payermix6 <- ggdid(overall_att_payermix6)
overall_payermix6_pretty <- overall_att_payermix6 +
  ggtitle("Payermix- Clustered at Hospital; Clean Hosp, Pre Beds Covariate") +
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
print(overall_payermix6_pretty)
ggsave("events_nov5/es_payermix6.png", plot = overall_payermix6_pretty, width = 10, height = 8, dpi = 300)


# drop outliers and remake pretty plots 
# Compute overall cutoffs
private_cut <- quantile(hospdata_clean$private_prop_discharges, c(0.01, 0.99), na.rm = TRUE)
mcaid_cut   <- quantile(hospdata_clean$mcaid_prop_discharges, c(0.01, 0.99), na.rm = TRUE)

# Filter hospitals
hospdata_clean <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(
    all(private_prop_discharges >= private_cut[1] & private_prop_discharges <= private_cut[2]),
    all(mcaid_prop_discharges >= mcaid_cut[1] & mcaid_prop_discharges <= mcaid_cut[2])
  ) %>%
  ungroup()

# redo 
# CLEAN data: clustered at the state leve, all hospitals, beds covariates. 
att_payermix7 <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "-1",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix7, type = "group")
ggdid(att_payermix7)

# event study; aggreegate treatment effects
overall_att_payermix7 <- aggte(att_payermix7, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix7)

# plot for overall
overall_att_payermix7 <- ggdid(overall_att_payermix7)
overall_payermix7_pretty <- overall_att_payermix7 +
  ggtitle("Payermix- Clustered at State; Clean Hosp, Pre Beds Covariate") +
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
print(overall_payermix7_pretty)
ggsave("events_nov5/es_payermix7.png", plot = overall_payermix7_pretty, width = 10, height = 8, dpi = 300)

# CLEAN data: clustered at the hosp level, all hospitals, beds covariates. 
att_payermix_urb <- att_gt(yname = "private_prop_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter(rural == 0),  # only urban hospitals
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_payermix_urb, type = "group")
ggdid(att_payermix_urb)

# event study; aggreegate treatment effects
overall_att_payermix_urb <- aggte(att_payermix_urb, type = "dynamic", na.rm = TRUE)
summary(overall_att_payermix_urb)

# plot for overall
overall_att_payermix_urb <- ggdid(overall_att_payermix_urb)
overall_payermix_urb_pretty <- overall_att_payermix_urb +
  ggtitle("Payermix- Clustered at State; Clean Hosp, Pre Beds Covariate Urban") +
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
print(overall_payermix_urb_pretty)
ggsave("events_nov5/es_payermix_urb.png", plot = overall_payermix_urb_pretty, width = 10, height = 8, dpi = 300)


# MCAID with the clean data 
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

hospdata_log <- hospdata_clean %>%
  filter(!is.na(net_pat_rev), net_pat_rev > 0) %>%
  mutate(log_npr = log(net_pat_rev))



# npr 
att_npr <- att_gt(yname = "net_pat_rev",
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
summary(att_npr, type = "group")
ggdid(att_npr)

# event study; aggreegate treatment effects
overall_att_npr <- aggte(att_npr, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_npr)

# plot for overall
overall_npr <- ggdid(overall_att_npr)
overall_npr_pretty <- overall_npr +
  ggtitle("Net Patient Revenue- Clustered at State; Clean Hosp, Beds and Income Covariate") +
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
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_npr_pretty)
ggsave("events_nov5/es_npr.png", plot = overall_npr_pretty, width = 10, height = 8, dpi = 300)

ggsave("events_nov5/es_nprlog.png", plot = overall_npr_pretty, width = 10, height = 8, dpi = 300)

colnames(hospdata_clean)

# medicaid discharges from hcris 
att_mcd_num <- att_gt(yname = "mcaid_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter (mcaid_discharges > 0, !is.na(mcaid_discharges)),
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_mcd_num, type = "group")
ggdid(att_mcd_num)

# event study; aggreegate treatment effects
overall_att_mcd_num <- aggte(att_mcd_num, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_mcd_num)

# plot for overall
overall_mcd_num <- ggdid(overall_att_mcd_num)
overall_mcd_num_pretty <- overall_mcd_num +
  ggtitle("Medicaid Discharges- Clustered at State; Clean Hosp, Pre Beds Covariate") +
  labs(
    x = "Years Relative to Treatment",
    y = "Medicaid Discharges"
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
print(overall_mcd_num_pretty)
ggsave("events_nov5/es_mcd_num.png", plot = overall_mcd_num_pretty, width = 10, height = 8, dpi = 300)

# plot for total discharges 
att_totdch <- att_gt(yname = "tot_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter (tot_discharges > 0, !is.na(tot_discharges), tot_discharges < 1000000),
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_totdch, type = "group")
ggdid(att_totdch)

# event study; aggreegate treatment effects
overall_att_totdch <- aggte(att_totdch, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_totdch)

# plot for overall
overall_totdch <- ggdid(overall_att_totdch)
overall_totdch_pretty <- overall_totdch +
  ggtitle("Total Discharges- Clustered at State; Clean Hosp, Pre Beds Covariate") +
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
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10   )),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.4) +
  guides(colour = "none", fill = "none") 
print(overall_totdch_pretty)

ggsave("events_nov5/es_totdch.png", plot = overall_totdch_pretty, width = 10, height = 8, dpi = 300)


# private discharges num 
hospdata_clean <- hospdata_clean %>%
  mutate(private_discharges = tot_discharges - mcaid_discharges - mcare_discharges)
  
att_privdch <- att_gt(yname = "private_discharges",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter (private_discharges > 0, !is.na(private_discharges)),
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )

# group level summary
summary(att_privdch, type = "group")
ggdid(att_privdch)  

# event study; aggreegate treatment effects
overall_att_privdch <- aggte(att_privdch, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_privdch)

# plot for overall
overall_privdch <- ggdid(overall_att_privdch)
overall_privdch_pretty <- overall_privdch +
  ggtitle("Private Discharges- Clustered at State; Clean Hosp, Pre Beds Covariate") +
  labs(
    x = "Years Relative to Treatment",
    y = "Private Discharges"
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
print(overall_privdch_pretty)
ggsave("events_nov5/es_privdch.png", plot = overall_privdch_pretty, width = 10, height = 8, dpi = 300)


### CLEAN again for service mix 
# drop hospitals that have NA for an outcome in all years 
hospdata_cleanserv <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(obbd))) %>%   # keep hospitals with at least one non-NA outcome
  ungroup()

# service mix clustered at the state leve, all hospitals, beds covariates. 
att_obbd <- att_gt(yname = "obbd",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean %>% filter (obbd > 0),
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
# group level summary
summary(att_obbd, type = "group")
ggdid(att_obbd)

# event study; aggreegate treatment effects
overall_att_obbd <- aggte(att_obbd, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_obbd) 

# plot for overall
overall_obbd <- ggdid(overall_att_obbd)
overall_obbd_pretty <- overall_obbd +
  ggtitle("Service Mix- Clustered at State; Clean Hosp, Pre Beds Covariate") +
  labs(
    x = "Years Relative to Treatment",
    y = "Obstetrics Beds"
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
ggsave("events_nov5/es_obbd.png", plot = overall_obbd_pretty, width = 10, height = 8, dpi = 300)


# now for alchhos (alchbd) 
hospdata_cleanserv <- hospdata_clean %>%
  group_by(mcrnum) %>%
  filter(!all(is.na(alchhos))) %>%   # keep hospitals with at least one non-NA outcome
  ungroup()

att_alchhos <- att_gt(yname = "alchhos",
                tname = "year",
                idname = "mcrnum",
                gname = "treatment_num",                  
                data = hospdata_clean,
                control_group = "notyettreated",  
                xformla = ~ prebeds,               # covariates (use ~1 if none)
                est_method = "dr",
                clustervars = "state",
                base_period = "universal",
                allow_unbalanced = TRUE
                )
# group level summary
summary(att_alchhos, type = "group")
ggdid(att_alchhos)

# event study; aggreegate treatment effects
overall_att_alchhos <- aggte(att_alchhos, type = "dynamic", na.rm = TRUE, min_e = -8, max_e = 8)
summary(overall_att_alchhos)

# plot for overall
overall_alchhos <- ggdid(overall_att_alchhos)
overall_alchhos_pretty <- overall_alchhos +
  ggtitle("Service Mix- Clustered at State; Clean Hosp, Pre Beds Covariate") +
  labs(
    x = "Years Relative to Treatment",
    y = "SUD Unit"
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
ggsave("events_nov5/es_alchhos.png", plot = overall_alchhos_pretty, width = 10, height = 8, dpi = 300)

