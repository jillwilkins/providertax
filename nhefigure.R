# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/20/2025
## Date Edited:   10/20/2025
## Goal:         Create motivational figure of total health expenditure     
##  

nhe <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/nhetable2.csv")
View(nhe)
 nhe <- nhe %>% 
 mutate(total = nhe2023dol / 1000000000) %>%
 mutate(percent_hosp = as.numeric(gsub("%", "", percent_hosp)))





# PANEL VIEW 
# if not already installed
install.packages('devtools', repos = 'http://cran.us.r-project.org') 

# note: "V" is capitalized
devtools::install_github('xuyiqing/panelView') 
library(panelView)

# for simple viewing add yes_tax to fmap_tax 
fmap_tax <- fmap_tax %>%
  mutate(
    yes_tax = case_when(
      !is.na(firsttax) & year >= firsttax ~ 1,  # taxed from firsttax onward
      TRUE ~ 0                                # never taxed or before firsttax
    )
  )
View(fmap_tax)
panelview <- panelview(
  firsttax ~ yes_tax,
  data = fmap_tax %>% filter(year < 2020),
  index = c("state", "year"),
  type = "treat",
  main = "Hospital Provider Tax Adoption Across States",
  legend.labs = c("Not Taxed", "Taxed"),
  xlab = "Year",
  ylab = "State"
)
ggsave("panelview.png", plot = panelview, width = 8, height = 8, dpi = 300)


