# Meta --------------------------------------------------------------------
## Author:        Jill Wilkins
## Date Created:  10/20/2025
## Date Edited:   10/20/2025
## Goal:         Create motivational figure of total health expenditure     
##  

nhe <- read.csv("/Users/jilldickens/Library/CloudStorage/OneDrive-Emory/data/input/nhetable2.csv")
View(nhe)
 nhe <- nhe %>% mutate(total = nhe2023dol / 1000000000)

library(ggplot2)
library(dplyr)






