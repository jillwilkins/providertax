hospdata %>%
  filter(!is.na(mcrnum)) %>%
  distinct(mcrnum, treatment_group) %>%
  count(treatment_group)

View(hospdata)
hospdata_st <- hospdata %>%
  filter(!is.na(state)) 
  
hospdata_st %>%
  distinct(mcrnum, treatment_group) %>%
  count(treatment_group)
