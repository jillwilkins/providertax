# ==============================================================================
# MEDICAL CARE CPI: INFLATION ADJUSTMENT TABLE
# Same CPI series as adjusted.R (Medical Care CPI, CUUR0000SAM, annual
# averages), but with an adjustment factor to 2023 dollars instead of 2020 --
# 2023 is the natural "current" reference year for the tax-scope tables
# (it's the most recent year with broad state coverage throughout this
# sub-analysis). adjusted.R's own 2020-dollar adjustment for the main event
# studies is unaffected by this.
# ==============================================================================

library(dplyr)

medical_cpi <- tibble::tibble(
  year = 2003:2024,
  medical_cpi = c(
    260.0,  # 2003
    269.3,  # 2004
    278.8,  # 2005
    287.5,  # 2006
    296.0,  # 2007
    305.1,  # 2008
    311.4,  # 2009
    317.0,  # 2010
    323.6,  # 2011
    329.8,  # 2012
    336.0,  # 2013
    341.3,  # 2014
    351.1,  # 2015
    364.1,  # 2016
    376.8,  # 2017
    388.4,  # 2018
    401.3,  # 2019
    414.9,  # 2020
    424.9,  # 2021
    450.6,  # 2022
    476.8,  # 2023
    498.1   # 2024 (through Oct, subject to revision)
  )
) %>%
  mutate(
    cpi_2023          = medical_cpi[year == 2023],
    inflation_factor_2023 = cpi_2023 / medical_cpi
  )
