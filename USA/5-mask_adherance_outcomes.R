library(tidyr)
library(lfe)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(broom)


mask_adherance_all_df <- read_csv("mandates/data/output/mask_adherance_merge_outcomes_all_states_produced_2020-12-15 11.01.csv") 

mask_adherance_all_df <- subset(mask_adherance_all_df, new_test_rate >= 0)


mask_adherance_all_df$state <- as.factor(mask_adherance_all_df$state)


reg <- felm(as.formula(
  confirmed_cases ~
    compliance
  +new_test_rate
  + precipitation_avg
  + temperature_avg
  + grocery_and_pharmacy
  + parks
  + transit_stations
  + workplaces
  + residential
  + grocery_and_pharmacy_sq
  + retail_and_recreation_sq
  + parks_sq
  + transit_stations_sq
  + workplaces_sq
  + residential_sq
), 
data = mask_adherance_all_df)

reg_confirmed_cases <- reg

# summary(reg)

reg <- felm(as.formula(
  deaths ~
    compliance
  +new_test_rate
  + precipitation_avg
  + temperature_avg
  + grocery_and_pharmacy
  + parks
  + transit_stations
  + workplaces
  + residential
  + grocery_and_pharmacy_sq
  + retail_and_recreation_sq
  + parks_sq
  + transit_stations_sq
  + workplaces_sq
  + residential_sq
), 
data = mask_adherance_all_df)

summary(reg)

reg_deaths <- reg


# star_out <- stargazer(
#   reg_confirmed_cases,
#   reg_deaths,
#   keep.stat = c("n", "rsq"),
#   title="Results",
#   type="latex"
# )
