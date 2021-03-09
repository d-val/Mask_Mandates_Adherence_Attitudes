library('survey')
library(tidyr)
library(lfe)
library(stargazer)
library(stringr)
library(broom)
library(stringi)
library(readr)
library(ggplot2)
library(RCurl)
library(httr)
library(cdlTools)
library(dplyr)

remove_na <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

survey_import_df <- read_csv("survey_mobility_cases_new.csv")

all_coef_df_cases = data.frame(matrix(ncol = 10, nrow = 0))
names(all_coef_df_cases) = c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "wave", 'outcome', 'independent')

list_reg_models_cases <- list()


simplified_survey <- remove_na(simplified_survey,
                               c("weight_full_survey", 
                                 "geoip_country",
                                 'community_norms_mask'
                               )
)

covid_svy_design <- svydesign(ids=~id,
                              strata=~geoip_country,
                              weights=~weight_full_survey,
                              data=simplified_survey)




reg <- svyglm(
    new_cases_smoothed_100K ~
    community_norms_mask
  + population_density
  + human_development_index
  + new_tests_smoothed_per_thousand
  
  # mobility controls
  + retail_and_recreation_percent_change_from_baseline
  + grocery_and_pharmacy_percent_change_from_baseline
  + parks_percent_change_from_baseline
  + transit_stations_percent_change_from_baseline
  + workplaces_percent_change_from_baseline
  + residential_percent_change_from_baseline
  
  , design=covid_svy_design
)


coef <- tidy(reg, conf.int = TRUE)
coef$outcome = 'Cases per 100K'
coef$independent = 'Mask Attitude'
coef <- subset(coef, term == 'community_norms_mask')
all_coef_df_cases <- rbind(all_coef_df_cases, coef)
reg1 <- reg
list_reg_models_cases['case_attitude'] <- reg 





reg <- svyglm(
  new_deaths_smoothed_100K ~
    community_norms_mask
  + population_density
  + human_development_index
  + new_tests_smoothed_per_thousand
  
  # mobility controls
  + retail_and_recreation_percent_change_from_baseline
  + grocery_and_pharmacy_percent_change_from_baseline
  + parks_percent_change_from_baseline
  + transit_stations_percent_change_from_baseline
  + workplaces_percent_change_from_baseline
  + residential_percent_change_from_baseline
  
  , design=covid_svy_design
)


coef <- tidy(reg, conf.int = TRUE)
coef$outcome = 'Deaths per 100K'
coef$independent = 'Mask Attitude'
coef <- subset(coef, term == 'community_norms_mask')
all_coef_df_cases <- rbind(all_coef_df_cases, coef)
reg2 <- reg
list_reg_models_cases['death_attitude'] <- reg 



simplified_survey <- remove_na(simplified_survey,
                               c("weight_full_survey", 
                                 "geoip_country",
                                 'distancing_norms_wear_a_face_mask_or_covering'
                               )
)

simplified_survey$new_cases_smoothed_100K <- simplified_survey$new_cases_smoothed_per_million*10
simplified_survey$new_deaths_smoothed_100K <- simplified_survey$new_deaths_smoothed_per_million*10

covid_svy_design <- svydesign(ids=~id,
                              strata=~geoip_country,
                              weights=~weight_full_survey,
                              data=simplified_survey)

reg <- svyglm(
    new_cases_smoothed_100K ~

    distancing_norms_wear_a_face_mask_or_covering
  + population_density
  + human_development_index
  + new_tests_smoothed_per_thousand
  
  # mobility controls
  + retail_and_recreation_percent_change_from_baseline
  + grocery_and_pharmacy_percent_change_from_baseline
  + parks_percent_change_from_baseline
  + transit_stations_percent_change_from_baseline
  + workplaces_percent_change_from_baseline
  + residential_percent_change_from_baseline
  
  , design=covid_svy_design
)


coef <- tidy(reg, conf.int = TRUE)
coef$outcome = 'Cases per 100K'
coef$independent = 'Mask Adherence'

coef <- subset(coef, term == 'distancing_norms_wear_a_face_mask_or_covering')

all_coef_df_cases <- rbind(all_coef_df_cases, coef)
reg3 <- reg
list_reg_models_cases['case_adherence'] <- reg 






reg <- svyglm(
  new_deaths_smoothed_100K ~
    distancing_norms_wear_a_face_mask_or_covering
  + population_density
  + human_development_index
  + new_tests_smoothed_per_thousand
  
  # mobility controls
  + retail_and_recreation_percent_change_from_baseline
  + grocery_and_pharmacy_percent_change_from_baseline
  + parks_percent_change_from_baseline
  + transit_stations_percent_change_from_baseline
  + workplaces_percent_change_from_baseline
  + residential_percent_change_from_baseline
  
  , design=covid_svy_design
)


coef <- tidy(reg, conf.int = TRUE)
coef$outcome = 'Deaths per 100K'
coef$independent = 'Mask Adherence'
coef <- subset(coef, term == 'distancing_norms_wear_a_face_mask_or_covering')

all_coef_df_cases <- rbind(all_coef_df_cases, coef)
reg4 <- reg
list_reg_models_cases['deaths_adherence'] <- reg 


all_coef_df_cases <- all_coef_df_cases[c(1:3, 8:10)]

all_coef_df_cases$CI = all_coef_df_cases$std.error * 1.96
all_coef_df_cases$high = all_coef_df_cases$estimate + all_coef_df_cases$CI
all_coef_df_cases$low = all_coef_df_cases$estimate - all_coef_df_cases$CI





ggplot(data=all_coef_df_cases, aes(x=independent, y=estimate)) +
  geom_point(color = 'darkblue',size = 2.0)+
  geom_errorbar(aes(ymin=estimate-CI, ymax=estimate+CI),
                width=.5,
                size=1,
                color = 'darkblue',
                position=position_dodge(.9)) +
  ggtitle("Effect of Community Mask Adherence and 
  Attitude on COVID-19 Outcomes") +
  xlab("Survey Question") +
  ylab("Estimated Change in COVID-19 Outcome")+
  geom_hline(yintercept=0, color = 'red', alpha = 1.0) +
  facet_wrap(.~outcome, scales = "free", ncol = 2)+
  theme_linedraw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.background =element_rect(color="white"))+
  theme(strip.text = element_text(colour = 'black'))


ggsave('plots/survey_main_plot.pdf',
       width = 7,
       height = 4,
       dpi = 300)

star_out <- stargazer(
                      reg1,
                      reg2,
                      reg3,
                      reg4,
                      title="Results",
                      align=TRUE
)

