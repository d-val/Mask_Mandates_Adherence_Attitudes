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

regression_runner_deaths <- function(design) {  
  reg <- 
    svyglm(
        new_deaths_smoothed_per_million ~

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
      
      , design=design
    )
  
  return( reg )
  
}

list_reg_models_deaths <- list()


all_coef_df_deaths = data.frame(matrix(ncol = 8, nrow = 0))
names(all_coef_df_deaths) = c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "wave")

for (wave_num in sort(unique(survey_import_df$wave))){
  
  cat(paste(wave_num, '\n'))
  

  simplified_survey <- subset(survey_import_df, wave == wave_num)
  simplified_survey <- subset(simplified_survey, weight_full_survey != 0)
  simplified_survey <- remove_na(simplified_survey,
                                 c("weight_full_survey", "geoip_country")
  )
  
  
  covid_svy_design <- svydesign(ids=~id,
                                strata=~geoip_country,
                                weights=~weight_full_survey,
                                data=simplified_survey)
  
  reg <- regression_runner_deaths(covid_svy_design)
  
  
  coef <- tidy(reg, conf.int = TRUE)
  coef$wave <- wave_num
  
  # list_reg_models <- append(list_reg_models, reg)
  list_reg_models_deaths[[as.name(wave_num)]] <- reg 
  
  all_coef_df_deaths <- rbind(all_coef_df_deaths, coef)
  
  
}

star_out <- stargazer(list_reg_models_deaths[['1']],
                      list_reg_models_deaths[['2']],
                      list_reg_models_deaths[['3']],
                      list_reg_models_deaths[['4']],
                      list_reg_models_deaths[['5']],
                      list_reg_models_deaths[['6']],
                      list_reg_models_deaths[['7']],
                      list_reg_models_deaths[['8']],
                      keep.stat = c("n", "rsq"),
                      title="Results",
                      align=TRUE
)


mask_plot_coef_df_deaths <- subset(all_coef_df_deaths, term == 'distancing_norms_wear_a_face_mask_or_covering')
mask_plot_coef_df_deaths$wave <- as.factor(mask_plot_coef_df_deaths$wave)


mask_plot_coef_df_deaths$CI = mask_plot_coef_df_deaths$std.error * 1.96

ggplot(data=mask_plot_coef_df_deaths, aes(x=wave, y=estimate)) +
  geom_point(color = 'darkblue',size = 2.0)+
  geom_errorbar(aes(ymin=estimate-CI, ymax=estimate+CI),
                width=.5,
                size=1,
                color = 'darkblue',
                position=position_dodge(.9)) +
  ggtitle("Deaths") +
  xlab("Survey Wave") +
  ylab("Estimaged Change in Deaths Associated 
with Community Mask Adherance")+
  geom_hline(yintercept=0, color = 'red', alpha = 1.0) +
  scale_x_discrete(labels=c(
    "1" = "1: July 6-
July 20",
    "2" = "2: July 21-
August 3",
    "3" = "3: Aug 3-
Aug 16",
    "4" = "4: Aug 17-
Aug 30",
    "5" = "5: Aug 31-
Sep 13",
    "6" = "6: Sep 14-
Sep 27",
    "7" = "7: Sep 28-
Oct 11",
    "8" = "8: Oct 12-
Oct 25"
  ))+
  theme_linedraw()+
  ggtitle("Effect of Community Mask Adherance on COVID-19 Deaths")+
  theme(plot.title = element_text(hjust = 0.5))




ggsave('plots/compliance_deaths_distancing_norms_wear_a_face_mask_or_covering.pdf',
       width = 7,
       height = 4,
       dpi = 300)









