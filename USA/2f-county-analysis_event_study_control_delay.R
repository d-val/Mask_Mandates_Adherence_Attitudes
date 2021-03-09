library(tidyr)
library(lfe)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(broom)
library(stargazer)
library(comprehenr)


daily_w_mandates_df <- read_csv("data/output/daily_data_county_produced_no_state_mandates_2021-02-19 20.15.csv")

length(unique(daily_w_mandates_df$county))

daily_w_mandates_df = subset(daily_w_mandates_df, deaths_std > 0)
daily_w_mandates_df = subset(daily_w_mandates_df, confirmed_cases_std > 0)
daily_w_mandates_df = subset(daily_w_mandates_df, deaths_normed_delay_14days_growth != "Inf")
daily_w_mandates_df = subset(daily_w_mandates_df, deaths_normed_delay_14days_growth != "-Inf")

terms_to_remove <- c('completely_home_prop',
                     'full_time_work_prop',
                     'part_time_work_prop',
                     'median_home_dwell_time',
                     'confirmed_cases',
                     'deaths_normed',
                     'confirmed_cases_normed',
                     'deaths',
                     'new_test_rate', 
                     'totalTestResultsIncrease',
                     '(Intercept)',
                     'rank',
                     'retail_and_recreation',
                     'grocery_and_pharmacy',
                     'parks',
                     'transit_stations',
                     'workplaces',
                     'residential',
                     'grocery_and_pharmacy_sq',
                     'retail_and_recreation_sq',
                     'parks_sq',
                     'transit_stations_sq',
                     'workplaces_sq',
                     'residential_sq',
                     'precipitation_avg',
                     'temperature_avg',
                     "deaths_normed_delay_14days",
                     "deaths_normed_delay_14days_growth",
                     "confirmed_cases_normed_delay_14days",
                     "confirmed_cases_normed_delay_14days_growth",
                     "hospitalization_delay_14days",
                     "hospitalization_delay_14days_growth",
                     "STAYHOME_delta_day",
                     "business_delta_day"
)

daily_w_mandates_df <- subset(daily_w_mandates_df, new_test_rate >= 0)


wide <- daily_w_mandates_df
wide$delta_day <- as.character(wide$delta_day)
wide$delta_day = sub("^", "t", wide$delta_day )
wide$delta_day <- str_replace_all(wide$delta_day, '-', 'minus')


wide$dummy = 1
wide  = spread(wide, 
               key= delta_day, 
               value='dummy')

wide[is.na(wide)] <- 0

wide$time <- as.numeric(wide$time)
wide$county <- as.factor(wide$county)

t_minus_str <- paste(to_vec(for(x in 10:1) paste("tminus", x, sep='')), collapse=" + ")
t_plus_str <- paste(to_vec(for(x in 1:70) paste("t", x, sep='')), collapse=" + ")
controls  <- c(
  "new_test_rate", 
  "precipitation_avg", 
  "temperature_avg", 
  "retail_and_recreation", 
  "grocery_and_pharmacy", 
  "parks", 
  "transit_stations", 
  "workplaces", 
  "residential  ", 
  "grocery_and_pharmacy_sq", 
  "retail_and_recreation_sq", 
  "parks_sq", 
  "transit_stations_sq", 
  "workplaces_sq", 
  "residential_sq"
  ,"STAYHOME_delta_day",
  "business_delta_day"
  )
independent_variables <- paste(
  paste(controls, collapse=" + "),
  t_minus_str,
  t_plus_str,
  sep = " + "
)

fixed_effects_formula <- paste(
  "county:time",   # fixed effects and trends
  0, # no IV
  "county" # clustered errors
  , sep = " | ")

tail_formula <- paste(independent_variables, fixed_effects_formula, sep = " | ")


outcome_str <- "deaths_normed"
new_controls <- c(
  "deaths_normed_delay_14days",
  "deaths_normed_delay_14days_growth"
)
custom_tail_formula <- paste(
  paste(new_controls, collapse=" + "),
  tail_formula,
  sep = " + "
)
deaths_normed <- paste(outcome_str, 
                       custom_tail_formula, sep=" ~ ")

reg <- felm(as.formula(deaths_normed), data = wide)

reg_deaths_normed <- reg
summary(reg)

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'Deaths per 100K'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]
coef$plot_order <- 3
deaths_normed_coeff <- coef



outcome_str <- "confirmed_cases_normed"
new_controls <- c(
  "confirmed_cases_normed_delay_14days",
  "confirmed_cases_normed_delay_14days_growth"
)
custom_tail_formula <- paste(
  paste(new_controls, collapse=" + "),
  tail_formula,
  sep = " + "
)
confirmed_cases_normed <- paste(outcome_str, 
                                custom_tail_formula, sep=" ~ ")

reg <- felm(as.formula(confirmed_cases_normed), data = wide)
reg_confirmed_cases_normed <- reg

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'Cases per 100K'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]
coef$plot_order <- 1
confirmed_cases_normed_coeff <- coef



outcome_str <- "hospitalization"
new_controls <- c(
  "hospitalization_delay_14days",
  "hospitalization_delay_14days_growth"
)
custom_tail_formula <- paste(
  paste(new_controls, collapse=" + "),
  tail_formula,
  sep = " + "
)
hospitalization <- paste(outcome_str, 
                         custom_tail_formula, sep=" ~ ")

reg <- felm(as.formula(hospitalization), data = wide)

#looking at the results of regression
# summary(reg)
reg_hospitalization <- reg

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'Hospitalization Proportion'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]
coef$plot_order <- 2
hospitalization_coeff <- coef




all_coeff <- bind_rows(
  confirmed_cases_normed_coeff,
  deaths_normed_coeff,
  hospitalization_coeff
)

all_coeff <- all_coeff[c(1:3, 8:9)]

all_coeff$CI = all_coeff$std.error * 1.96
all_coeff$high = all_coeff$estimate + all_coeff$CI 
all_coeff$low = all_coeff$estimate - all_coeff$CI 

all_coeff$outcome_plot_order <- reorder(all_coeff$outcome, all_coeff$plot_order)


ggplot(data=all_coeff,
       aes(x=term, y=estimate, color = outcome, fill = outcome)) +
  geom_line()+
  geom_point()+
  # geom_ribbon(aes(ymin=estimate-std.error, ymax=estimate+std.error), alpha=0.2)+
  geom_ribbon(aes(ymin=estimate-CI, ymax=estimate+CI), alpha=0.2)+
  geom_hline(yintercept=0, color = 'red', alpha = 0.3) +
  geom_vline(xintercept=0, color = 'red', alpha = 0.3) +
  facet_wrap(.~outcome_plot_order,
             scales = "free", 
             ncol = 1)  +
  xlab("Days after mandate") +
  ggtitle("Effect of Mask Mandates on COVID-19 Outcomes")+
  ylab("Estimated Change in COVID-19 outcome")+
  theme_bw()+
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.background =element_rect(color="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  xlim(-10, 70)


# ggsave('plots/county_daily_event_study_county_level_mandates_delay.pdf',
# width = 8,
# height = 10,
# dpi = 300)
#
# star_out <- stargazer(
#   reg_confirmed_cases_normed,
#   reg_deaths_normed,
#   reg_hospitalization,
#   title="Results",
#   align=TRUE
# )


