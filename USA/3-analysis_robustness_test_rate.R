library(tidyr)
library(lfe)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(broom)
library(stargazer)

daily_w_mandates_df <- read_csv("mandates/data/output/daily_data_with_other_NPI_df_produced_2021-02-12 15.14.csv")

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
                     'temperature_avg'
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
wide$state <- as.factor(wide$state)


t_minus_str <- paste(to_vec(for(x in 10:1) paste("tminus", x, sep='')), collapse=" + ")
t_plus_str <- paste(to_vec(for(x in 1:40) paste("t", x, sep='')), collapse=" + ")
controls  <- c(
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
  "residential_sq")

independent_variables <- paste(
  paste(controls, collapse=" + "),
  t_minus_str,
  t_plus_str,
  sep = " + "
)

fixed_effects_formula <- paste(
  "state:time",   # fixed effects and trends
  "0", # no IV 
  "state" # clustered errors
  , sep = " | ")

tail_formula <- paste(independent_variables, fixed_effects_formula, sep = " | ")

outcome_str <- "new_test_rate"
full_formula <- paste(outcome_str, 
                      tail_formula, sep=" ~ ")

reg <- felm(as.formula(full_formula), data = wide)

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'new_test_rate'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]

coef$CI = coef$std.error * 1.96

ggplot(data=coef, aes(x=term, y=estimate)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=estimate-CI, ymax=estimate+CI), alpha=0.2, fill = 'darkgreen')+
  geom_hline(yintercept=0, color = 'red', alpha = 0.3) +
  geom_vline(xintercept=0, color = 'red', alpha = 0.3) +
  xlab("Weeks after mandate") +
  ylab("Estimated Change in COVID-19 
Test Rate (per 100K)")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("COVID-19 Testing Rate")+
  theme(plot.title = element_text(hjust = 0.5))


star_out <- stargazer(reg,
                      # keep.stat = c("n", "rsq"),
                      title="Results",
                      type="latex"
                      # style = "qje"
                      # align=TRUE
)
# ggsave('plots/robustness_testing_rate.pdf',
#        width = 8,
#        height = 5,
#        dpi = 300)

