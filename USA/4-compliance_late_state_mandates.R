library(tidyr)
library(lfe)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(broom)

daily_w_mandates_df <- read_csv("mandates/data/output/mask_adherance_merge_outcomes_produced_2020-12-15 11.01.csv")


allowed_daily_values = c(
  -15, -14, -13, -12, -11,
  -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 
  0, 
  100000,
  10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
  20, 19, 18, 17, 16, 15, 14, 13, 12, 11
)

terms_to_remove <- c('completely_home_prop',
                     'full_time_work_prop',
                     'part_time_work_prop',
                     'median_home_dwell_time',
                     'confirmed_cases',
                     'deaths',
                     'new_test_rate', 
                     'totalTestResultsIncrease',
                     '(Intercept)',
                     'rank',
                     'grocery_and_pharmacy',
                     'parks',
                     'transit_stations',
                     'workplaces',
                     'residential',
                     "grocery_and_pharmacy_sq", 
                     "retail_and_recreation_sq", 
                     "parks_sq", 
                     "transit_stations_sq", 
                     "workplaces_sq", 
                     "residential_sq"
)

daily_w_mandates_df$date <- as.Date(daily_w_mandates_df$date)
daily_w_mandates_df$time <- daily_w_mandates_df$date - as.Date('2020-08-01')
daily_w_mandates_df$time <- as.integer(daily_w_mandates_df$time)

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
reg <- felm(as.formula(
  compliance ~

    deaths
  + confirmed_cases
  + new_test_rate
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
  + tminus8
  + tminus7
  + tminus6
  + tminus5
  + tminus4
  + tminus3
  + tminus2
  + tminus1
  + t0
  + t1 
  + t2 
  + t3 
  + t4 
  + t5
  + t6
  + t7
  + t8
  + t9
  + t10
  + t11
  + t12
  + t13
  | state:time   # fixed effects and trends
  | 0 # no IV
  | state # clustered errors
), 
data = wide)

#looking at the results of regression
# summary(reg)

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]

coef <- coef[c(1:3, 8)]

coef$CI = coef$std.error * 1.96
coef$high = coef$estimate + coef$CI 
coef$low = coef$estimate - coef$CI 

ggplot(data=coef, aes(x=term, y=estimate)) +
  geom_line()+
  geom_point()+
  geom_pointrange(aes(ymin=estimate-CI, ymax=estimate+CI), alpha=0.9, fill = 'darkgreen')+
  geom_hline(yintercept=0, color = 'red', alpha = 0.3) +
  geom_vline(xintercept=0, color = 'red', alpha = 0.3) +
  xlab("Days after mandate") +
  ylab("Estimated Change in Mask Adherance 
       (percentage points)")+
  theme_bw()+
  theme(legend.position = "none")+
ggtitle("Effect of Mask Mandates on Mask Adherence")+
  theme(plot.title = element_text(hjust = 0.5))


# star_out <- stargazer(reg,
#                       reg_no_control,
#                       title="Results",
#                       type="latex"
#                       # style = "qje"
#                       # align=TRUE
# )

# ggsave('plots/late_state_compliance.pdf',
# ggsave('plots/late_state_compliance_no_case_death_control.pdf',
#        width = 8,
#        height = 5,
#        dpi = 300)






