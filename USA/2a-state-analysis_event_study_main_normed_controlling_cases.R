#### THIS FILE ASSUMES YOU ALREADY RUN 2-state-analysis-main-state-analysis_event_study_delay.R to get first two regression results (not controlling for cases) 

t_minus_str <- paste(to_vec(for(x in 10:1) paste("tminus", x, sep='')), collapse=" + ")
t_plus_str <- paste(to_vec(for(x in 1:40) paste("t", x, sep='')), collapse=" + ")
controls  <- c(
  "confirmed_cases_normed",
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
  "residential_sq",
  "STAYHOME_delta_day",
  "business_delta_day")
independent_variables <- paste(
  t_minus_str,
  paste(controls, collapse=" + "),
  t_plus_str,
  sep = " + "
)

fixed_effects_formula <- paste(
  "state:time",   # fixed effects and trends
  "0", # no IV 
  "state" # clustered errors
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
deaths_normed_cont <- paste(outcome_str, 
                       custom_tail_formula, sep=" ~ ")


reg <- felm(as.formula(deaths_normed_cont), data = wide)

reg_deaths_normed_control <- reg

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'Deaths per 100K (controlling for cases)'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]
deaths_normed_coeff_controlling <- coef





outcome_str <- "hospitalization"
hospitalization_cont <- paste(outcome_str,
                      tail_formula, sep=" ~ ")

reg <- felm(as.formula(hospitalization_cont), data = wide)


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
hospitalization_cont <- paste(outcome_str, 
                         custom_tail_formula, sep=" ~ ")

reg <- felm(as.formula(hospitalization_cont), data = wide)

#looking at the results of regression
# summary(reg)
reg_hospitalization_control <- reg

# prepare betas for coefficient plot
coef <- tidy(reg, conf.int = TRUE)
coef <- subset(coef, !term %in% terms_to_remove )

coef$term <- str_replace_all(coef$term, 'minus', '-')
coef$term <- str_replace_all(coef$term, 't', '')
coef$term <- as.numeric(coef$term)
coef$outcome <- 'Hospitalization proportion (controlling for cases)'
coef$estimate <- coef$estimate - coef[coef$term == -1, 'estimate'][[1]]
hospitalization_coeff_controlling <- coef




all_coeff <- bind_rows(
  deaths_normed_coeff,
  deaths_normed_coeff_controlling,
  hospitalization_coeff,
  hospitalization_coeff_controlling
)

all_coeff <- all_coeff[c(1:3, 8)]

all_coeff$CI = all_coeff$std.error * 1.96
all_coeff$high = all_coeff$estimate + all_coeff$CI 
all_coeff$low = all_coeff$estimate - all_coeff$CI 

ggplot(data=all_coeff, aes(x=term, y=estimate, color = outcome, fill = outcome)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin=estimate-CI, ymax=estimate+CI), alpha=0.2)+
  geom_hline(yintercept=0, color = 'red', alpha = 0.3) +
  geom_vline(xintercept=0, color = 'red', alpha = 0.3) +
  facet_wrap(.~outcome, 
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
  theme(strip.text = element_text(colour = 'black'))


# ggsave('plots/event_study_state_level_mandates_controlling.pdf',
#        width = 8,
#        height = 10,
#        dpi = 300)

star_out <- stargazer(
  reg_deaths_normed,
  reg_deaths_normed_control,
  reg_hospitalization,
  reg_hospitalization_control,
  title="Results",
  align=TRUE
)
