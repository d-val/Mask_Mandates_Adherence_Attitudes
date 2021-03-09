# Organization of code and data
This file describes the content of this data and code folder. 

<img src="https://github.com/d-val/Mask_Mandates_Adherence_Attitudes/blob/main/USA/plot.png" width="500">


## Mask mandate analysis
The `USA` folder contains the code and data required to 
reproduce Result sections 2.1 and 2.2 in the manuscript (and their
accompanying robustness checks):

The files `1-data_preparation.ipynb` and
`1a-weather_data.ipynb` are used to download data from public APIs and for
data cleaning.

The file `2-state-analysis-main-state-analysis_event_study_delay.R` is used to produce Fig. 1 
and the files: `2a-state-analysis_event_study_main_normed_controlling_cases.R`
`2b-state-analysis-waves_analysis_event_study_main_normed.R`
`2d-state-analysis_event_study_main_normed_FM_ALL_mandates.R`
`2d-state-analysis_event_study_main_normed_FM_EMP_mandates.R`
`2e-state-analysis_event_study_main_normed_no_delay.R`
`2f-county-analysis_event_study_control_delay.R` and 
`3-analysis_robustness_test_rate.R` are for robutness checks.

The file `4-compliance_late_state_mandates.R` is used to produce Fig. 4 and it's 
accompanying robustness checks. `5-mask_adherance_outcomes.R` is used for the 
multilinear regression used in section 2.2.

## International survey data
All code and data is present in the `World` folder. 

Although we cannot share this data directly ourselves it is readily available
for researchers at `https://dataforgood.fb.com/docs/preventive-health-survey-request-for-data-access`
upon acceptance of the Facebook Data Use Agreement. Place this dataset in the 
`international_survey/CovidBeliefsBehaviorsNormsSurvey` folder.  

The `1-preparing-survey-data-for-regression.ipynb` file is used to process, clean and
merge the survey data with control variables and then outputs a file ready for complex
survey regression. 

The file `2-survey_all_together.R` is used to produce Fig. 5 while the other files 
`2a-survey_analysis_cases_adherence.R`, 
`2b-survey_analysis_cases_norms.R`, 
`2c-survey_analysis_deaths_adherence.R` and 
`2d-survey_analysis_deaths_norms.R` are used to produce the robustness checks. 