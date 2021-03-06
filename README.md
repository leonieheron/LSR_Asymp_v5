# Overview
This repository contains the code for the fifth update of a living systematic review on asymptomatic and presymptomatic SARS-CoV-2 infection.

You can find more information on the current status of the review here : https://ispmbern.github.io/covid-19/#living-systematic-review

### Review questions

We conducted this living systematic review to address three questions: 
1. Amongst people who become infected with SARS-CoV-2, what proportion does not experience symptoms at all during their infection?
2. What is the infectiousness of asymptomatic and presymptomatic, compared with symptomatic, SARS-CoV-2 infection?
3. What proportion of SARS-CoV-2 transmission in a population is accounted for by people who are either asymptomatic throughout infection, or presymptomatic? 

## Extracted data

The code for the tables and figures is available here, if they were generated using an R script. Please note that we do not share the API tokens used to download the data from our REDCap database. All data extracted from the included studies are available for each question: 

-[All data extracted from the included studies](ExtractedData_v1.csv)  
-[Extracted data from studies included for Q1](Q1_ExtractedData.csv)  
-[Extracted data from studies included for Q2](Q2_ExtractedData.csv)  
-[Extracted data from studies included for Q3](Q3_ExtractedData.csv)  
-[Study characteristics for Q1 and Q2](study_characteristics.csv)  


### Forest plots

To create each forest plot, first download the corresponding .csv file above.  

<i>Q1.</i> Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection, stratified by setting:  [Q1.R](Q1.R)  
<i>Q2.</i> Forest plot of the secondary attack rate of SARS-CoV-2 infections comparing infections in contacts of asymptomatic and presymptomatic index cases: [Q2.R](Q2.R)   
<i>Q3.</i> Forest plot of proportion (‘Prop.’) of SARS-CoV-2 infection resulting from asymptomatic or presymptomatic transmission: [Q3.R](Q3.R)



### Risk of bias assessment
We used developed a risk of bias tool (see supplementary information in article) to assess risk of bias in studies included in Q1 and Q2.   
A summary table and plots of the risk-of-bias assessments were formatted using the [robvis](https://github.com/mcguinlu/robvis) package: [robisfunctions.R](robisfunctions.R)  


### RShiny applications

We used RShiny apps to screen titles and abstracts of studies identified in the search, perform data extraction, and assess risk of bias. The apps allowed the core team to delegate tasks to the ['crowd'](https://ispmbern.github.io/covid-19/crowd/) members: a group of volunteers who are helping us with the review.

#### Selection process apps
We built two shiny apps that communicate with the central database: one to screen potentially eligible studies and one to verify the screening decision.
Records are attibuted to members of the crowd for <i>screening</i> [(RshinyApp-Screening)](RshinyApp-Screening). When the task is completed, the decisions are <i>verified</i> [(RshinyApp-Verification)](RshinyApp-Verification) by a second member of the crowd.
Disagreement is resolved by the coordinator or by a third crowd member.

#### Data extraction apps
For included studies, one reviewer extracted data from full-text articles using either the extraction form in REDCap or [Data Extraction App](RshinyApp-Extraction), and a second reviewer verified the extracted data in REDCap.

#### Risk of bias apps
Two authors  independently assessed the <i>risk of bias</i> using customised shiny apps, [RshinyApp-RiskOfBias-FirstReviewer](RshinyApp-RiskOfBias-FirstReviewer) and [RshinyApp-RiskOfBias-SecondReviewer](RshinyApp-RiskOfBias-SecondReviewer), which saved responses into the REDCap database. A third reviewer resolved disagreements directly in REDCap.

### Tables and figures and corresponding R scripts

|          Name of table/figure                                                                                                                    |  Name of R script                |
|--------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------|
|Fig 1 - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection, stratified by study design. The x-axis displays proportions. Where more than one cluster was reported, clusters are annotated with '[cluster]'. The diamond shows the summary estimate and its 95% confidence interval. The red bar and red text show the prediction interval. 	                                       | Q1.R                              |
|Fig 2 - Forest plot of the secondary attack rate of SARS-CoV-2 infections comparing infections in contacts of asymptomatic and presymptomatic index cases with infections in contacts of symptomatic cases.| Q2.R                            |
|Fig 3 - Forest plot of proportion (‘Prop.’) of SARS-CoV-2 infection resulting from asymptomatic or presymptomatic transmission. For studies that report outcomes in multiple settings, these are annotated in brackets. | Q3.R                            |
|S2 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection.	                           | Q1.R                              |
|S3 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection, stratified by study design.  | Q1_stratified_all_settings.R      |
|S4 Fig - Risk of bias assessment of studies in question 1 and 2.1	                                                                               | ROB_analysis.R                    |
|S5 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection, restricted to studies with a sample size of at least 10.     | Q1.R                              |
|S6 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in contact and outbreak investigations by date of publication.| Q1.R |
|S7 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by date of publication.| Q1.R|
|S8 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in contact and outbreak investigations by risk of selection bias|Q1.R|
|S9 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by risk of selection bias.| Q1.R|
|S10 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in  contact and outbreak investigations by risk of information bias.| Q1.R|
|S11 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by risk of information bias.| Q1.R|
|S12 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in  contact and outbreak investigations by risk of misclassification bias.|Q1.R|
|S13 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by risk of misclassification bias.  |Q1.R|
|S14 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in contact and outbreak investigations by risk of attrition bias.  |Q1.R|
|S15 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by risk of attrition bias.|Q1.R |
|S16 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in contact and outbreak investigations by risk of bias in all domains. |Q1.R |
|S17 Fig - Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection in screening studies by risk of bias in all domains. |Q1.R |
|S18 Fig - Assessment of credibility of mathematical modelling studies.|Q1.R|
|S1 Table - Characteristics of studies reporting on proportion of asymptomatic SARS-CoV-2 infections (review question 1)|Q1_table_characteristics_full.Rmd |
|S3 Table - Location of studies contributing data to review question 1	                                                | S3_countries.Rmd        |
