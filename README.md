# LSR_Asymp_v4
Analysis for update 4 of living systematic review on asymptomatic SARS-CoV-2 infections

## R scripts

### Forest Plots
<i>Q1.</i> Forest plot of proportion of people with asymptomatic SARS-CoV-2 infection, stratified by setting:   <b>Q1.R</b> <br/>
<i>Q2.1.</i> Forest plot of the secondary attack rate of SARS-CoV-2 infections comparing infections in contacts of asymptomatic and presymptomatic index cases: <b>Q2_1.R</b> <br/>
<i>Q2.2.</i> Forest plot of proportion (‘Prop.’) of SARS-CoV-2 infection resulting from asymptomatic or presymptomatic transmission: <b>Q2_2.R</b><br/>

### Risk of Bias Analysis
 
### Shiny Applications
To be able to distribute screening tasks to a ‘crowd’, we build two shiny apps that communicate with the central database.
Records are attibuted to members of the crowd for <i>screening</i> <b>(RshinyApp-Screening)</b>. When the task is completed, the decisions are <i>verified</i> <b>(RshinyApp-Verification)<b> by a second member of the crowd.
Disagreement is resolved by the coordinator or by a third crowd member