####Complete Case Analysis QoL#######
#patients that had responses for all time points

#Summary Statistics
tapply(completed$uncertain_score, completed$timing_of_survey, mean)
#0        1        3 
#38.47518 25.00000 24.82270 

##Mean, minimum and maximum values of QoL and uncertainty for each age group
tapply(completed$uncertain_score, completed$Age_Group, mean)

tapply(completed$uncertain_score, completed$Age_Group, min)

tapply(completed$uncertain_score, completed$Age_Group, max)





