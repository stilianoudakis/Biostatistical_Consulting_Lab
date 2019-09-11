####Complete Case Analysis QoL#######
#patients that had responses for all time points

#Summary Statistics
tapply(completed$QoL_score, completed$timing_of_survey, mean)
#0        1        3 
#64.53901 63.12057 67.73050 


##Mean, minimum and maximum values of QoL and uncertainty for each age group
tapply(completed$QoL_score, completed$Age_Group, mean)
#0        1 
#67.59259 62.56039 

tapply(completed$QoL_score, completed$Age_Group, min)
#0        1 
#16.66667  0.00000 

tapply(completed$QoL_score, completed$Age_Group, max)
#0   1 
#100 100 

########################################################################

#####Plots of QoL over time and a function of Age#####

completed$ID <- as.numeric(as.character(completed$ID))

#Spaghetti plots for each patient
xyplot(QoL_score~timing_of_survey | ID, 
       data=completed, 
       panel=function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x,y)
       }, as.table=T)

##smoothing
xyplot(QoL_score~timing_of_survey | ID, data=completed,
       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
       xlab = "Time", ylab = "QoL",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") }, as.table=T)

##ols plots (same as first plot)
#xyplot(QoL_score ~ timing_of_survey | ID, data=completed, 
#       panel = function(x, y){
#         panel.xyplot(x, y)
#        panel.lmline(x, y)
#       }, as.table=T)

#Plotting regressions for each subject
qol_ols_pp <- by(completed, factor(completed$ID), function(x) summary(lm(QoL_score ~ timing_of_survey, data=x)))

## stem plot for fitted rate of change
rate <- by(completed, factor(completed$ID), function(data) coefficients(lm(QoL_score ~ timing_of_survey, data = data))[[2]])
rate <- unlist(rate)
names(rate) <- NULL
summary(rate)
stem(rate, scale=2)

## stem plot for R sq
rsq <- by(completed, factor(completed$ID), function(data) summary(lm(QoL_score~ timing_of_survey, data = data))$r.squared)
rsq <- unlist(rsq)
names(rsq) <- NULL
summary(rsq)
stem(rsq, scale=2)

##Using ggplot
p <- ggplot(data = completed, aes(x = timing_of_survey, y = QoL_score, group = ID))
p + geom_line() #raw data
p + stat_smooth( method = "lm", se = FALSE) #ols for each patient across time
p + geom_line() + stat_smooth(aes(group=1), method = "lm", se = FALSE)

#obtaining the slopes from linear model by id
slopes <- by(completed, completed$ID,
             function(data) coefficients(lm(QoL_score ~ timing_of_survey, data=data))[[2]])  
slopes1 <- unlist(slopes)
names(slopes1) <- NULL
mean(slopes1)
sqrt(var(slopes1))

p + geom_line() + facet_grid(. ~ Age_Group) #separate by age group

p + stat_smooth( method = "lm", se = FALSE) + 
  facet_grid(.~Age_Group) #ols by age group

p + geom_line() + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) + 
  facet_grid(. ~ Age_Group) #grand mean at each time point for each age group

p + geom_line() + 
  stat_summary(aes(group = 1), geom = "point", fun.y = quantile, fun.args=(list(probs = c(0.25, 0.75))), shape = 17, size = 3) + 
  facet_grid(. ~ Age_Group) #25th and 75th percentile

p + geom_line() + 
  stat_smooth(aes(group = 1)) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) + 
  facet_grid(. ~ Age_Group)  #smoothing for each age group

p + geom_line() + 
  stat_smooth(aes(group = 1), method = "lm", se = FALSE) +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
  facet_grid(. ~ Age_Group)  #grand ols for each age group
 


