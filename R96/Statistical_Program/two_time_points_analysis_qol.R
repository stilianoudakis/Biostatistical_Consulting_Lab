####Baseline and T1 Analysis QoL#######
#patients that had responses for baseline and at 1 month only

#Summary Statistics
tapply(two_time_pts$QoL_score, two_time_pts$timing_of_survey, mean)
#0        1 
#61.78862 59.85772 

##Mean, minimum and maximum values of QoL and uncertainty for each age group
tapply(two_time_pts$QoL_score, two_time_pts$Age_Group, mean)
#0        1 
#60.36585 61.28049 

tapply(two_time_pts$QoL_score, two_time_pts$Age_Group, min)
#0 1 
#0 0

tapply(two_time_pts$QoL_score, two_time_pts$Age_Group, max)
#0   1 
#100 100 

########################################################################

#####Plots of QoL over time and a function of Age#####

two_time_pts$ID <- as.numeric(as.character(two_time_pts$ID))

#Spaghetti plots for each patient
xyplot(QoL_score~timing_of_survey | ID, 
       data=two_time_pts, 
       panel=function(x,y){
         panel.xyplot(x, y)
         panel.lmline(x,y)
       }, as.table=T)

##smoothing
#xyplot(QoL_score~timing_of_survey | ID, data=two_time_pts,
#       prepanel = function(x, y) prepanel.loess(x, y, family="gaussian"),
#       xlab = "Time", ylab = "QoL",
#       panel = function(x, y) {
#         panel.xyplot(x, y)
#         panel.loess(x,y, family="gaussian") }, as.table=T)


##Using ggplot
p <- ggplot(data = two_time_pts, aes(x = timing_of_survey, y = QoL_score, group = ID))
p + geom_line() #raw data
p + geom_line() + stat_smooth(aes(group=1), method = "lm", se = FALSE)
#p + stat_smooth( method = "lm", se = FALSE) #ols for each patient across time

#obtaining the slopes from linear model by id
slopes <- by(two_time_pts, two_time_pts$ID,
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


