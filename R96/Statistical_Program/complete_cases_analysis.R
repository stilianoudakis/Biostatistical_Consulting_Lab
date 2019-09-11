####Complete Case Analysis#######
#patients that had responses for all time points

#Summary Statistics
tapply(completed$QoL_score, completed$timing_of_survey, mean)
tapply(completed$uncertain_score, completed$timing_of_survey, mean)

##Mean, minimum and maximum values of QoL and uncertainty for each age group
tapply(completed$QoL_score, completed$Age_Group, mean)
tapply(completed$uncertain_score, completed$Age_Group, mean)

tapply(completed$QoL_score, completed$Age_Group, min)
tapply(completed$uncertain_score, completed$Age_Group, min)

tapply(completed$QoL_score, completed$Age_Group, max)
tapply(completed$uncertain_score, completed$Age_Group, max)

########################################################################

#####Plots of QoL over time and a function of Age#####

completed$ID <- as.numeric(as.character(completed$ID))

#lattice plots

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

##ols plots
xyplot(QoL_score ~ timing_of_survey | ID, data=completed, 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
       }, as.table=T)

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

##for QoL
###plot of the raw data
interaction.plot(factor(completed$timing_of_survey), completed$ID, completed$QoL_score, type = "l", legend = FALSE)
# fitting the linear model by ID
fit <- by(completed, completed$ID, function(bydata) fitted(lm(QoL_score ~ timing_of_survey, data=bydata))) 
fit <- unlist(fit)
# plotting the linear fit by id
interaction.plot(completed$timing_of_survey,completed$ID, fit, xlab="Timing", ylab="QoL", legend = FALSE)
#obtaining the slopes from linear model by id
slopes <- by(completed, completed$ID,
             function(data) coefficients(lm(QoL_score ~ timing_of_survey, data=data))[[2]])  
slopes1 <- unlist(slopes)
names(slopes1) <- NULL
mean(slopes1)
sqrt(var(slopes1))

##Using ggplot
p <- ggplot(data = completed, aes(x = timing_of_survey, y = QoL_score, group = ID))
p + geom_line()
p + stat_smooth( method = "lm", se = FALSE)
p + geom_line() + facet_grid(. ~ Age_Group)
p + stat_smooth( method = "lm", se = FALSE) + facet_grid(.~Age_Group)
p + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = mean,
                               shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = quantile, 
                               fun.args=(list(probs = c(0.25, 0.75))), shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),
                                                             geom = "point", fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p + geom_line() + stat_smooth(aes(group = 1), method = "lm", se = FALSE) +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
  facet_grid(. ~ Age_Group)
 
#More plots for QoL
trellis.device(color = FALSE) #turn color off

age_group_plot <- groupedData(QoL_score ~ timing_of_survey | ID,
                              outer = ~ Age_Group,
                              data = completed)
plot(age_group_plot, display="ID", outer=TRUE, aspect=2, key=FALSE, xlab = "Timing of Survey", ylab="QoL",main="QoL vs Time as a function of Age Group")

#plotting means

age_group_plot3 <- groupedData(QoL_score ~ timing_of_survey | Age_Group,
                               order.groups = FALSE,
                               data = completed)
plot(age_group_plot3, display="Age_Group", aspect=2, key=FALSE, xlab = "Timing of Survey", ylab="QoL",main="Mean QoL across time for each age group")


#####Plots of Uncertainty over time and a function of Age#####

p2 <- ggplot(data = completed, aes(x = timing_of_survey, y = uncertain_score, group = ID))
p2 + geom_line()
p2 + geom_line() + facet_grid(. ~ Age_Group)
p2 + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = mean,
                                shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p2 + geom_line() + stat_summary(aes(group = 1), geom = "point", fun.y = quantile, 
                                fun.args=(list(probs = c(0.25, 0.75))), shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p2 + geom_line() + stat_smooth(aes(group = 1)) + stat_summary(aes(group = 1),
                                                              geom = "point", fun.y = mean, shape = 17, size = 3) + facet_grid(. ~ Age_Group)
p2 + geom_line() + stat_smooth(aes(group = 1), method = "lm", se = FALSE) +
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3) +
  facet_grid(. ~ Age_Group)

#More plots for Uncertainty
age_group_plot2 <- groupedData(uncertain_score ~ timing_of_survey | ID,
                               outer = ~ Age_Group,
                               data = completed)
plot(age_group_plot2, display="ID", outer=TRUE, aspect=2, key=FALSE, xlab = "Timing of Survey", ylab="Uncertainty",main="Uncertainty vs Time as a function of Age Group")


##Means
age_group_plot4 <- groupedData(uncertain_score ~ timing_of_survey | Age_Group,
                               order.groups = FALSE,
                               data = completed)
plot(age_group_plot4, display="Age_Group", aspect=2, key=FALSE, xlab = "Timing of Survey", ylab="Uncertainty",main="Mean Uncertainty across time for each age group")

