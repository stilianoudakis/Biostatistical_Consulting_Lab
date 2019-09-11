####Model Buiding for Complete Cases QoL#######

completed$ID <- factor(completed$ID)
completed$timing_of_survey <- factor(completed$timing_of_survey)
#completed$timing_of_survey <- as.numeric(as.character(completed$timing_of_survey))

#####
#QoL
#####

model1<- lme(QoL_score~factor(timing_of_survey)*Age_cont, data=completed, random= ~timing_of_survey | ID, method="ML")
summary(model1)

model1a<- lme(QoL_score~factor(timing_of_survey)*Age_Group, data=completed, random= ~timing_of_survey | ID, method="ML")
summary(model1a)
a<-fitted(lme(QoL_score~factor(timing_of_survey)*Age_Group, data=completed, random= ~timing_of_survey | ID, method="REML"))
a<-unlist(a)
interaction.plot(factor(completed$timing_of_survey), completed$Age_Group, a, xlab="Timing", ylab="QoL", lwd=4, lty=1, col=4:5)
