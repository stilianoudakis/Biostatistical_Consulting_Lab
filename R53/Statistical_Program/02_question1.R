#########################################
#Question 1:
#What is the relationship between peg tube 
#and chronic dysphagia
######################################

dysphagia <- dysphagia_chronic[-c(which(is.na(time_nutri_tube) | is.na(dysphagia_chronic)))]
tube_place <- time_nutri_tube[-c(which(is.na(time_nutri_tube) | is.na(dysphagia_chronic)))]

dysphagia1 <- rep(NA,length(dysphagia))
dysphagia1[which(dysphagia <= 2)] <- 0
dysphagia1[which(dysphagia >= 3)] <- 1
dysphagia1 <- factor(dysphagia1)
table(dysphagia1)
#dysphagia1
#0   1 
#175  24

table(tube_place)
tube_place1 <- rep(NA, length(tube_place))
tube_place1[which(tube_place==0)] <- 0
tube_place1[which(tube_place >= 1)] <- 1
tube_place1 <- factor(tube_place1)
table(tube_place1)
#tube_place1
#0   1 
#125  74

q1table <- table(tube_place1,dysphagia1)
q1matrix <- matrix(c(12,62,12,113),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q1matrix)		#not significant
chisq.test(q1matrix)    #p-value = 0.2461
epi.2by2(q1matrix,method="cohort.count",conf.level=0.95)	#RR 1.69 (0.80, 3.56)	
#odds ratio 1.82 (0.77, 4.30)

q1dframe <- cbind.data.frame(tube_place1,dysphagia1)
ggplot(q1dframe) + aes(x=tube_place1,fill=dysphagia1) + geom_bar(position='dodge')
ggplot(q1dframe) + aes(x=tube_place1,fill=dysphagia1) + geom_bar(position='fill')	#proportions

#####################################################
#Adjusting for demographic variables using logistic regression
q1dframe <- cbind.data.frame(dysphagia=dysphagia_chronic[complete.cases(peg[,c(2,3,4,11,12,40)])], 
                             tube_placement=time_nutri_tube[complete.cases(peg[,c(2,3,4,11,12,40)])], 
                             dob=dob[complete.cases(peg[,c(2,3,4,11,12,40)])], 
                             gender=gender[complete.cases(peg[,c(2,3,4,11,12,40)])], 
                             enddate=date_diagnosis[complete.cases(peg[,c(2,3,4,11,12,40)])], 
                             ethnicity=ethnicity[complete.cases(peg[,c(2,3,4,11,12,40)])])

q1dframe$dysphagia <- ifelse(q1dframe$dysphagia <= 2, 0, 1)
q1dframe$tube_placement <- ifelse(q1dframe$tube_placement >= 1, 1, 0)

which(!grepl("/",q1dframe$dob))
which(!grepl("/",q1dframe$enddate))
q1dframe2 <- q1dframe[-c(42,107),]

q1dframe2$dob <- mdy(q1dframe2$dob)
q1dframe2$enddate <- mdy(q1dframe2$enddate)
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}
q1dframe2$patient_age <- age(q1dframe2$dob,q1dframe2$enddate)
which(q1dframe2$enddate<q1dframe2$dob)   #integer(0)

q1dframe2$dysphagia <- as.factor(q1dframe2$dysphagia)
q1dframe2$tube_placement <- as.factor(q1dframe2$tube_placement)
q1dframe2$gender <- as.factor(q1dframe2$gender)
q1dframe2$ethnicity <- as.factor(q1dframe2$ethnicity)
str(q1dframe2)

logit1 <- glm(dysphagia ~ tube_placement + 
                gender + 
                ethnicity + 
                patient_age,
              data=q1dframe2,
              family=binomial(link="logit"))
summary(logit1)
########################################################################

#performing multinomial regression
q1dframe <- cbind.data.frame(dysphagia=dysphagia_chronic[complete.cases(peg[,c(2,3,4,11,12,13)])], tube_placement=time_nutri_tube[complete.cases(peg[,c(2,3,4,11,12,13)])], dob=dob[complete.cases(peg[,c(2,3,4,11,12,13)])], gender=gender[complete.cases(peg[,c(2,3,4,11,12,13)])], dlf=date_last_followup[complete.cases(peg[,c(2,3,4,11,12,13)])], ethnicity=ethnicity[complete.cases(peg[,c(2,3,4,11,12,13)])])

q1dframe$dysphagia[which(q1dframe$dysphagia >= 2)] <- 2
q1dframe$tube_placement <- ifelse(q1dframe$tube_placement >= 1, 1, 0)
q1dframe <- q1dframe[-42,] 
q1dframe$dob <- mdy(q1dframe$dob)
q1dframe$dlf <- mdy(q1dframe$dlf)

q1dframe$patient.age <- age(q1dframe$dob,q1dframe$dlf)

q1dframe$dysphagia <- as.factor(q1dframe$dysphagia)
q1dframe$tube_placement <- as.factor(q1dframe$tube_placement)
q1dframe$gender <- as.factor(q1dframe$gender)
q1dframe$ethnicity <- as.factor(q1dframe$ethnicity)

ordinal1 <- polr(dysphagia~tube_placement + gender + ethnicity + patient.age,data=q1dframe,Hess=TRUE)
summary(ordinal1)
ctable1 <- coef(summary(ordinal1))
pval1 <- pnorm(abs(ctable1[,"t value"]),lower.tail = FALSE)*2
pval1