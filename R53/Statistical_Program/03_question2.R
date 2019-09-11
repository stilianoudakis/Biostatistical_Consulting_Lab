#######################################
#Question 2:
#Whether the patient had surgical resection
#before radiation changed the rates of dysphagia 
#for those with PEG tubes?
########################################

#Question 2

surgery <- ifelse(is.na(type_of_surgery),0,1)
surgery2 <- surgery[complete.cases(time_nutri_tube, 
                                   dysphagia_chronic, 
                                   surgery)]
surgery2 <- factor(surgery2)

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              surgery)]
dysphagia2 <- rep(NA,length(dysphagia))
dysphagia2[which(dysphagia <= 2)] <- 0
dysphagia2[which(dysphagia >= 3)] <- 1
dysphagia2 <- factor(dysphagia2)
table(dysphagia2)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             surgery)]
tube_place2 <- rep(NA, length(tube_place))
tube_place2[which(tube_place==0)] <- 0
tube_place2[which(tube_place >= 1)] <- 1
tube_place2 <- factor(tube_place2)
table(tube_place2)

q2table <- table(surgery2,dysphagia2)
q2matrix <- matrix(c(9,32,15,143),byrow=T,ncol=2,dimnames=list(Surgery=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q2matrix)		#significant
fisher.test(q2matrix)
epi.2by2(q2matrix,method="cohort.count",conf.level=0.95)	#RR 2.31 (1.09, 4.90)
#odds ratio 2.68 (1.08, 6.67)

#Check for confounding of the Surgery variable

q2table2 <- table(tube_place2,dysphagia2,surgery2,dnn=c("Timing of Tube","Chronic Dyshpagia","Surgery"))
q2matrix2_nosurg <- matrix(c(6,54,9,89),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))
q2matrix2_yessurg <- matrix(c(6,8,3,24),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

q2table3 <- table(tube_place2,surgery2,dnn=c("Timing of Tube", "Surgery"))
q2matrix3 <- matrix(c(14,60,27,98),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Surgery"=c("Yes","No")))

epi.2by2(q2matrix3)     #not sig RR  0.88 (0.49, 1.56) OR 0.85 (0.41, 1.74)

epi.2by2(q2table2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
#Test of homogeneity of IRR: X2 test statistic: 4.896 p-value: 0.027
#Test of homogeneity of  OR: X2 test statistic: 3.075 p-value: 0.08

mantelhaen.test(q2table2) #notsignificant

epi.2by2(q2matrix2_nosurg)	#not significant RR  1.09 (0.41, 2.91)  OR 1.10 (0.37, 3.26)
procfrq(q2matrix2_nosurg)
epi.2by2(q2matrix2_yessurg)	#significant RR 1.56 (0.97, 2.50)  OR 6.00 (1.21, 29.73)
procfrq(q2matrix2_yessurg)
#surgery is possible effect modifier

q2dframe <- cbind.data.frame(surgery2,dysphagia2)
q2dframe2 <- cbind.data.frame(surgery2,dysphagia2,tube_place2)
#ggplot(q2dframe) + aes(x=surgery2,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q2dframe) + aes(x=surgery2,fill=dysphagia2) + geom_bar(position='fill')



library(mosaic)
mosaic(q2table2,gp=shading_Friendly,split_vertical = TRUE)

########################################################################
logit2 <- glm(dysphagia2~tube_place2+surgery2,data=q2dframe2,family=binomial(link="logit"))
summary(logit2)

############################################################################
