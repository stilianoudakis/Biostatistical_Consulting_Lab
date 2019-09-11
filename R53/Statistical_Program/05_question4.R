##################################################
#Question 4
#Those patients that had concurrent or neoadjuvant 
#chemotherapy, was their rate of chronic dysphagia higher?
################################################

#Question 4 dysphagia vs concurrent chemotherapy

chemo <- ifelse(is.na(concurrent_chemo_type),0,1)
chemo4 <- chemo[complete.cases(time_nutri_tube, 
                               dysphagia_chronic, 
                               chemo)]
chemo4 <- factor(chemo4)

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              chemo)]
dysphagia4 <- rep(NA,length(dysphagia))
dysphagia4[which(dysphagia <= 2)] <- 0
dysphagia4[which(dysphagia >= 3)] <- 1
dysphagia4 <- factor(dysphagia4)
table(dysphagia4)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             chemo)]
tube_place4 <- rep(NA, length(tube_place))
tube_place4[which(tube_place==0)] <- 0
tube_place4[which(tube_place >= 1)] <- 1
tube_place4 <- factor(tube_place4)
table(tube_place4)

q4table <- table(chemo4,dysphagia4)
q4matrix <- matrix(c(18,6,109,66),byrow=F,ncol=2,dimnames=list("Concurrent Chemotherapy"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q4matrix)		#not significant
chisq.test(q4matrix) #not sig
epi.2by2(q4matrix,method="cohort.count",conf.level=0.95)	#RR 1.70 (0.71, 4.09)
#odds ratio 1.82 (0.69, 4.81)


####Checking for confounding of Concurrent Chemotherapy

q4table2 <- table(tube_place4,dysphagia4,chemo4)
q4matrix2_nochemo <- matrix(c(4,22,2,44),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))
q4matrix2_yeschemo <- matrix(c(8,40,10,69),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

q4table3 <- table(tube_place4,chemo4,dnn=c("Timing of Tube", "Chemotherapy"))
q4matrix3 <- matrix(c(48,26,79,46),byrow=T,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chemotherapy"=c("Yes","No")))

epi.2by2(q4matrix3)     #not sig RR  1.03 (0.83, 1.27) OR 1.07 (0.59, 1.96)

epi.2by2(q4table2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
#Test of homogeneity of IRR: X2 test statistic: 2.572 p-value: 0.109
#Test of homogeneity of  OR: X2 test statistic: 1.079 p-value: 0.299

mantelhaen.test(q4table2) #notsignificant

epi.2by2(q4matrix2_nochemo)	#not significant RR  3.54 (0.69, 18.02)  OR 4.00 (0.68, 23.55)
procfrq(q4matrix2_nochemo)
epi.2by2(q4matrix2_yeschemo)	#not significant RR 1.32 (0.56, 3.10)  OR 1.38 (0.50, 3.78)
procfrq(q4matrix2_yeschemo)
#chemo is not effect modifier

q4dframe <- cbind.data.frame(chemo4,dysphagia4)
q4dframe2 <- cbind.data.frame(chemo4,dysphagia4,tube_place4)
#ggplot(q2dframe) + aes(x=surgery2,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q4dframe) + aes(x=chemo4,fill=dysphagia4) + geom_bar(position='fill')


logit4 <- glm(dysphagia2 ~ tube_place1 + chemo1+ tube_place1*chemo1 ,data=q4dframe2,family=binomial(link="logit"))
summary(logit4)
####################################################################
