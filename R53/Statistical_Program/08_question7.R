###################################################
#Question 7
#Is the rate of chronic dysphagia after PEG tube 
#placement more likely in those patients who were 
#tobacco users or EtOH use? Does this change based 
#on whether patients had PEG placement before or 
#during radiation?
###################################################

#Question 7 dysphagia vs smoking

packyears <- pack_yrs_smoking[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic,
                                             pack_yrs_smoking)]
packyears7 <- factor(packyears)

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              pack_yrs_smoking)]
dysphagia7 <- rep(NA,length(dysphagia))
dysphagia7[which(dysphagia <= 2)] <- 0
dysphagia7[which(dysphagia >= 3)] <- 1
dysphagia7 <- factor(dysphagia7)
table(dysphagia7)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             pack_yrs_smoking)]
tube_place7 <- rep(NA, length(tube_place))
tube_place7[which(tube_place==0)] <- 0
tube_place7[which(tube_place >= 1)] <- 1
tube_place7 <- factor(tube_place7)
table(tube_place7)

table(packyears7)
levels(packyears7) <- c("<5 pk/yr",
                        "5-10 pk/yr",
                        "10-19 pk/yr",
                        "20-30 pk/yr",
                        ">30 pk/yr")
table(packyears7)
q7table <- table(packyears7, dysphagia7)
q7matrix <- matrix(c(3,2,1,3,9,9,10,12,29,78),
                   byrow=F,
                   ncol=2,
                   dimnames=list("Pack Years"=c("<5 pk/yr",
                                                "5-10 pk/yr",
                                                "10-19 pk/yr",
                                                "20-30 pk/yr",
                                                ">30 pk/yr"),
                                 "Chronic Dysphagia"=c("Yes",
                                                       "No")))

procfrq(q3bmatrix)

pk1vpk2 <- matrix(c(3,9,2,10),
                  byrow = T,
                  ncol = 2,
                  dimnames = list("Pack Years"=c("<5 pk/yr",
                                                 "5-10 pk/yr"),
                                  "Chronic Dysphagia"=c("Yes",
                                                        "No")))
epi.2by2(pk1vpk2)

pk1vpk3 <- matrix(c(3,9,1,12),
                  byrow = T,
                  ncol = 2,
                  dimnames = list("Pack Years"=c("<5 pk/yr",
                                                 "10-19 pk/yr"),
                                  "Chronic Dysphagia"=c("Yes",
                                                        "No")))
epi.2by2(pk1vpk3)

pk1vpk4 <- matrix(c(3,9,3,29),
                  byrow = T,
                  ncol = 2,
                  dimnames = list("Pack Years"=c("<5 pk/yr",
                                                 "20-30 pk/yr"),
                                  "Chronic Dysphagia"=c("Yes",
                                                        "No")))
epi.2by2(pk1vpk4)

pk1vpk5 <- matrix(c(3,9,9,78),
                  byrow = T,
                  ncol = 2,
                  dimnames = list("Pack Years"=c("<5 pk/yr",
                                                 ">30 pk/yr"),
                                  "Chronic Dysphagia"=c("Yes",
                                                        "No")))
epi.2by2(pk1vpk5)

q7dframe <- cbind.data.frame(tube_place7,dysphagia7,packyears7)

smokeplot <- ggplot(q7dframe, aes(x=dysphagia7,fill=packyears7)) + 
  scale_fill_discrete(name="Pack Years") + #,
  #                  labels=c("Not Chronic", "Chronic")) +
  scale_x_discrete(labels=c("No","Yes")) +
  geom_bar(
    width=.5,
    position= 'fill') + 
  xlab("Chronic Dysphagia") + 
  ylab("Count") +
  ggtitle("Dysphagia Count by Smoking Status") #+
#theme(axis.text.x=element_text(angle=90,hjust=1))



###################dysphagia vs etoh

etoh7 <- etoh[complete.cases(time_nutri_tube, 
                             dysphagia_chronic, 
                             etoh)]
etoh7 <- factor(etoh7)

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              etoh)]
dysphagia7 <- rep(NA,length(dysphagia))
dysphagia7[which(dysphagia <= 2)] <- 0
dysphagia7[which(dysphagia >= 3)] <- 1
dysphagia7 <- factor(dysphagia7)
table(dysphagia7)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             etoh)]
tube_place7 <- rep(NA, length(tube_place))
tube_place7[which(tube_place==0)] <- 0
tube_place7[which(tube_place >= 1)] <- 1
tube_place7 <- factor(tube_place7)
table(tube_place7)

q7table2 <- table(etoh7,dysphagia7)
q7matrix2 <- matrix(c(17,123,7,52),byrow=T,ncol=2,dimnames=list(EtOH=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q7matrix2)		#not significant
chisq.test(q7matrix2)
epi.2by2(q7matrix2,method="cohort.count",conf.level=0.95)	#RR 1.02 (0.45, 2.34)
#odds ratio 1.03 (0.40, 2.62)


#Checking for confounder of etoh on tube placement vs dysphagia

q7btable2 <- table(tube_place7,dysphagia7,etoh7)
epi.2by2(q7btable2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
mantelhaen.test(q7btable2)
#p-value = 0.2306
#common odds ratio 
#1.908685 
q7matrixa <- matrix(c(7,10,35,88),byrow=F,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))
epi.2by2(q7matrixa)  #RR 1.63 (0.67, 4.00) OR 1.76 (0.62, 4.99)
q7matrixb <- matrix(c(5,2,27,25),byrow=F,ncol=2,dimnames=list("Tube Placed Prior"=c("Yes","No"),"Chronic Dysphagia"=c("Yes","No")))
epi.2by2(q7matrixb)  #RR 1.32 (0.56, 3.10) OR 1.38 (0.50, 3.78)

q7bdframe <- cbind.data.frame(smoking7,etoh7,tube_place7,dysphagia7)
ggplot(q7bdframe) + aes(etoh7,fill=dysphagia7) + geom_bar(position='dodge')
ggplot(q7bdframe) + aes(etoh7,fill=dysphagia7) + geom_bar(position='fill')

##################################################################################################
