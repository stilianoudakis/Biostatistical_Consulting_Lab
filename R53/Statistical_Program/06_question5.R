###################################################
#Question 5
#Does the weight loss from beginning to end of 
#radiation treatment based on whether the patient 
#had a PEG tube placed prior vs during radiation?
###################################################

#Question 5 tube placement vs wieght loss

summary(initial_weight)
summary(end_xrt_weight)
wtstart <- initial_weight[complete.cases(time_nutri_tube, 
                                         dysphagia_chronic, 
                                         initial_weight,
                                         end_xrt_weight)]
wtend <- end_xrt_weight[complete.cases(time_nutri_tube, 
                                       dysphagia_chronic, 
                                       initial_weight,
                                       end_xrt_weight)]

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              initial_weight,
                                              end_xrt_weight)]
dysphagia5 <- rep(NA,length(dysphagia))
dysphagia5[which(dysphagia <= 2)] <- 0
dysphagia5[which(dysphagia >= 3)] <- 1
dysphagia5 <- factor(dysphagia5)
table(dysphagia5)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             initial_weight,
                                             end_xrt_weight)]
tube_place5 <- rep(NA, length(tube_place))
tube_place5[which(tube_place==0)] <- 0
tube_place5[which(tube_place >= 1)] <- 1
tube_place5 <- factor(tube_place5)
table(tube_place5)

wtdiff <- wtstart - wtend

t.test(wtstart,wtend,paired=T)	#significant

t.test(wtdiff)

#which(tube_place5=="Prior")
wtstartprior <- wtstart[c(which(tube_place5==0))]
wtendprior <- wtend[c(which(tube_place5==0))]
t.test(wtstartprior,wtendprior,paired=TRUE)	#significant

#which(tube_place5=="Not Prior")
wtstartafter <- wtstart[c(which(tube_place5==1))]
wtendafter <- wtend[c(which(tube_place5==1))]
t.test(wtstartafter,wtendafter,paired=TRUE)	#significant

wtdiffprior <- wtstartprior-wtendprior
wtdiffafter <- wtstartafter-wtendafter
t.test(wtdiffprior,wtdiffafter)	#not significant
t.test(wtdiffprior)
t.test(wtdiffafter)

q5dframe <- cbind.data.frame(wtdiff,dysphagia5,tube_place5)

wtdiffplot <- qplot(wtdiff,geom='blank',fill=tube_place5,data=q5dframe) +
  geom_line(aes(y=..density..,colour=tube_place5),stat='density') +
  #scale_color_manual(labels = c("Prior", "Not Prior")) +
  geom_histogram(aes(y=..density..),alpha=.7,position='identity') +
  scale_colour_manual(name=c('Density'),
                      values=c('red','blue'),
                      labels = c("Prior", "Not Prior"))+
  scale_fill_discrete(guide=FALSE) +
  #geom_segment(aes(x = 9.146868, y = 0, xend = 14.502627, yend = 0),size=4,colour='red') +
  #theme(legend.position=c(.85,.85)) +
  #geom_segment(aes(x = 5.107884, y = 0, xend = 12.823366, yend = 0),size=2,colour='blue',alpha=.5) +
  #theme(legend.position=c(.85,.85)) +
  #stat_bin(bins = 20) +
  labs(title = "Difference in Weight for Patients With PEG Tubes") +
  xlab("Difference in Weight") +
  ylab("Density")


logit5 <- glm(dysphagia5 ~ tube_place5 + wtdiff + wtdiff*tube_place5, data=q5dframe, family = "binomial")
summary(logit5)
