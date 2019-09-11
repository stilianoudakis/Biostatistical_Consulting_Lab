#####################################################
#Question 6
#Is the hemoglobin affected by the use of a PEG
#tube before or during radiation?
#####################################################

#Question 6 tube placement vs difference in hemoglobin level

summary(hgb_prert)
summary(hgb_postrt)
hemostart <- hgb_prert[complete.cases(time_nutri_tube, 
                                      dysphagia_chronic, 
                                      hgb_prert,
                                      hgb_postrt)]
hemoend <- hgb_postrt[complete.cases(time_nutri_tube, 
                                     dysphagia_chronic, 
                                     hgb_prert,
                                     hgb_postrt)]

dysphagia <- dysphagia_chronic[complete.cases(time_nutri_tube, 
                                              dysphagia_chronic, 
                                              hgb_prert,
                                              hgb_postrt)]
dysphagia6 <- rep(NA,length(dysphagia))
dysphagia6[which(dysphagia <= 2)] <- 0
dysphagia6[which(dysphagia >= 3)] <- 1
dysphagia6 <- factor(dysphagia6)
table(dysphagia6)

tube_place <- time_nutri_tube[complete.cases(time_nutri_tube, 
                                             dysphagia_chronic, 
                                             hgb_prert,
                                             hgb_postrt)]
tube_place6 <- rep(NA, length(tube_place))
tube_place6[which(tube_place==0)] <- 0
tube_place6[which(tube_place >= 1)] <- 1
tube_place6 <- factor(tube_place6)
table(tube_place6)

hemodiff <- hemostart - hemoend

t.test(hemostart,hemoend,paired=T)	#significant

t.test(wtdiff)

hemostartprior <- hemostart[c(which(tube_place6==0))]
hemoendprior <- hemoend[c(which(tube_place6==0))]
t.test(hemostartprior,hemoendprior,paired=TRUE)		#significant

#which(tube_place5=="Not Prior")
hemostartafter <- hemostart[c(which(tube_place6==1))]
hemoendafter <- hemoend[c(which(tube_place6==1))]
t.test(hemostartafter,hemoendafter,paired=TRUE)		#significant

hemodiffprior <- hemostartprior-hemoendprior
hemodiffafter <- hemostartafter-hemoendafter
t.test(hemodiffprior,hemodiffafter)		#not significant
t.test(hemodiffprior)
t.test(hemodiffafter)

q6dframe <- cbind.data.frame(hemodiff,dysphagia6,tube_place6)

hemodiffplot <- qplot(hemodiff,geom='blank',fill=tube_place6,data=q6dframe) +
  geom_line(aes(y=..density..,colour=tube_place6),stat='density') +
  geom_histogram(aes(y=..density..),alpha=.7,position='identity')+
  scale_colour_manual(name=c('Density'),
                      values=c('red','blue'),
                      labels = c("Prior", "Not Prior"))+
  scale_fill_discrete(guide=FALSE) +
  #geom_segment(aes(x = .251, y = 0, xend = 1.16898, yend = 0),colour='red',size=4) +
  #geom_segment(aes(x = .1314, y = 0, xend = 1.62722, yend = 0),colour='blue',size=2) +
  #theme(legend.position=c(.85,.85)) +
  labs(title = "Difference in Hemoglobin for Patients With PEG Tubes") +
  xlab("Difference in Hemoglobin") +
  ylab("Density")


logit6 <- glm(dysphagia6 ~ tube_place6  , data=data6, family = "binomial")
summary(logit6)

############################################################################
