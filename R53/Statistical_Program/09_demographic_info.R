######################################################
#Demographic Information
####################################################

#Age

d_o_b <- dob[complete.cases(peg[,c("dob",
                                   "date_diagnosis",
                                   "time_nutri_tube",
                                   "dysphagia_chronic")])]

enddate <- date_diagnosis[complete.cases(peg[,c("dob",
                                                "date_diagnosis",
                                                "time_nutri_tube",
                                                "dysphagia_chronic")])]

dysphagia_age <- dysphagia_chronic[complete.cases(peg[,c("dob",
                                                         "date_diagnosis",
                                                         "time_nutri_tube",
                                                         "dysphagia_chronic")])]

tube_place_age <- time_nutri_tube[complete.cases(peg[,c("dob",
                                                        "date_diagnosis",
                                                        "time_nutri_tube",
                                                        "dysphagia_chronic")])]								   

which(!grepl("/",d_o_b))
d_o_b2 <- d_o_b[-which(!grepl("/",d_o_b))]
enddate2 <- enddate[-which(!grepl("/",d_o_b))]
dysphagia_age2 <- dysphagia_age[-which(!grepl("/",d_o_b))]
tube_place_age2 <- tube_place_age[-which(!grepl("/",d_o_b))]

which(!grepl("/",enddate2))
d_o_b3 <- d_o_b2[-which(!grepl("/",enddate2))]
enddate3 <- enddate2[-which(!grepl("/",enddate2))]
dysphagia_age3 <- dysphagia_age2[-which(!grepl("/",enddate2))]
tube_place_age3 <- tube_place_age2[-which(!grepl("/",enddate2))]

dysphagia_demo <- rep(NA,length(dysphagia_age3))
dysphagia_demo[which(dysphagia_age3 <= 2)] <- 0
dysphagia_demo[which(dysphagia_age3 >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_age3)
tube_place_demo <- rep(NA, length(tube_place_age3))
tube_place_demo[which(tube_place_age3==0)] <- 0
tube_place_demo[which(tube_place_age3 >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

d_o_b3 <- mdy(d_o_b3)
enddate3 <- mdy(enddate3)
#subset(age,enddate<dob)
#which(is.na(dob))

which(enddate3<d_o_b3)
#d_o_b4 <- d_o_b3[-which(enddate3<d_o_b3)]
#enddate4 <- enddate3[-which(enddate3<d_o_b3)]

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

patient.age <- age(d_o_b3,enddate3)	

age_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,patient.age)

dim(age_dframe)
summary(age_dframe$patient.age[which(age_dframe$dysphagia_demo==0)])
summary(age_dframe$patient.age[which(age_dframe$dysphagia_demo==1)])
summary(age_dframe$patient.age[which(age_dframe$tube_place_demo==0)])
summary(age_dframe$patient.age[which(age_dframe$tube_place_demo==1)])
#######################################################################
#CT scan

dysphagia_ctscan <- dysphagia_chronic[complete.cases(peg[,c("ct_primary_size",
                                                            "time_nutri_tube",
                                                            "dysphagia_chronic")])]

tube_place_ctscan <- time_nutri_tube[complete.cases(peg[,c("ct_primary_size",
                                                           "time_nutri_tube",
                                                           "dysphagia_chronic")])]								   

ctscan <- ct_primary_size[complete.cases(peg[,c("ct_primary_size",
                                                "time_nutri_tube",
                                                "dysphagia_chronic")])]

dysphagia_demo <- rep(NA,length(dysphagia_ctscan))
dysphagia_demo[which(dysphagia_ctscan <= 2)] <- 0
dysphagia_demo[which(dysphagia_ctscan >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_ctscan)
tube_place_demo <- rep(NA, length(tube_place_ctscan))
tube_place_demo[which(tube_place_ctscan==0)] <- 0
tube_place_demo[which(tube_place_ctscan >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

ctscan_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,ctscan)

dim(ctscan_dframe)
summary(ctscan_dframe$ctscan[which(ctscan_dframe$dysphagia_demo==0)])
summary(ctscan_dframe$ctscan[which(ctscan_dframe$dysphagia_demo==1)])
summary(ctscan_dframe$ctscan[which(ctscan_dframe$tube_place_demo==0)])
summary(ctscan_dframe$ctscan[which(ctscan_dframe$tube_place_demo==1)])			
######################################################################
#Gender

dysphagia_gender <- dysphagia_chronic[complete.cases(peg[,c("gender",
                                                            "time_nutri_tube",
                                                            "dysphagia_chronic")])]

tube_place_gender <- time_nutri_tube[complete.cases(peg[,c("gender",
                                                           "time_nutri_tube",
                                                           "dysphagia_chronic")])]								   

gender <- gender[complete.cases(peg[,c("gender",
                                       "time_nutri_tube",
                                       "dysphagia_chronic")])]

dysphagia_demo <- rep(NA,length(dysphagia_gender))
dysphagia_demo[which(dysphagia_gender <= 2)] <- 0
dysphagia_demo[which(dysphagia_gender >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_gender)
tube_place_demo <- rep(NA, length(tube_place_gender))
tube_place_demo[which(tube_place_gender==0)] <- 0
tube_place_demo[which(tube_place_gender >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

gender_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,gender)

dim(gender_dframe)

prop.table(table(gender_dframe$gender[which(gender_dframe$dysphagia_demo==0)]))
prop.table(table(gender_dframe$gender[which(gender_dframe$dysphagia_demo==1)]))
prop.table(table(gender_dframe$gender[which(gender_dframe$tube_place_demo==0)]))
prop.table(table(gender_dframe$gender[which(gender_dframe$tube_place_demo==1)]))
########################################################################
#Ethnicity

dysphagia_eth <- dysphagia_chronic[complete.cases(peg[,c("ethnicity",
                                                         "time_nutri_tube",
                                                         "dysphagia_chronic")])]

tube_place_eth <- time_nutri_tube[complete.cases(peg[,c("ethnicity",
                                                        "time_nutri_tube",
                                                        "dysphagia_chronic")])]								   

eth <- ethnicity[complete.cases(peg[,c("ethnicity",
                                       "time_nutri_tube",
                                       "dysphagia_chronic")])]

dysphagia_demo <- rep(NA,length(dysphagia_eth))
dysphagia_demo[which(dysphagia_eth <= 2)] <- 0
dysphagia_demo[which(dysphagia_eth >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_eth)
tube_place_demo <- rep(NA, length(tube_place_eth))
tube_place_demo[which(tube_place_eth==0)] <- 0
tube_place_demo[which(tube_place_eth >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

eth_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,eth)

dim(eth_dframe)

prop.table(table(eth_dframe$eth[which(eth_dframe$dysphagia_demo==0)]))
prop.table(table(eth_dframe$eth[which(eth_dframe$dysphagia_demo==1)]))
prop.table(table(eth_dframe$eth[which(eth_dframe$tube_place_demo==0)]))
prop.table(table(eth_dframe$eth[which(eth_dframe$tube_place_demo==1)]))
######################################################################
#Radiation Dose

dysphagia_rad <- dysphagia_chronic[complete.cases(peg[,c("radiation_dose",
                                                         "time_nutri_tube",
                                                         "dysphagia_chronic")])]

tube_place_rad <- time_nutri_tube[complete.cases(peg[,c("radiation_dose",
                                                        "time_nutri_tube",
                                                        "dysphagia_chronic")])]								   

rad <- radiation_dose[complete.cases(peg[,c("radiation_dose",
                                            "time_nutri_tube",
                                            "dysphagia_chronic")])]

dysphagia_demo <- rep(NA,length(dysphagia_rad))
dysphagia_demo[which(dysphagia_rad <= 2)] <- 0
dysphagia_demo[which(dysphagia_rad >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_rad)
tube_place_demo <- rep(NA, length(tube_place_rad))
tube_place_demo[which(tube_place_rad==0)] <- 0
tube_place_demo[which(tube_place_rad >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

rad_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,rad)

dim(rad_dframe)

prop.table(table(rad_dframe$rad[which(rad_dframe$dysphagia_demo==0)]))
prop.table(table(rad_dframe$rad[which(rad_dframe$dysphagia_demo==1)]))
prop.table(table(rad_dframe$rad[which(rad_dframe$tube_place_demo==0)]))
prop.table(table(rad_dframe$rad[which(rad_dframe$tube_place_demo==1)]))
######################################################################
#Radiation Type

dysphagia_type <- dysphagia_chronic[complete.cases(peg[,c("radiation_modality",
                                                          "time_nutri_tube",
                                                          "dysphagia_chronic")])]

tube_place_type <- time_nutri_tube[complete.cases(peg[,c("radiation_modality",
                                                         "time_nutri_tube",
                                                         "dysphagia_chronic")])]								   

type <- radiation_modality[complete.cases(peg[,c("radiation_modality",
                                                 "time_nutri_tube",
                                                 "dysphagia_chronic")])]

dysphagia_demo <- rep(NA,length(dysphagia_type))
dysphagia_demo[which(dysphagia_type <= 2)] <- 0
dysphagia_demo[which(dysphagia_type >= 3)] <- 1
dysphagia_demo <- factor(dysphagia_demo)
table(dysphagia_demo)


table(tube_place_type)
tube_place_demo <- rep(NA, length(tube_place_type))
tube_place_demo[which(tube_place_type==0)] <- 0
tube_place_demo[which(tube_place_type >= 1)] <- 1
tube_place_demo <- factor(tube_place_demo)
table(tube_place_demo)

type_dframe <- cbind.data.frame(tube_place_demo,dysphagia_demo,type)

dim(type_dframe)

prop.table(table(type_dframe$type[which(type_dframe$dysphagia_demo==0)]))
prop.table(table(type_dframe$type[which(type_dframe$dysphagia_demo==1)]))
prop.table(table(type_dframe$type[which(type_dframe$tube_place_demo==0)]))
prop.table(table(type_dframe$type[which(type_dframe$tube_place_demo==1)]))
######################################################################