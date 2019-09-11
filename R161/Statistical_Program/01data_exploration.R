#######################################################################
# Study ID  								: 2016-Template
# Investigator Name					: Dr. Adam Sima
# Purpose										: create R template for BCL projects 
#
# BIOS Programmer						: Spiro Stilianoudakis
# Project Start Date				: 30Oct2018
#	
# Modification Programmer 	: 
# Modification Purpose			: 
#
# Modification Programmer 	: 
# Modification Purpose			: 
#
# Line Numbers:
# Data Management           : 
# Analysis                  : 
# Tables                    :
# Figures                   :
#######################################################################;


# Loading Libraries
library(openxlsx)
library(dplyr)
library(zoo)
library(stringr)

# Setting working directory
setwd("Y:/Shares/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2018 - R161 (Cho - Radiology)/1 - Data")

#reading in 2005-2018 data
contrasts_2005_2018 <- read.csv("all_contrasts_2005_2018.csv",
                       header = TRUE,
                      na.strings = "NA")

names(contrasts_2005_2018) <- c("id",
                      "age",
                      "sex",
                      "exam_date",
                      "id_and_date",
                      "procedure",
                      "order_id",
                      "area",
                      "count",
                      "contrast")

##recoding exam_date
contrasts_2005_2018$exam_date <- as.character(contrasts_2005_2018$exam_date)
contrasts_2005_2018$exam_date <- as.Date(contrasts_2005_2018$exam_date,"%m/%d/%Y")
class(contrasts_2005_2018$exam_date)


#reading in 2019 data
contrasts_2019 <- read.csv("all_contrasts_2019.csv",
                                header = TRUE,
                                na.strings = "NA")

names(contrasts_2019) <- c("age",
                                "sex",
                                "exam_date",
                                "id_and_date",
                                "procedure",
                                "order_id",
                                "area",
                                "count",
                                "contrast")

## cleaning "exam_date" by removing time
contrasts_2019$exam_date <- substr(as.character(contrasts_2019$exam_date), 1,10)
contrasts_2019$exam_date <- gsub(paste0(c("  ", " "), collapse = "|"), "",contrasts_2019$exam_date)

## creating an id variable similar to contrasts_2005_2018 using the "id_and_date" and "exam_date" variables
contrasts_2019$id <- str_remove(as.character(contrasts_2019$id_and_date), contrasts_2019$exam_date)

## changing exam_date to date variable
contrasts_2019$exam_date <- as.Date(contrasts_2019$exam_date,"%m/%d/%Y")
class(contrasts_2019$exam_date)

contrasts_2019 <- contrasts_2019[,c(10,1:9)]

# Merging the 2005-2018 data with the 2019 data
contrasts <- rbind.data.frame(contrasts_2005_2018, contrasts_2019)
dim(contrasts)
#79512    10


# Cleaning the data

##Dates

names(contrasts)[4] <- "date"

###dropping id_and_date variable
contrasts <- contrasts[,-5]

###checking if any dates are missing
length(which(is.na(contrasts$date))) #0

###use the id_and_date variable instead
###chop off the id portion
###determine the lengths of ids
#table(nchar(contrasts$id))
#ids with less than 7 characters mean they have leading 0s that excel removed

#contrasts$id_and_date[which(nchar(contrasts$id)==5)] <- substring(contrasts$id_and_date[which(nchar(contrasts$id)==5)],6)
#contrasts$id_and_date[which(nchar(contrasts$id)==6)] <- substring(contrasts$id_and_date[which(nchar(contrasts$id)==6)],7)
#contrasts$id_and_date[which(nchar(contrasts$id)==7)] <- substring(contrasts$id_and_date[which(nchar(contrasts$id)==7)],8)

#contrasts$id_and_date <- substring(contrasts$id_and_date,8)

#contrasts$id_and_date <- as.Date(contrasts$id_and_date,"%m/%d/%Y")

#names(contrasts)[4] <- "date"

#sort by id and date
contrasts <- contrasts[order(contrasts$date,contrasts$id),]

##Age
contrasts$age <- as.character(contrasts$age)
x <- grep(paste(LETTERS, collapse = "|"),contrasts$age)
contrasts$age[x] <- gsub(paste(LETTERS, collapse = "|"),"",contrasts$age[x])
contrasts$age <- as.numeric(contrasts$age)
###checking for outliers
plot(contrasts$age)
#2 outliers
which(contrasts$age>100)
contrasts <- contrasts[-which(contrasts$age>100),]
dim(contrasts)
#79510     9
hist(contrasts$age)

##Sex
table(contrasts$sex)
#F     M     U 
#40540 38969     1 
contrasts <- contrasts[-which(contrasts$sex=="U"),]
dim(contrasts)
#79509     9
contrasts$sex <- factor(contrasts$sex)
table(contrasts$sex)

##Contrast
table(contrasts$contrast)
#Ablavar   ABlavar   Dotarem    Eovist  Gadavist Magnevist  Prohance   Unknown 
#1       131       133       678     24924     21892       166     31584 
#combining unknown, ablavar, dotarem, eovist, and prohance into "Other"
contrasts$contrast <- as.character(contrasts$contrast)
contrasts$contrast[-which(contrasts$contrast %in% 
                            c("Gadavist","Magnevist"))] <- "Other"
contrasts$contrast <- factor(contrasts$contrast)
table(contrasts$contrast)
#Gadavist Magnevist     Other 
#24924     21892     32693

##Adding column to denote before or after 2015 cutoff
contrasts$switch <- ifelse(contrasts$date < as.Date("2015-03-22"), 
                           "Before",
                           "After")
table(contrasts$switch)
#After Before 
#26578  52931 

##reducing same day contrasts down to 1 injection by Removing duplicate ids across dates
dim(contrasts) #72788    10; 79509    10
contrasts <- contrasts[!duplicated(contrasts[,c(1:4)]),]
dim(contrasts) #63287    10; 69110    10

# Reading in reaction data 2005-2015
reaction_pre <- read.csv("reactions2005to2015.csv",
                         header = TRUE)
vars <- c("Reaction",
          "MRN",
          "Type.of.Contrast",
          "Severity.of.Rxn",
          "Study.Date")

reaction_pre <- reaction_pre[,which(names(reaction_pre) %in% vars)]

##Cleaning data
reaction_pre <- reaction_pre[1:17,]
reaction_pre$Study.Date <- as.Date(reaction_pre$Study.Date, "%m/%d/%Y")
reaction_pre$switch <- "Before"
 
# Reading in reaction data 2015-2018
reaction_post <- read.csv("reactions2015to2018.csv",
                         header = TRUE)

reaction_post <- reaction_post[,which(names(reaction_post) %in% vars)]

##Cleaning data
reaction_post <- reaction_post[1:14,]
reaction_post$Study.Date <- as.Date(reaction_post$Study.Date, "%m/%d/%Y")
reaction_post$switch <- "After"

##Binding reaction data
reactions <- rbind.data.frame(reaction_pre,reaction_post)
names(reactions)[2] <- "id"
names(reactions)[3] <- "date"
reactions$id <- as.numeric(reactions$id)
reactions$Severity.of.Rxn[grep("mild",reactions$Severity.of.Rxn)] <- "mild"
reactions$Reaction <- as.character(reactions$Reaction)
reactions$Type.of.Contrast <- as.character(reactions$Type.of.Contrast)
reactions$Severity.of.Rxn <- as.character(reactions$Severity.of.Rxn)

##combining contrast agents for reaction data
table(reactions$Type.of.Contrast)
reactions$Type.of.Contrast[-which(reactions$Type.of.Contrast 
                                  %in% c("Gadavist","Magnevist"))] <- "Other"
table(reactions$Type.of.Contrast)

dim(reactions) #31  6

#Joining reaction data to contrast data by id and date
length(intersect(contrasts$id, reactions$id)) #30
setdiff(reactions$id,contrasts$id) #4098599 (physiologic reaction; ignore)
contrast2 <- left_join(contrasts, reactions, by=c("id","date"))
dim(contrast2) #63287    14

#removing duplicate ids that had reactions
#cwr <- contrast3[!is.na(contrast3$Reaction),]
#cwnr <- contrast3[is.na(contrast3$Reaction),]

#cwr <- cwr[!duplicated(cwr$id),]
#dim(cwr) #27

#contrast4 <- rbind.data.frame(cwr, cwnr)

#contrast4$contrast <- as.character(contrast4$contrast)

#assigning unknown contrasts to contrasts defined by reaction data
table(contrast2$contrast)
contrast2$contrast[-which(is.na(contrast2$Type.of.Contrast))] <- contrast2$Type.of.Contrast[-which(is.na(contrast2$Type.of.Contrast))]
table(contrast2$contrast)

table(contrast2$Reaction, exclude="ifany")
table(contrast2$Severity.of.Rxn, exclude="ifany")
contrast2$Reaction[is.na(contrast2$Reaction)] <- "none"
contrast2$Severity.of.Rxn[is.na(contrast2$Severity.of.Rxn)] <- "none"
table(contrast2$Reaction, exclude="ifany")
table(contrast2$Severity.of.Rxn, exclude="ifany")

contrast2 <- contrast2[,-c(12,14)]

#sorting by date
contrast2 <- contrast2[order(contrast2$date, contrast2$id),]

dim(contrast2) 
#63284    12
#72781    12
#63287    12

#creating a variable denoting the quarter-time and half-time
##move backwards/forwards from date of switch
##breaking up data before and after switch
contrast2$quarter <- as.yearqtr(contrast2$date, 
                                format = "%Y-%m-%d", 
                                frequency = 2)
#changing injections that occured in 2015 Q1 bu after 03/21/15 as 2015 Q2
table(contrast2$quarter)
contrast2$quarter[which(contrast2$quarter=="2015 Q1" & contrast2$date > as.Date("2015-03-21"))] <- as.yearqtr("2015 Q2")
table(contrast2$quarter)

#contrast2$half <- cut(contrast2$date, 
#                      breaks='6 month')

#creating a binary reaction variable
contrast2$react_binary <- ifelse(contrast2$Reaction=="none", "No", "Yes")

#saving dataset
saveRDS(contrast2, "Z:/Shares/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2018 - R161 (Cho - Radiology)/1 - Data/contrast_data.rds")

