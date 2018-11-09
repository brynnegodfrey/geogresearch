#setwd("~/Desktop/GEOG Research/Turkey/compression")
#directory <- "~/Desktop/GEOG Research/Turkey/compression"
#ndirectory <- "~/Desktop/GEOG Research/Turkey/Dataset 2 csv files"

setwd("~/Desktop/GEOG Research/Turkey/compression")
library(plyr)
library(rlang)
library(dplyr)
library(stringr)
library(tidyverse)


# Move all files to one list
file_list <- list.files(pattern="Dataset 2.*txt")

# Read files
out.file <- NULL
count_PHONECALLRECORDS <- 0
count_CALLERID <- 0
for (i in 1:length(file_list)){
  file <- read.table(file_list[i], header=TRUE, sep=",")
  out.file <- rbind(out.file, file)
}

# Count total number phone call records
count_PHONECALLRECORDS <- length(out.file$CALLER_ID)
  
# Count number unique caller id's
count_CALLERID <- length(unique(out.file$CALLER_ID))
  
# Construct contingency matrix
tb_1 <- with(out.file, table(CALLEE_PREFIX, CALLER = substr(CALLER_ID, 0, 1)))
colnames(tb_1) <- c("Refugee Caller", "Non-Refugee Caller")
rownames(tb_1) <- c("Refugee Callee", "Non-Refugee Callee", "Unknown Callee")
tb_1 

#Save combined csv
write.csv(out.file, "Dataset2_combined.csv")

#Convert to date format
dates <- as.Date(out.file$TIMESTAMP, format = "%d-%m-%Y")

#Convert to weekdays
weekdays <- weekdays(dates)

#Add column to dataframe 
out.file$weekdays <- weekdays
STR_CALLERID <- toString(out.file$CALLER_ID)
out.file$STR_CALLERID <- STR_CALLERID

CALLER_CHAR <-as.character(out.file$CALLER_ID)
STR_CALLERID %>%
  group_by(grp = paste0('grp', substr(STR_CALLERID, 1, 1))) %>%
  mutate(i = row_number()) %>%
  spread(grp, STR_CALLERID) %>%
  select(-i)

STR_CALLERID  %>% 
  group_by(grp = paste0('grp', substr(STR_CALLERID, 1, 1))) %>% 
  mutate(i = row_number()) %>% 
  spread(grp, STR_ID) %>%
  select(-i)
REFUGEE_CALLERS <- head(select(STR_CALLERID, starts_with("1")))
#Count frequency of calls per date
CALLS_BYDATE <- out.file %>%
  group_by(weekdays) %>%
  summarise(n = n())
colnames(CALLS_BYDATE) <- c("WEEKDAY", "#CALLS")

#Count unique sites/frequency
SITE_COUNT <- out.file %>%
  group_by(SITE_ID) %>%
  summarise(n_distinct(CALLER_ID))
colnames(SITE_COUNT) <- c("SITE_ID", "UNIQUE_CALLERS")

#Remove Outlier
SITE_COUNT <- SITE_COUNT[-c(1), ]

#Summary Stats
summary_SITECOUNT <- summary(SITE_COUNT)
toString(out.file$CALLER_ID)
d <- out.file$CALLER_ID
d %>% select(starts_with('1'))

#Convert txt files to csv
#files.to.read <- paste(directory, file_list, sep="/")
#files.to.write <- paste(ndirectory, paste0(sub(".txt","", file_list), ".csv"), sep="")

#Combine csv files
#for (i in 1:length(files.to.read)) {
 # temp <- (read.csv(files.to.read[i], header = TRUE, fill = TRUE))
  #write.csv(temp, file = files.to.write[i])
#}

#Save combined csv
#write.csv(temp, "Dataset2_combined.csv")

#for (i in 1:length(files.to.read)) {
 # file <- read.table(files.to.read[i], header=TRUE, sep=",")
  #out.file <-rbind(file)
#}
#Read files
#for (i in 1:length(file_list)){
 # file <- read.table(file_list[i], header=TRUE, sep=",")
  #out.file <- rbind(file)
#}

#Count total number phone call records
#count_PHONECALLRECORDS <- length(out.file$CALLER_ID)

#Count number unique caller id's
#count_CALLERID <- length(unique(out.file$CALLER_ID))
#count_CALLERID <- count(out.file, "CALLER_ID")

#Calculate Refugees/Natives at each site
#tb_2 <- with(out.file, table(SITE_ID, CALLER = substr(CALLERID, 0, 1)))
#colnames(tb_2) <- c("Refugee Caller", "Non-Refugee Caller")
#rownames(tb_2) <- c("SITE_ID")
#tb_2