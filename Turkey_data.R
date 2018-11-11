#setwd("~/Desktop/GEOG Research/Turkey/compression")
#directory <- "~/Desktop/GEOG Research/Turkey/compression"
#ndirectory <- "~/Desktop/GEOG Research/Turkey/Dataset 2 csv files"

setwd("~/Desktop/GEOG Research/Turkey/compression")
library(plyr)
library(rlang)
library(dplyr)
library(stringr)


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

#Group refugee CALLER_ID's together
REFUGEE_BYSITE <- out.file[, c("SITE_ID", "CALLER_ID")]
REFUGEE_BYSITE <- subset(REFUGEE_BYSITE, CALLER_ID == grep("^1.{9}", out.file$CALLER_ID, value=TRUE))

COUNT_REFUGEES_BYSITE <- REFUGEE_BYSITE %>%
  group_by(SITE_ID) %>%
  summarise(n_distinct(CALLER_ID))
colnames(COUNT_REFUGEES_BYSITE) <- c("SITE_ID", "REFUGEES")
COUNT_REFUGEES_BYSITE <- COUNT_REFUGEES_BYSITE[-c(1), ]

#Merge dataframes 
length(COUNT_REFUGEES_BYSITE) = length(SITE_COUNT)
DISTINCT_CALLERS <- merge(SITE_COUNT, COUNT_REFUGEES_BYSITE, by="SITE_ID")

#Calculate Diversity Stat
DIVERSITY_STAT <- DISTINCT_CALLERS$REFUGEES/DISTINCT_CALLERS$UNIQUE_CALLERS
DISTINCT_CALLERS <- cbind(DISTINCT_CALLERS, DIVERSITY_STAT)
summary_DIVERSITYSTAT <- summary(DIVERSITY_STAT)

#Count frequency of calls per date
CALLS_BYDATE <- out.file %>%
  group_by(weekdays) %>%
  summarise(n = n())
colnames(CALLS_BYDATE) <- c("WEEKDAY", "#CALLS")

#Clean data to include only refugee calls
REFUGEECALLS_BYDATE <- out.file[, c("CALLER_ID","CALLEE_PREFIX", "weekdays")]
REFUGEECALLS_BYDATE <- subset(REFUGEECALLS_BYDATE, CALLER_ID == grep("^1.{9}", out.file$CALLER_ID, value=TRUE))
REFCALLS_BYDATE <- subset(REFUGEECALLS_BYDATE, CALLEE_PREFIX == "2")

#Total Refugee Calls by Weekdays
NewREFUGEECALLS_BYDATE <- REFUGEECALLS_BYDATE %>%
  group_by(weekdays) %>%
  summarise(n=n())
colnames(NewREFUGEECALLS_BYDATE) <- c("Weekday", "Total_Refugee_Calls")

#Total Refugee Calls to Natives by Weekdays
REFCALLS_BYDATE <- REFCALLS_BYDATE %>%
  group_by(weekdays) %>%
  summarise(n=n())
colnames(REFCALLS_BYDATE) <- c("Weekday", "Total_REFtoNATIVES")

#Merge by weekday
Percent_REFStoNATIVES <-merge(NewREFUGEECALLS_BYDATE, REFCALLS_BYDATE, by="Weekday")
Percent_Calls_REFStoNATIVES <- Percent_REFStoNATIVES$Total_REFtoNATIVES/Percent_REFStoNATIVES$Total_Refugee_Calls
REFtoNATIVE_BYDATE <- cbind(Percent_REFStoNATIVES, Percent_Calls_REFStoNATIVES)

#Clean data to include only native calls
NATIVECALLS_BYDATE <- out.file[, c("CALLER_ID","CALLEE_PREFIX", "weekdays")]
NATIVECALLS_BYDATE <- subset(NATIVECALLS_BYDATE, CALLER_ID == grep("^2.{9}", out.file$CALLER_ID, value=TRUE))
NATCALLS_BYDATE <- subset(NATIVECALLS_BYDATE, CALLEE_PREFIX == "1")

#Total Native Calls by Weekdays
NewNATIVECALLS_BYDATE <- NATIVECALLS_BYDATE %>%
  group_by(weekdays) %>%
  summarise(n=n())
colnames(NewNATIVECALLS_BYDATE) <- c("Weekday", "Total_Native_Calls")

#Total Native Calls to Refugees by Weekdays
NATCALLS_BYDATE <- NATCALLS_BYDATE %>%
  group_by(weekdays) %>%
  summarise(n=n())
colnames(NATCALLS_BYDATE) <- c("Weekday", "Total_NATtoREF")

#Merge by weekday
Percent_NATIVEStoREFS <-merge(NewNATIVECALLS_BYDATE, NATCALLS_BYDATE, by="Weekday")
Percent_Calls_NATIVEStoREFS <- Percent_NATIVEStoREFS$Total_NATtoREF/Percent_NATIVEStoREFS$Total_Native_Calls
REFtoNATIVE_BYDATE <- cbind(Percent_NATIVEStoREFS, Percent_Calls_NATIVEStoREFS)

#Count unique sites/frequency
SITE_COUNT <- out.file %>%
  group_by(SITE_ID) %>%
  summarise(n_distinct(CALLER_ID))
colnames(SITE_COUNT) <- c("SITE_ID", "UNIQUE_CALLERS")
SITE_COUNT <- SITE_COUNT[-c(1), ]


REFUGEES_BYSITE <- REFUGEES %>%
  group_by(SITE_ID) %>%
  summarise(n_distinct(REFUGEES))
colnames(REFUGEES_BYSITE) <- c("SITE_ID", "#REFUGEES")

#Remove Outlier
SITE_COUNT <- SITE_COUNT[-c(1), ]

#Summary Stats
summary_SITECOUNT <- summary(SITE_COUNT)

#Stats for each refugee caller to native
UNIQUE_CALLER <- out.file[, c("CALLER_ID","CALLEE_PREFIX")]
UNIQUE_CALLER <- subset(UNIQUE_CALLER, CALLER_ID == grep("^1.{9}", out.file$CALLER_ID, value=TRUE))
UNIQUEtoNATIVE <- subset(UNIQUE_CALLER, CALLEE_PREFIX == "2")

#Total Refugee Calls to Natives
REFtoNATIVE <- UNIQUE_CALLER %>%
  group_by(CALLER_ID) %>%
  summarise(n=n())
colnames(REFtoNATIVE) <- c("CALLER_ID", "NATIVE_CALLEE")

#Stats for each refugee caller to unknown
U_CALLER <- out.file[, c("CALLER_ID","CALLEE_PREFIX")]
U_CALLER <- subset(U_CALLER, CALLER_ID == grep("^1.{9}", out.file$CALLER_ID, value=TRUE))
U_CALLER <- subset(U_CALLER, CALLEE_PREFIX == "3")

#Total Refugee Calls to Unknown
REFtoU <- U_CALLER %>%
  group_by(CALLER_ID) %>%
  summarise(n=n())
colnames(REFtoU) <- c("CALLER_ID", "UNKNOWN_CALLEE")

#Merge columns (4)
REF_CALLS <- merge(REFtoNATIVE, REFtoU, by="CALLER_ID")
TOTAL_CALLS <- REF_CALLS$NATIVE_CALLEE + REF_CALLS$UNKNOWN_CALLEE
REF_CALLS <- cbind(REF_CALLS, TOTAL_CALLS)

#Percent calls each refugee places to natives
Percent.REFtoNAT <- REF_CALLS$
Percent_Calls_NATIVEStoREFS <- Percent_NATIVEStoREFS$Total_NATtoREF/Percent_NATIVEStoREFS$Total_Native_Calls
REFtoNATIVE_BYDATE <- cbind(Percent_NATIVEStoREFS, Percent_Calls_NATIVEStoREFS)merge
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