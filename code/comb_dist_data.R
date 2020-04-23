#Read in and subset the data required for assessing the distribution

#read in the data
setwd("~/Documents/GitHub/los_review/data/")
dat <- read.csv("LOS analysis dataset - Sheet1.csv")

# Extract relevant columns
sub_dat <- dat[which(dat$plot_cat== "main"),c("Included","Author" ,"los_group", "End.point", "Country", "plot_cat", "N", "LOS_med", "LOS_q25", "LOS_q75", 
                  "LOS_mean", "LOS_sd", "LOS_ICU_med", "LOS_ICU_q25", "LOS_ICU_q75", "LOS_ICU_mean",
                  "LOS_ICU_sd")]

# Exclude thise  with no_group, and those with a 0 in included column
sub_dat <- sub_dat[which(sub_dat$los_group != ""),]
sub_dat <- sub_dat[which(sub_dat$Included==1), ]

# split into general vs icu
los_icu <- sub_dat[which(sub_dat$los_group == "ICU" |
                           sub_dat$los_group == "general/ICU"),]
los_general <- sub_dat[which(sub_dat$los_group == "general" |
                               sub_dat$los_group == "general/ICU"),]
#split into china vs world
los_general_china <- los_general[which(los_general$Country=="China"),]
los_general_world <- los_general[which(los_general$Country!="China"),]
los_icu_china <- los_icu[which(los_icu$Country=="China"),]
los_icu_world <- los_icu[which(los_icu$Country!="China"),]

#Define important parameters for general and ICU
icu_parameters <- c("N", "LOS_ICU_med", "LOS_ICU_q25", "LOS_ICU_q75", "LOS_ICU_mean", "LOS_ICU_sd")
general_parameters <- c("N", "LOS_med", "LOS_q25", "LOS_q75", "LOS_mean", "LOS_sd")

# remove ones that do no have at least two bits of information
los_general$info_count <- apply(los_general[, general_parameters], 1, function(x) sum(!is.na(x)))
los_general_china$info_count <- apply(los_general_china[, general_parameters], 1, function(x) sum(!is.na(x)))
los_general_world$info_count <- apply(los_general_world[, general_parameters], 1, function(x) sum(!is.na(x)))
los_icu$info_count <- apply(los_icu[, icu_parameters], 1, function(x) sum(!is.na(x)))
los_icu_china$info_count <- apply(los_icu_china[, icu_parameters], 1, function(x) sum(!is.na(x)))
los_icu_world$info_count <- apply(los_icu_world[, icu_parameters], 1, function(x) sum(!is.na(x)))

los_general <- los_general[which(los_general$info_count>2),]
los_general_china <- los_general_china[which(los_general_china$info_count>2),]
los_general_world <- los_general_world[which(los_general_world$info_count>2),]
los_icu <- los_icu[which(los_icu$info_count>2),]
los_icu_china <- los_icu_china[which(los_icu_china$info_count>2),]
los_icu_world <- los_icu_world[which(los_icu_world$info_count>2),]

# remove those with no sample size
los_general <- los_general[which(!is.na(los_general$N)),]
los_general_china <- los_general_china[which(!is.na(los_general_china$N)),]
los_general_world <- los_general_world[which(!is.na(los_general_world$N)),]
los_icu <- los_icu[which(!is.na(los_icu$N)),]
los_icu_china <- los_icu_china[which(!is.na(los_icu_china$N)),]
los_icu_world <- los_icu_world[which(!is.na(los_icu_world$N)),]

#save Includedd studies
write.csv(los_general, "Included_general.csv")
write.csv(los_icu, "Included_general.csv")

#subset number parameters
los_general <- los_general[,general_parameters]
los_general_china <- los_general_china[,general_parameters]
los_general_world <- los_general_world[,general_parameters]
los_icu <- los_icu[,icu_parameters]
los_icu_china <- los_icu_china[,icu_parameters]
los_icu_world <- los_icu_world[,icu_parameters]

