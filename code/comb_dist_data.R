#Read in and subset the data required for assessing the distribution


#read in the data
dat <- read.csv("LOS analysis dataset - Sheet1.csv")

# want to extract median, iqr, mean, sd, which of those it is, hospital or icu, sample size
sub_dat <- dat[which(dat$plot_cat== "main"),c("los_group", "plot_cat", "N", "LOS_med", "LOS_q25", "LOS_q75", 
                  "LOS_mean", "LOS_sd", "LOS_ICU_med", "LOS_ICU_q25", "LOS_ICU_q75", "LOS_ICU_mean",
                  "LOS_ICU_sd")]

# Will need to check these - just excluding confusing ones atm
sub_dat[which(sub_dat$los_group != "general/ICU" & 
                sub_dat$los_group != "" 
                ), ]

sub_dat[,3:11] <- apply(sub_dat[,3:11], 2, function(x) as.numeric(x))

los_icu <- sub_dat[which(sub_dat$los_group == "ICU"),c("N", "LOS_ICU_med", "LOS_ICU_q25", "LOS_ICU_q75",
                                                       "LOS_ICU_mean", "LOS_ICU_sd")]
los_general <- sub_dat[which(sub_dat$los_group == "general"),c("N", "LOS_med", "LOS_q25", "LOS_q75","LOS_mean", "LOS_sd")]

# remove ones that do no have at least two bits of information
los_general$info_count <- apply(los_general, 1, function(x) sum(!is.na(x)))

los_general <- los_general[which(los_general$info_count>2),]
los_general <- los_general[which(!is.na(los_general$LOS_med) | !is.na(los_general$LOS_mean)),]

