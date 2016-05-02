setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package02-Processing Watch Data/")

# Select the feature set (just variables) of ROAMM
load("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d03 data for analysis/ROAMM_042516.Rdata")

# Reading MET Scores
met.df <- read.csv("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/ChoresXL_metScores.csv")

# Unifying the task names
load(file = "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/taskNames_mapping.Rdata")
met.df$task <- NA
for(taskName in levels(met.df$Activity)) {
     met.df$task[met.df$Activity == taskName] <- as.character(taskName.mapping$new[taskName.mapping$old == taskName])
}

# Setting target variables
roamm.df$MET <- NA
roamm.df$Sedentary <- "NO"

for(i in 1:nrow(roamm.df)) {
     PID <- roamm.df$PID[i]
     ppt.tasks <- met.df[met.df$Name == PID, ]
     idx <- which(ppt.tasks$task == roamm.df$task[i])
     if(length(idx) > 0) {
          roamm.df$MET[i] <- (ppt.tasks$avg_vo2_mlminkg[idx] / 3.5)
     }
}
rm(i, PID, ppt.tasks, idx)

# Sedentary task
roamm.df$Sedentary[roamm.df$task == "Computer Work"] <- "YES"

# Appending demographic features
roamm.df$dem.Gender <- "F"
roamm.df$dem.Age <- NA
roamm.df$dem.BMI <- NA
roamm.df$dem.Height <- NA
roamm.df$dem.Weight <- NA
demographic.df <- read.csv(file = "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/d01_demographics.csv")
for(i in 1:nrow(demographic.df)) {
     PID <- as.character(demographic.df$PID[i])
     roamm.df$dem.Gender[roamm.df$PID == PID] <- as.character(demographic.df$GENDER[i])
     roamm.df$dem.Age[roamm.df$PID == PID] <- demographic.df$AGE[i]
     roamm.df$dem.BMI[roamm.df$PID == PID] <- demographic.df$BMI[i]
     roamm.df$dem.Height[roamm.df$PID == PID] <- demographic.df$HEIHGT[i]
     roamm.df$dem.Weight[roamm.df$PID == PID] <- demographic.df$WEIGHT[i]
}
rm(i, PID)

save(roamm.df, file = "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d03 data for analysis/ROAMM_042616.Rdata")





