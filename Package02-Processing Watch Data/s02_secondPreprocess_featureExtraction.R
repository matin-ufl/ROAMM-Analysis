library(reshape2)
library(ggplot2)
library(gridExtra)

setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package02-Processing Watch Data/")
source("f02_secondPreprocess_featureExtraction.R")
# Parameters ------------------------------------
directory.taskTimes <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/task times/02 five/"
directory.raw <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/half clean 02/"
directory.output <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d02 cleaned data/"
should.plot <- TRUE
figureFolder <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Documents/042116 - Figures for tasks of 5 participants/"

# Script ----------------------------------------
roamm.df <- data.frame(matrix(nrow = 0, ncol = 9))
l <- dir(directory.taskTimes)
for(fileName in l) {
     taskTimes.df <- read.csv(paste(directory.taskTimes, fileName, sep = ""))
     PID <- as.character(taskTimes.df$PID[1])
     load(paste(directory.raw, PID, ".Rdata", sep = ""))
     
     # Appending the Vector Magnitude and Second-Passed-Day
     gearS.10hz.df$vm <- sqrt(gearS.10hz.df$accelX^2 + gearS.10hz.df$accelY^2 + gearS.10hz.df$accelZ^2)
     gearS.10hz.df$secondPassedDay <- (gearS.10hz.df$hour * 3600) + (gearS.10hz.df$minute * 60) + gearS.10hz.df$second
     
     # Fifteen second features are constructed
     fifteen.sec.df <- fifteenSecondFeatureConstruction(df = gearS.10hz.df, PID = PID, ppt.taskTimes = taskTimes.df, should.plot = should.plot, figureFolder = figureFolder)
     save(fifteen.sec.df, file = paste(directory.output, "fifteenSecond_", PID, ".Rdata", sep = ""))
     # Then it is aggregated for the final dataset
     ppt.roamm.df <- aggregateOneParticipant(fifteen.sec.df)
     roamm.df <- rbind(roamm.df, ppt.roamm.df)
     colnames(roamm.df) <- colnames(ppt.roamm.df)
}
rm(fileName, PID, should.plot, gearS.10hz.df, ppt.roamm.df, fifteen.sec.df, taskTimes.df, l)

save(roamm.df, file = paste(directory.output, "roamm_042116.Rdata", sep = ""))








