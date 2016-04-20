library(jsonlite)
setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package02-Processing Watch Data/")
# Functions --------------------------------------
source("f01_firstPreprocessOnWatch_functions.R")

# Script -----------------------------------------
PID <- "ANAD152"
ppt.directory <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/raw data/gear s/415/"
l <- dir(ppt.directory)
new.df <- data.frame(matrix(nrow = 0, ncol = 7))

for(fileName in l) {
     print(fileName)
     df <- fromJSON(paste(ppt.directory, fileName, sep = ""))
     df <- df[, c(3:5, 11)]
     df$accelX <- as.numeric(df$accelX)
     df$accelY <- as.numeric(df$accelY)
     df$accelZ <- as.numeric(df$accelZ)
     
     df$hour <- sapply(df$timestamp, separate.timeParts, whichPart = "hour")
     df$minute <- sapply(df$timestamp, separate.timeParts, whichPart = "minute")
     df$second <- sapply(df$timestamp, separate.timeParts, whichPart = "second")
     
     
     i <- 1
     while(i < nrow(df)) {
          curr.sec <- df$second[i]
          endMin.idx <- min((i+59), nrow(df))
          check.df <- df[i:endMin.idx, ]
          curr.sec.df <- check.df[check.df$second == curr.sec, ]
          new.df <- rbind(new.df, uniformSecondData(curr.sec.df))
          colnames(new.df) <- colnames(df)
          i <- i + max(1, nrow(curr.sec.df))
     }
     rm(i, check.df, curr.sec.df, curr.sec, endMin.idx, df)
}

# Data needs to be sorted based on hour and minute, because it is out of order for some reason!
gearS.10hz.df <- NULL
for(hour in min(new.df$hour):max(new.df$hour)) {
     hour.df <- new.df[new.df$hour == hour, ]
     gearS.10hz.df <- rbind(gearS.10hz.df,
                            hour.df[with(data = hour.df, order(minute, decreasing = F)), ])
}
rm(hour, hour.df)
save(gearS.10hz.df, file = paste("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/", PID, ".Rdata", sep = ""))

rm(list = ls())


