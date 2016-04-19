library(jsonlite)
setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package02-Processing Watch Data/")
# Functions --------------------------------------
uniformSecondData <- function(curr.sec.df) {
     noNaN.df <- curr.sec.df[complete.cases(curr.sec.df), ]
     if(nrow(noNaN.df) > 10) {
          noNaN.df <- noNaN.df[1:10, ]
     } else {
          while(nrow(noNaN.df) < 10 && nrow(noNaN.df) > 0) {
               accelX <- mean(noNaN.df$accelX)
               accelY <- mean(noNaN.df$accelY)
               accelZ <- mean(noNaN.df$accelZ)
               noNaN.df <- rbind(noNaN.df,
                                 data.frame(accelX, accelY, accelZ,
                                            timestamp = noNaN.df$timestamp[nrow(noNaN.df)],
                                            hour = noNaN.df$hour[nrow(noNaN.df)],
                                            minute = noNaN.df$minute[nrow(noNaN.df)],
                                            second = noNaN.df$second[nrow(noNaN.df)]))
          }
          colnames(noNaN.df) <- colnames(curr.sec.df)
     }
     noNaN.df
}


separate.timeParts <- function(timeStamp, whichPart = "hour") {
     time <- unlist(strsplit(timeStamp, split = "T"))[2]
     time <- unlist(strsplit(time, split = "-"))[1]
     tokens <- unlist(strsplit(time, split = ":"))
     result <- as.numeric(tokens[1])
     if(whichPart == "minute") {
          result <- as.numeric(tokens[2])
     } else if(whichPart == "second") {
          result <- as.numeric(tokens[3])
     }
     result
}


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


