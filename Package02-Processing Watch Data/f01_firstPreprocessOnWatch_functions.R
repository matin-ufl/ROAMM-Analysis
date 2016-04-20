# This functions does two things:
# 1. removes NaNs
# 2. Reduces the number of samples to 10 (makes the data 10Hz)
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

# Return a specific part (hour, minute, or second) part of the time
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