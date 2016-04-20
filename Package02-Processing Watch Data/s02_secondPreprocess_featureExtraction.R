library(reshape2)
library(ggplot2)
library(gridExtra)
setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package02-Processing Watch Data/")
source("f02_secondPreprocess_featureExtraction.R")
# Parameters ------------------------------------
directory.taskTimes <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/"
directory.raw <- "~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d01 half cleaned data/"
     
# Script ----------------------------------------
result <- data.frame(matrix(nrow = 0, ncol = 9))
l <- dir(directory.taskTimes)
for(fileName in l) {
     taskTimes.df <- read.csv(paste(directory.taskTimes, fileName, sep = ""))
     PID <- as.character(taskTimes.df$PID[1])
     load(paste(directory.raw, PID, ".Rdata", sep = ""))
     
     # Appending the Vector Magnitude and Second-Passed-Day
     gearS.10hz.df$vm <- sqrt(gearS.10hz.df$accelX^2 + gearS.10hz.df$accelY^2 + gearS.10hz.df$accelZ^2)
     gearS.10hz.df$secondPassedDay <- (gearS.10hz.df$hour * 3600) + (gearS.10hz.df$minute * 60) + gearS.10hz.df$second
     
     # Extracting features
     for(task.idx in 1:nrow(taskTimes.df)) {
          result.row <- data.frame(PID = PID, task = taskTimes.df$Task[task.idx],
                                   mvm = NA,
                                   sdvm = NA,
                                   mangle = NA,
                                   sdangle = NA,
                                   p625 = NA,
                                   df = NA,
                                   fpdf = NA)
          start.idx <- find.index.basedOnTime(df = gearS.10hz.df, timeStr = taskTimes.df$StartTime[task.idx], mode = "start")
          end.idx <- find.index.basedOnTime(df = gearS.10hz.df, timeStr = taskTimes.df$EndTime[task.idx], mode = "end")
          if(!is.na(start.idx) && !is.na(end.idx)) {
               task.chunk <- gearS.10hz.df[start.idx:end.idx, c(1:4, 8)]
               if(nrow(task.chunk) > 60) { 
                    # Constructing the features
                    result.row$mvm <- mvm(task.chunk$vm)
                    result.row$sdvm <- sdvm(task.chunk$vm)
                    result.row$mangle <- mangle(x = task.chunk$accelX, task.chunk$vm)
                    result.row$sdangle <- sdangle(x = task.chunk$accelX, task.chunk$vm)
                    result.row$p625 <- p625(task.chunk$vm, sample.rate = 10)
                    result.row$df <- df(task.chunk$vm, sample.rate = 10)
                    result.row$fpdf <- fpdf(task.chunk$vm, sample.rate = 10)
                    # Plot the data
                    plot.df <- melt(task.chunk)
                    g.time <- ggplot(data = plot.df) + geom_line(aes(x = timestamp, y = value, group = variable, colour = variable))
                    g.time <- g.time + scale_colour_manual(values = c("green", "blue", "red", "black")) + theme_bw() +
                         scale_x_discrete(breaks = task.chunk$timestamp[seq(1, nrow(task.chunk), by = 200)]) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                         labs(title = paste(result.row$PID, "-", result.row$task), y = "Acceleration (m/s2)")
                    freq.df <- giveFrequencyData(task.chunk$vm, sampling.rate = 10)
                    g.freq <- ggplot(data = freq.df[2:nrow(freq.df),]) + geom_line(aes(x = freq, y = strength, group = "frequency", colour = "frequency")) +
                         theme_bw() + labs(title = "Frequency") + scale_colour_manual(values = c("black"))
                    grid.arrange(g.time, g.freq, ncol = 1)
               } else {
                    print(paste("Too few samples (", nrow(task.chunk),") for [", result.row$PID, " - ", result.row$task, "]", sep = ""))
               }
          }
          result <- rbind(result, result.row)
          colnames(result) <- colnames(result.row)
     }
}


