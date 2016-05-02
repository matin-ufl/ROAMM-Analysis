# Functions --------------------------------------
find.index.basedOnTime <- function(df = gearS.10hz.df, timeStr, mode = "start") {
     # This threshold sets how many minutes we are allowing to be missing.
     THRESHOLD.MINUTE <- 2
     secondPassedDay <- calculateSecondPassedDay(timeStr)
     result.idx <- NA
     # For start, the data should appear at or some time after the written time
     if(mode == "start") {
          idx <- which(df$secondPassedDay == secondPassedDay)
          if(length(idx) > 0) {
               result.idx <- min(idx)
          } else { # Not found
               idx <- which(df$secondPassedDay > secondPassedDay)
               if(length(idx) > 0) {
                    result.idx <- min(idx)
               }
          }
     } else { # For end, the data should not be considered any time after the written time
          idx <- which(df$secondPassedDay == secondPassedDay)
          if(length(idx) > 0) {
               result.idx <- max(idx)
          } else { # Not found
               idx <- which(df$secondPassedDay < secondPassedDay)
               if(length(idx) > 0) {
                    result.idx <- max(idx)
               }
          }
     }
     # If the found time is very far from the ideal point (we have a lot of not recorded data), we should not consider it
     if(!is.na(result.idx)) {
          if(abs(secondPassedDay - df$secondPassedDay[idx]) > (THRESHOLD.MINUTE * 600)) {
               print(paste("The found time (", paste(df$hour, df$minute, df$second, sep = ":"), ") is more than ", THRESHOLD.MINUTE, " away from the ideal point (", timeStr, ")", sep = ""))
               result.idx <- NA
          }
     }
     result.idx
}

calculateSecondPassedDay <- function(timeStr) {
     hour <- as.numeric(substr(timeStr, start = 1, stop = 2))
     if(is.na(hour)) {
          return(NA)
     }
     if(hour < 7) {
          hour <- hour + 12
     }
     minute <- as.numeric(substr(timeStr, start = 4, stop = 5))
     second <- as.numeric(substr(timeStr, start = 7, stop = 8))
     result <- (hour * 3600) + (minute * 60) + second
     result
}


convert.fft <- function(cs, sample.rate = 10) {
     cs <- cs / length(cs) # normalize
     
     distance.center <- function(c) signif(Mod(c), 4)
     angle           <- function(c) signif(180 * Arg(c) / pi, 3)
     
     df <- data.frame(cycle    = 0:(length(cs)-1),
                      freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                      strength = sapply(cs, distance.center),
                      delay    = sapply(cs, angle))
     df
}

p625 <- function(VM, sample.rate = 10) {
     VM_freq <- convert.fft(fft(VM), sample.rate = sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     idx0_6 <- min(which(signif(VM_freq$freq, digits = 1) >= 0.6))
     idx2_5 <- max(which(signif(VM_freq$freq, digits = 1) <= 2.5))
     result <- NA
     if(idx0_6 > 1 && idx2_5 < length(VM)) {
          result <- sum(VM_freq$strength[idx0_6:idx2_5]) / sum(VM_freq$strength[2:(nrow(VM_freq)-1)])
     }
     result
}

df <- function(VM, sample.rate = 10) {
     VM_freq <- convert.fft(fft(VM), sample.rate = sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     idx_max <- min(which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)])))
     result <- VM_freq$freq[idx_max]
     result
}

fpdf <- function(VM, sample.rate = 10) {
     VM_freq <- convert.fft(fft(VM), sample.rate = sample.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     idx_max <- which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)]))
     indices <- which(round(VM_freq$freq, digits = 1) == round(VM_freq$freq[idx_max], digits = 1))
     result <- sum(VM_freq$strength[indices]) / sum(VM_freq$strength[2:(nrow(VM_freq))])
     result
}

mangle <- function(x, VM, sample.rate = 10) {
     angle <- (90 * asin(x / VM)) / (pi/2)
     result <- mean(angle, na.rm = T)
     result
}

sdangle <- function(x, VM, sample.rate = 10) {
     angle <- (90 * asin(x / VM)) / (pi/2)
     result <- sd(angle, na.rm = T)
     result
}

mvm <- function(VM) {
     result <- mean(VM, na.rm = T)
     result
}

sdvm <- function(VM) {
     result <- sd(VM, na.rm = T)
     result
}

giveFrequencyData <- function(VM, sampling.rate = 10) {
     VM_freq <- convert.fft(fft(VM), sample.rate = sampling.rate)
     VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
     VM_freq
}

findValidParts <- function(task.chunk, row.threshold = 100, row.inChunks = 150) {
     df <- task.chunk
     valid.parts <- data.frame(matrix(nrow = 0, ncol = 2))
     colnames(valid.parts) <- c("start.idx", "end.idx")
     
     second.differences <- diff(df$secondPassedDay)
     # Finding those parts which do not have data for more than 1 sec
     large.gap.idx <- which(second.differences > 1)
     if(length(large.gap.idx) == 0) {
          valid.parts <- rbind(valid.parts,
                               data.frame(start.idx = 1, end.idx = nrow(df)))
     } else {
          large.gap.idx <- c(1, large.gap.idx, nrow(df))
          for(i in 2:length(large.gap.idx)) {
               if((large.gap.idx[i] - large.gap.idx[i-1] + 1) >= row.threshold) {
                    valid.parts <- rbind(valid.parts,
                                         data.frame(start.idx = large.gap.idx[i-1], end.idx = large.gap.idx[i]))
               }
          }
     }
     colnames(valid.parts) <- c("start.idx", "end.idx")
     result <- data.frame(matrix(nrow = 0, ncol = 2))
     
     # The second part is to divide the data into 15-sec parts     
     for(i in 1:nrow(valid.parts)) {
          if((valid.parts$end.idx[i] - valid.parts$start.idx[i]) < row.inChunks) {
               result <- rbind(result, valid.parts[i, ])
          } else {
               for(j in seq(valid.parts$start.idx[i], valid.parts$end.idx[i], by = row.inChunks)) {
                    result <- rbind(result,
                                    data.frame(start.idx = j, end.idx = j + row.inChunks))
               }
          }
     }
     colnames(result) <- colnames(valid.parts)
     result
}


plot.timeDomain.freqDomain <- function(task.df, title = NULL, figureFileName = NULL) {
     # Plot the data
     temp.df <- task.df
     temp.df$Time <- sapply(temp.df$timestamp, FUN = function(x) {
          a <- as.character(unlist(strsplit(x, split = "T"))[2])
          b <- as.character(unlist(strsplit(a, split = "-"))[1])
          b
     }) 
     plot.df <- melt(temp.df[seq(1, nrow(temp.df), by = 10), -(ncol(temp.df)-1)])
     g.time <- ggplot(data = plot.df) + geom_line(aes(x = Time, y = value, group = variable, colour = variable))
     g.time <- g.time + scale_colour_manual(values = c("blue", "red", "green", "black")) + theme_bw() +
          scale_x_discrete(breaks = temp.df$Time[seq(1, nrow(task.df), by = 150)]) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = title, y = "Acceleration (m/s2)")
     freq.df <- giveFrequencyData(task.df$vm, sampling.rate = 10)
     freq.df$frequency <- round(freq.df$freq, digits = 2)
     plot.freq.df <- data.frame(frequency = seq(0, 5, by = 0.01), strength = rep(NA, 501))
     for(f in 1:nrow(plot.freq.df)) {
          plot.freq.df$strength[f] <- mean(freq.df$strength[as.character(freq.df$frequency) == as.character(plot.freq.df$frequency[f])], na.rm = T)
     }
     rm(f)
     g.freq <- ggplot(data = plot.freq.df[2:nrow(plot.freq.df),]) + geom_line(aes(x = frequency, y = strength, group = "frequency", colour = "frequency")) +
          theme_bw() + labs(title = "Frequency") + scale_colour_manual(values = c("black"))
     
     if(!is.null(figureFileName)) {
          png(filename = figureFileName, width = 1400, height = 600)
     }
     grid.arrange(g.time, g.freq, ncol = 1)
     dev.off()
}


# Final step for one participant. Averages all the fifteen second data obtained for one specific participant and returns it.
aggregateOneParticipant <- function(ppt.df) {
     ppt.df <- ppt.df[complete.cases(ppt.df), ]
     result <- data.frame(matrix(nrow = 0, ncol = 9))
     
     PID <- as.character(ppt.df$PID[1])
     
     for(t.idx in 1:max(ppt.df$taskIndex)) {
          chunked.df <- ppt.df[ppt.df$taskIndex == t.idx, ]
          task <- chunked.df$task[1]
          task.df <- data.frame(PID, task,
                                mvm = mean(chunked.df$mvm, na.rm = T),
                                sdvm = mean(chunked.df$sdvm, na.rm = T),
                                mangle = mean(chunked.df$mangle, na.rm = T),
                                sdangle = mean(chunked.df$sdangle, na.rm = T),
                                p625 = mean(chunked.df$p625, na.rm = T),
                                df = mean(chunked.df$df, na.rm = T),
                                fpdf = mean(chunked.df$fpdf, na.rm = T))
          result <- rbind(result, task.df)
     }
     colnames(result) <- colnames(task.df)
     
     result <- result[complete.cases(result), ]
}

# Constructing features for every 10-15 seconds of data
fifteenSecondFeatureConstruction <- function(df = gearS.10hz.df, PID, ppt.taskTimes = taskTimes.df, should.plot = FALSE, figureFolder = NULL) {
     result <- data.frame(matrix(nrow = 0, ncol = 10))
     for(task.idx in 1:nrow(ppt.taskTimes)) {
          result.row <- data.frame(PID = PID, task = ppt.taskTimes$Task[task.idx], taskIndex = task.idx,
                                   mvm = NA,
                                   sdvm = NA,
                                   mangle = NA,
                                   sdangle = NA,
                                   p625 = NA,
                                   df = NA,
                                   fpdf = NA)
          start.idx <- find.index.basedOnTime(df, timeStr = ppt.taskTimes$StartTime[task.idx], mode = "start")
          end.idx <- find.index.basedOnTime(df, timeStr = ppt.taskTimes$EndTime[task.idx], mode = "end")
          if(!is.na(start.idx) && !is.na(end.idx)) {
               # X, Y, Z, VM, and SecondPassedDay
               task.df <- df[start.idx:end.idx, c(1:4, 8, ncol(df))]
               if(should.plot && nrow(task.df) > 60) {
                    # Plot the data
                    figureFileName = NULL
                    if(!is.null(figureFolder)) {
                         figureFileName <- paste(figureFolder, PID, "-", as.character(result.row$task[1]), ".png", sep = "")
                    }
                    title <- paste(result.row$PID, "-", result.row$task)
                    plot.timeDomain.freqDomain(task.df, title = title, figureFileName = figureFileName)
               }
               valid.parts <- findValidParts(task.df, row.threshold = 100, row.inChunks = 150)
               for(validPart.idx in 1:nrow(valid.parts)) {
                    task.chunk <- task.df[valid.parts$start.idx[validPart.idx]:valid.parts$end.idx[validPart.idx], ]
                    if(nrow(task.chunk) > 60) { 
                         # Constructing the features
                         result.row$mvm <- mvm(task.chunk$vm)
                         result.row$sdvm <- sdvm(task.chunk$vm)
                         result.row$mangle <- mangle(x = task.chunk$accelX, task.chunk$vm)
                         result.row$sdangle <- sdangle(x = task.chunk$accelX, task.chunk$vm)
                         result.row$p625 <- p625(task.chunk$vm, sample.rate = 10)
                         result.row$df <- df(task.chunk$vm, sample.rate = 10)
                         result.row$fpdf <- fpdf(task.chunk$vm, sample.rate = 10)
                         
                    } else {
                         print(paste("Too few samples (", nrow(task.chunk),") for [", result.row$PID, " - ", result.row$task, "]", sep = ""))
                    }
                    result <- rbind(result, result.row)
                    colnames(result) <- colnames(result.row)
               }
          }
     }
     result
}
