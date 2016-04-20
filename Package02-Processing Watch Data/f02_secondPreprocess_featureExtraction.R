# Functions --------------------------------------
find.index.basedOnTime <- function(df = gearS.10hz.df, timeStr, mode = "start") {
     # This threshold sets how many minutes we are allowing to be missing.
     THRESHOLD.MINUTE <- 2
     secondPassedDay <- calculateSecondPassedDay(timeStr)
     idx <- NA
     # For start, the data should appear at or some time after the written time
     if(mode == "start") {
          idx <- which(df$secondPassedDay == secondPassedDay)
          if(length(idx) > 0) {
               idx <- min(idx)
          } else { # Not found
               idx <- min(which(df$secondPassedDay > secondPassedDay))
          }
     } else { # For end, the data should not be considered any time after the written time
          idx <- which(df$secondPassedDay == secondPassedDay)
          if(length(idx) > 0) {
               idx <- max(idx)
          } else { # Not found
               idx <- max(which(df$secondPassedDay < secondPassedDay))
          }
     }
     # If the found time is very far from the ideal point (we have a lot of not recorded data), we should not consider it
     if(!is.na(idx)) {
          if(abs(secondPassedDay - df$secondPassedDay[idx]) > (THRESHOLD.MINUTE * 600)) {
               print(paste("The found time (", paste(df$hour, df$minute, df$second, sep = ":"), ") is more than ", THRESHOLD.MINUTE, " away from the ideal point (", timeStr, ")", sep = ""))
               idx <- NA
          }
     }
     idx
}

calculateSecondPassedDay <- function(timeStr) {
     hour <- as.numeric(substr(timeStr, start = 1, stop = 2))
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