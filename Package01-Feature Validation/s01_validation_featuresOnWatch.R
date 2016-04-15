library(jsonlite)
library(ggplot2)
library(gridExtra)
# Functions ----------------------------------
second.character <- function(timeStamp) {
     a <- timeStamp
     b <- unlist(strsplit(a, split = ":"))[3]
     b <- substr(b, start = 1, stop = 2)
     as.character(b)
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

calculate.VM <- function (x, y, z) {
     VM <- sqrt(x^2 + y^2 + z^2)
     VM
}
# Script -------------------------------------

setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package01-Feature Validation/")

watch.15min.df <- fromJSON("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Samples/041316/featureData_Tue_Apr_05_2016_13_55_56_GMT-0400_(EDT).txt")
watch.15sec.df <- fromJSON("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Samples/041316/tempFeatureData_Tue_Apr_05_2016_13_55_57_GMT-0400_(EDT).txt")
raw.df <- fromJSON("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Samples/041316/rawData_Tue_Apr_05_2016_13_56_10_GMT-0400_(EDT).txt")

# Adding second.column to the raw information
second.col <- unlist(lapply(raw.df$timestamp, second.character))
sec.col <- rep(1, length(second.col))
for(i in 2:length(second.col)) {
     if(second.col[i] == second.col[i-1]) {
          sec.col[i] <- sec.col[i - 1]
     } else {
          sec.col[i] <- sec.col[i - 1] + 1
     }
}
rm(i, second.col)
raw.df$relative.second <- sec.col
rm(sec.col)

# Appending VM.column to the raw information
raw.df$vm <- -100
for(i in 1:nrow(raw.df)) {
     raw.df$vm[i] <- calculate.VM(raw.df$x[i], raw.df$y[i], raw.df$z[i])
}
rm(i)

# Calculating 15-sec data
matin2.15sec.df <- data.frame(matrix(nrow = 0, ncol = 10))
for(sec in seq(1, (max(raw.df$relative.second) - 15), by = 15)) {
     start.idx <- min(which(raw.df$relative.second == sec))
     end.idx <- max(which(raw.df$relative.second == (sec + 15)))
     currPart <- raw.df[start.idx:end.idx, ]
     curr.feature <- data.frame(second = sec,
                                startIdx = start.idx,
                                endIdx = end.idx,
                                mvm = mvm(VM = currPart$vm),
                                sdvm = sdvm(VM = currPart$vm),
                                df = df(VM = currPart$vm),
                                fpdf = fpdf(VM = currPart$vm),
                                p625 = p625(VM = currPart$vm),
                                mangle = mangle(x = currPart$x, VM = currPart$vm),
                                sdangle = sdangle(x = currPart$x, VM = currPart$vm))
     matin2.15sec.df <- rbind(matin.15sec.df, curr.feature)
     colnames(matin.15sec.df) <- colnames(curr.feature)
}


rm(start.idx, end.idx, currPart, curr.feature, sec)

# Comparing 15-sec data from watch and mine
fifteen.second <- 1:501
column.name <- "fpdf"
plot.df <- data.frame(fifteen.second, watch.data = watch.15sec.df[fifteen.second, column.name], data = matin.15sec.df[(fifteen.second), column.name])
plot.df <- melt(plot.df, id.vars = 1)
g1 <- ggplot(data = plot.df) + geom_line(aes(x = fifteen.second, y = value, group = variable, colour = variable))
g1 <- g1 + scale_colour_manual(values = c("red", "blue")) + theme_bw() + labs(title = column.name)
resid.plot.df <- data.frame(fifteen.second, residual = (watch.15sec.df[fifteen.second, column.name] - matin.15sec.df[fifteen.second, column.name]))
g2 <- ggplot(data = resid.plot.df) + geom_line(aes(x = fifteen.second, y = residual, group = "resid", colour = "watch - data"))
g2 <- g2 + scale_colour_manual(values = c("black")) + theme_bw()
grid.arrange(g1, g2, nrow = 2)


write.csv(matin.15sec.df, file = "~/Dropbox/Work-Research/Current Directory/ROAMM/Documents/041516 - Validation of constructed features on the watch/matin_15sec.csv", row.names = F)




