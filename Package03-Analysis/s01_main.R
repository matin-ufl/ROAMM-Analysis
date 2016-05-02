# Functions --------------------------
spit.out.classificationResult <- function(Method, tbl) {
     accuracy <- round((tbl[1, 1] + tbl[2, 2]) * 100 / sum(tbl), digits = 2)
     sensitivity <- round(tbl[2, 2] * 100 / sum(tbl[2, ]), digits = 2)
     specificity <- round(tbl[1, 1] * 100 / sum(tbl[1, ]), digits = 2)
     result <- data.frame(Method, accuracy, sensitivity, specificity)
     result
}

# Libraries ----------------------------
library(MASS)
library(rpart)
library(randomForest)
library(ggplot2)
library(reshape2)
library(e1071)
library(class)
library(usdm)

# Loading datasets --------------------------
setwd("~/Workspaces/R workspace/ROAMM-Analysis/Package03-Analysis/")

# Loading the dataset
load("~/Dropbox/Work-Research/Current Directory/ROAMM/Data/Data for ChoresXL/d03 data for analysis/ROAMM_042616.Rdata")

roamm.df$task[roamm.df$task == "Heavy Lifting"] <- "Heavy Weight Lifting"

# Selecting the part which is related to Sedentary-vs-nonsedentary classification problem
classification.df <- roamm.df[, c(1:9, 11)]
classification.df$Sedentary <- as.factor(classification.df$Sedentary)
classification.df <- classification.df[complete.cases(classification.df), ]

# Correlation Heatmap
r <- cor(classification.df[, 3:9])
plot.df <- melt(r)
plot.df$Var1 <- factor(plot.df$Var1, levels = plot.df$Var1[1:7])
plot.df$Var2 <- factor(plot.df$Var2, levels = plot.df$Var1[1:7])
plot.df$correlation <- plot.df$value
ggplot(data = plot.df) + geom_tile(aes(x = Var1, y = Var2, fill = correlation), colour = "white") +
     scale_fill_gradient2(low = "blue", mid = "yellow", high = "red") + theme_bw() + labs(x = "", y = "")

rm(r, plot.df)
# Checking the features explaining Sedentary - can be skipped
tree.control <- rpart.control(minsplit = 3, minbucket = 3, xval = 0)
sedentary.dt <- rpart(Sedentary~., data = classification.df[, -c(1, 2)], control = tree.control)
plot(sedentary.dt, compress = T, margin = 0.2, uniform = T)
text(sedentary.dt, use.n = T); title("Sedentary VS Non-Sedentary")
rm(tree.control, sedentary.dt)

# Scaling the dataset (without centering) for analysis
scaled.classification.df <- classification.df
for (j in 3:(ncol(classification.df) - 1)) {
     currMin <- min(classification.df[, j])
     currMax <- max(classification.df[, j])
     print(paste(colnames(classification.df)[j], currMin, currMax, sep = " - "))
     for (i in 1:nrow(classification.df)) {
          scaled.classification.df[i, j] <- (classification.df[i, j] - currMin) / (currMax - currMin)
     } 
}
rm(i, j, currMax, currMin)

# Classification based on Sedentary ---------------------
# Check the difference between groups
feat.df <- classification.df[, -c(1, 10)]
g1.idx <- which(feat.df$task == "Computer Work")
diff.df <- data.frame(matrix(nrow = (ncol(feat.df)), ncol = 6))
colnames(diff.df) <- c("Feature", "Mean.Sed", "SD.Sed", "Mean.Nonsed", "SD.Nonsed", "p.value")
diff.df$Feature <- as.character(diff.df$Feature)
diff.df[1, ] <- data.frame(Feature = "a", Mean.Sed = length(g1.idx), SD.Sed = NA, Mean.Nonsed = (nrow(feat.df) - length(g1.idx)), SD.Nonsed = NA, p.value = NA)
for(i in 2:(ncol(feat.df))) {
     diff.res <- t.test(x = feat.df[g1.idx, i], y = feat.df[-g1.idx, i], var.equal = F)
     diff.df[i, ] <- data.frame(Feature = colnames(feat.df)[i], mean(feat.df[g1.idx, i]), sd(feat.df[g1.idx, i]), mean(feat.df[-g1.idx, i]), sd(feat.df[-g1.idx, i]), diff.res$p.value)
}
rm(feat.df, g1.idx, diff.res, i)
diff.df
rm(diff.df)
# Let the classification begin
loocv.classification.result <- data.frame(matrix(nrow = 0, ncol = 4))
set.seed(5855)
shuffled.idx <- sample.int(n = nrow(classification.df))
shuffled.df <- classification.df[shuffled.idx, ]
scaled.shuffled.df <- scaled.classification.df[shuffled.idx, ]

# SVM
svm.fit <- tune.svm(Sedentary~., data = shuffled.df[, -c(1:2)], gamma=10^(-6:-1), cost=10^(1:4),
                    tunecontrol = tune.control(sampling = "cross", cross = nrow(shuffled.df)))
svm.fit # gamma = 0.1, cost = 10
outcome <- data.frame(actual = shuffled.df$Sedentary, predicted = NA)
for(test.idx in 1:nrow(shuffled.df)) {
     svm.fit <- svm(Sedentary~., data = shuffled.df[-test.idx, -c(1:2)], gamma = 0.1, cost = 10)
     outcome$predicted[test.idx] <- levels(shuffled.df$Sedentary)[predict(svm.fit, shuffled.df[test.idx, -c(1:2, 10)])]
}
rm(svm.fit, test.idx)
stat.outcome <- spit.out.classificationResult("SVM", table(outcome))
loocv.classification.result <- rbind(loocv.classification.result, stat.outcome)
rm(outcome)

# KNN
knn.fit <- tune.knn(x = scaled.shuffled.df[, -c(1:2, 10)], y = scaled.shuffled.df$Sedentary, k = 1:5,
                    tunecontrol = tune.control(sampling = "cross", cross = nrow(shuffled.df)))
knn.fit # k = 1
outcome <- data.frame(actual = shuffled.df$Sedentary, predicted = NA)
for(test.idx in 1:nrow(shuffled.df)) {
     outcome$predicted[test.idx] <- as.character(knn(train = scaled.shuffled.df[-test.idx, -c(1:2, 10)],
                    cl = scaled.shuffled.df$Sedentary[-test.idx],
                    test = scaled.shuffled.df[test.idx, -c(1:2, 10)], k = 1))
}
rm(test.idx)
stat.outcome <- spit.out.classificationResult("1-NN", table(outcome))
loocv.classification.result <- rbind(loocv.classification.result, stat.outcome)
rm(outcome, stat.outcome, rf)

# Random Forest
rf <- tune.randomForest(Sedentary~., data = shuffled.df[, -c(1:2)], mtry = 1:3, ntree = 100000)
rf <- randomForest(Sedentary~., data = shuffled.df[, -c(1:2)], mtry = 2, ntree = 100000)
varImpPlot(rf)
stat.outcome <- spit.out.classificationResult("RF", rf$confusion[1:2, 1:2])
loocv.classification.result <- rbind(loocv.classification.result, stat.outcome)
rm(shuffled.df, scaled.shuffled.df, shuffled.idx, stat.outcome, rf)

rm(scaled.classification.df, spit.out.classificationResult)













# Met Score regression ----------------
normal.df <- roamm.df[, c(1:10)]
normal.df <- normal.df[complete.cases(normal.df), ]
normal.df <- cbind(normal.df[, 1:2], scale(normal.df[, 3:9]), normal.df$MET)
colnames(normal.df) <- colnames(roamm.df)[1:10]

# Checking correlation of features and MET
r <- cor(roamm.df[complete.cases(roamm.df), c(3:10)])
r <- data.frame(correlation = r[8, ])
r$variables <- as.character(rownames(r))
idx <- with(data = r, order(correlation, decreasing = F))
r <- r[idx, ]
r <- r[-8, ]
r$variables <- factor(r$variables, levels = r$variables)
ggplot(data = r) + geom_bar(aes(x = variables, y = correlation), stat = "identity", colour = "red", fill = "blue", alpha = 0.7, position = "dodge") + theme_bw()

rm(r, idx)
# Checking VIF
vifstep(normal.df[, 3:9])

# Making data ready for regressions
set.seed(5855)
shuffled.idx <- sample.int(n = nrow(normal.df))
normal.df <- normal.df[shuffled.idx, ]

# Linear Regression - LOOCV
outcome <- data.frame(actual = normal.df$MET, predicted = NA)
for(test.idx in 1:nrow(normal.df)) {
     linFit <- lm(MET~., data = normal.df[-test.idx, -c(1:2)])
     outcome$predicted[test.idx] <- predict(linFit, normal.df[test.idx, -c(1:2, 10)])
}
rm(test.idx, shuffled.idx, linFit)
met.linFit <- lm(actual~predicted, data = outcome)
summary(met.linFit)
summary(met.linFit)$sigma

linFit <- lm(MET~., normal.df[, 3:10])
linFit
plot.df <- outcome
plot.df$predictedLine <- linFit$fitted.values
ggplot(data = plot.df) + geom_point(aes(x = actual, y = predicted), size = 3) + theme_classic() + geom_smooth(aes(x = actual, y = predicted), method = "lm") +
     labs(title = "Linear Regression") + xlim(1, 5.5) + ylim(1, 4.5)
rm(linFit, plot.df, met.linFit, outcome)

# Support Vector Regression
tune.svm(MET~., data = normal.df[, -c(1:2)], gamma=10^(-6:-1), cost=10^(1:4),
         tunecontrol = tune.control(sampling = "cross", cross = nrow(normal.df)))
outcome <- data.frame(actual = normal.df$MET, predicted = NA)
for(test.idx in 1:nrow(normal.df)) {
     svmFit <- svm(MET~., data = normal.df[-test.idx, -c(1:2)], gamma = 0.1, cost = 10)
     outcome$predicted[test.idx] <- predict(svmFit, normal.df[test.idx, -c(1:2, 10)])
}
rm(test.idx, svmFit)
met.linFit <- lm(actual~predicted, data = outcome)
summary(met.linFit)
summary(met.linFit)$sigma
plot.df <- outcome
ggplot(data = plot.df) + geom_point(aes(x = actual, y = predicted), size = 3) + theme_classic() + geom_smooth(aes(x = actual, y = predicted), method = "lm") +
     labs(title = "SVR") + xlim(1, 5.5) + ylim(1, 4.5)
rm(plot.df, met.linFit, outcome)

# Random Forest
outcome <- data.frame(actual = normal.df$MET, predicted = NA)

tune.randomForest(MET~., data = normal.df[, -c(1:2)], mtry = 1:3, ntree = 100000)
rfFit <- randomForest(MET~., data = normal.df[, -c(1:2)], mtry = 1, ntree = 100000)
outcome <- data.frame(actual = normal.df$MET, predicted = rfFit$predicted)
rm(rfFit)
met.linFit <- lm(actual~predicted, data = outcome)
summary(met.linFit)
summary(met.linFit)$sigma
plot.df <- outcome
ggplot(data = plot.df) + geom_point(aes(x = actual, y = predicted), size = 3) + theme_classic() + geom_smooth(aes(x = actual, y = predicted), method = "lm") +
     labs(title = "Random Forest") + xlim(1, 5.5) + ylim(1, 4.5)
rm(plot.df, met.linFit, outcome)


# Exploration -------------------------
rm(normal.df)
# Lets apply PCA
scaled.classification.df <- classification.df
for (j in 3:(ncol(classification.df) - 1)) {
     currMin <- min(classification.df[, j])
     currMax <- max(classification.df[, j])
     print(paste(colnames(classification.df)[j], currMin, currMax, sep = " - "))
     for (i in 1:nrow(classification.df)) {
          scaled.classification.df[i, j] <- (classification.df[i, j] - currMin) / (currMax - currMin)
     } 
}
rm(i, j, currMax, currMin)
rownames(scaled.classification.df) <- paste(scaled.classification.df$PID, scaled.classification.df$task, sep = "-")
exclude.rows <- c(78, 50)
pca.out <- prcomp(scaled.classification.df[-exclude.rows, -c(1:2, 5, 10)])
summary(pca.out)
biplot(pca.out)

new.df <- cbind(classification.df[-exclude.rows, c(1:2)], pca.out$x[, 1:2])
rm(exclude.rows)
colnames(new.df) <- c(colnames(new.df)[1:2], "Hand.Movement", "Total.to.Hand.Ratio")

new.df$cluster <- "Ambulation"
new.df$cluster[new.df$task %in% c("Computer Work", "Leg Curl", "Leg Extension", "Chest Press", "Yoga")] <- "Not-Ambulation"
new.df$cluster <- factor(new.df$cluster)
g <- ggplot(data = new.df) + geom_point(aes(x = Hand.Movement, y = Total.to.Hand.Ratio, colour = cluster, shape = cluster), size = 3) + scale_colour_manual(values = c("darkgreen", "darkred")) + theme_bw() + scale_shape_manual(values = c(16, 17))
g + geom_density2d(data = new.df[new.df$cluster == "Not-Ambulation", ], aes(x = Hand.Movement, y = Total.to.Hand.Ratio), colour = "red") + 
     geom_density2d(data = new.df[new.df$cluster == "Ambulation", ], aes(x = Hand.Movement, y = Total.to.Hand.Ratio), colour = "green") + xlim(-1.5, 1) + ylim(-0.5, 0.7)
