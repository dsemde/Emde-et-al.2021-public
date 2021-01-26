# Load libraries
library(tidyverse)
library(randomForest)
library(missForest)



t1vt2_csv <- read_csv("Data sets\\analysis datasets\\csvs\\irrt1vt2.csv")
t1vt2_full <- data.frame(t1vt2_csv)

# Factor non-numerical data
t1vt2_full[sapply(t1vt2_full, is.character)] <- lapply(t1vt2_full[sapply(t1vt2_full, is.character)], as.factor)
#str(t1vt2_full)


# Use missForest to temporarily impute missing, non-BD values
set.seed(123)
t1vt2_filled <- missForest(t1vt2_full[c(8:45, 48:96)])
t1vt2_filled <- data.frame(t1vt2_filled$ximp, t1vt2_full[46:47])


# Split t1 vs. t2 dataset by depth category
temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "0-10cm")
t1vt2_0_10 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "10-20cm")
t1vt2_10_20 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "20-30cm")
t1vt2_20_30 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "30-40cm")
t1vt2_30_40 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "40-50cm")
t1vt2_40_50 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "50-60cm")
t1vt2_50_60 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "60-70cm")
t1vt2_60_70 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "70-80cm")
t1vt2_70_80 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "80-90cm")
t1vt2_80_90 <- temp_t1vt2$"TRUE"

temp_t1vt2 <- split(t1vt2_filled, t1vt2_filled$depth_category == "100+cm")
t1vt2_100 <- temp_t1vt2$"TRUE"


# T1 vs. T2 - BD Estimates
# Run random forest model for BD_t1 and BD_t2 separately
t1vt2_rf1 <- randomForest(BD_t1 ~ ., data = t1vt2_filled, na.action = na.omit, ntree = 580, importance = TRUE, mse = T, rsq = T)
t1vt2_rf2 <- randomForest(BD_t2 ~ ., data = t1vt2_filled, na.action = na.omit, ntree = 1000, importance = TRUE, mse = T, rsq = T)

#round(importance(t1vt2_rf1), 2)
#round(importance(t1vt2_rf2), 2)
#plot(t1vt2_rf1)
#plot(t1vt2_rf2)
#varImpPlot(t1vt2_rf1)
#varImpPlot(t1vt2_rf2)



# Predict BD_t1 and BD_t2 for the full dataset based on randomForest models above
BD_t1_rf <- predict(t1vt2_rf1, newdata = na.roughfix(t1vt2_filled))
BD_t2_rf <- predict(t1vt2_rf2, newdata = na.roughfix(t1vt2_filled))
t1vt2_estBD <- cbind(t1vt2_full, BD_t1_rf, BD_t2_rf)
#t1vt2_estBD

write.csv(t1vt2_estBD, "t1vt2_estBD.csv")
