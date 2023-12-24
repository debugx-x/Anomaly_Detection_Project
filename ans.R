# TERM PROJECT

#Libraries
library(depmixS4)
library(dplyr)
library(ggbiplot)
library(ggplot2)


# read in the data
data <- read.table("./Data/Term_Project_Dataset.txt", header = TRUE, sep = ",")

# create a new column with date and time
data$DateTime <- paste(data$Date, data$Time, sep = " ")
data$DateTime <- as.POSIXlt(data$DateTime, format = "%d/%m/%Y %H:%M:%S")

# print data in first 5 rows
head(data)

### Feature Engineering

# separate numberic data
data_numeric = data[,4:ncol(data)-1]

# impute data with missing values using mean of adjacent values
data_numeric[is.na(data_numeric)] <- 0

#calculate principal components
results <- prcomp(data_numeric, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#print results
results$rotation

#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

# print results summary 
summary(results)

#print biplot
biplot(results, scale = 0)

#calculate total variance explained by each principal component
print(results$sdev^2 / sum(results$sdev^2))

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
plot(c(1:7), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot")


## Based on the summary 66% of the information is present in the first 3 columns 
##- Global_active_power, Global_reactive_power, Voltage
## And 80% of the information is present in the first 4 columns 
##- Global_active_power, Global_reactive_power, Voltage, Global_intensity

### HMM Training and Testing

# Lets Choose first 4 columns as variables for HMM

# scale the numeric data
data_scaled <- scale(data_numeric[1:4])

# merge of data and scaled data
data_scaled <- cbind(data[,c("Date", "Time", "DateTime")], data_scaled)

# print scaled data in first 5 rows
head(data_scaled)

# filter time window between 5pm and 10pm based on 24/11/2009 (Tuesday)
data_train <- data_scaled[data_scaled$Time >= "15:00:00" & data_scaled$Time <= "22:00:00",]
data_train <- data_train[weekdays(data_train$DateTime) %in% c("Tuesday"), ]

# set seed
set.seed(301386847)

# Generate models by loop through 4 to 24 states
fitmodels <- list()
BICvector <- list()
logLikVector <- list()

for (state in 4:24) {
  print(paste("Number of States: ", state))
  model <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = state,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
  fitModel <- fit(model)
  fitmodels[[state]] <- fitModel
  logLikVector[[state]] <- BIC(fitModel)
  BICvector[[state]] <- logLik(fitModel)
}
#

# Choosen States: :- 5, 9, 12, 15, 21

# create multivariate HMM model data_train[4:ncol(data_train)] using depmixS4 package nstate = 5
model_train_1 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = 5,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fitModel_train_1 <- fit(model_train_1)


# create HMM model using depmixS4 package nstate = 9
model_train_2 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = 9,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fitModel_train_2 <- fit(model_train_2)

# create HMM model using depmixS4 package nstate = 12
model_train_3 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = 12,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fitModel_train_3 <- fit(model_train_3)

# create HMM model using depmixS4 package nstate = 15
model_train_4 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = 15,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fitModel_train_4 <- fit(model_train_4)

# create HMM model using depmixS4 package nstate = 21
model_train_5 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                  data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = 21,
                  family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fitModel_train_5 <- fit(model_train_5)


## Assuming Best Model is nstate 21 


### Anomaly Detection

# Train Best model
n_best = 21
mod <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
              data = data_train[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], nstates = n_best,
              family = list(gaussian(), gaussian(),gaussian(),gaussian()), ntimes = length(data_train[, c("Global_intensity")]))
fm <- fit(mod)

# read in the Test data 1
anomaly_data_1 <- read.table("./Data/Data_with_Anomalies/Dataset_with_Anomalies_1.txt", header = TRUE, sep = ",")

# create a new column with date and time
anomaly_data_1$DateTime <- paste(anomaly_data_1$Date, anomaly_data_1$Time, sep = " ")
anomaly_data_1$DateTime <- as.POSIXlt(anomaly_data_1$DateTime, format = "%d/%m/%Y %H:%M:%S")

# scale the numeric data
anomaly_data_1_scaled <- scale(anomaly_data_1[,3:6])

# merge of data and scaled data
anomaly_data_1_scaled <- cbind(anomaly_data_1[,c("Date", "Time", "DateTime")], anomaly_data_1_scaled)

# print scaled data in first 5 rows
head(anomaly_data_1_scaled)

# filter time window between 5pm and 10pm based on 24/11/2009 (Tuesday)
data_test_1 <- anomaly_data_1_scaled[anomaly_data_1_scaled$Time >= "15:00:00" & anomaly_data_1_scaled$Time <= "22:00:00",]
data_test_1 <- data_test_1[weekdays(data_test_1$DateTime) %in% c("Tuesday"), ]

# Fit model 
ncount <- nrow(data_test_1)
model_test_1 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                       data = data_test_1[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], 
                       nstates = n_best, family = list(gaussian(), gaussian(), gaussian(), gaussian()), ntimes = ncount)
model_test_1 <- setpars(model_test_1, getpars(fm))
fitModel_test_1 <- forwardbackward(model_test_1)


# read in the Test data 2
anomaly_data_2 <- read.table("./Data/Data_with_Anomalies/Dataset_with_Anomalies_2.txt", header = TRUE, sep = ",")

# create a new column with date and time
anomaly_data_2$DateTime <- paste(anomaly_data_2$Date, anomaly_data_2$Time, sep = " ")
anomaly_data_2$DateTime <- as.POSIXlt(anomaly_data_2$DateTime, format = "%d/%m/%Y %H:%M:%S")

# scale the numeric data
anomaly_data_2_scaled <- scale(anomaly_data_2[,3:6])

# merge of data and scaled data
anomaly_data_2_scaled <- cbind(anomaly_data_2[,c("Date", "Time", "DateTime")], anomaly_data_2_scaled)

# print scaled data in first 5 rows
head(anomaly_data_2_scaled)

# filter time window between 5pm and 10pm based on 24/11/2009 (Tuesday)
data_test_2 <- anomaly_data_2_scaled[anomaly_data_2_scaled$Time >= "15:00:00" & anomaly_data_2_scaled$Time <= "22:00:00",]
data_test_2 <- data_test_2[weekdays(data_test_2$DateTime) %in% c("Tuesday"), ]

# Fit model 
ncount <- nrow(data_test_2)
model_test_2 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                       data = data_test_2[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], 
                       nstates = n_best, family = list(gaussian(), gaussian(), gaussian(), gaussian()), ntimes = ncount)
model_test_2 <- setpars(model_test_2, getpars(fm))
fitModel_test_2 <- forwardbackward(model_test_2)


# read in the Test data 3
anomaly_data_3 <- read.table("./Data/Data_with_Anomalies/Dataset_with_Anomalies_3.txt", header = TRUE, sep = ",")

# create a new column with date and time
anomaly_data_3$DateTime <- paste(anomaly_data_3$Date, anomaly_data_3$Time, sep = " ")
anomaly_data_3$DateTime <- as.POSIXlt(anomaly_data_3$DateTime, format = "%d/%m/%Y %H:%M:%S")

# scale the numeric data
anomaly_data_3_scaled <- scale(anomaly_data_3[,3:6])

# merge of data and scaled data
anomaly_data_3_scaled <- cbind(anomaly_data_3[,c("Date", "Time", "DateTime")], anomaly_data_3_scaled)

# print scaled data in first 5 rows
head(anomaly_data_3_scaled)

# filter time window between 5pm and 10pm based on 24/11/2009 (Tuesday)
data_test_3 <- anomaly_data_3_scaled[anomaly_data_3_scaled$Time >= "15:00:00" & anomaly_data_3_scaled$Time <= "22:00:00",]
data_test_3 <- data_test_3[weekdays(data_test_3$DateTime) %in% c("Tuesday"), ]

# Fit model 
ncount <- nrow(data_test_3)
model_test_3 <- depmix(response = list(Global_active_power ~ 1, Global_reactive_power ~ 1, Voltage ~ 1, Global_intensity ~ 1),
                       data = data_test_3[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Time")], 
                       nstates = n_best, family = list(gaussian(), gaussian(), gaussian(), gaussian()), ntimes = ncount)
model_test_3 <- setpars(model_test_3, getpars(fm))
fitModel_test_3 <- forwardbackward(model_test_3)

# print results
print(fitModel_test_1[6])
print(fitModel_test_2[6])
print(fitModel_test_3[6])
print(fm)
