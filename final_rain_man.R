library(dplyr)
library(chron)
library(makeR) #didn't work
library(hash)
library(reshape2)
library(ggplot2)
library(mvtnorm)
library(e1071)
library(splines)
library(MASS)
library(ISLR)
library(xtable)
library(class)
library(tree)
library(randomForest)
library(gbm)


rm(list = ls())
getwd()
rain_data <- read.csv("rain_man_uf.csv")
colnames(rain_data)[11] <- "Wet.Bulb.Temp"
colnames(rain_data)[10] <- "UV.Rad.Sens"
colnames(rain_data)[8] <- "Solar.Rad.Sens"
#rain_data <- rain_data[,1:15]

#convert data values to 1 or 0, get rid of useless covariate
rain_data$Rain.Gauge <- rain_data$Rain.Gauge > 0
rain_data$Rain.Gauge <- as.integer(rain_data$Rain.Gauge)

#convert some chars to floats
rain_data$Solar.Rad.Sens <- as.numeric(rain_data$Solar.Rad.Sens)

#throw away an NA
rain_data <- rain_data[-which(is.na(rain_data$Solar.Rad.Sens)), ]

rain_data$UV.Rad.Sens <- as.numeric(rain_data$UV.Rad.Sens)
rain_data$Wet.Bulb.Temp <- as.numeric(rain_data$Wet.Bulb.Temp)




#converting character dates to dates in r
times <- rain_data$Timestamp
parts = t(as.data.frame(strsplit(times, " ")))
row.names(parts) = NULL
for (i in 1:dim(parts)[1])
{
  parts[i,2] = paste(parts[i,2], ":00", sep = "")
}
dates = chron(dates = parts[,1], times = parts[,2], format = c('m/d/y','h:m:s'))
rain_data$Timestamp <- dates


#converting to cyclic
rain_data_y_early <- rain_data[,14]
rain_data_x_early <- rain_data[,1:13]
rain_data_x_early_cyclic <- rain_data_x_early

timestamp_secs <- as.numeric(as.POSIXct(rain_data_x_early$Timestamp))
secs_per_year = 365*24*60*60

rain_data_x_early_cyclic$SinTime <- sin(timestamp_secs * 2 * pi / secs_per_year)
rain_data_x_early_cyclic$CosTime <- cos(timestamp_secs * 2 * pi / secs_per_year)
plot(SinTime ~ Timestamp, rain_data_x_early_cyclic)
points(CosTime~ Timestamp, rain_data_x_early_cyclic, col = "red")

#get rid of the dates in the cyclic dataset
rain_data_x_early_cyclic <- rain_data_x_early_cyclic[,2:15]


#the plots show missing dates - find them!
dates <- rain_data_x_early$Timestamp
date_range_hrs <- seq(min(dates), max(dates), by = (1/24))
missing_hrs <- date_range_hrs[!date_range_hrs %in% dates]
days_missing <- format(as.Date(missing_hrs, format=c("%Y-%m-%d", "h:m:s")), format = "%Y-%m-%d")
sum_days_missing <- table(days_missing)
unique_days_missing <- unique(days_missing)
plot_helper <- as.numeric(sum_days_missing)


#plot of the missing days:
#plot_helper <- as.list(rep(1, length(missing_days)))
calendarHeat(dates = unique_days_missing, values = plot_helper)

#FOR UF DATA
# essentially keeps every third hour, kinda sucks for time series stuff, but really does
# not matter for our purposes, bc looking at 6, 12, 24, 48 hrs ahead (so skip over those times)




#first trim the data for stuff (as then we get rid of lines, as maxs/mins might be in the lines)

#first make hashmap, set date as key, if rain as value

true_hash <- hash()
for (i in 1:length(rain_data_y_early))
{
  true_hash[[as.character(rain_data_x_early$Timestamp[i])]] <- rain_data_y_early[i]
}





#6 hours
indices_remove_1 <- c()
y_6hrs <- rain_data_y_early
x_6hrs <- rain_data_x_early_cyclic

for (i in 1:length(rain_data_y_early))
{
  if (is.null(true_hash[[as.character(rain_data_x_early$Timestamp[i] + 6/24)]]))
  {
    indices_remove_1 <- append(indices_remove_1, i)
    #y_6hrs[i] <- 20
  }
  else
  {
    y_6hrs[i] <- true_hash[[as.character(rain_data_x_early$Timestamp[i] + 6/24)]]
  }
}

#remove from x and y
y_6hrs <- y_6hrs[-indices_remove_1]
x_6hrs <- x_6hrs[-indices_remove_1, ]


#12 hours

indices_remove_2 <- c()
y_12hrs <- rain_data_y_early
x_12hrs <- rain_data_x_early_cyclic
for (i in 1:length(rain_data_y_early))
{
  if (is.null(true_hash[[as.character(rain_data_x_early$Timestamp[i] + 12/24)]]))
  {
    indices_remove_2 <- append(indices_remove_2, i)
  }
  else
  {
    y_12hrs[i] <- true_hash[[as.character(rain_data_x_early$Timestamp[i] + 12/24)]]
  }
}
#remove from x and y
y_12hrs <- y_12hrs[-indices_remove_2]
x_12hrs <- x_12hrs[-indices_remove_2, ]


#24 hours
indices_remove <- c()
y_24hrs <- rain_data_y_early
x_24hrs <- rain_data_x_early_cyclic
for (i in 1:length(rain_data_y_early))
{
  if (is.null(true_hash[[as.character(rain_data_x_early$Timestamp[i] + 24/24)]]))
  {
    indices_remove <- append(indices_remove, i)
  }
  else
  {
    y_24hrs[i] <- true_hash[[as.character(rain_data_x_early$Timestamp[i] + 24/24)]]
  }
}
#remove from x and y
y_24hrs <- y_24hrs[-indices_remove]
x_24hrs <- x_24hrs[-indices_remove, ]




#48 hours
indices_remove <- c()
y_48hrs <- rain_data_y_early
x_48hrs <- rain_data_x_early_cyclic
for (i in 1:length(rain_data_y_early))
{
  if (is.null(true_hash[[as.character(rain_data_x_early$Timestamp[i] + 48/24)]]))
  {
    indices_remove <- append(indices_remove, i)
  }
  else
  {
    y_48hrs[i] <- true_hash[[as.character(rain_data_x_early$Timestamp[i] + 48/24)]]
  }
}
#remove from x and y
y_48hrs <- y_48hrs[-indices_remove]
x_48hrs <- x_48hrs[-indices_remove, ]

#min-max normalize each covariate (i.e. X-min(x) / (max(x) - min(x)))

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#make sure to not normalize 2 time cols
x_6hrs_norm <- as.data.frame(lapply(x_6hrs[,1:(dim(x_6hrs)[2])], min_max_norm))
x_12hrs_norm <- as.data.frame(lapply(x_12hrs[,1:(dim(x_12hrs)[2])], min_max_norm))
x_24hrs_norm <- as.data.frame(lapply(x_24hrs[,1:(dim(x_24hrs)[2])], min_max_norm))
x_48hrs_norm <- as.data.frame(lapply(x_48hrs[,1:(dim(x_48hrs)[2])], min_max_norm))

rain_data_x_cyclic_norm <- as.data.frame(lapply(rain_data_x_early_cyclic[,1:14], min_max_norm))


#explore covariates (how ig?) maybe PCA?
#going to use heatmap
corr_mat <- round(cor(rain_data_x_cyclic_norm), 2)

melt_corr_mat <- melt(corr_mat)

ggplot(data = melt_corr_mat, aes(x=Var1, y=Var2,
                                 fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4) +
  theme(axis.text = element_text(size = 10))


x <- melt(rain_data_x_cyclic_norm)
plt <- ggplot(data = x, aes(x = variable, y = value))
plt + geom_boxplot() + theme_minimal() + labs(x = "Title", y = "x")


#talk about the insights here:


#correlation btwn covariates and y:

corr_matrix <- data.frame(NA, 1, dim(rain_data_x_cyclic_norm)[2])
for (i in 1:dim(rain_data_x_cyclic_norm)[2])
{
  #print(i)
  corr_matrix[i] <- cor(rain_data_y_early, rain_data_x_cyclic_norm[, i])
  colnames(corr_matrix)[i] <- colnames(rain_data_x_early_cyclic)[i]
}
corr_matrix




#realized at this point that I need to combine x and y for all the models so recomboing here:

hrs_6 <- x_6hrs_norm
hrs_12 <- x_12hrs_norm
hrs_24 <- x_24hrs_norm
hrs_48 <- x_48hrs_norm

hrs_6$y <- y_6hrs
hrs_12$y <- y_12hrs
hrs_24$y <- y_24hrs
hrs_48$y <- y_48hrs






#NOW MODEL TIME

set.seed(42)


prop = c(.8, .2)

#first split into train test validate


train_size <- floor(prop[1] * nrow(x_6hrs_norm))
test_size <- nrow(x_6hrs_norm) - train_size

num_rows_6hrs <- nrow(x_6hrs_norm)

#do everything on the 6hr data for the moment, do it for three others in a second/



errorMat <- matrix(NA, 1, 9)
compute_error <- matrix(NA, 50, 3)

for (i in (1:50))
{
  indicesTraining    <- sort(sample(seq_len(num_rows_6hrs), size=train_size))
  
  indicesTest        <- setdiff(seq_len(num_rows_6hrs), indicesTraining)
  
  train_6hrs <- hrs_6[indicesTraining, ]
  test_6hrs <- hrs_6[indicesTest, ]
  
  train_6hrs$y <- as.factor(train_6hrs$y)
  test_6hrs$y <- as.factor(test_6hrs$y)
  
  #Logistic Regression
  mod <- glm(y ~., data = train_6hrs, family = binomial)
  #summary(mod_lr)
  test_pred_1 <- 1 * (predict(mod, test_6hrs, type = "response") > 0.5)
  compute_error[i, 1] <- mean(test_pred_1 != test_6hrs$y)
  
  #LDA
  mod <- lda(y ~., data = train_6hrs)
  #summary(mod_lr)
  test_pred_2 <- as.numeric(predict(mod, test_6hrs)$class) - 1
  compute_error[i, 2] <- mean(test_pred_2 != test_6hrs$y)
  
  
  #QDA
  mod <- qda(y ~., data = train_6hrs)
  test_pred_3 <- as.numeric(predict(mod, test_6hrs)$class) - 1
  compute_error[i, 3] <- mean(test_pred_3 != test_6hrs$y)
}

avged_3 <- apply(compute_error, 2, mean)
errorMat[1:3] <- as.numeric(avged_3)

#do this on the 50th iteration of sampled data

#KNN
tune_knn <- tune.knn(train_6hrs[,1:14], as.factor(train_6hrs[,15]), k = 1:25)
test_pred_4 <- knn(train_6hrs[,1:14], test_6hrs[,1:14], train_6hrs[,15], k=tune_knn$best.parameters)
errorMat[4] <- mean(test_pred_4 != test_6hrs$y)



#Basic Decision 
fit = tree(y ~., data = train_6hrs)
cv.fit = cv.tree(fit)
plot(cv.fit$size, cv.fit$dev, type='b')
#best = something in plot
prune.fit = prune.tree(fit, best = 5)
test_pred_5 <- as.numeric(predict(prune.fit, newdata = test_6hrs)[, 2] > 0.5)
errorMat[5] <- mean(test_pred_5 != test_6hrs$y)

#RF if the others are quick
fitRF = randomForest(y ~., data = train_6hrs, mtry = 5)
test_pred_6 = (as.numeric(predict(fitRF, newdata = test_6hrs)) - 1)
errorMat[6] <- mean(test_pred_6 != test_6hrs$y)

#Boosting
train_6hrs$y <- (as.numeric(train_6hrs$y) - 1)
test_6hrs$y <- (as.numeric(test_6hrs$y) - 1)



fitGBM = gbm(y ~ ., data=train_6hrs, distribution="bernoulli",
             n.trees=5000, interaction.depth=2, shrinkage=0.001)

test_pred_7 = as.numeric(predict(fitGBM, newdata=test_6hrs, n.trees=5000, type = 'response') > 0.5)
errorMat[7] <- mean(test_pred_7 != test_6hrs$y)


#Combo of KNN and RandomForest (for the False Negative Purpose)
total_v1 <- ((as.numeric(test_pred_4) - 1) + test_pred_6)
test_pred_8 <- as.numeric(total_v1 >= 1)
errorMat[8] <- mean(test_pred_8 != test_6hrs$y)


#Combo of All Models (if one says yes, all say yes)
total_v2 <- as.numeric(test_pred_1) + test_pred_2 +
  test_pred_3 +
  (as.numeric(test_pred_4) - 1) +
  test_pred_5 +
  test_pred_6 +
  test_pred_7
test_pred_9 <- as.numeric(total_v2 >= 1)
errorMat[9] <- mean(test_pred_9 != test_6hrs$y)






data.frame("Logistic" = errorMat[1], "LDA" = errorMat[2], "QDA" = errorMat[3], "K" = as.numeric(tune_knn$best.parameters),
           "KNN" = errorMat[4], "Basic Dec. Tree" = errorMat[5], "RandomForest" = errorMat[6], "GBM" = errorMat[7],
           "Ensemble 1" = errorMat[8], "Ensemble 2" = errorMat[9])

fnr <- matrix(NA, 1, 9)
total_raining <- sum(test_6hrs$y == 1)
fnr[1] <- sum(test_pred_1 == 0 & test_6hrs$y == 1) / total_raining
fnr[2] <- sum(test_pred_2 == 0 & test_6hrs$y == 1) / total_raining
fnr[3] <- sum(test_pred_3 == 0 & test_6hrs$y == 1) / total_raining
fnr[4] <- sum(test_pred_4 == 0 & test_6hrs$y == 1) / total_raining
fnr[5] <- sum(test_pred_5 == 0 & test_6hrs$y == 1) / total_raining
fnr[6] <- sum(test_pred_6 == 0 & test_6hrs$y == 1) / total_raining
fnr[7] <- sum(test_pred_7 == 0 & test_6hrs$y == 1) / total_raining
fnr[8] <- sum(test_pred_8 == 0 & test_6hrs$y == 1) / total_raining
fnr[9] <- sum(test_pred_9 == 0 & test_6hrs$y == 1) / total_raining

data.frame("Logistic" = fnr[1], "LDA" = fnr[2], "QDA" = fnr[3],
           "KNN" = fnr[4], "Basic Dec. Tree" = fnr[5], "RandomForest" = fnr[6], "GBM" = fnr[7],
           "Ensemble 1" = fnr[8], "Ensemble 2" = fnr[9])
total_raining / length(test_6hrs$y)



#Now at 12 hrs

prop = c(.8, .2)

#first split into train test


train_size <- floor(prop[1] * nrow(x_12hrs_norm))
test_size <- nrow(x_12hrs_norm) - train_size

num_rows_12hrs <- nrow(x_12hrs_norm)

#do everything on the 12hr data for the moment, do it for three others in a second/



errorMat <- matrix(NA, 1, 9)
compute_error <- matrix(NA, 50, 3)

for (i in (1:50))
{
  indicesTraining    <- sort(sample(seq_len(num_rows_12hrs), size=train_size))
  
  indicesTest        <- setdiff(seq_len(num_rows_12hrs), indicesTraining)
  
  train_12hrs <- hrs_12[indicesTraining, ]
  test_12hrs <- hrs_12[indicesTest, ]
  
  train_12hrs$y <- as.factor(train_12hrs$y)
  test_12hrs$y <- as.factor(test_12hrs$y)
  
  #Logistic Regression
  mod <- glm(y ~., data = train_12hrs, family = binomial)
  #summary(mod_lr)
  test_pred_1 <- 1 * (predict(mod, test_12hrs, type = "response") > 0.5)
  compute_error[i, 1] <- mean(test_pred_1 != test_12hrs$y)
  
  #LDA
  mod <- lda(y ~., data = train_12hrs)
  #summary(mod_lr)
  test_pred_2 <- as.numeric(predict(mod, test_12hrs)$class) - 1
  compute_error[i, 2] <- mean(test_pred_2 != test_12hrs$y)
  
  
  #QDA
  mod <- qda(y ~., data = train_12hrs)
  test_pred_3 <- as.numeric(predict(mod, test_12hrs)$class) - 1
  compute_error[i, 3] <- mean(test_pred_3 != test_12hrs$y)
}

avged_3 <- apply(compute_error, 2, mean)
errorMat[1:3] <- as.numeric(avged_3)

#do this on the 50th iteration of sampled data

#KNN
tune_knn <- tune.knn(train_12hrs[,1:14], as.factor(train_12hrs[,15]), k = 1:25)
test_pred_4 <- knn(train_12hrs[,1:14], test_12hrs[,1:14], train_12hrs[,15], k=tune_knn$best.parameters)
errorMat[4] <- mean(test_pred_4 != test_12hrs$y)



#Basic Decision 
fit = tree(y ~., data = train_12hrs)
cv.fit = cv.tree(fit)
plot(cv.fit$size, cv.fit$dev, type='b')
#best = something in plot
prune.fit = prune.tree(fit, best = 4)
test_pred_5 <- as.numeric(predict(prune.fit, newdata = test_12hrs)[, 2] > 0.5)
errorMat[5] <- mean(test_pred_5 != test_12hrs$y)

#RF if the others are quick
fitRF = randomForest(y ~., data = train_12hrs, mtry = 5)
test_pred_6 = (as.numeric(predict(fitRF, newdata = test_12hrs)) - 1)
errorMat[6] <- mean(test_pred_6 != test_12hrs$y)

#Boosting
train_12hrs$y <- (as.numeric(train_12hrs$y) - 1)
test_12hrs$y <- (as.numeric(test_12hrs$y) - 1)



fitGBM = gbm(y ~ ., data=train_12hrs, distribution="bernoulli",
             n.trees=5000, interaction.depth=1, shrinkage=0.001)

test_pred_7 = as.numeric(predict(fitGBM, newdata=test_12hrs, n.trees=5000, type = 'response') > 0.5)
errorMat[7] <- mean(test_pred_7 != test_12hrs$y)


#Combo of KNN and RandomForest (for the False Negative Purpose)
total_v1 <- ((as.numeric(test_pred_4) - 1) + test_pred_6)
test_pred_8 <- as.numeric(total_v1 >= 1)
errorMat[8] <- mean(test_pred_8 != test_12hrs$y)


#Combo of All Models (if one says yes, all say yes)
total_v2 <- as.numeric(test_pred_1) + test_pred_2 +
  test_pred_3 +
  (as.numeric(test_pred_4) - 1) +
  test_pred_5 +
  test_pred_6 +
  test_pred_7
test_pred_9 <- as.numeric(total_v2 >= 1)
errorMat[9] <- mean(test_pred_9 != test_12hrs$y)






data.frame("Logistic" = errorMat[1], "LDA" = errorMat[2], "QDA" = errorMat[3], "K" = as.numeric(tune_knn$best.parameters),
           "KNN" = errorMat[4], "Basic Dec. Tree" = errorMat[5], "RandomForest" = errorMat[6], "GBM" = errorMat[7],
           "Ensemble 1" = errorMat[8], "Ensemble 2" = errorMat[9])

fnr <- matrix(NA, 1, 9)
total_raining <- sum(test_12hrs$y == 1)
fnr[1] <- sum(test_pred_1 == 0 & test_12hrs$y == 1) / total_raining
fnr[2] <- sum(test_pred_2 == 0 & test_12hrs$y == 1) / total_raining
fnr[3] <- sum(test_pred_3 == 0 & test_12hrs$y == 1) / total_raining
fnr[4] <- sum(test_pred_4 == 0 & test_12hrs$y == 1) / total_raining
fnr[5] <- sum(test_pred_5 == 0 & test_12hrs$y == 1) / total_raining
fnr[6] <- sum(test_pred_6 == 0 & test_12hrs$y == 1) / total_raining
fnr[7] <- sum(test_pred_7 == 0 & test_12hrs$y == 1) / total_raining
fnr[8] <- sum(test_pred_8 == 0 & test_12hrs$y == 1) / total_raining
fnr[9] <- sum(test_pred_9 == 0 & test_12hrs$y == 1) / total_raining

data.frame("Logistic" = fnr[1], "LDA" = fnr[2], "QDA" = fnr[3],
           "KNN" = fnr[4], "Basic Dec. Tree" = fnr[5], "RandomForest" = fnr[6], "GBM" = fnr[7],
           "Ensemble 1" = fnr[8], "Ensemble 2" = fnr[9])
total_raining / dim(test_12hrs)[1]



#for 24 hrs:

prop = c(.8, .2)

#first split into train test


train_size <- floor(prop[1] * nrow(x_24hrs_norm))
test_size <- nrow(x_24hrs_norm) - train_size

num_rows_24hrs <- nrow(x_24hrs_norm)

#do everything on the 24hr data for the moment, do it for three others in a second/



errorMat <- matrix(NA, 1, 9)
compute_error <- matrix(NA, 50, 3)

for (i in (1:50))
{
  indicesTraining    <- sort(sample(seq_len(num_rows_24hrs), size=train_size))
  
  indicesTest        <- setdiff(seq_len(num_rows_24hrs), indicesTraining)
  
  train_24hrs <- hrs_24[indicesTraining, ]
  test_24hrs <- hrs_24[indicesTest, ]
  
  train_24hrs$y <- as.factor(train_24hrs$y)
  test_24hrs$y <- as.factor(test_24hrs$y)
  
  #Logistic Regression
  mod <- glm(y ~., data = train_24hrs, family = binomial)
  #summary(mod_lr)
  test_pred_1 <- 1 * (predict(mod, test_24hrs, type = "response") > 0.5)
  compute_error[i, 1] <- mean(test_pred_1 != test_24hrs$y)
  
  #LDA
  mod <- lda(y ~., data = train_24hrs)
  #summary(mod_lr)
  test_pred_2 <- as.numeric(predict(mod, test_24hrs)$class) - 1
  compute_error[i, 2] <- mean(test_pred_2 != test_24hrs$y)
  
  
  #QDA
  mod <- qda(y ~., data = train_24hrs)
  test_pred_3 <- as.numeric(predict(mod, test_24hrs)$class) - 1
  compute_error[i, 3] <- mean(test_pred_3 != test_24hrs$y)
}

avged_3 <- apply(compute_error, 2, mean)
errorMat[1:3] <- as.numeric(avged_3)

#do this on the 50th iteration of sampled data

#KNN
tune_knn <- tune.knn(train_24hrs[,1:14], as.factor(train_24hrs[,15]), k = 1:25)
test_pred_4 <- knn(train_24hrs[,1:14], test_24hrs[,1:14], train_24hrs[,15], k=tune_knn$best.parameters)
errorMat[4] <- mean(test_pred_4 != test_24hrs$y)



#Basic Decision 
fit = tree(y ~., data = train_24hrs)
cv.fit = cv.tree(fit)
plot(cv.fit$size, cv.fit$dev, type='b')
#best = something in plot
prune.fit = prune.tree(fit, best = 5)
test_pred_5 <- as.numeric(predict(prune.fit, newdata = test_24hrs)[, 2] > 0.5)
errorMat[5] <- mean(test_pred_5 != test_24hrs$y)

#RF if the others are quick
fitRF = randomForest(y ~., data = train_24hrs, mtry = 5)
test_pred_6 = (as.numeric(predict(fitRF, newdata = test_24hrs)) - 1)
errorMat[6] <- mean(test_pred_6 != test_24hrs$y)

#Boosting
train_24hrs$y <- (as.numeric(train_24hrs$y) - 1)
test_24hrs$y <- (as.numeric(test_24hrs$y) - 1)



fitGBM = gbm(y ~ ., data=train_24hrs, distribution="adaboost",
             n.trees=5000, interaction.depth=3, shrinkage=0.1)

test_pred_7 = as.numeric(predict(fitGBM, newdata=test_24hrs, n.trees=5000, type = 'response') > 0.5)
errorMat[7] <- mean(test_pred_7 != test_24hrs$y)


#Combo of KNN and RandomForest (for the False Negative Purpose)
total_v1 <- ((as.numeric(test_pred_4) - 1) + test_pred_6)
test_pred_8 <- as.numeric(total_v1 >= 1)
errorMat[8] <- mean(test_pred_8 != test_24hrs$y)


#Combo of All Models (if one says yes, all say yes)
total_v2 <- as.numeric(test_pred_1) + test_pred_2 +
  test_pred_3 +
  (as.numeric(test_pred_4) - 1) +
  test_pred_5 +
  test_pred_6 +
  test_pred_7
test_pred_9 <- as.numeric(total_v2 >= 1)
errorMat[9] <- mean(test_pred_9 != test_24hrs$y)






data.frame("Logistic" = errorMat[1], "LDA" = errorMat[2], "QDA" = errorMat[3], "K" = as.numeric(tune_knn$best.parameters),
           "KNN" = errorMat[4], "Basic Dec. Tree" = errorMat[5], "RandomForest" = errorMat[6], "GBM" = errorMat[7],
           "Ensemble 1" = errorMat[8], "Ensemble 2" = errorMat[9])

fnr <- matrix(NA, 1, 9)
total_raining <- sum(test_24hrs$y == 1)
fnr[1] <- sum(test_pred_1 == 0 & test_24hrs$y == 1) / total_raining
fnr[2] <- sum(test_pred_2 == 0 & test_24hrs$y == 1) / total_raining
fnr[3] <- sum(test_pred_3 == 0 & test_24hrs$y == 1) / total_raining
fnr[4] <- sum(test_pred_4 == 0 & test_24hrs$y == 1) / total_raining
fnr[5] <- sum(test_pred_5 == 0 & test_24hrs$y == 1) / total_raining
fnr[6] <- sum(test_pred_6 == 0 & test_24hrs$y == 1) / total_raining
fnr[7] <- sum(test_pred_7 == 0 & test_24hrs$y == 1) / total_raining
fnr[8] <- sum(test_pred_8 == 0 & test_24hrs$y == 1) / total_raining
fnr[9] <- sum(test_pred_9 == 0 & test_24hrs$y == 1) / total_raining

data.frame("Logistic" = fnr[1], "LDA" = fnr[2], "QDA" = fnr[3],
           "KNN" = fnr[4], "Basic Dec. Tree" = fnr[5], "RandomForest" = fnr[6], "GBM" = fnr[7],
           "Ensemble 1" = fnr[8], "Ensemble 2" = fnr[9])

total_raining / dim(test_24hrs)[1]



#for 48 hrs

prop = c(.8, .2)

#first split into train test


train_size <- floor(prop[1] * nrow(x_48hrs_norm))
test_size <- nrow(x_48hrs_norm) - train_size

num_rows_48hrs <- nrow(x_48hrs_norm)

#do everything on the 48hr data for the moment, do it for three others in a second/



errorMat <- matrix(NA, 1, 9)
compute_error <- matrix(NA, 50, 3)

for (i in (1:50))
{
  indicesTraining    <- sort(sample(seq_len(num_rows_48hrs), size=train_size))
  
  indicesTest        <- setdiff(seq_len(num_rows_48hrs), indicesTraining)
  
  train_48hrs <- hrs_48[indicesTraining, ]
  test_48hrs <- hrs_48[indicesTest, ]
  
  train_48hrs$y <- as.factor(train_48hrs$y)
  test_48hrs$y <- as.factor(test_48hrs$y)
  
  #Logistic Regression
  mod <- glm(y ~., data = train_48hrs, family = binomial)
  #summary(mod_lr)
  test_pred_1 <- 1 * (predict(mod, test_48hrs, type = "response") > 0.5)
  compute_error[i, 1] <- mean(test_pred_1 != test_48hrs$y)
  
  #LDA
  mod <- lda(y ~., data = train_48hrs)
  #summary(mod_lr)
  test_pred_2 <- as.numeric(predict(mod, test_48hrs)$class) - 1
  compute_error[i, 2] <- mean(test_pred_2 != test_48hrs$y)
  
  
  #QDA
  mod <- qda(y ~., data = train_48hrs)
  test_pred_3 <- as.numeric(predict(mod, test_48hrs)$class) - 1
  compute_error[i, 3] <- mean(test_pred_3 != test_48hrs$y)
}

avged_3 <- apply(compute_error, 2, mean)
errorMat[1:3] <- as.numeric(avged_3)

#do this on the 50th iteration of sampled data

#KNN
tune_knn <- tune.knn(train_48hrs[,1:14], as.factor(train_48hrs[,15]), k = 1:25)
test_pred_4 <- knn(train_48hrs[,1:14], test_48hrs[,1:14], train_48hrs[,15], k=tune_knn$best.parameters)
errorMat[4] <- mean(test_pred_4 != test_48hrs$y)



#Basic Decision 
fit = tree(y ~., data = train_48hrs)
cv.fit = cv.tree(fit)
plot(cv.fit$size, cv.fit$dev, type='b')
#best = something in plot
prune.fit = prune.tree(fit, best = 3)
test_pred_5 <- as.numeric(predict(prune.fit, newdata = test_48hrs)[, 2] > 0.5)
errorMat[5] <- mean(test_pred_5 != test_48hrs$y)

#RF if the others are quick
fitRF = randomForest(y ~., data = train_48hrs, mtry = 5)
test_pred_6 = (as.numeric(predict(fitRF, newdata = test_48hrs)) - 1)
errorMat[6] <- mean(test_pred_6 != test_48hrs$y)

#Boosting
train_48hrs$y <- (as.numeric(train_48hrs$y) - 1)
test_48hrs$y <- (as.numeric(test_48hrs$y) - 1)



fitGBM = gbm(y ~ ., data=train_48hrs, distribution="bernoulli",
             n.trees=5000, interaction.depth=1, shrinkage=0.001)

test_pred_7 = as.numeric(predict(fitGBM, newdata=test_48hrs, n.trees=5000, type = 'response') > 0.5)
errorMat[7] <- mean(test_pred_7 != test_48hrs$y)


#Combo of KNN and RandomForest (for the False Negative Purpose)
total_v1 <- ((as.numeric(test_pred_4) - 1) + test_pred_6)
test_pred_8 <- as.numeric(total_v1 >= 1)
errorMat[8] <- mean(test_pred_8 != test_48hrs$y)


#Combo of All Models (if one says yes, all say yes)
total_v2 <- as.numeric(test_pred_1) + test_pred_2 +
  test_pred_3 +
  (as.numeric(test_pred_4) - 1) +
  test_pred_5 +
  test_pred_6 +
  test_pred_7
test_pred_9 <- as.numeric(total_v2 >= 1)
errorMat[9] <- mean(test_pred_9 != test_48hrs$y)






data.frame("Logistic" = errorMat[1], "LDA" = errorMat[2], "QDA" = errorMat[3], "K" = as.numeric(tune_knn$best.parameters),
           "KNN" = errorMat[4], "Basic Dec. Tree" = errorMat[5], "RandomForest" = errorMat[6], "GBM" = errorMat[7],
           "Ensemble 1" = errorMat[8], "Ensemble 2" = errorMat[9])

fnr <- matrix(NA, 1, 9)
total_raining <- sum(test_48hrs$y == 1)
fnr[1] <- sum(test_pred_1 == 0 & test_48hrs$y == 1) / total_raining
fnr[2] <- sum(test_pred_2 == 0 & test_48hrs$y == 1) / total_raining
fnr[3] <- sum(test_pred_3 == 0 & test_48hrs$y == 1) / total_raining
fnr[4] <- sum(test_pred_4 == 0 & test_48hrs$y == 1) / total_raining
fnr[5] <- sum(test_pred_5 == 0 & test_48hrs$y == 1) / total_raining
fnr[6] <- sum(test_pred_6 == 0 & test_48hrs$y == 1) / total_raining
fnr[7] <- sum(test_pred_7 == 0 & test_48hrs$y == 1) / total_raining
fnr[8] <- sum(test_pred_8 == 0 & test_48hrs$y == 1) / total_raining
fnr[9] <- sum(test_pred_9 == 0 & test_48hrs$y == 1) / total_raining

data.frame("Logistic" = fnr[1], "LDA" = fnr[2], "QDA" = fnr[3],
           "KNN" = fnr[4], "Basic Dec. Tree" = fnr[5], "RandomForest" = fnr[6], "GBM" = fnr[7],
           "Ensemble 1" = fnr[8], "Ensemble 2" = fnr[9])


total_raining / dim(test_48hrs)[1]


total_raining



