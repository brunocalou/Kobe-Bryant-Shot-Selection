setwd("Github Projects/Kobe-Bryant-Shot-Selection/")
source('R/utils/devdependencies.R');
source('R/pre-processing/preprocessing.R');
source('R/pre-processing/datavis.R');
install_dependencies();
library(ggplot2)
library(GGally)
library(corrplot)
library(caret)
library(nnet)
require(grid)

# Get the csv file from the zip file
raw_data <- read.table(unz("data/rawdata_csv.zip", "data.csv"), header=T, quote="\"", sep=",");

# Print the head of the dataframe to understand the data structure
head(raw_data)

dataframe <- na.omit(raw_data)
submission_sample <- raw_data[is.na(raw_data$shot_made_flag),]

#Remove unnecessary columns. Note that lat and lon are correlated with loc_x and loc_y
data_cl <- subset(dataframe, select = -c(shot_id, team_id, lat, lon, game_id, game_event_id, team_name))

#Time
data_cl['seconds_to_end'] = data_cl['minutes_remaining'] * 60 + data_cl['seconds_remaining']
data_cl <- subset(data_cl, select = -c(minutes_remaining, seconds_remaining))

#Game date
dates = as.Date(data_cl$game_date, format="%Y-%m-%d")

data_cl['game_year'] = as.numeric(strftime(dates, "%Y"))
data_cl['game_month'] = as.numeric(strftime(dates, "%m"))
data_cl <- subset(data_cl, select = -c(game_date))

#Away home
data_cl['away_home'] = as.integer(grepl("vs.", data_cl$matchup))
data_cl <- subset(data_cl, select = -c(matchup))

#'loc_x', 'loc_y', 'lat', 'lon', 'shot_distance'
num_columns <- c('loc_x', 'loc_y', 'lat', 'lon', 'shot_distance', 'seconds_to_end');
preprocessParams <- preProcess(data_cl[, (colnames(data_cl) %in% num_columns)], method=c("center", "scale"))
data_cl <- predict(preprocessParams, data_cl);

# Period
data_cl$period[data_cl$period > 4] <- 'Others'
print(paste("Elements on data_cl$period: ", paste(unique(data_cl$period), collapse = ', ')))

#Get the categorical columns

cols_to_be_dummy = list(
  'action_type', 'combined_shot_type', 'season', 'shot_type',
  'shot_zone_area', 'shot_zone_basic', 'shot_zone_range', 'opponent',
  'period'
)

for (col in cols_to_be_dummy) {
  dummy_cols = unique(getElement(data_cl, col))
  concat_dummy_cols = paste(col ,"_", dummy_cols, sep = "")
  
  data_cl[col] <- as.character(getElement(data_cl, col))
  
  for (i in 1:length(dummy_cols)) {
    dummy_col = dummy_cols[i]
    concat_dummy_col = concat_dummy_cols[i]
    
    #Add the column
    data_cl[concat_dummy_col] <- 0
    
    #Fill with 1's
    b = getElement(data_cl, concat_dummy_col)
    binary = getElement(data_cl, col) == dummy_col
    b[binary] <- 1
    data_cl[concat_dummy_col] <- b
  }
}

#Remove the categorical columns
`%ni%` <- Negate(`%in%`)
data_cl <- subset(data_cl, select = names(data_cl) %ni% cols_to_be_dummy)

############### SUBSET ##################
data_cl <- data_cl[1:1000,]

set.seed(300)

data_cl$shot_made_flag[data_cl$shot_made_flag == 1] <- "Yes"
data_cl$shot_made_flag[data_cl$shot_made_flag == 0] <- "No"
data_cl$shot_made_flag <- factor(data_cl$shot_made_flag)

# Spliting data as training and test set. Using createDataPartition() function from caret
idxTrain <- createDataPartition(y = data_cl$shot_made_flag, p = 0.8, list = FALSE)
training <- data_cl[idxTrain,]
testing <- data_cl[-idxTrain,]

set.seed(400)
ctrl <- trainControl(method="repeatedcv", number = 10, repeats = 1, verboseIter = TRUE)

##################### KNN ############################
start.time <- Sys.time()
knnFit <- train(shot_made_flag ~ ., data = training, method = "knn", 
                trControl = ctrl, tuneLength = 10)

#Output of kNN fit
knnFit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Plot Accuracy of the cross validation
plot(knnFit)

# Now predict on test set
knnPredict <- predict(knnFit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$shot_made_flag, positive = "Yes")


##################### LR ############################
start.time <- Sys.time()
LRfit <- train(shot_made_flag ~ ., data = training, method = "glm", family = binomial, trControl = ctrl)

#Output of LR fit
LRfit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
LRPredict <- predict(LRfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(LRPredict, testing$shot_made_flag, positive = "Yes")

##################### Naive Bayes ############################
start.time <- Sys.time()
NBfit <- train(shot_made_flag ~ ., data = training, method = "nb", trControl = ctrl)

#Output of NB fit
NBfit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
NBPredict <- predict(NBfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(NBPredict, testing$shot_made_flag, positive = "Yes")

##################### SVM ############################
start.time <- Sys.time()
svmctrl <- trainControl(method="repeatedcv", number = 10, repeats = 1, verboseIter = TRUE, classProbs = TRUE)
SVMfit <- train(shot_made_flag ~ ., data = training, method = "svmRadial", tuneLength = 10, metric="ROC", trControl = svmctrl)

#Output of NB fit
SVMfit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
SVMPredict <- predict(SVMfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(SVMPredict, testing$shot_made_flag, positive = "Yes")

##################### RF ############################
start.time <- Sys.time()
rfctrl <- trainControl(method="repeatedcv", number = 10, repeats = 1, verboseIter = TRUE, classProbs = TRUE)
RFfit <- train(shot_made_flag ~ ., data = training, method = "rf", tuneLength = 10, metric="ROC", trControl = rfctrl)

#Output of NB fit
RFfit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
RFPredict <- predict(RFfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(RFPredict, testing$shot_made_flag, positive = "Yes")

##################### ANN ############################
start.time <- Sys.time()
my.grid <- expand.grid(.decay = c(2, 1), .size = c(1,2,3,4,5,6))
NNETfit <- train(shot_made_flag ~ ., data = training, method = "nnet", maxit = 10000, tuneGrid = my.grid, trace = F, trControl = rfctrl)

#Output of NB fit
NNETfit

# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
NNETPredict <- predict(NNETfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(NNETPredict, testing$shot_made_flag, positive = "Yes")

##################### MLP ############################
start.time <- Sys.time()

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:54)
cctrl1 <- trainControl(method = "cv", number = 10, verboseIter = TRUE)

grid <- expand.grid(decay = c(0, .01), layer1 = 1:3, layer2 = 1:3, layer3 = 1:3)

NEURALNETfit <- caret:::train(data_cl[, !(colnames(data_cl) %in% c("shot_made_flag"))],
                                     data_cl$shot_made_flag,
                                     method = "mlpWeightDecayML",
                                     trControl = cctrl1,
                                     tuneGrid = grid)


# Time elapsed
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Now predict on test set
NEURALNETPredict <- predict(NEURALNETfit, newdata = testing)
# Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(NEURALNETPredict, testing$shot_made_flag, positive = "Yes")
