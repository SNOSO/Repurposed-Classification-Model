#load the data
library(datasets)
data("dhfr")

View(dhfr)

summary(dhfr)
summary(dhfr$Y)

# check to see if there is any missing data
sum(is.na(dhfr))

# skimr() expands on summary() by providing larger set of statistics
library(skimr)

#perform skim to display summary statistics
skim(dhfr)

# group data by bioactivity then perform skim
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

############################
# quick data visualization
#
# R base plot()
############################

# panel plots
#plot(dhfr)
#plot(dhfr, col = "red")

# scatter plot
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol)

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red") # makes red circles

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red", 
     xlab = "moe2D_zagreb", ylab ="moe2D_weinerPo")

#Histogram
hist(dhfr$moe2D_zagreb)
hist(dhfr$moe2D_zagreb, col = "red") # makes red bars
hist(dhfr$moe2D_zagreb, col = "blue",
     main = "Histogram of moe2D_zagreb", xlab = "moe2D_zagreb")

library(caret)

# feature plots
featurePlot(x = dhfr[,2:5],
            y = dhfr$Y,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation = "free")))


###############################

# assign a seed number to a fixed number so when you rerun same model you will get the same results each time code is ran
set.seed(100)

# data splitting - want to simulate a situation where we have a dataset which we use to train the model and want to see if model will be applicable to future data
# training and testing
# perfroms stratified random split of the data set
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Training set
TestingSet <- dhfr[-TrainingIndex,] # Test set

# compare scatter plot of the 80/20 data subsets

plot(TestingSet, main= "Testing Set")
plot(TrainingSet, main= "Training Set")

#####################
#SVM model (polynomial kernel)

# build training model- uses training set to build the model (80% subset)
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit, #omits any na data
               preProcess=c("scale","center"), #preprocess data according to mean centering
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1))

# build CV model - uses training model to predict class label in testing set (20% subset)
Model.cv <- train(Y ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv",number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <- varImp(Model)
plot(Importance, top = 25)
plot(Importance, col = "red")
