library(tidyverse)
library(pROC)
library(here)


#Function to calculate the AUC of a fitted caret object
# in the caret train control, you'll have to set the 
# savePredictions = 'final'
calc_auc <- function(caretobj) {
  rocobj <- roc(
    response = caretobj$pred$obs,
    predictor = caretobj$pred$Default
  )
  auc(rocobj)
}

# read in the consumer data
trainfile <- here::here('Assignments', 'ConsumerCredit', 'ConsumerCred-train.csv')
consumer <- read_csv(trainfile)

# change the column names
# many of the caret functions were complaining 
# because the column names were really wierd
colnames(consumer) <- c("id", "SeriousDlqin2yrs", 
                        "RevUtil", "age",
                        "Num30_60", "DebtRatio",
                        "MonthlyIncome", "NumOpenCred",
                        "Num90Plus", "NumRELoan",
                        "Num60_90", "NumDependents"
)

# Make the objective column into a factor
consumer <- consumer %>% 
  mutate(SeriousDlqin2yrs = factor(SeriousDlqin2yrs, 
                                        levels=0:1, 
                                        labels=c('NonDefault', 'Default')))

#remove the id column
consumer <- consumer %>% select(-id)

# split the data into training and testing, and 
# create the train/test split
set.seed(1234)
library(rsample)
split <- consumer %>% initial_split(strata='SeriousDlqin2yrs')

train <- training(split)
test <- testing(split)

# use this recipe for preprocessing
library(recipes)
rec_fact <- recipe(SeriousDlqin2yrs ~ ., data=train) %>%
  step_center(all_predictors()) %>% #center 
  step_scale(all_predictors()) %>% #scale
  step_meanimpute(all_predictors()) %>% #impute missing values
  step_upsample(SeriousDlqin2yrs) #address class imbalance

# Use cross validation
# classification requires classProbs=TRUE
library(caret)
ctrl <- trainControl(
  method='cv',
  savePredictions = 'final',
  verboseIter = TRUE, 
  classProbs = TRUE
)

# Create a simple logit model as baseline
set.seed(1234)
glm_mod <- train(
  rec_fact,
  data=train,
  method='glm',
  trControl=ctrl,
  metric='ROC')

# AUC is .79...not bad
calc_auc(glm_mod)

# I'd like to use xgboost, so need to prep the data 
# xgboost like a matrix as the input...I had trouble using
# the recipe as an input.
tr_rec <- prep(rec_fact, training = train, retain = TRUE)
tib_tr <-juice(tr_rec)
trainX <- as.matrix(tib_tr[,-11]) #col 11 is the default/no default flag
trainY <- pull(tib_tr[,11])

# use fewer cv splits
cctrl1 <- trainControl(method = "cv", number = 3, savePredictions = 'final',
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary, verbose=TRUE)

# I got this hyper parameter grid from 
# various blogs...could do more research into what
# these represent
xgbGrid <- expand.grid(nrounds = c(1, 10),
                       max_depth = c(1, 4),
                       eta = c(.1, .4),
                       gamma = 0,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = c(.8, 1))

# train the model using CARET
xgbmod <- train(x = trainX, y=trainY,
                        method = "xgbTree", 
                        trControl = cctrl1,
                        metric = "ROC", 
                        tuneGrid = xgbGrid)

calc_auc(xgbmod)
# AUC = .87...improvement over logit model

#check performance on test dataset
#apply model to test dataset
tib_test <-bake(tr_rec, new_data = test)
testX <- as.matrix(tib_test %>% select(-SeriousDlqin2yrs))
testY <- pull(tib_test %>% select(SeriousDlqin2yrs))

rocobj <- roc(
  response = testY,
  predictor = predict(xgbmod, testX, type='prob')$Default
  #predictor = predict(glm_mod, test, type='prob')$Default
  # If the first level is the event of interest:
)
auc(rocobj)
#AUC on test is .85

#Apply the xgboost model to the out of sample data to 
# create the submission file.
testfile <- here::here('Assignments', 'ConsumerCredit', 'ConsumerCred-test.csv')
newdat <- read_csv(testfile)
colnames(newdat) <- c("id",
                        "RevUtil", "age",
                        "Num30_60", "DebtRatio",
                        "MonthlyIncome", "NumOpenCred",
                        "Num90Plus", "NumRELoan",
                        "Num60_90", "NumDependents"
)

newids <- newdat$id
newdat <- newdat %>% select(-id)

newproc <-bake(tr_rec, new_data = newdat)
newX <- as.matrix(newproc %>% select(-SeriousDlqin2yrs))
output <- tibble(id = newids, 
                 probability = predict(xgbmod, newX, type='prob')$Default)
write_csv(output, here::here('Assignments', 'ConsumerCredit', 'MDMSubmission.csv'))
