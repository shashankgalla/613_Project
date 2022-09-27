install.packages("readxl") #Library installation to read excel files
library("readxl") #Loading the library to read excel files
my.frame = read_excel("Snails.xlsx") #reading excel files and loading into data frame
head(my.frame) 
View (my.frame)
names (my.frame)
dim (my.frame)
sum (is.na(my.frame$Rings))


#Fitting a basic Linear Regression Model and checking the variability.

fit<- lm(Rings ~ Type+LongestShell+Diameter+Height+WholeWeight+ShuckedWeight+VisceraWeight+ShellWeight, my.frame)
summary(fit) 
#R2 value is 54.31

#Subset selection method:
library (leaps)
regfit.full <- regsubsets (Rings~.,my.frame, nvmax = 9)
summ=summary (regfit.full)
summ

#Plotting graphs of various parameters
par (mfrow = c(2, 2))
plot (summ$rss , xlab = " Number of Variables ",
        ylab = " RSS ", type = "l")
points (7, summ$rss[7], col = " red ", cex = 2,
        pch = 20)
plot (summ$adjr2 , xlab = " Number of Variables ",
        ylab = " Adjusted RSq ", type = "l")
which.max (summ$adjr2)
points (7, summ$adjr2[7], col = " red ", cex = 2,
        pch = 20)

plot (summ$cp, xlab = " Number of Variables ",
        ylab = "Cp", type = "l")
which.min (summ$cp)
points (7, summ$cp[7], col = " red ", cex = 2,
          pch = 20)

which.min (summ$bic)
plot (summ$bic , xlab = " Number of Variables ",
        ylab = " BIC ", type = "l")
points (7, summ$bic[6], col = " red ", cex = 2,
        pch = 20)

par (mfrow = c(2, 2))

#All the parameters indicate that having 7 predictors is best

#printing co-efficients of the model
coef (regfit.full , 7)


#Forward selection method
regfit.fwd <- regsubsets (Rings~.,my.frame, nvmax = 9, method = "forward")
summary (regfit.fwd)

#Backward selection method
regfit.bwd <- regsubsets (Rings~.,my.frame, nvmax = 9, method = "backward")
summary (regfit.bwd)

coef (regfit.full , 7)
coef (regfit.fwd , 7)
coef (regfit.bwd , 7)

#Performing cross validation (Validation set approach) on the best subset method
set.seed (829)
train <- sample (c(TRUE , FALSE), nrow (my.frame),
                   replace = TRUE)
test <- (!train)

regfit.best <- regsubsets (Rings~.,
                           data = my.frame[train , ], nvmax = 9)

test.mat <- model.matrix (Rings~., data = my.frame[test , ])



val.errors <- rep (NA, 10)
for (i in 1:9) 
  {
  coefi <- coef (regfit.best , id = i)
  pred <- test.mat[, names (coefi)] %*% coefi
  val.errors[i] <- mean ((my.frame$Rings[test] - pred)^2)
  }
val.errors
which.min (val.errors)
coef (regfit.best , which.min (val.errors))


predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix (form , newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}

#Cross Validation also indicates that best model contains 7 predictors

regfit.best <- regsubsets (Rings~., data = my.frame ,
                             nvmax = 9)
coef (regfit.best , 7)
#Printing co-efficient of the predictors

##Performing cross validation (K-Fold cross validation) on the best subset method
k <- 10
n <- nrow (my.frame)
set.seed (1)
folds <- sample ( rep (1:k, length = n))
cv.errors <- matrix (NA, k, 9,
                       dimnames = list (NULL , paste (1:9)))


for (j in 1:k) {
  best.fit <- regsubsets (Rings~.,
                            data = my.frame[folds != j, ],
                            nvmax = 9)
  for (i in 1:9) {
    pred <- predict (best.fit , my.frame[folds == j, ], id = i)
    cv.errors[j, i] <-
    mean ((my.frame$Rings[folds == j] - pred)^2)
    }
}

mean.cv.errors <- apply (cv.errors , 2, mean)
which.min (mean.cv.errors) #Same 7 predictors for the best model.

par (mfrow = c(1, 1))
plot (mean.cv.errors , type = "b")

reg.best <- regsubsets (Rings~., data = my.frame ,
                        nvmax = 9)
coef (reg.best , 7)



######################################################################
#Fitting model with the subset selected predictors
fit<- lm(Rings ~ Type+Diameter+Height+WholeWeight+ShuckedWeight+VisceraWeight+ShellWeight, my.frame)
summary(fit)

#Plotting graphs to find the relationship between predicvtor and response

plot(my.frame$Diameter,my.frame$Rings, col="red")
plot(my.frame$Height,my.frame$Rings, col="red")
plot(my.frame$WholeWeight,my.frame$Rings, col="red")
plot(my.frame$ShuckedWeight,my.frame$Rings, col="red")
plot(my.frame$VisceraWeight,my.frame$Rings, col="red")
plot(my.frame$TypeI,my.frame$Rings, col="red")

#####One Hot Encoding!!!! ###This is performed to seperate the categorical v ariables
#install.packages("caret")
library(caret)

dummy <- dummyVars(" ~ .", data=my.frame)
my.frame <- data.frame(predict(dummy, newdata = my.frame)) 
head(my.frame)

fit<- lm(log(Rings) ~ (TypeI)+ exp(log(Diameter))+(sqrt(Height))+log(WholeWeight)+log(ShuckedWeight)+log((VisceraWeight))+log(ShellWeight), my.frame)
summary(fit)
par(mfrow=c(2,2))
plot(fit)


logg= log(my.frame$Rings)
LMFitPred <- predict(fit, my.frame)
RSS <- sum((LMFitPred - logg)^2)
TSS <- sum((mean(logg)-logg)^2)
1-RSS/TSS



library(Metrics)
rmse(my.frame$Rings, LMFitPred)

########################################################################
#PCA:

install.packages("pls")
library (pls)
set.seed (2)
pcr.fit <- pcr (Rings~., data = my.frame , scale = TRUE ,validation = "CV")
summary (pcr.fit)
validationplot (pcr.fit , val.type = "MSEP")
validationplot (pcr.fit , val.type = "R2")
validationplot (pcr.fit , val.type = "RMSEP")


set.seed (1)
train <- sample (1: nrow (my.frame), nrow (my.frame) / 2)
pcr.fit <- pcr (Rings~., data = my.frame , subset = train , scale = TRUE , validation = "CV")
validationplot (pcr.fit , val.type = "MSEP")
summary (pcr.fit)

pcr.pred <- predict (pcr.fit , x[test , ], ncomp = 3)
mean ((pcr.pred - y.test)^2)

val.errors <- rep (NA, 10)
for (i in 1:10) 
{
  pcr.pred <- predict (pcr.fit , x[test , ], ncomp = i)
  val.errors[i] <- mean ((pcr.pred - y.test)^2)
}
val.errors
which.min (val.errors)

pcr.fit <- pcr (y~x, scale = TRUE , ncomp = 10)
summary (pcr.fit)


################################################################################
########Cross Validation of the Model:

##Validation Set Method:
set.seed (829)
trainingindex<- sample(1:nrow(my.frame), nrow(my.frame)/2,replace=F)
###we will use half of our data to train

TrainingData<- my.frame[trainingindex,]
TestingData <- my.frame[-trainingindex,]


logtest= log(TestingData$Rings)

fit<- lm(log(Rings) ~ (TypeI)+ exp(log(Diameter))+(sqrt(Height))+log(WholeWeight)+log(ShuckedWeight)+log((VisceraWeight))+log(ShellWeight), TrainingData)
lmPred <- predict(fit, TestingData)
RSS <- sum((lmPred - logtest)^2)
TSS <- sum((mean(logtest)-logtest)^2)
1-RSS/TSS

# computing model performance metrics
data.frame(
            RMSE = RMSE(lmPred, logtest),
            MAE = MAE(lmPred, logtest))

#####################################Leave One Out Cross Validation:

# R program to implement
# Leave one out cross validation

# defining training control
# as Leave One Out Cross Validation
train_control <- trainControl(method = "LOOCV")

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(log(Rings) ~ (TypeI)+ exp(log(Diameter))+(sqrt(Height))+log(WholeWeight)+log(ShuckedWeight)+log((VisceraWeight))+log(ShellWeight), data = my.frame,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)
###################################K-Fold Cross Validation:

# R program to implement
# K-fold cross-validation

# setting seed to generate a
# reproducible random sampling
set.seed(829)

# defining training control
# as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(log(Rings) ~ (TypeI)+ exp(log(Diameter))+(sqrt(Height))+log(WholeWeight)+log(ShuckedWeight)+log((VisceraWeight))+log(ShellWeight), data = my.frame,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)

##After performing K-Fold Cross validation and appropriate transformations the R2 was found to be: 0.6434824 for the Linear Model.

######Final Model.
fit<- lm(log(Rings) ~ (TypeI)+ exp(log(Diameter))+(sqrt(Height))+log(WholeWeight)+log(ShuckedWeight)+log((VisceraWeight))+log(ShellWeight), my.frame)
summary(fit)

