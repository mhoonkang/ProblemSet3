#### Problem Set 3

### Sampling distributions and p-values

## 1. Making a array with dim=c(20,5,1000) of random data.
set.seed(2005)
data <- array(rnorm(100000,0,1), dim=c(20,5,1000))

## 2. Making a function to create Y values.
calcul.y <- function(x){
  Beta <- matrix(c(1,2,0,4,0), ncol=1)
  apply(x,3, function(x) x %*% Beta+rnorm(nrow(x)))
}
Y <- calcul.y(data)

## 3. Running 1,000 regressions
beta.hat <- sapply(1:1000, function(i){summary(lm(Y[,i]~data[, ,i]))$coefficient[,1]})
# Taking 6 coefficients from each summary table of the 1000 regressions. 
# After doing so, we can obtain a 1000 by 6 matrix by transposing. 
beta.hat <- t(beta.hat)

## 4. Creating a density plot for each of the 6 coefficients
# setting to creat plots in a 2 by 3 matrix
par(mfrow=c(2,3))
colnames(beta.hat) <- paste("Beta_hat_",c(0:5),sep="")
sapply(colnames(beta.hat),function(x) {plot(density(beta.hat[,x]),main=paste("Density plot for",x),type="l")})

# Each distribution represents the distribution of estimated coefficients 
# from 1000 samples for 5 covariates and the intercept. As the central limit 
# theorem implies, each distrinbution for 5 covariates approximates the normal 
# distribution whose mean is given by the vector of 'Beta'. 

## 5. Collecting t-statistics for 1000 regressinos for all six coefficients. 
# Making a 1000 by 6 t-statistics matrix. This process is the same as in 1,2,3 above
# except that 6 t-statistics are taken from each summary table of the 1000 regressions. 

set.seed(2005)
data <- array(rnorm(100000,0,1), dim=c(20,5,1000))

calcul.y <- function(x){
  Beta <- matrix(c(1,2,0,4,0), ncol=1)
  apply(x,3, function(x) x %*% Beta+rnorm(nrow(x)))
}
Y <- calcul.y(data)
t.beta.hat <- sapply(1:1000, function(i){summary(lm(Y[,i]~data[, ,i]))$coefficient[,3]})
t.beta.hat <- t(t.beta.hat)

## 6. Clculating how many t-statistics are statistically "significant" (p<=0.5)
# When we say a t-statistics is Statistically "significant" at p<=0.05, it means 
# that the t-statistics lies either less or equal to 0.025 quantile or greater or
# equal to 0.0975 qunatile in a cumulative t-distribution.

# Using qt() function, we can obtain the value of a t-distributed random variable 
# whose quantile is 0.975 in a cumulative t-distribution with the degree of 
# freedom of 14. Because of symmetry of t-distribution, if we change the sign
# of the value, the outcome is exactly the value of a t-distributed random variable
# whose quantile is 0.025 in a cumulative t-distribution with the degree of freedom 
# 14. So we know that the absolute values of the two values are exactly same. 

# So, we can say that the absolute value of statistically "significant" t-statistics 
# are greater or equal to the absolute value of qt(0.975,14).  
critical.t <- abs(qt(0.975,14))
signi.1 <- apply(t.beta.hat, 2, function(x) sum(abs(x)>=critical.t))
names(signi.1) <- paste("Beta_hat_",c(0:5),sep="")
signi.1

# Or simply we can take p-value from each summary table of the 1000 regressions as follows.
p.beta.hat <- sapply(1:1000, function(i){summary(lm(Y[,i]~data[, ,i]))$coefficient[,4]})
p.beta.hat <- t(p.beta.hat)
signi.2 <- apply(p.beta.hat, 2, function(x) sum(x<=0.05))
names(signi.2) <- paste("Beta_hat_",c(0:5),sep="")
signi.2

# In consequence, we know how many t-statistics are statistically "significant"
# (p <= 0.05) for each variables
# beta_hat_0: 46    beta_hat_1: 907    beta_hat_2: 1000
# beta_hat_3: 48    beta_hat_4: 1000   beta_hat_5: 47

## 7. Re-runing that code in parallel, and estimaing how much time is saved. 


###. Calculating fit statistics
## 1. Model building and making predictions. 

# Random subsetting.
set.seed(2005)
incumbents <- read.table("incumbents_0.txt", header=TRUE)
i <- sample(1:6687, 3343)
training <-  incumbents[i,] # training set
test <- incumbents[-i,] # test set

# model 1. (Linear Square Regression Model)
LSmodel <- lm(voteshare ~ chalspend+incspend+presvote+inparty+incparty+seniority+midterm+chalquality+south+population+urban+age65+milpop+unemployed, data=training)
# predictions
LS.model <- predict(LSmodel, test)

# model 2. (Bayesian Linear Regression Model)
library(arm)
BLmodel <- bayesglm(voteshare ~ chalspend+incspend+presvote+inparty+incparty+seniority+midterm+chalquality+south+population+urban+age65+milpop+unemployed, data=training) 
# predictions
Bayse.lm <- predict(BLmodel, test)

# model 3. (Random Forest Regression Model)
library(randomForest)
RFmodel <- randomForest(voteshare ~ chalspend+incspend+presvote+inparty+incparty+seniority+midterm+chalquality+south+population+urban+age65+milpop+unemployed, na.action="na.omit", data=training) 
# predictions
Random.forest <- predict(RFmodel, test)


## 2. Writing a function.
y <- test$voteshare # vector of "true" observed outcomes
r <- c(NA, test$voteshare[-nrow(test)]) # naive forecasts vector, that is, the vector of the lagged observation
P <- cbind(LS.model, Bayse.lm, Random.forest) # a matrix of predictions.

# function

# This function has 3 arguments as the problem asked. 'y' is a vector of "true" observed
# toucomes, 'P' is a matrix of predictions, and 'r' is a vector of naive forecasts.

# Note that there can be missing values in the vector 'y', 'r' or the matrix 'P'.
# This will cause the problem. So, we need to drop observations that have a missing
# value in at least one of the three objects.
# In addition, we need to consider the possibility that each model would yield a missing value
# at different obsevations. For example, we have lots of observations with missing values
# in 'chalspend' and 'incspend'. If we compare a model which includes these variables as 
# a independent variables to a model which does not include these variables, then the observations
# with missing values will be different between the two models. The following codes
# aims to separately drop observations with missing values for each model.
# So, the following codes try to make a list comprised of k lists for each model. 
# For example, the list 'P' includes k lists of the values of predictions and naive forecast
# which do not have missing values in 'r','y' and 'P' for each k models. Similarly, the list 'y' 
# comprised of k lists, each list has 'true' observed outcomes which do not have missing values 
# in 'r','y', and 'P' for each model.

fit.stat <- function(y, P, r){  
  P <- cbind(P, r) 
  k <- ncol(P)
  Model.name <- colnames(P)[1:(k-1)]
  y <- lapply(1:(k-1), function(i) y[which(is.na(P[,i])==FALSE & is.na(P[,k])==FALSE)])  
  P <- lapply(1:(k-1), function(i) P[which(is.na(P[,i])==FALSE & is.na(P[,k])==FALSE), c(i,k)])
  P <- lapply(1:(k-1), function(i) P[[i]][which(is.na(y[[i]])==FALSE),])
  y <- lapply(y, na.omit)

  # the absolute error
  e <- lapply(1:(k-1), function(i) abs(P[[i]][,1]-y[[i]]))
  # the absolute percentage error
  a <- lapply(1:(k-1), function(i) e[[i]]/abs(y[[i]])*100)
  # the number of observations
  n <- lapply(P, nrow)
  #RMSE
  RMSE <- sapply(1:(k-1), function(i) sqrt(sum(e[[i]]^2)/n[[i]]))
  #MAD
  MAD <- sapply(e, median)
  #RMSLE
  RMSLE <- sapply(1:(k-1), function(i) sqrt(sum((log(P[[i]][,1]+1)-log(y[[i]]+1))^2)/n[[i]]))
  #MAPE
  MAPE <- sapply(1:(k-1), function(i) sum(a[[i]])/n[[i]])
  #MEAPE
  MEAPE <- sapply(a, median)
  #MRAE
  MRAE <- sapply(1:(k-1), function(i) median(e[[i]]/P[[i]][,2]))

  # return the output as a matrix
  output <- cbind(RMSE, MAD, RMSLE, MAPE, MEAPE, MRAE)
  rownames(output) <- Model.name 
  return(output)
}
fit.stat(y,P,r)

## 3. Testing function
y <- test$voteshare
r <- c(NA, test$voteshare[-nrow(test)])
P <- cbind(LS.model, Bayse.lm, Random.forest)

# The following code is basically same with the above function except that
# 1) NULL is set as the default for argument 'r'. If 'r' is not specified,
# then the function arbitarily make a vector of 1 as the replacement for 'r' 
# ,and will be excuted in the same way as the above function. But, this function 
# does not include 'MRAE' in the output when 'r' is NULL, 
# 2) 'm' is added as another arguement. The default for this argument is "ALL".
# If "ALL" is specified, then all fit statistics will be included in the output.
# (of course 'MRAE' will be excluded if 'r' is NULL.) The other possible values
# for this argument are "RMSE", "MAD" ,"RMSLE", "MAPE", "MEAPE", and "MRAE".
# We can include multiple statistics by using a vector, e.g., c("RMSE", "MAPE").
# If "ALL" and other values are specified at the same time, then 
# this function shows all fit statistics. 


fit.stat <- function(y, P, r=NULL, m="ALL"){
  if(is.null(r)) P <- cbind(P, rep(1,nrow(P))) else P <-cbind(P,r)
  k <- ncol(P)       
  Model.name <- colnames(P)[1:(k-1)]
  y <- lapply(1:(k-1), function(i) y[which(is.na(P[,i])==FALSE & is.na(P[,k])==FALSE)])  
  P <- lapply(1:(k-1), function(i) P[which(is.na(P[,i])==FALSE & is.na(P[,k])==FALSE), c(i,k)])
  P <- lapply(1:(k-1), function(i) P[[i]][which(is.na(y[[i]])==FALSE),])
  y <- lapply(y, na.omit)
  
  # the absolute error
  e <- lapply(1:(k-1), function(i) abs(P[[i]][,1]-y[[i]]))
  # the absolute percentage error
  a <- lapply(1:(k-1), function(i) e[[i]]/abs(y[[i]])*100)
  # the number of observations
  n <- lapply(P, nrow)
  #RMSE
  RMSE <- sapply(1:(k-1), function(i) sqrt(sum(e[[i]]^2)/n[[i]]))
  #MAD
  MAD <- sapply(e, median)
  #RMSLE
  RMSLE <- sapply(1:(k-1), function(i) sqrt(sum((log(P[[i]][,1]+1)-log(y[[i]]+1))^2)/n[[i]]))
  #MAPE
  MAPE <- sapply(1:(k-1), function(i) sum(a[[i]])/n[[i]])
  #MEAPE
  MEAPE <- sapply(a, median)
  #MRAE
  MRAE <- sapply(1:(k-1), function(i) median(e[[i]]/P[[i]][,2]))
  
  output <- cbind(RMSE, MAD, RMSLE, MAPE, MEAPE, MRAE)
  rownames(output) <- Model.name 
  # If we specifies something other than the correct names of fit statistics for'm',
  # then a error message will be printed out.
  if(FALSE %in% (m %in% c("ALL", "RMSE", "MAD", "RMSLE", "MAPE", "MEAPE", "MRAE"))) stop("choose correct statistics(s)")
  # If we specfies "MRAE" for 'm' when 'r' is NULL, then warning message will be printed
  # out and "MRAE" will be deleted from 'm'. 
  if(is.null(r) & TRUE %in% ("MRAE" %in% m)) {
    cat("\n Warning: cannot calculate MRAE without navie forecast 'r'. \n") 
    m <- m[m != "MRAE"]}
  # If we specifies "ALL" with others values for 'm', then other values for 'm'
  # will be deleted from 'm'.
  if("ALL" %in% m) m <- "ALL"
  # If we specfies "ALL" for 'm' when 'r' is NULL, then the function returns
  # all statistics except "MRAE".
  if(is.null(r) & "ALL" %in% m) return(output[,1:5,drop=FALSE])
  # If we specfies only "MRAE" when 'r' is NULL, then the function returns NULL.
  if(is.null(r) & length(m)==0) return(NULL)
  # If we specfies "ALL" for 'm' when 'r' is not NULL, then all the statistics
  # will be returned as a matrix.
  # If we specfies multiple values for 'm', then the function returns the
  # corresponding statistics as a matrix. Also, there is no possibility
  # that "MRAE" will be included in the output when 'r' is NULL because
  # "MRAE" will be included in the output only when we specifies "NO" for 'm'.
  # However, if we specifies "NO" for 'm', then the function will not be excuted
  # and print a error message by the line 219.
  if(!is.null(r)) print.option <- c("RMSE", "MAD", "RMSLE", "MAPE", "MEAPE", "MRAE") %in% m else
         print.option <- c("RMSE", "MAD", "RMSLE", "MAPE", "MEAPE", "NO") %in% m
  if("ALL" %in% m) return(output)
   else  return(output[,print.option==TRUE,drop=FALSE])
}
# test
fit.stat(y,P)
fit.stat(y,P,m="NO")
fit.stat(y,P,m="MRAE")
fit.stat(y,P,m="RMSE")
fit.stat(y,P,m=c("RMSE","MAD"))
fit.stat(y,P,m=c("ALL","MRAE"))
fit.stat(y,P,r)
fit.stat(y,P,m="NO")
fit.stat(y,P,r,"MRAE")
fit.stat(y,P,r,"RMSE")
fit.stat(y,P,r,c("RMSE","MAD"))
fit.stat(y,P,r,c("ALL","MRAE"))

## 4.Evaluating the accuracy of the models.
# The results are as follows.
#
# When 'r' is specified
#
#                     RMSE       MAD      RMSLE     MAPE    MEAPE       MRAE
#LS.model      0.05782173 0.03899922 0.03506557 7.198618 5.917964 0.05931066
#Bayse.lm      0.05782149 0.03898568 0.03506539 7.198561 5.916097 0.05931469
#Random.forest 0.05383445 0.03583445 0.03274138 6.729241 5.452995 0.05388265
#
#When 'r' is not specified.
#
#                    RMSE        MAD      RMSLE     MAPE    MEAPE
#LS.model      0.05774535 0.03897873 0.03501992 7.189647 5.916549
#Bayse.lm      0.05774512 0.03896744 0.03501975 7.189593 5.915171
#Random.forest 0.05371429 0.03583340 0.03267305 6.714839 5.452924
#
# Note that the statistics are slightly different according to the exxistence
# of 'r'. This is because there are some missing values in 'r'. When 'r' is NULL,
# new observations is added, which would have been dropped when 'r' is not NULL.
# Of course, 'n' also increases. 
#
# As we can see in the above tables, the random forest model has the minimum 
# values over all statistics. The Linear square model and the Baysian model have 
# similar values over all statistics. From this results, we can say that
# the random forest model is the most accurate. The accuracy of both the linear 
# sqaure model and the baysian linear model is similar and less accurate than
# the random forest model. 