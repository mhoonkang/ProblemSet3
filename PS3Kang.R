## Problem Set 3

## Sampling distributions and p-values

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
beta.hat <- lapply(1:1000, function(i){ 
                      summary(lm(Y[,i]~data[,1,i]+data[,2,i]+data[,3,i]
                      +data[,4,i]+data[,5,i]))$coefficient[,1]
                      }
                  )
beta.hat <- unlist(beta.hat)
# Since each list in beta.hat has 6 coefficients, we need to make a 6 by 1000 matrix.
# After doing so, we can obtain a 1000 by 6 matrix by transposing. 
beta.hat <- t(array(beta.hat, dim=c(6,1000)))

## 4. Creating a density plot for each of the 6 coefficients
# setting to creat plots in a 2 by 3 matrix
par(mfrow=c(2,3))
colnames(beta.hat) <- paste("Beta_hat_",c(0:5),sep="")
lapply(colnames(beta.hat),function(x) {
          plot(density(beta.hat[,x]),main=paste("Density plot for",x),type="l")
          }
      )
# Each distribution represents the distribution of estimated coefficients 
# from 1000 samples for 5 covariates and the intercept. As the central limit 
# theorem implies, each distrinbution for 5 covariates approximates the normal 
# distribution whose mean is given by the vector Beta. 

## 5. Collecting t-statistics for 1000 regressinos for all six coefficients. 
# Making a 1000 by 6 t-statistics matrix. This process is the same as in 1,2,3 above.
set.seed(2005)
data <- array(rnorm(100000,0,1), dim=c(20,5,1000))

calcul.y <- function(x){
  Beta <- matrix(c(1,2,0,4,0), ncol=1)
  apply(x,3, function(x) x %*% Beta+rnorm(nrow(x)))
}
Y <- calcul.y(data)

t.beta.hat <- lapply(1:1000, function(i) {
                        summary(lm(Y[,i]~data[,1,i]+data[,2,i]+data[,3,i]
                        +data[,4,i]+data[,5,i]))$coefficient[,3]
                        }
                    )
t.beta.hat <- unlist(t.beta.hat)
t.beta.hat <- t(array(t.beta.hat, dim=c(6,1000)))

## 6. Clculating how many t-statistics are statistically "significant" (p<=0.5)
critical.t <- abs(qt(0.975,14))
signi <- apply(t.beta.hat, 2, function(x) sum(abs(x)>=critical.t))
names(signi) <- paste("Beta_hat_",c(0:5),sep="")
signi

# We have 5 covariates. So the degree of freedom is 14. 

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

# In consequence, we know how many t-statistics are statistically "significant"
# (p <= 0.05) for each variables
# beta_hat_0: 46    beta_hat_1: 907    beta_hat_2: 1000
# beta_hat_3: 48    beta_hat_4: 1000   beta_hat_5: 47

## 7. Re-runing that code in parallel, and estimaing how much time is saved. 

system.time(signif.t <- apply(t(array(unlist(lapply(1:1000, function(i) {
  (summary(lm(Y[,i]~data[,1,i]+data[,2,i]+data[,3,i]
              +data[,4,i]+data[,5,i]))$coefficient[,4]<=0.05)}
)), dim=c(6,1000))), 2, sum))
            

registerDoMC(cores=8)
system.time(signif.t <- apply(t(array(unlist(lapply(1:1000, function(i) {
  (summary(lm(Y[,i]~data[,1,i]+data[,2,i]+data[,3,i]
              +data[,4,i]+data[,5,i]))$coefficient[,4]<=0.05)}
)), dim=c(6,1000))), 2, sum))
