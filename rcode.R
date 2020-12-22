#load the datasets
load("/cloud/project/NHANES2.Rdata")
load("/cloud/project/dataex4(1).Rdata")
load("/cloud/project/dataex2(1).Rdata")
#download packages and open them 
library(mice)
library(JointAI)

############### Question 1.a) ###############
#get the dataset nhanes from mice package and inspect the data
dataex1 <- nhanes
summary(dataex1)

#check for complete/incomplete cases 
md_pattern(dataex1, pattern = FALSE, color = c('#34111b', '#e30f41'))
#from this we can see that we have 13 complete cases, lets investigate further
nhanes_comp <- complete.cases(dataex1)
sum(nhanes_comp)
#or through the mice package
nrow(cc(dataex1))
# This also show 13 complete cases, 
# so (25-13)=12 -> 12/25= 0.48. 48% are incomplete cases.  

############### Question 1.b) ###############
#do the first step of imputation in mice and inspect, seed=1
q1b_imp <- mice(dataex1, seed=1, printFlag = FALSE)
q1b_imp
#fit the linear regression with the imputed data
q1b_fit <- with(q1b_imp, lm(bmi ~ age + hyp + chl))
q1b_fit$analyses  # see the estimates for our parameters
#and pool them together
q1b_pooled<- pool(q1b_fit)
q1b_pooled #see pooled estimated and inspect lambda and riv

############### Question 1.c) ###############
#do the same steps but for seed={2,3,4,5,6} with for-loop
for(i in 2:6){
  q1c_imp <- mice(dataex1 ,seed=i, printFlag = FALSE)
  q1c_fit <- with(q1c_imp, lm(bmi ~ age + hyp + chl))
  q1c_pooled <- pool(q1c_fit)
  print(q1c_pooled) #print pooled estimated and look at lambda and riv
}

############### Question 1.d) ###############
#use m=100 and do the same for the seeds={1,2,3,4,5,6}
for(i in 1:6){
  q1d_imp <- mice(dataex1, m=100, seed=i, printFlag = FALSE)  
  q1d_fit <- with(q1d_imp, lm(bmi ~ age + hyp + chl))
  q1d_pooled <- pool(q1d_fit)
  print(q1d_pooled) #print pooled estimated and look at lambda and riv
}

############### Question 2 ###############
#look at the data 
summary(dataex2[,,i])

#create a matrix to store c.i's in 
mat = matrix(NA, ncol = 1, nrow = 100)

#for loop the 100 datasets and impute without acknowledging parameter uncertainties 
for( i in 1:100){
  #use norm.nob for the stochastic regression imputation
  q2_imp<- mice(dataex2[,,i], m=20, method= 'norm.nob', seed=1, printFlag = FALSE)
  q2_fit <- with(q2_imp, lm(Y ~ X))
  q2_pooled <- pool(q2_fit)
  #get the summary with the confidence intervals
  sum <- summary(q2_pooled, conf.int = TRUE)
  #make if statements to see if beta1 are inside the 95% ci. 
  if(sum$`2.5 %`[2] <= 3 & sum$`97.5 %`[2] >= 3){
    mat[i,1] = 1 #if they are we get a 1 in the matrix
  }
  else{
    mat[i,1] = 0 #if not we get a 0 in the matrix
  }
}

#now loop the 100 datasets and impute with acknowledging parameter uncertainties 
mat1 = matrix(NA, ncol = 1, nrow = 100)
for(i in 1:100){
  #use the bootstrapped version norm.boot when imputating.
  q2_imp_boot<- mice(dataex2[,,i], m = 20, method = 'norm.boot', seed = 1, printFlag = FALSE)
  q2_fit_boot <- with(q2_imp_boot, lm(Y ~ X))
  q2_pooled_boot <- pool(q2_fit_boot)
  sum <- summary(q2_pooled_boot, conf.int = TRUE)
  #make sure that beta1's estimates are covered in 95% of the cases
  if(sum$`2.5 %`[2] <= 3 & sum$`97.5 %`[2] >= 3){
    mat1[i,1] = 1 #if they are we get a 1 in the matrix
  }
  else{
    mat1[i,1] = 0 #if not we get a 0 in the matrix
  }
}
#look at how well the imputations cover the 95% ci of the true value. 
mat
mat1
mean(mat) #88 times
mean(mat1) # 95 times

############### Question 3 ###############
#I'll use the dataset from Q1 for simplicity   
#first I will impute with seed 1 and the default specifications 
q3i_imp <- mice(dataex1, seed=1, printFlag = FALSE)

#now i will fit it with the previous model
q3i_fit <- with(q3i_imp , lm(bmi ~ age + hyp + chl))
#look at the estimates of all intercepts
q3i_fit$analyses
#create for-loops that puts the estimates into a matrix
intercept1 = matrix(NA, ncol = 1, nrow = 5)
for (i in 1:5){
  intercept <- q3i_fit$analyses[[i]]$coefficients[1]
  intercept1[i,1] = intercept
}
#average the variable to get the estimate
mean(intercept1) #19.618

#do the same for the other variables 
age1= matrix(NA, ncol = 1, nrow = 5)
for (i in 1:5){
  age <- q3i_fit$analyses[[i]]$coefficients[2]
  age1[i,1] = age
}
mean(age1) #-3.553

hyp1 = matrix(NA, ncol = 1, nrow = 5)
for (i in 1:5){
  hyp <- q3i_fit$analyses[[i]]$coefficients[3]
  hyp1[i,1] = hyp
}
mean(hyp1) #2.197

chl1 = matrix(NA, ncol = 1, nrow = 5)
for (i in 1:5){
  chl <- q3i_fit$analyses[[i]]$coefficients[4]
  chl1[i,1] = chl
}
mean(chl1) # 0.054

#ii version, here we pool the fitted values using mice 
pooled <- pool(q3i_fit)
summary(pooled) #the same estimates as above 

############### Question 4.a) ###############
#study the data
str(dataex4)
summary(dataex4)

#look at the missing data pattern for the dataset 
md_pattern(dataex4, pattern = FALSE, color = c('#34111b', '#e30f41'))
#568 fully observed variables, 182 observations missing y,
#182 missing x1 and 68 missing y and x1

#inspect the plots of the observed parts
plot_all(dataex4, breaks = 12, ncol = 2)

#default imputation
q4a_imp <- mice(dataex4, maxit = 0)
q4a_imp

#specify the first step  
q4a_imp <- mice(dataex4,  maxit = 20, m = 50, seed = 1, printFlag = FALSE)
#has mice() detected any problems
q4a_imp$loggedEvents #NULL
q4a_imp$imp  #imputed values for y and x1, x2 is complete

#fit the model and then pool
q4a_fit <- with(q4a_imp, lm(y ~ x1 + x2 + x1*x2))
q4a_pooled <- pool(q4a_fit)
#summary and 95% confidence interval
summary(q4a_pooled, conf.int = TRUE)

############### Question 4.b) ###############
# add x1*x2 to the dataframe and look at the new dataset 
x1x2 <- dataex4$x1*dataex4$x2
dataex4_1 <- data.frame(data=dataex4, x1x2)
head(dataex4_1)

#default imputation 
q4b_imp <- mice(dataex4_1, maxit = 0)
q4b_imp 

#the method of imputation need to change for the interaction variable
meth <- q4b_imp$method
meth["x1x2"] <- "~I(data.x1*data.x2)"
meth

#change the predictor matrix
pred <- q4b_imp$predictorMatrix
pred["data.x1", "x1x2"] = 0
pred["x1x2", "data.x1"] = 0
pred

#impute with the new changes
q4b_imp <- mice(dataex4_1, method = meth, predictorMatrix = pred,
                maxit = 20, m = 50, seed = 1, printFlag = FALSE)
#has mice() has detected any problems
q4b_imp$loggedEvents #NULL

#fit to the linear model
q4b_fit <- with(q4b_imp, lm(data.y ~ data.x1 + data.x2 + x1x2))

#pool together
q4b_pooled <- pool(q4b_fit)
#parameter estimates and the 95% ci
summary(q4b_pooled, conf.int = TRUE)


############### Question 4.c) ###############

#impute without changing the method or predictive matrix
q4c_imp <- mice(dataex4_1, maxit = 20, m = 50, seed = 1, printFlag = FALSE)
q4c_imp
#has mice() detected any problems
q4c_imp$loggedEvents  #NULL

#fit the desired analysis
q4c_fit <- with(q4c_imp, lm(data.y ~ data.x1 + data.x2 + x1x2))
#pool
q4c_pooled <- pool(q4c_fit)
#parameter estimates and the 95% ci
summary(q4c_pooled, conf.int = TRUE)


############### Question 5 ###############
#summarize the dataset and look at the pattern of missingness
summary(NHANES2)
md_pattern(NHANES2, pattern = FALSE, color = c('#34111b', '#e30f41'))

#inspect the plots of the observed parts
par(mar = c(3, 3, 2, 1), mgp = c(2, 0.6, 0))
plot_all(NHANES2, breaks = 15 , ncol = 4) #we might suspect hgt to be normal

#impute with defaults to get the methods 
q5_imp0 <- mice(NHANES2, maxit = 0)
q5_imp0  #hgt seems to be pmm
#change hgt to be normal 
meth <- q5_imp0$method
meth["hgt"] <- "norm"

#use the new method when imputating 
q5_imp <- mice(NHANES2, method = meth,  maxit = 20, m = 30, seed = 1, printFlag = FALSE)
q5_imp
#see if there is any problem detected
q5_imp$loggedEvents #NULL

#check the convergence and the density plots
plot(q5_imp, layout = c(2,4))
densityplot(q5_imp)

#fit the model with the linear regression model
q5_fit <- with(q5_imp, lm(wgt ~ gender + age + hgt + WC))
summary(q5_fit$analyses[[1]])

#plot the residuals against the fitted values
par(mfrow = c(1,1))
plot(q5_fit$analyses[[1]]$fitted.values, residuals(q5_fit$analyses[[1]]),
     ylab = "Residuals", xlab = "Fitted values")

#pool the imputations
q5_pooled <- pool(q5_fit)
summary(q5_pooled, conf.int = TRUE)

#find the r-squared for the pooled imputation  
pool.r.squared(q5_pooled, adjusted = TRUE)
