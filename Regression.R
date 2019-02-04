require(data.table)
require(caret)
require(DBI)
require(Biglm)
#Q1a Fit simple linear regression (separately) for each covariate. Provide scatter plots
#with fitted regression line. Which covariate provides best prediction?
# return: string(“cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction
SLR <- function(path='../data/linear.csv'){
  data.linear <- read.csv(path)
  
  # plot cylinders and mpg
  plot(data.linear$cylinders, data.linear$mpg, xlab="cylinders", ylab="mpg")
  lm_cylinders <- lm(formula = data.linear$mpg ~ data.linear$cylinders)
  abline(lm_cylinders,col="blue")
  
  # plot displacement and mpg
  plot(data.linear$displacement, data.linear$mpg, xlab="displacement", ylab="mpg")
  lm_displacement <- lm(formula = data.linear$mpg ~ data.linear$displacement)
  abline(lm_displacement,col="blue")
  
  # plot horsepower and mpg
  plot(data.linear$horsepower, data.linear$mpg, xlab="horsepower", ylab="mpg")
  lm_horsepower <- lm(formula = data.linear$mpg ~ data.linear$horsepower)
  abline(lm_horsepower,col="blue")
  
  # plot weight and mpg
  plot(data.linear$weight, data.linear$mpg, xlab="weight", ylab="mpg")
  lm_weight <- lm(formula = data.linear$mpg ~ data.linear$weight)
  abline(lm_weight,col="blue")
  
  # plot acceleration and mpg
  plot(data.linear$acceleration, data.linear$mpg, xlab="acceleration", ylab="mpg")
  lm_acceleration <- lm(formula = data.linear$mpg ~ data.linear$acceleration)
  abline(lm_acceleration,col="blue")
  
  
  # plot year and mpg
  plot(data.linear$year, data.linear$mpg, xlab="year", ylab="mpg")
  lm_year <- lm(formula = data.linear$mpg ~ data.linear$year)
  abline(lm_year,col="blue")
  
  # plot origin and mpg
  plot(data.linear$origin, data.linear$mpg, xlab="origin", ylab="mpg")
  lm_origin <- lm(formula = data.linear$mpg ~ data.linear$origin)
  abline(lm_origin,col="blue")
  
  #Checkig R^2 value for finding best covariate
  
  R_Squared_list = list("cylinders"=summary(lm_cylinders)$r.squared,
                        "displacement"=summary(lm_displacement)$r.squared,
                        "horsepower"=summary(lm_horsepower)$r.squared,
                        "acceleration"=summary(lm_acceleration)$r.squared,
                        "weight"=summary(lm_weight)$r.squared,
                        "year"=summary(lm_year)$r.squared,
                        "origin"=summary(lm_origin)$r.squared)
  
  return(names(R_Squared_list)[which.max(R_Squared_list)])
  
}


#Q1b Fit multiple linear regression model for the data
# return: list of following variables, Intercept, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff
MLR <- function(path='../data/linear.csv'){

  data.linear <- read.csv(path)
  
  lm_mlr= lm(data.linear$mpg ~ ., data = data.linear)
  summary(lm_mlr)
  
  # fill in the list with the coeffes you compute
  summ = summary(lm_mlr)
  result <- list("Intercept"=summ$coefficients[1,1], "CylindersCoeff"=summ$coefficients[2,1], "DispCoeff"=summ$coefficients[3,1], "HPCoeff"=summ$coefficients[4,1], "WeightCoeff"=summ$coefficients[5,1], "AccCoeff"=summ$coefficients[6,1], "YearCoeff"=summ$coefficients[7,1], "OriginCoeff"=summ$coefficients[8,1])
  
  #Resulting Equation
  print(paste0("Resulting equation is mpg = ",result$Intercept," + (", result$CylindersCoeff,")*Cylinders + (",result$DispCoeff,")*Displacement +(",
               result$HPCoeff,")*horsepower + (",result$WeightCoeff,")*weight + (",result$AccCoeff,")*acceleration +(",
               result$YearCoeff,")*year + (",result$OriginCoeff,")*origin + Epsilon"))
  
  return(result)
}
  
#Q2 Fit logistic regression model for the dataset logistic.txt. Note that by
#ignoring the ‘year’ column, this dataset contains 6 covariates, therefore you should use
#multiple logistic regression which is straightforward generalization of simple logistic
#regression. Note that “Direction” attribute is your class label.
# return: list of following variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff
LogisticRegression <- function(path='../data/logistic.txt'){

  data.logistic.main <- read.csv(path)
  
  #Splitting data into training data and testing data
  data.logistic <- data.logistic.main[-c(1)]
  set.seed(200)
  sample = sample(nrow(data.logistic), size= floor(0.8*nrow(data.logistic)), replace = F)
  
  sample_train = data.logistic[sample, ]
  sample_test = data.logistic[-sample, ]
  
  
  glm_logistic_train <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4
                            +Lag5+Volume+Today, 
                            data= sample_train, family = binomial())
  
  fit.results <- predict.glm(glm_logistic_train, newdata = sample_test, type = 'response')
  
  fit.results <- ifelse(fit.results >0.5,1,0)
  
  #Changing Up and down to 1 and 0 respectively
  test_Direction = sample_test$Direction
  test_Direction <- ifelse(test_Direction == "Up",1,0)
  
  print(paste0("Accuracy: ",mean(fit.results == test_Direction)))
  
  summ_glm <- summary(glm_logistic_train)
  # fill in the list with the coeffes you compute
  result <- list("Intercept" = summ_glm$coefficients[1,1],"Lag1Coeff" =summ_glm$coefficients[2,1], "Lag2Coeff" =summ_glm$coefficients[3,1], "Lag3Coeff" =summ_glm$coefficients[4,1],"Lag4Coeff" =summ_glm$coefficients[5,1], "Lag5Coeff" =summ_glm$coefficients[6,1],"TodayCoeff"=summ_glm$coefficients[8,1],"VolumeCoeff"=summ_glm$coefficients[7,1])
  
  return(result)
}

#Q3 Apply your data science skills to improve the model fitted in Q2. In what sense your
#improved model is better than the model found in (Q2)
# return: float of training accuracy 
LogisticRegressionImproved <- function(path='../data/logistic.txt'){
  data.logistic.main <- read.csv(path)
  
  #Splitting data into training data and testing data
  
  data.logistic <- data.logistic.main[-c(1)]
  
  data.logistic$Direction <- ifelse(data.logistic$Direction == "Up",1,0)
  
  cor(data.logistic$Lag1,data.logistic$Direction)
  cor(data.logistic$Lag2,data.logistic$Direction)
  cor(data.logistic$Lag3,data.logistic$Direction)
  cor(data.logistic$Lag4,data.logistic$Direction)
  cor(data.logistic$Lag5,data.logistic$Direction)
  cor(data.logistic$Volume,data.logistic$Direction)
  cor(data.logistic$Today,data.logistic$Direction)
  
  #Splitting data into training data and testing data
  data.logistic <- data.logistic.main[-c(1)]
  set.seed(200)
  sample = sample(nrow(data.logistic), size= floor(0.8*nrow(data.logistic)), replace = F)
  
  sample_train = data.logistic[sample, ]
  sample_test = data.logistic[-sample, ]
  
  
  glm_logistic_train <- glm(Direction ~ Lag3+Lag4
                            +Lag5+Volume+Today, 
                            data= sample_train, family = binomial())
  
  fit.results <- predict.glm(glm_logistic_train, newdata = sample_test, type = 'response')
  
  fit.results <- ifelse(fit.results >0.5,1,0)
  
  #Changing Up and down to 1 and 0 respectively
  test_Direction = sample_test$Direction
  test_Direction <- ifelse(test_Direction == "Up",1,0)
  
  print(paste0("Accuracy: ",mean(fit.results == test_Direction)))
  
  return(mean(fit.results == test_Direction))

}

#Q4 Big data (is everywhere now). The supplied file “slr-90m-data.csv” contain 90
#million (x,y) data points. Regular lm() model typically fails on this data unless you have
#16gb+ memory. So you objective is to explore alternative scalable packages to fit slr to
#this data
#Find linear regression fit to this data
#In general, how do you deal with such big data problems? One simple solution is
#to generate samples
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
  
  library(data.table)
  library(biglm)
  data.big.slr <- fread(path)
  
  model <- biglm(y~x,data = data.big.slr)
  
  # fill in the list with the coeffes you compute
  # from the next line, you should infer that model should be the variable name of your model
  model$coefficients <- list (summary(model)$mat[1,1],summary(model)$mat[2,2])
  
  row_count = nrow(data.big.slr)
  set.seed(123)
  sample_1_per = sample(row_count, size = floor(0.01*row_count), replace =F)
  sample_2_per = sample(row_count, size = floor(0.02*row_count), replace =F)
  sample_3_per = sample(row_count, size = floor(0.03*row_count), replace =F)
  sample_4_per = sample(row_count, size = floor(0.04*row_count), replace =F)
  sample_5_per = sample(row_count, size = floor(0.05*row_count), replace =F)
  
  plot(data.big.slr$x, data.big.slr$y, xlab="x", ylab="y", type="n")
  abline(model,col="red")
  
  model_1=lm(y~x,data = data.big.slr[sample_1_per,])
  model_2=lm(y~x,data = data.big.slr[sample_2_per,])
  model_3=lm(y~x,data = data.big.slr[sample_3_per,])
  model_4=lm(y~x,data = data.big.slr[sample_4_per,])
  model_5=lm(y~x,data = data.big.slr[sample_5_per,])
  
  abline(model_1, col= "yellow")
  abline(model_2, col= "black")
  abline(model_3, col= "blue")
  abline(model_4, col= "green")
  abline(model_5, col= "pink")
  
  legend("topleft", c("Full Data", "sample1", "sample2", "sample3", "sample4", "sample5"),
         col=c("red","yellow","black","blue","green","pink"), 
         lty = c(1,1,1,1,1,1))
  
  coefficients(model_1)
  coefficients(model_2)
  coefficients(model_3)
  coefficients(model_4)
  coefficients(model_5)
  
  
  
  # fill in the list with the coeffes you compute
  # from the next line, you should infer that model should be the variable name of your model
  result <- list("Intercept"=model$coefficients[1], "xCoeff"=model$coefficients[2])
  
  return(result)
}

#Q5 Implement a function ZTest to perform a simple z-test automatically on the data
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){
  
  # finding test value 
  z = (mean(x)-pop_mean)/(pop_sd/sqrt(length(x)))
  
  # finding Critical value using qnorm function  
  
  if (test_type == "left-tailed") {
    CriticalVal <- qnorm(alpha)
    if (z < CriticalVal) {
      print ("reject")
    } else {
      print ("not-reject")
    }
  }
  if (test_type == "right-tailed") {
    CriticalVal <- qnorm(1-alpha)
    if (z > CriticalVal) {
      return ("reject")
    } else {
      return ("not-reject")
    }
  } 
  if (test_type == "two-tailed") {
    tt1 <- qnorm(alpha/2)
    tt2 <- qnorm(1- (alpha/2))
    if (z < tt1 || z> tt2) {
      return ("reject")
    } else {
      return ("not-reject")
    }
  }
}


#Q6 Implement an R program to demonstrate CLT(Central Limit Theorem)
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se
CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  
  if (populationDistribution == "uniform") {
    x = runif(sampleSize*numberOfSamples)
    x_matrix = matrix(x, nrow = numberOfSamples, ncol = sampleSize)
    mean_vector = rowMeans(x_matrix)
    hist(mean_vector)
  }
  
  if (populationDistribution == "normal") {
    x = rnorm(sampleSize*numberOfSamples)
    x_matrix = matrix(x, nrow = numberOfSamples, ncol = sampleSize)
    mean_vector = rowMeans(x_matrix)
    hist(mean_vector)
  }
  
  # fill in the list with the mean and std you compute
  result <- list("mean"=mean(mean_vector), "se"=sd(x)/sqrt(sampleSize))
  return(result)
}

