housing = read.csv("housingprice.csv", header = TRUE)
attach(housing)

#part 0: EDA
str(housing)
summary(housing[,-c(1,2)])



#part 1: rank the zipcode by average housing prices

ave.zip = tapply(price, zipcode, mean)
head(sort(ave.zip)) #bottom 5 zip code with the lowest average housing price
tail(sort(ave.zip)) #top 5 zip code with the highest average housing price

top.zip = housing[c(which(zipcode == "98039"),
                    which(zipcode == "98004"),
                    which(zipcode == "98040")),]

#generate boxplot
library(ggplot2) # for ggplot
p = ggplot(top.zip, aes(x = as.factor(zipcode), y = price))+
  geom_boxplot()+
  xlab("zip code")+
  ylab("housing price")+
  labs(title = "Top 3 zipcode with the highest housing price")+
  theme(plot.title = element_text(hjust = 0.5))
p




#part2: visualize relationship between variables
#square feet and housing price

p1 = ggplot(housing, aes(x = sqft_living, y = price))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("square feet of living room")+
  ylab("housing price")+
  labs(title = "Sqft of living room and housing price")+
  theme(plot.title = element_text(hjust = 0.5))
p1

p2 = ggplot(housing, aes(x = sqft_lot, y = price))+
  geom_point()+
  geom_smooth(method=lm)+
  xlab("square feet of parking lot")+
  ylab("housing price")+
  labs(title = "Sqft of parking lot and housing price")+
  theme(plot.title = element_text(hjust = 0.5))
p2



#part3: build a linear model
#variables: bedroom, bathroom, sqft_living, sqft_lot
#using k-fold cross validation for model assessment
library(caret)
##k-fold cross validation (approach 1)
k = 10 #number of folds
n = length(housing[,1])
set.seed(1)
folds = createFolds(seq(1:n),k) #split the data in k groups

cv.error.10 = rep(0,k)
for (i in 1:k){
  index = unlist(folds[i],use.names = FALSE)
  train = housing[-index,]
  test = housing[index,]
  model = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot,
             data = train)
  cv.error.10[i] = mean((test$price - predict(model, test))^2)
}
mean(cv.error.10) ##CV ERROR WHEN K = 10
#cv MSE is $66,351,069,200

summary(model)$adj.r.squared
#adj r squared is 0.502




##k-fold cross validation (approach 2: using glm)
library(boot) #for glm
set.seed(1)
cv.error.10.2 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot,
                data = housing)
  cv.error.10.2[i] = cv.glm(housing,glm.fit, K = 10)$delta[1]
}
mean(cv.error.10.2)
#cv MSE is $66,351,830,466




##k-fold cross validation (approach 3)
library(caret) #for trainControl
set.seed(1)
train.control = trainControl(method = "cv", number = 10) 
model.train = train(price ~ bedrooms + bathrooms + sqft_living + sqft_lot,
                    data = housing, method = "lm", trControl = train.control)
summary(model.train)$adj.r.squared
#adjusted r squared is 0.508





#part4: adding zipcode in the linear model 

##k-fold cross validation (approach 2: using glm)
library(boot) #for glm
set.seed(1)
cv.error.zip = rep(0,10)
for(i in 1:10){
  glm.fit.zip = glm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + as.factor(zipcode),
                    data = housing)
  cv.error.zip[i] = cv.glm(housing, glm.fit.zip, K = 10)$delta[1]
}
mean(cv.error.zip)
#CV MSE is $35,510,919,131 (decreased from the previous model)


#part 5: now predict the housing price of the fancy house
fancy = read.csv("fancyhouse.csv", header = TRUE)
fancy$zipcode = as.factor(fancy$zipcode)
predict(glm.fit.zip, fancy)
#predicted housing price for the fancy house is $14,761,285




#part 6: feature engineering
#improving the linear model based on the CV MSE


k = 10 #number of folds
n = length(housing[,1])
set.seed(1)
folds = createFolds(seq(1:n),k) #split the data in k groups


#adding terms
cv.error0 = rep(0,k)
cv.error1 = rep(0,k)
cv.error2 = rep(0,k)
cv.error3 = rep(0,k)
cv.error4 = rep(0,k)
cv.error5 = rep(0,k)

for (i in 1:k){
  index = unlist(folds[i],use.names = FALSE)
  train = housing[-index,]
  test = housing[index,]
  
  #adding zipcode
  m0 = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + as.factor(zipcode), 
          data = train)
  m1 = lm(price ~ bedrooms + bathrooms + sqft_living + I(sqft_living^2) + sqft_lot + as.factor(zipcode),
          data = train)
  m2 = lm(price ~ bedrooms + bathrooms + sqft_living + I(sqft_lot^2) + as.factor(zipcode),
          data = train)
  m3 = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + yr_built + as.factor(zipcode),
          data = train)
  m4 = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + sqft_above + as.factor(zipcode))
  m5 = lm(price ~ bedrooms + bathrooms + bedrooms*bathrooms + sqft_living + I(sqft_living^2) + sqft_lot + as.factor(zipcode),
          data = train)
  cv.error0[i] = mean((test$price - predict(m0, test))^2)
  cv.error1[i] = mean((test$price - predict(m1, test))^2)
  cv.error2[i] = mean((test$price - predict(m2, test))^2)
  cv.error3[i] = mean((test$price - predict(m3, test))^2)
  cv.error4[i] = mean((test$price - predict(m4, test))^2)
  cv.error5[i] = mean((test$price - predict(m5, test))^2)
}
mean(cv.error0)
mean(cv.error1) 
mean(cv.error2)
mean(cv.error3)
mean(cv.error4)
mean(cv.error5) #lowest CV ERROR

summary(m0)$adj.r.squared
summary(m1)$adj.r.squared 
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared
summary(m5)$adj.r.squared #highest adjusted r squared

