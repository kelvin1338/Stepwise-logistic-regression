abalone <- read.csv(file = 'C:/Users/Owner/Desktop/Warwick/Year 4/Foundations of Data Analytics/abalone.data.csv')
sex_data <- abalone$Sex
Length_data <- abalone$Length
Diameter_data <- abalone$Diameter
Height_data <- abalone$Height
Whole.weight_data <- abalone$Whole.weight
Shucked.weight_data <- abalone$Shucked.weight
Viscera.weight_data <- abalone$Viscera.weight
Shell.weight_data <- abalone$Shell.weight
Class_rings_data <- abalone$Class_rings

#1)simple linear regression model, diameter as a function of length, lm(y~x)
lm(Diameter_data ~ Length_data)
cor(Length_data, Diameter_data)
#y=ax+b, a = 0.81546, b = -0.01941


#2)multilinear model, give whole weight as a function of shucked weight, viscera weight and shell weight. Give parameters
lm(Whole.weight_data ~ Shucked.weight_data + Viscera.weight_data + Shell.weight_data)
cor(Shucked.weight_data + Viscera.weight_data + Shell.weight_data, Whole.weight_data)
#y = ax + by + cz + d, a = 0.93656, b = 1.11165, c = 1.25296, d = -0.00783
#Cor = 0.9950548






#Relationship between whole weight and diameter, plot

#3i) Simple linear model
summary(lm(Whole.weight_data ~ Diameter_data)) #y = whole weight, x = diameter
cor(Diameter_data,Whole.weight_data)
#y = ax + b, a = 4.573, b = -1.037
#corr = 0.9254521

#Sum of Squared Error
sm1 <- summary(lm(Whole.weight_data ~ Diameter_data))
sum(sm1$residuals^2)
#144.1485






#3ii) Quadratic model
Diameter_data2 <- Diameter_data^2
summary(lm(Whole.weight_data ~ Diameter_data + Diameter_data2))
##lm(Whole.weight_data ~ poly(Diameter_data,2)) different result?
cor(Diameter_data + Diameter_data2, Whole.weight_data)
#y = ax^2 + bx + c, a = 10.4968, b = -3.3555, c = 0.3477
#corr = 0.9419275 ?

#Sum of Squared Error
sm2 <- summary(lm(Whole.weight_data ~ Diameter_data + Diameter_data2))
sum(sm2$residuals^2)
#73.54913






#3iii) Cubic model without lower order or constant terms
Diameter_data3 <- Diameter_data^3
summary(lm(Whole.weight_data ~ 0 + Diameter_data3)) #0 forces intercept=0
cor(0 + Diameter_data3, Whole.weight_data)
#y = ax^3, a = 10.34 ?
#cor = 0.9630787

#Sum of Squared Error
sm3 <- summary(lm(Whole.weight_data ~ 0 + Diameter_data3))
sum(sm3$residuals^2)
#74.04224






#3iv) Exponential model
summary(lm(log(Whole.weight_data) ~ Diameter_data))
cor(Diameter_data, log(Whole.weight_data))
#log(y) = ax + b, a = 8.11667, b = -3.75098
#cor = 0.9635121

#Sum of Squared Error
sm4 <- summary(lm(log(Whole.weight_data) ~ Diameter_data))
sum(sm4$residuals^2)
#209.1015



#Plot
fit1 <- lm(Whole.weight_data ~ Diameter_data)
fit2 <- lm(Whole.weight_data ~ Diameter_data + Diameter_data2)
fit3 <- lm(Whole.weight_data ~ 0 + Diameter_data3)
fit4 <- lm(log(Whole.weight_data) ~ exp(Diameter_data))
plot(Diameter_data, Whole.weight_data, main = "Diameter vs Whole Weight", ylab="Whole weight", xlab="Diameter", col = "black")
points(Diameter_data, fitted(fit1), col = "blue")
points(Diameter_data, fitted(fit2), col = "red")
points(Diameter_data, fitted(fit3),col="green")
points(Diameter_data, fitted(fit4),col="yellow")
legend("topleft", legend=c("Original data", "Linear model", "Quadratic model", "Cubic model", "Exponential model"),
       col=c("black", "blue", "red", "green", "yellow"), lty=1,
       text.font=1, bg="white")


library(tidyverse)
data <- tibble(Whole.weight_data, Diameter_data) %>%
  na.omit

data %>%
  ggplot(aes(x = Diameter_data, y = Whole.weight_data)) +
  geom_point() +
  stat_smooth(aes(colour="Linear"), method = "lm", formula = y ~ x) +
  stat_smooth(aes(colour="Quadratic"), method = "lm", formula = y ~ x + I(x^2)) +
  stat_smooth(aes(colour="Cubic"), method = "lm", formula = y ~ 0 + I(x^3)) +
  stat_smooth(aes(colour="Exponential"), method = "lm", formula = log(y) ~ x) +
  scale_x_continuous(limits = c(0, 0.7)) +
  scale_y_continuous(limits = c(0, 3)) +
  #theme(legend.position = c(0.95, 0.95), legend.justification = c("left", "top")) +
  scale_colour_manual(name="Line Color",
                      values=c(Linear="blue", Quadratic="red", Cubic="green", Exponential="yellow")) +
  ggtitle("Diameter vs Whole weight (line of best fits)") +
  xlab("Diameter") + 
  ylab("Whole weight") +
  theme_bw()





#4) Building a logistic regression model
#Determine if Sex: I, M or F based on length, whole weight, class rings (separately) and together
#Here, I created a new file abaloneq4.data, the sex column was modified such that all M,F = 1 (adult), I = 0
abalone_q4 <- read.csv(file = 'C:/Users/Owner/Desktop/Warwick/Year 4/Foundations of Data Analytics/abaloneq4.data.csv')
sex_data_q4 <- abalone_q4$Sex
Length_data_q4 <- abalone_q4$Length
Diameter_data_q4 <- abalone_q4$Diameter
Height_data_q4 <- abalone_q4$Height
Whole.weight_data_q4 <- abalone_q4$Whole.weight
Shucked.weight_data_q4 <- abalone_q4$Shucked.weight
Viscera.weight_data_q4 <- abalone_q4$Viscera.weight
Shell.weight_data_q4 <- abalone_q4$Shell.weight
Class_rings_data_q4 <- abalone_q4$Class_rings

set.seed(1337)
library(caTools)
split <- sample.split(abalone_q4$Sex, SplitRatio = 0.75)
traindata <- subset(abalone_q4, split == TRUE)
testdata <- subset(abalone_q4, split == FALSE)

model <- glm(Sex ~ Length, 
             data = traindata, family=binomial)
summary(model)

pred <- predict(model, type = "response") #model predicts probability of each entry being '0' or '1'
summary(pred)
tapply(pred, traindata$Sex, mean) #average prediction for each true outcome
#i.e. in all true infant cases, we predict an average of about 0.466, and in all true adult cases 0.779
table(traindata$Sex, pred > 0.5)
#i.e. sensitivity = 1909/(1909+217) = 0.8979, specificity = 536/(536+470) = 0.5328


predicttestdata = predict(model, type = "response", newdata = testdata)
table(testdata$Sex, predicttestdata >= 0.5) #if p>=0.5 then adult, otherwise infant
a <- table(testdata$Sex, predicttestdata >= 0.5)
accuracy1 <- (a[1,1] + a[2,2])/(a[1,1] + a[2,2] + a[1,2] + a[2,1])
accuracy1
#accuracy at 0.5: (171+655)/(171+655+165+54) = 0.7904



#4ii)
model2 <- glm(Sex ~ Whole.weight, 
              data = traindata, family=binomial)

summary(model2)

pred2 <- predict(model2, type = "response")
summary(pred2)
tapply(pred2, traindata$Sex, mean)

table(traindata$Sex, pred > 0.5)

predicttestdata2 = predict(model2, type = "response", newdata = testdata)
table(testdata$Sex, predicttestdata2 >= 0.5)
a2 <- table(testdata$Sex, predicttestdata2 >= 0.5)
accuracy2 <- (a2[1,1] + a2[2,2])/(a2[1,1] + a2[2,2] + a2[1,2] + a2[2,1])
accuracy2
#accuracy at 0.5: (217+628)/(217+628+119+81) = 0.8086

#4iii)
model3 <- glm(Sex ~ Class_rings, 
              data = traindata, family=binomial)

summary(model3)

pred3 <- predict(model3, type = "response")
summary(pred3)
tapply(pred3, traindata$Sex, mean)

table(traindata$Sex, pred > 0.5)

predicttestdata3 = predict(model3, type = "response", newdata = testdata)
table(testdata$Sex, predicttestdata3 >= 0.5)
a3 <- table(testdata$Sex, predicttestdata3 >= 0.5)
accuracy3 <- (a3[1,1] + a3[2,2])/(a3[1,1] + a3[2,2] + a3[1,2] + a3[2,1])
accuracy3
#accuracy at 0.5: (161+672)/(161+672+175+37) = 0.7971

#4iv)
model4 <- glm(Sex ~ Length + Whole.weight + Class_rings, 
              data = traindata, family=binomial)

summary(model4)

pred4 <- predict(model4, type = "response")
summary(pred4)
tapply(pred4, traindata$Sex, mean)

table(traindata$Sex, pred > 0.5)

predicttestdata4 = predict(model4, type = "response", newdata = testdata)
table(testdata$Sex, predicttestdata4 >= 0.5)
a4 <- table(testdata$Sex, predicttestdata4 >= 0.5)
accuracy4 <- (a4[1,1] + a4[2,2])/(a4[1,1] + a4[2,2] + a4[1,2] + a4[2,1])
accuracy4
#accuracy at 0.5: (245+620)/(245+620+91+89) = 0.8278



#5i)
adult <- read.csv(file = 'C:/Users/Owner/Desktop/Warwick/Year 4/Foundations of Data Analytics/adult1.data.csv')
#Replaced all Female with 0, Male with 1
#Replaced all <=50K with 0, >50K with 1
adult1 <- na.omit(adult)
completemod <- glm(sex~ age + workclass + fnlwgt + education_num + marital_status + occupation + relationship + race + capital_gain + capital_loss + hours_per_week + native_country + income, data = adult1, family=binomial)

#completemod <- glm(sex~ age + workclass + fnlwgt + education_num + marital_status + occupation + relationship + race + capital_gain + capital_loss + hours_per_week + native_country + income, data = na.omit(adult), family=binomial)
step(completemod)

a5 <- table(adult1$sex, predict(completemod, type= "response") >= 0.5)
accuracycomplete <- (a5[1,1] + a5[2,2])/(a5[1,1] + a5[2,2] + a5[1,2] + a5[2,1])
accuracycomplete
#accuracy 0.851734

back1 <- glm(sex ~ age + workclass + fnlwgt + marital_status + occupation + 
               relationship + race + hours_per_week + income,
             data = adult1, family=binomial)
summary(back1)
a6 <- table(adult1$sex, predict(back1, type= "response") >= 0.5)
accuracyback1 <- (a6[1,1] + a6[2,2])/(a6[1,1] + a6[2,2] + a6[1,2] + a6[2,1])
accuracyback1
#accuracy 0.850441
accuracytradeoff1 = accuracycomplete - accuracyback1
accuracytradeoff1
#accuracy tradeoff 0.001293018
#remove race because all its values are the least statistically significant

back2 <- glm(sex ~ age + workclass + fnlwgt + marital_status + occupation + 
               relationship + hours_per_week + income,
             data = adult1, family=binomial)
summary(back2)
a7 <- table(adult1$sex, predict(back2, type= "response") >= 0.5)
accuracyback2 <- (a7[1,1] + a7[2,2])/(a7[1,1] + a7[2,2] + a7[1,2] + a7[2,1])
accuracyback2
#accuracy 0.8503415
accuracytradeoff2 = accuracycomplete - accuracyback2
accuracytradeoff2
#accuracy tradeoff 0.001392481
#remove marital status because all of its values on average have a less p value than the other variables, therefore least statistically significant

back3 <- glm(sex ~ age + workclass + fnlwgt + occupation + 
               relationship + hours_per_week + income,
             data = adult1, family=binomial)
summary(back3)
a8 <- table(adult1$sex, predict(back3, type= "response") >= 0.5)
accuracyback3 <- (a8[1,1] + a8[2,2])/(a8[1,1] + a8[2,2] + a8[1,2] + a8[2,1])
accuracyback3
#accuracy 0.8491811
accuracytradeoff3 = accuracycomplete - accuracyback3
accuracytradeoff3
#accuracy tradeoff 0.002552881
#remove workclass because all of its values on average have a less p value than the other variables, therefore least statistically significant

back4 <- glm(sex ~ age + fnlwgt + occupation + 
               relationship + hours_per_week + income,
             data = adult1, family=binomial)
summary(back4)
a9 <- table(adult1$sex, predict(back4, type= "response") >= 0.5)
accuracyback4 <- (a9[1,1] + a9[2,2])/(a9[1,1] + a9[2,2] + a9[1,2] + a9[2,1])
accuracyback4
#accuracy 0.8474239
accuracytradeoff4 = accuracycomplete - accuracyback4
accuracytradeoff4
#accuracy 0.004310059
#remove fnlwgt because it has the largest p value compared to the overall (average) p values of other attributes


back5 <- glm(sex ~ age + occupation + 
               relationship + hours_per_week + income,
             data = adult1, family=binomial)
summary(back5)
a10 <- table(adult1$sex, predict(back5, type= "response") >= 0.5)
accuracyback5 <- (a10[1,1] + a10[2,2])/(a10[1,1] + a10[2,2] + a10[1,2] + a10[2,1])
accuracyback5
#accuracy 0.8456667
accuracytradeoff5 = accuracycomplete - accuracyback5
accuracytradeoff5
#accuracy tradeoff 0.006067237
#remove age because it has the smallest z value hence least statistically significant


back6 <- glm(sex ~ occupation + 
               relationship + hours_per_week + income,
             data = adult1, family=binomial)
summary(back6)
a11 <- table(adult1$sex, predict(back6, type= "response") >= 0.5)
accuracyback6 <- (a11[1,1] + a11[2,2])/(a11[1,1] + a11[2,2] + a11[1,2] + a11[2,1])
accuracyback6
#accuracy 0.8435449
accuracytradeoff6 = accuracycomplete - accuracyback6
accuracytradeoff6
#accuracy tradeoff 0.008189112
#Remove income because it has the smallest z value hence least statistically significant


back7 <- glm(sex ~ occupation + 
               relationship + hours_per_week,
             data = adult1, family=binomial)
summary(back7)
a12 <- table(adult1$sex, predict(back7, type= "response") >= 0.5)
accuracyback7 <- (a12[1,1] + a12[2,2])/(a12[1,1] + a12[2,2] + a12[1,2] + a12[2,1])
accuracyback7
#accuracy 0.8434454
accuracytradeoff7 = accuracycomplete - accuracyback7
accuracytradeoff7
#accuracytradeoff 0.008288575
#remove relationship because it has the smallest average z value hence least statistically significant




back8 <- glm(sex ~ occupation + hours_per_week,
             data = adult1, family=binomial)
summary(back8)
a13 <- table(adult1$sex, predict(back8, type= "response") >= 0.5)
accuracyback8 <- (a13[1,1] + a13[2,2])/(a13[1,1] + a13[2,2] + a13[1,2] + a13[2,1])
accuracyback8
#accuracy 0.7405334
accuracytradeoff8 = accuracycomplete - accuracyback8
accuracytradeoff8
#accuracy tradeoff 0.1111995
#Do not remove relationship because accuracy tradeoff is above 1%