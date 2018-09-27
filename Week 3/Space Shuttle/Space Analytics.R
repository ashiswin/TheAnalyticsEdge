setwd('/Users/ashiswin/Documents/School/Analytics Edge/Week 3/Space Shuttle')

orings <- read.csv('Orings.csv')
str(orings)
head(orings)

m1 <- glm(Field ~ Temp, data = orings, family = binomial)
summary(m1)
# null deviance: -2LL(intercept) to see how well the intercept is fit
# residual deviance: -2LL(beta)
# AIC: Akaika information criterion: -2LL(beta) + 2(p + 1), where p is number of predictor variables in model
# Smaller AIC -> better

m2 <- glm(Field ~ Temp + Pres, orings, family = binomial)
summary(m2)

plot(orings$Temp, orings $Field)
curve(exp(6.75183 - (0.13971 * x)) / (1 + exp(6.75183 - (0.13971 * x))), add = T)
predict1 <- predict(m1, newdata = orings, type="response")
table(predict1 >= 0.5, orings$Field)

library(caTools)
library(ROCR)

ROCRpred <- prediction(predict1[1:138], orings$Field[1:138])
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf)
performance(ROCRpred, measure = "auc")
