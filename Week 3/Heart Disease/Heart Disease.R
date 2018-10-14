setwd('/Users/ashiswin/Documents/School/Analytics Edge/Week 3/Heart Disease')

framing <- read.csv('framingham.csv')
heart1 <- subset(framing, PERIOD == 1 & PREVCHD == 0)
heart1$TENCHD <- as.integer((heart1$TIMECHD) / 365 <= 10)
table(heart1$TENCHD)
644 / (644 + 3596)
colnames(heart1)

heart1 <- heart1[, c(1:21, 40)]

library(caTools)

set.seed(1)
spl <- sample.split(heart1$TENCHD, SplitRatio = 0.65)
head(spl)
training <- subset(heart1, spl == TRUE)
test <- subset(heart1, spl == FALSE)
table(training$TENCHD)
table(test$TENCHD)

model1 <- glm(TENCHD ~ ., data = training, family = "binomial")
summary(model1)

model2 <- glm(TENCHD ~ SEX + AGE + SYSBP + CIGPDAY + GLUCOSE, data = training, family = 'binomial')
summary(model2)

predict2 <- predict(model2, newdata = test, type = "response")
table(predict2 >= 0.5, test$TENCHD)
