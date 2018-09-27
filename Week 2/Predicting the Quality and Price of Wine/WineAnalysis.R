setwd('/Users/ashiswin/Documents/School/Analytics Edge/Week 2/Predicting the Quality and Price of Wine')
wine <- read.csv("wine.csv")
str(wine)
summary(wine)

wine[wine$VINT == 1966,]

plot(wine$VINT, wine$LPRICE)
pairs(wine)

# Make my training and testing set
winetrain <- subset(wine, wine$VINT <= 1978 & !is.na(wine$LPRICE)) # Remove all NA items
winetest <- subset(wine, wine$VINT > 1978)
winetrain$HRAINDEGREE <- winetrain$HRAIN * winetrain$DEGREES
m1 <- lm(LPRICE ~ WRAIN, winetrain)
m1a <- lm(LPRICE ~ WRAIN + VINT, winetrain)
m2 <- lm(LPRICE ~ VINT + WRAIN + DEGREES + HRAIN, winetrain)
m3 <- lm(LPRICE ~ VINT + WRAIN + DEGREES + HRAIN + HRAINDEGREE, winetrain)

summary(m1)
summary(m2)
summary(m1a)
summary(m3)

plot(wine$VINT, wine$LPRICE)
abline(m1)

# Get correlation between variables in wine set
cor(winetrain)

# Get coefficients from the model
m2$coefficients

# Get residuals from model
m2$residuals

# Lets get R^2 manually
ssttrain <- sum((winetrain$LPRICE - mean(winetrain$LPRICE)) ^ 2)
ssetrain <- sum(m2$residuals ^ 2)

rsquared <- 1 - (ssetrain / ssttrain)

# Perform a prediction into the future
wineprediction <- predict(m2, newdata = winetest)
wineprediction
winetest$LPRICE

# Calculate R^2 of the prediction
ssetest <- sum((wineprediction[1:2] - winetest$LPRICE[1:2]) ^ 2)
ssttest <- sum((winetest$LPRICE[1:2] - mean(winetrain$LPRICE)) ^ 2)
rsquaredtest <- 1 - (ssetest / ssttest)
