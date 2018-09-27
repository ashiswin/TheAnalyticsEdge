setwd('/Users/ashiswin/Documents/School/Analytics Edge/Week 2/Moneyball and Sports Analytics')
baseball <- read.csv('baseball.csv')

str(baseball)

baseball2002 <- subset(baseball, Year < 2002)
table(baseball2002$Year)

plot(baseball2002$W, baseball2002$Team, col = ifelse(baseball2002$Playoffs == 1, "red", "black"), pch = 16)
axis(1, at=seq(60, 120, by=5))
abline(v=95)
baseball2002$RD <- baseball2002$RS - baseball2002$RA
plot(baseball2002$RD, baseball2002$W)

model <- lm(W ~ RD, baseball2002)
summary(model)
abline(model)

m1 <- lm(RS~BA, baseball2002)
summary(m1)

m2 <- lm(RS ~ SLG, baseball2002)
summary(m2)

m3 <- lm(RS ~ OBP, baseball2002)
summary(m3)

m4 <- lm(RS ~ OPS, baseball2002)
summary(m4)

m5 <- lm(RS ~ SLG + OBP, baseball2002)
summary(m5)
