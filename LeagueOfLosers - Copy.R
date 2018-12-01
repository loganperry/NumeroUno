```{r}
# all players in 2017 Worlds

player<-read.table(header=TRUE,stringsAsFactors=FALSE,text="
name team position
CuVee SamsungGalaxy Top
Ambition SamsungGalaxy Jungle
Crown SamsungGalaxy Mid
Ruler SamsungGalaxy ADC
CoreJJ SamsungGalaxy Support
Untara SKTelecomT1 Top
Blank SKTelecomT1 Jungle
Faker SKTelecomT1 Mid
Bang SKTelecomT1 ADC
LetMe RoyalNeverGiveUp Top
Mlxg RoyalNeverGiveUp Jungle
xiaohu RoyalNeverGiveUp Mid
957 TeamWE Top
Condi TeamWE Jungle
xiye TeamWE Mid
Mystic TeamWE ADC
Ben TeamWE Support
Khan LongzhuGaming Top
Peanut LongzhuGaming Jungle
Bdd LongzhuGaming Mid
PraY LongzhuGaming ADC
GorillA LongzhuGaming Support
Alphari MisfitsGaming Top
Maxlore MisfitsGaming Jungle
PowerOfEvil MisfitsGaming Mid
IgNar MisfitsGaming Support
sOAZ Fnatic Top
Broxah Fnatic Jungle
Caps Fnatic Mid
Rekkles Fnatic ADC
Jesiz Fnatic Support
Impact Cloud9 Top
Contractz Cloud9 Jungle
Jensen Cloud9 Mid
Sneaky Cloud9 ADC
Smoothie Cloud9 Support
")
# players where the webscraper doesn't work because of spaces within name
#'Hans sama' MisfitsGaming ADC 
#'Wolf (Lee Jae-wan)' SKTelecomT1 Support
#'Uzi (Jian Zi-Hao)' RoyalNeverGiveUp ADC
#'ming (Shi Sen-Ming)' RoyalNeverGiveUp Support


nplayers<-dim(player)[1]

# example to figure out the webscraper
#url<-"http://lol.gamepedia.com/Special:RunQuery/MatchHistoryPlayer?MHP%5Bname%5D=Faker&MHP%5Blimit%5D=100&MHP%5Btext%5D=Yes&wpRunQuery=true"
# sadly, it appears they think people are stealing their data to create their own webpages and stealing their web traffic

# this is the data I obtained when I ran the webscraper code 14 March 2018
all.players<-read.table("http://grimshawville.byu.edu/eSports2017.dat",header=TRUE)
```

```{r}
set.seed(2319)
nrow(all.players) #3881 rows
train <- sample(x = 1:3881, 3100) #choose 3100 because it's about 80%
train.data <- all.players[train,]
test.data <- all.players[-train,]

summary(train.data) #compare to make sure they aren't super different
summary(test.data)

#EDA for TIME with table of summary statistics
summary(train.data$Time)
sd(train.data$Time)

#create boxplots for Kills, Assists, Deaths, Time
par(mfrow = c(2, 2))
boxplot(Kills ~ Win, data = train.data, main = "Kills")
boxplot(Assists ~ Win, data = train.data, main = "Assists")
boxplot(Deaths ~ Win, data = train.data, main = "Deaths")
boxplot(Time ~ Win, data = train.data, main = "Time")

#Model
#response variable is Win = 1 if player's team wins

#logit(Win=1) = beta0 + beta1 Kills + beat2 Deaths + beta3 Assists + beta4 Time
out.LOL <- glm(Win ~ Kills + Deaths + Assists + Time, data = train.data, family = binomial) #binomial because Win is either a 0 or 1

summary(out.LOL) 
confint(out.LOL)

#What does 0.5 for Kills represent
exp(0.4308)
exp(confint(out.LOL))[-1,] #1.5 estimated increase in odds of winning by 55% for increasing kills by 1

#demonstrate the effect of kills on P(Win) ... holding all else at median values
x.star <- data.frame(Kills = seq(0, 10, length = 100), Deaths = 2, Assists = 6, Time = 34) #do sequence to get a smooth curve
plot(x.star$Kills, predict(out.LOL, newdata = x.star, type = "response"), 
     type = "l",
     ylim = c(0, 1),
     xlab = "Kills",
     ylab = "P(Win) with all other factors at median") #for some reason response means prediction
#do this for everything else: Deaths, Assists, Time
#Deaths
x.star2 <- data.frame(Deaths = seq(0, 10, length = 100), Kills = 2, Assists = 6, Time = 34)
plot(x.star2$Deaths, predict(out.LOL, newdata = x.star2, type = "response"),
     type = "l",
     xlab = "Deaths",
     ylab = "P(Win) with all other factors at median")
#Assists
x.star3 <- data.frame(Assists = seq(0, 15, length = 100), Kills = 2, Deaths = 2, Time = 34)
plot(x.star3$Assists, predict(out.LOL, newdata = x.star3, type = "response"),
     type = "l",
     xlab = "Assists",
     ylab = "P(Win) with all other factors at median")
#Time
x.star4 <- data.frame(Time = seq(10, 60, length = 100), Kills = 2, Assists = 6, Deaths = 2)
plot(x.star4$Time, predict(out.LOL, newdata = x.star4, type = "response"),
     type = "l",
     xlab = "Time",
     ylab = "P(Win) with all other factors at median")

#We should do a hypothesis test to check for significance
#Z-test gives us the p-value for summary(out.LOL)
#We will do a confidence interval so our increase is from 45% to 63%
#There is a statistically significant effect of time on winning after adjusting for kills, deaths, and assists (p-value < 0.001). For each additional minute played, we estimate a decrease of 0.08 or 8% in the odds of winning and holding all else constant (95% CI 7%, 10%)
#Time comes from 1 - exp(coef(out.LOL)[-1])
1 - exp(coef(out.LOL)[-1])

#Prediction
predict(out.LOL, newdata = data.frame(Kills = 2, Deaths = 3, Assists = 8, Time = 40), type = "response") #use response for prediction

faker.logit <- predict(out.LOL, newdata = data.frame(Kills = 2, Deaths = 3, Assists = 8, Time = 40), type = "link", se.fit = TRUE)
logit.L <- faker.logit$fit - 1.96 * faker.logit$se.fit
logit.U <- faker.logit$fit + 1.96 * faker.logit$se.fit
faker.phat.L <- 1 / (1 + exp(-logit.L))
faker.phat.U <- 1 / (1 + exp(-logit.U))

ambition.logit <- predict(out.LOL, newdata = data.frame(Kills = 2, Deaths = 2, Assists = 14, Time = 40), type = "link", se.fit = TRUE)
logit.L <- ambition.logit$fit - 1.96 * ambition.logit$se.fit
logit.U <- ambition.logit$fit + 1.96 * ambition.logit$se.fit
ambition.phat.L <- 1 / (1 + exp(-logit.L))
ambition.phat.U <- 1 / (1 + exp(-logit.U))

#Prediction Performance
#classification
#choosing 0.5 as cutoff
yhat.train <- as.numeric(predict(out.LOL, type = "response") > 0.5) 
table(yhat.train, train.data$Win) #first row is the prediction, second row is actual

#one way to judge fit is based on the misclassification rate aka were you wrong
#in our case is would be (172 + 218) /3000 = 0.13

#Sensitivity is the proportion of true positives
# = said they would win and won / 
1804 / (171 + 1804) #the bottom row

#Specefiticy
#said they would lose and lost / pred lost + actually lost
906 / (906+218)


#Prediction Performance
#classification
#choosing 0.4 as cutoff
yhat.train2 <- as.numeric(predict(out.LOL, type = "response") > 0.4) 
table(yhat.train2, train.data$Win) #first row is the prediction, second row is actual

#one way to judge fit is based on the misclassification rate aka were you wrong
#in our case is would be (172 + 218) /3000 = 0.13

#Sensitivity is the proportion of true positives
# = said they would win and won / 
1857 / (119 + 1857) #2nd column

#Specefiticy
#said they would lose and lost / pred lost + actually lost
841 / (841+283)

#Create the ROC curve
library(ROCR)
train.pred <- prediction(predict(out.LOL, type = "response"), train.data$Win) #prediction(probabilities, true values)
train.perf <- performance(train.pred, measure = "tpr", x.measure = "fpr") #tpr = true positive rate, fpr = false positive rate
plot(train.perf, xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve") 

#AUC
performance(train.pred, measure = "auc")

#Compare to the test data (out of sample validation)
test.pred <- prediction(predict(out.LOL, newdata = test.data, type = "response"), test.data$Win)
test.perf <- performance(test.pred, measure = "tpr", x.measure = "fpr")

#plot both curves
plot(train.perf, xlab = "1 - Specificity", ylab = "Sensitivity", main = "ROC Curve") 
plot(test.perf, col = "red", add = TRUE)
abline(0, 1, col = "gray") #worst case scenario line abline(intercept, slope)

performance(test.pred, measure = "auc") #the AUC went up, but that's unusual
```

