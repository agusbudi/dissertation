## RANDOMNESS
#simple linear : lm(dist ~ speed, data=cars)
#multiple linear regression
Outlook <- c(1, 1, 1, 1, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0)
Temperature <- c( 2, 0, 1, 2, 1, 0, 0, 1, 1, 2, 2, 1, 0, 1)
Humidity <- c( 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0)
Wind <- c( 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1)
Play <- c( 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1)

weather <- data.frame(cbind(Outlook, Temperature, Humidity, Wind, Play))
multilm <- lm(formula = Play ~ Outlook + Temperature + Humidity + Wind, data= weather)

cor(weather$Play,weather)
predict(multilm,weather[,1:4])
#0.5578035 ~ 1

#new condition:  rain	cool	high	false
newcase <- data.frame(Outlook=2, Temperature=0, Humidity=1, Wind=0)
predict(multilm,newcase)

#random 1

rand1= c(1,2,4,5,7,8,9,10,11,14)
weather1 <- weather[rand1,]
multilm1 <- lm(formula = Play ~ Outlook + Temperature + Humidity + Wind, data= weather1)
predict(multilm1,newcase)
#0.2857024  ~ 0

#random 2
rand2= c(1,3,5,6,7,9,11,12,13,14)
weather2 <- weather[rand2,]
multilm2 <- lm(formula = Play ~ Outlook + Temperature + Humidity + Wind, data= weather2)
predict(multilm2,newcase)
#0.6698472 ~ 1




##FUZZINESS
#numeric Data
Outlook<-c(1, 1, 1, 1, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0)
Temperature<-c(83, 64, 72, 81, 70, 68, 65, 75, 71, 85, 80, 72, 69, 75)
Humidity<-c(86, 65, 90, 75, 96, 80, 70, 80, 91, 85, 90, 95, 70, 70)
Wind<-c(0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1)
Play<-c(1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 1)


weather <- data.frame(cbind(Outlook, Temperature, Humidity, Wind, Play))
multilm <- lm(formula = Play ~ Outlook + Temperature + Humidity + Wind, data= weather)

cor(weather$Play,weather)
predict(multilm,weather[,1:4])
#0.5578035 ~ 1

#new condition:  rain	cool	high	false
newcase <- data.frame(Outlook=2, Temperature=64, Humidity=85, Wind=0)
predict(multilm,newcase)
#0.8453938 ~ 1


##ROUGHNESS

#new condition:  sunny	mild	high	false
newcase <- data.frame(Outlook=0, Temperature=79.4145, Humidity=73.25, Wind=1)
predict(multilm,newcase)


##NONSPECIFICITY
Swim<- c(1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1)

weatherSwim <- data.frame(cbind(Outlook, Temperature, Humidity, Wind, Swim))
multilm4 <- lm(formula = Swim ~ Outlook + Temperature + Humidity + Wind, data= weatherSwim)

cor(weatherSwim$Swim,weatherSwim)
predict(multilm4,weatherSwim[,1:4])
#0.5578035 ~ 1


#new condition:  rain	mild	normal	false
newcase <- data.frame(Outlook=2, Temperature=77, Humidity=80, Wind=0)
#swim
predict(multilm4,newcase)

#playTennis
predict(multilm,newcase)