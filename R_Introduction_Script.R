# rm(list = ls())

# library(tprstats)
# tprstats::setup()

# setwd("~/Desktop/CODE/stat-decision-making-2024/")

head(Pgh_May_Temps)

hist(Pgh_May_Temps$Temperature)

mean(Pgh_May_Temps$Temperature)

F=Pgh_May_Temps$Temperature

# formula to convert fahrenheit to celsius
C=(5/9)*(F-32)

plot(F,C)

plot(F,C,col="red",xlab="Fahrenheit",ylab="Celsius", main="Fahrenheit to Celsius")

keep2018=subset(Pgh_May_Temps,Pgh_May_Temps$year==2018)

plot(keep2018$Day,keep2018$Temperature)
lines(keep2018$Day,keep2018$Temperature)

