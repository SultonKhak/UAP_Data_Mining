#import
animalData <- read.csv("AnimalData.csv")
animalZooLocation <- read.csv("AnimalZooLocation.csv")
View(animalData)
View(animalZooLocation)

#1a
dat1 <- animalData
dat1[dat1$aquatic==0,]$aquatic = "Non Aquatic"
dat1[dat1$aquatic==1,]$aquatic = "Aquatic"
temp <- table(dat1$aquatic)

barplot(
  temp,
  col = c("red","cyan"),
  xlab = "Type",
  ylab = "Count",
  main = "Aquatic and Non Aquatic Animal comparison"
)
legend("topright", legend = c("Aquatic","Non Aquatic"), cex = 0.7, fill = c("red","cyan"))


#1b
dat_backbone <- animalData[animalData$backbone>0, ]
dat.dat_backbone <- table(dat_backbone$predator)
pie(
  dat.dat_backbone,
  data.name <- paste(c("Non Predator","Predator"), dat.dat_backbone, "Species"),
  main = "Predatorial Animal Percentage Statistic",
  col = c("red", "cyan")
)


#1c
dat_legs <- animalData[animalData$legs != 5 & animalData$legs !=8, ]
data_temp <- table(dat_legs$legs)

plot(
  data_temp,
  main = "Animal Leg Count Statistics",
  col = "red",
  type = "p",
  xlim = c(0,6),
  xlab = "Leg Count",
  ylim = c(0,40),
  ylab = "Frequency"
)
  

#2a
data_location <- merge(animalData,animalZooLocation, by = "animal_name")
data_location

data_location <- data_location[data_location$legs != 5,]
data_location <- data_location[data_location$zoo_location != "",]
data_location <- data_location[data_location$venomous == 1,]
data_location <- data_location[!duplicated(data_location),]


#2b
data_transformation <- split(data_location$animal_name,data_location$zoo_location)
data_transformation
View(data_transformation)


install.packages("arules")
library(arules)

#2c
data_location <- unique(data_location)
frequent_Species <- apriori(
  data_transformation,
  parameter = list(supp = 0.05, target = 'frequent itemsets')
)
inspect(frequent_Species)

data_association_rules <- ruleInduction(frequent_Species, confidence = 0.5)
inspect(data_association_rules)



  
  
  
  