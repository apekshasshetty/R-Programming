#install packages
install.packages("xlsx")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("rworldmap")
# Get the path.
print(getwd())
# Set the path.
setwd("C:/Users/Dhanya/OneDrive/TEAM 3")
# Load the library to get the excel file.
library("xlsx")
# Read the excel file.
data=read.xlsx("R_DATASET.xlsx",sheetIndex=1)
# Convert into csv file.
write.csv(data,"data.csv")
# Reading the csv file.
db=read.csv("data.csv")
# Our source data set.
View(db)
# Regions of various countries.
Region=c("Western Europe","Western Europe","Western Europe","Western Europe","Western Europe",
          "Western Europe","Western Europe","Western Europe","Middle East","Australia and New Zealand",
          "Western Europe","Australia and New Zealand","Western Europe","Western Europe","North America",
          "North America","Western Europe","Central Europe","Western Europe","Western Europe",
          "Middle East","Central Europe","South America","Middle East","Middle East",
          "Southeast Asia","Southeast Asia","Eastern Europe","Western Europe","South America",
          "Central Europe","Eastern Europe","Central Europe","Eastern Europe","Central Europe",
          "Eastern Europe","Central America","South America","Central America","Central Asia",
          "Middle East","Eastern Europe","Eastern Europe","South America","Central America",
          "North America","Eastern Europe","Central Europe","Central America","Middle East",
          "Central Europe","Eastern Africa","Central Asia","East Asia","Central America",
          "Western Europe","South America","Eastern Europe","East Asia","Southeast Asia",
          "Southeast Asia","Eastern Europe","South America","Central Asia","Eastern Europe",
          "South America","Western Europe","Central Asia","South America","Southeast Asia",
          "South America","East Asia","South America","South America","Eastern Europe",
          "South America","Southeast Asia","Central Asia","Middle East","Eastern Europe",
          "East Asia","Eastern Europe","Central Asia","South Asia","Eastern Europe",
          "North Africa","Southeast Asia","Western Africa","Eastern Europe","Eastern Europe",
          "South Africa","West Asia","Western Africa","South Asia","Southeast Asia",
          "North Africa","Western Africa","Eastern Europe","Central Africa","North Africa",
          "South Africa","Western Africa","Western Africa","North Africa","Eastern Europe",
          "Central Africa","Middle East","South America","Western Africa","Middle East",
          "Western Africa","Eastern Europe","Western Africa","Southeast Asia","Western Africa",
          "Eastern Africa","Eastern Africa","Western Africa","Eastern Africa","North Africa",
          "South Asia","Middle East","Western Africa","South Africa","South Africa",
          "Southeast Asia","South Asia","Eastern Africa","Eastern Africa","North Africa",
          "Eastern Africa","Middle East","Western Africa","Middle East","Western Africa",
          "South Asia","South Africa","South Africa","Eastern Africa","Western Africa",
          "South Africa","South Africa","Central Africa","South Africa","Middle East",
          "Central Asia"
         )
# Adding regions to our data set.
new_db=cbind(db,Region)
# Our new data set with regions column.
View(new_db)
# Load the library to use some functions to categorize our data.
library("dplyr")
# Printing total no of regions.
distinct(new_db,Region)
# Creating a continents column.
new_db$Continent=NA
# Data set with continents column.
View(new_db)
# Assigning various regions to their respective continents.
new_db$Continent[which(new_db$Region %in% c("Australia and New Zealand"))]<-"Australia"
new_db$Continent[which(new_db$Region %in% c("North America","Central America"))]<-"North America"
new_db$Continent[which(new_db$Region %in% c("Western Europe","Central Europe" ,"Eastern Europe"))]<-"Europe"
new_db$Continent[which(new_db$Region %in% c("North Africa","Western Africa","Eastern Africa","South Africa","Central Africa"))]<-"Africa"
new_db$Continent[which(new_db$Region %in% c("East Asia","Southeast Asia","South Asia","West Asia","Central Asia","Middle East"))]<-"Asia"
new_db$Continent[which(new_db$Region %in% c("South America"))]<-"South America"
# Our new data set with continents column.
View(new_db)
# Summary of our new data set.
summary(new_db)
# Further categorization of data set according to continents.
hp=aggregate(new_db[,c(2,4)],list(new_db$Continent),mean)
# Happiness scores of continents.
View(hp)
# Top ten countries.
View(head(new_db,10))
# Bottom ten countries.
View(tail(new_db,10))
# Factors affecting happiness score.
hp1=aggregate(new_db[,c(8:13)],list(new_db$Continent),mean)
# Data set of continents with factors.
View(hp1)
# Creating a stacked bar chart showing the impact of various factors on happiness score.
colors=c("Pink","blue","green","yellow","orange","red")
factors=c("GDP","Support","Life","Free","Generous","Corrupt")
cont=c("Africa","Asia","Australia","Europe","North America","South America")
Values=matrix(c(hp1[[2]][[1]],hp1[[2]][[2]],hp1[[2]][[3]],hp1[[2]][[4]],hp1[[2]][[5]],hp1[[2]][[6]],
                hp1[[3]][[1]],hp1[[3]][[2]],hp1[[3]][[3]],hp1[[3]][[4]],hp1[[3]][[5]],hp1[[3]][[6]],
                hp1[[4]][[1]],hp1[[4]][[2]],hp1[[4]][[3]],hp1[[4]][[4]],hp1[[4]][[5]],hp1[[4]][[6]],
                hp1[[5]][[1]],hp1[[5]][[2]],hp1[[5]][[3]],hp1[[5]][[4]],hp1[[5]][[5]],hp1[[5]][[6]],
                hp1[[6]][[1]],hp1[[6]][[2]],hp1[[6]][[3]],hp1[[6]][[4]],hp1[[6]][[5]],hp1[[6]][[6]],
                hp1[[7]][[1]],hp1[[7]][[2]],hp1[[7]][[3]],hp1[[7]][[4]],hp1[[7]][[5]],hp1[[7]][[6]])
                ,nrow=6)
png(file="Factors_stacked_bar_graph.png")
barplot(Values,main="Factors_affecting_happiness_score",names.arg=factors,xlab="Factors",ylab="Score",col=colors)
legend("topright",cont,cex=1.3,fill=colors)
dev.off()
# Load the library to execute more graphs.
library("ggplot2")
# Graph of happiness score vs GDP per capita.
png(file="GDP_per_capita.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..GDP.per.capita))
dev.off()
# Graph of happiness score vs social support.
png(file="Social_Support.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..Social.support))
dev.off()
# Graph of happiness score vs healthy life expectancy.
png(file="Healthy_life_expectancy.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..Healthy.life.expectancy))
dev.off()
# Graph of happiness score vs freedom to make life choices.
png(file="Freedom_to_make_life_choices.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..Freedom.to.make.life.choices))
dev.off()
# Graph of happiness score vs generosity.
png(file="Generosity.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..Generosity))
dev.off()
# Graph of happiness score vs perceptions of corruption.
png(file="Perceptions_of_Corruption.png")
ggplot(data=new_db)+geom_point(mapping = aes(x=Happiness.score,y=Explained.by..Perceptions.of.corruption))
dev.off()
# Plotting a 3D pie chart (Continents).
library(plotrix)
x=c(4.527645,5.360067,7.180950,6.369605,6.375938,5.850085)
labels=c("Africa","Asia","Australia","Europe","North America","South America")
piepercent=round(100*x/sum(x),1)
png(file="Continents_Pie_Chart.png")
pie3D(x,labels=piepercent,main="Continents_Happiness_Score",col=rainbow(length(x)),explode=0.1,border="white")
legend("topright",labels,cex=0.8,fill=rainbow(length(x)))
dev.off()
# Bar graph of Happiness score of the continents.
png(file="Continents_bar_graph.png")
ggplot(hp,aes(x=Group.1,y=Happiness.score,fill=Group.1))+ geom_bar(stat="identity")+ggtitle("Happiness Score of Continent") + ylab("Happiness.score") + xlab("Continent")
dev.off()
# Box plot of happiness score of continents.
png(file="Continents_box_plot.png")
ggplot(new_db,aes(x=Continent,y=Happiness.score,color=Continent)) + geom_boxplot() + ggtitle("Happiness score for Continents")
dev.off()
# Load the library to plot a world map.
library("rworldmap")
# Plot the happiness score of various countries in the world map.
png(file="World_map.png")
d<-data.frame(country = new_db$Country,value = new_db$Happiness.score)
n<-joinCountryData2Map(d, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(n, nameColumnToPlot = "value",mapTitle = "World Map for Happiness score 2022",colourPalette = "terrain")
dev.off()
# THANK YOU.
