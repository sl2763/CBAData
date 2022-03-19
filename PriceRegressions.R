lib#Import packages
library("ggplot2")  
library("ggthemes")
library("texreg")
library("tidyverse")
library("htmltools")

#Import file
inputfile<-"Table4Data.csv"
inputfile_import <- read.csv(inputfile, stringsAsFactors = FALSE, header = TRUE)

#check structures
str(inputfile_import)

#Only use the product (UP vs HTST), price, and shelf life columns
producttype<- inputfile_import$Processing
price<- inputfile_import$Price
shelflife<- inputfile_import$Shelf.life
milkfat<- inputfile_import$Product

#rename the in store/online column format
names(inputfile_import)[1]<-"Format"

#assumptions with linear model
#1. independent observations
#2. linear relationship (Scatterplot)
#3. normality of error terms (histogram, qqplot of residuals)
#4. homogeneous variance (scatterplot of the residuals vs the predicted values)

#Basic model with price and shelf life
lm0 = lm(price ~ shelflife, data = inputfile_import) 
plot(lm0)
summary(lm0)
htmlreg(lm0)
htmlreg(lm0, file = "lm0.html")


#Log linear regression
inputfile_import$logPRICE <- log(inputfile_import$Price)
plot(inputfile_import$Shelf.life, inputfile_import$logPRICE)
lm1 <- lm(logPRICE ~ Shelf.life, data = inputfile_import)
plot(lm1)
summary(lm1)

#Look at instore variable
lm2 = lm(price ~ shelflife + I(Format!="In store"), data = inputfile_import) 
summary(lm2)

#Instore + organic variable
lm3 = lm(price ~ shelflife + I(Format!="In store") +I(Type== "Organic"), data = inputfile_import) 
summary(lm3)

#looking at interactions
lm4 = lm(price ~ I(Processing=="Ultra-pasteurized") + I(Format!="In store") +I(Type== "Organic"), data = inputfile_import) 
summary(lm4)


lm5 = lm(price ~ I(Processing=="Ultra-pasteurized") + I(milkfat=="Whole Milk")+ I(Format!="In store") +I(Type== "Organic"), data = inputfile_import) 
summary(lm5)

lm6 = lm(price ~ milkfat + I(Processing=="Ultra-pasteurized") + I(Format!="In store") +I(Type== "Organic"), data = inputfile_import) 
summary(lm6)

lm7 = lm(logPRICE ~ + I(Processing=="Ultra-pasteurized") + I(Format!="In store") +I(Type== "Organic"), data = inputfile_import) 
summary(lm7)


lm8 = lm(price ~ shelflife*I(Format!= "In store"), data = inputfile_import) 
summary(lm8)


#Analyzing results
summary(lm1)
summary(lm3)
summary(lm4)


