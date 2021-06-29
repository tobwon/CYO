## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, error = FALSE, warning = FALSE, message = FALSE, comment=NA)
options(scipen = 100, digits = 2)


## ----RMSE--------------------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
  }


## ----London Housing Prices Data, results = FALSE-----------------------------------------------------
# Load Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(PostcodesioR)) install.packages("PostcodesioR", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

# Download London housing dataset
dl <- tempfile()
download.file("https://github.com/tobwon/CYO/raw/main/London.csv.zip", dl)
data<-unz(dl, "London.csv")
data<-read.csv(data, header = TRUE, sep = ",")
data<-as.data.frame(data)
rm(dl)

# Add borough data and verify location of properties to by using postcode with PostcodesioR package
# This will take some time and internet connection is needed
x<-1:3480
geo_info<-sapply(x, function(n){
  postcode_lookup(as.character(data[n,11]))
             })
geo_info<-as.data.frame(t(geo_info))

# Tidy up the London housing data-set and convert the varibles to appropriate types
temp_data<-data%>%
  mutate(price=as.numeric(Price),
         type=as.factor(House.Type),
         area=as.numeric(Area.in.sq.ft),
         bedroom=as.numeric(No..of.Bedrooms),
         bathroom=as.numeric(No..of.Bathrooms),
         reception=as.numeric(No..of.Receptions),
         ID = row_number())%>%
  select(ID, price, type, area, bedroom, bathroom, reception)

# Tidy up the postcode data-set and convert the varibles to appropriate types
temp_info<-as.data.frame(geo_info)%>%
  mutate(postcode=as.character(postcode),
         region=as.character(region),
         borough=as.character(admin_district),
         ID = row_number())%>%
  select(ID, postcode, region, borough)
  
# Tidy up the data-set by removing properties not in London and simplify the property types to "Apartment", "House" and "Penthouse" 
london <- left_join(temp_data, temp_info, by="ID")%>%
  filter(str_detect(region, "London"))
london$type<-str_replace_all(london$type, c("New development"="Apartment", "Flat / Apartment"="Apartment", "Bungalow"="House", "Mews"="House", "Studio"="Apartment", "Duplex" = "House"))%>%as.factor()

# Convert to price per square feet
london <- london%>%mutate(ppsf=price/area)

# Download London unemployment data
temp = tempfile(fileext = ".xlsx")
download.file("https://data.london.gov.uk/download/employment-rates-by-ethnicity/cf8a5d62-6918-4046-a53b-627ef5dae54e/employment-rate-ethnic-borough.xlsx", destfile=temp, mode='wb')

# Remove un-needed data and tidy up the unemployment data-set
data3 <- read_xlsx(temp, sheet=4)
data3 <-as.data.frame(data3)
london_unemployment<-data3[-c(1:3, 36:57), c(2, 3, 4, 7, 8, 11, 12, 15, 16, 19, 20, 23, 24, 27, 28)]
london_unemployment[london_unemployment == "!"] <- "0"
london_unemployment[london_unemployment == "#"] <- "0"
london_unemployment[, c(2:15)] <- sapply(london_unemployment[, c(2:15)], as.numeric)

# Replace City of London with Hackeny
london$borough<-str_replace_all(london$borough, c("City of London"="Hackney"))

# Calculate the unemployment rate in different boroughs
london_unemployment$population<-london_unemployment$...4+london_unemployment$...8+london_unemployment$...12+london_unemployment$...16+london_unemployment$...20+london_unemployment$...24+london_unemployment$...28
london_unemployment$employment<-london_unemployment$`working age employment rate - white`+london_unemployment$`working age employment rate - ethnic minority`+london_unemployment$`working age employment rate for all mixed ethnic group`+london_unemployment$`working age employment rate for all Indians`+london_unemployment$`working age employment rate for all Pakistanis/Bangladeshis`+london_unemployment$`working age employment rate for all Black or black British`+london_unemployment$`working age employment rate for all other ethnic group`
london_unemployment$unemployment<-london_unemployment$population-london_unemployment$employment
london_unemployment$unemployment_rate<-london_unemployment$unemployment/london_unemployment$population
london_unemployment<-london_unemployment%>%rename(borough=...2)%>%select(borough, population, unemployment_rate)

# Download London crime data
data2 <- read.csv("https://data.london.gov.uk/download/recorded_crime_summary/d2e9ccfc-a054-41e3-89fb-53c2bc3ed87a/MPS%20Borough%20Level%20Crime%20%28most%20recent%2024%20months%29.csv", header = TRUE, sep = ",")

# Tidy up the London crime data-set
data2 <- as.data.frame(data2)
london_crime<-data2%>%mutate(sum=rowSums(.[4:27]))%>%select(c("LookUp_BoroughName", "sum"))%>%group_by(LookUp_BoroughName)%>%summarise(sum=sum(sum))%>%rename(borough=LookUp_BoroughName, crime=sum)

# Combine London un-employment and crime data-sets
london_e_c<-left_join(london_crime, london_unemployment, by="borough")

# Calculate crime rate in different boroughs
london_e_c<-london_e_c%>%mutate(crime_rate=crime/population)%>%select(c("borough", "population", "unemployment_rate", "crime_rate"))

# Add population, unemployment rate, and crime rate to "London" data-set
london <-left_join(london, london_e_c, by="borough")
london$borough<-as.factor(london$borough)

# Remove un-needed columns
london<-subset(london, select=c(-ID, -region, -postcode, -price)) 

# Rearrange columns to put ppsf as the first column
london<-london[,c(7,1,2,3,4,5,6,8,9,10)]

# Remove temporary files
rm(temp_data, temp_info, x, london_e_c, london_crime, london_unemployment)



## ----Summary of london-------------------------------------------------------------------------------

# Summary of "london" data-set
summary(london)


## ----Header of london--------------------------------------------------------------------------------

# Display the first 6 rows of "london"
head(london)


## ----Price range boxplot-----------------------------------------------------------------------------

# Boxplot of ppsf
boxplot(london$ppsf, horizontal = TRUE, xlab = "Price per Square Foot (Pounds)", main = "PRICE OF PROPERTIES", ylim=c(0,8000), las=1)



## ----Types of property-------------------------------------------------------------------------------

# Types of property
london%>%group_by(type) %>%
  summarize(count = n(), .groups='drop') %>%
  arrange(desc(count))



## ----Size of Properties------------------------------------------------------------------------------

# Size of Properties Boxplot
boxplot(london$area, horizontal = TRUE, main = "SIZE OF PROPERTIES", xlab = "Area (Sq Ft)")



## ----Bedroom number----------------------------------------------------------------------------------

# Bedroom Boxplot
boxplot(london$bedroom, horizontal = TRUE, main = "BEDROOM NUMBER", xlab = "No of Bedroom")



## ----Borough-----------------------------------------------------------------------------------------

# Property Transactions in Different Boroughs
london%>%group_by(borough) %>%
  summarize(count = n(), .groups='drop') %>%
  arrange(desc(count))


## ----ppsf of property by borough---------------------------------------------------------------------

# ppsf of Property by Borough
boxplot(london$ppsf ~ london$borough, horizontal = TRUE,  ylab= NULL, xlab = "Price per Square Foot (Pounds)", main = "PRICE OF PROPERTY BY BOROUGH", xaxt="n", ylim=c(0,8000), las=1, par(cex.axis=0.45))
axis(1,cex.axis=1)



## ----Price of property by property type--------------------------------------------------------------

# ppsf by Property Type
boxplot(london$ppsf ~ london$type, horizontal = TRUE, ylab = NULL, xlab = "Price per Square Foot (Pounds)", main = "PRICE OF PROPERTY BY PROPERTY TYPE", ylim=c(0,8000), las=1, par(cex.axis=0.8))



## ----Price of property by number of bedroom----------------------------------------------------------

# ppsf by Number of Bedroom
boxplot(london$ppsf ~ london$bedroom, horizontal = TRUE, ylab = "No of Bedroom", xlab = "Price per Square Foot (Pounds)", main = "PRICE OF PROPERTY BY NUMBER OF BEDROOM", ylim=c(0,8000), las=1)



## ----Population--------------------------------------------------------------------------------------

# Population in different boroughs
temp<-london%>%select(borough, population)%>%distinct()
temp[order(temp$population),]

rm(temp)


## ----Unemployment rate-------------------------------------------------------------------------------

# Unemployment Rate in Different Boroughs
temp<-london%>%select(borough, unemployment_rate)%>%distinct()
temp[order(temp$unemployment_rate),]

rm(temp)


## ----Crime rate--------------------------------------------------------------------------------------

# Crime Rate in Different Boroughs
temp<-london%>%select(borough, crime_rate)%>%distinct()
temp[order(temp$crime_rate),]

rm(temp)


## ----Correlation Matrix------------------------------------------------------------------------------

# Produce Correlation Matrix
london_cor<-london
london_cor$type<-as.numeric(london_cor$type)
london_cor$borough<-as.numeric(london_cor$borough)
corlondon<-cor(london_cor)
corrplot(corlondon, method="color")



## ----Splitting "london" data-set---------------------------------------------------------------------

# Splitting "london" data-set into traning set and test set.  Test set is 10% of "london" data-set
set.seed(2)
test_index <- createDataPartition(london$ppsf, times = 1, p = 0.1, list = FALSE)
train_set <- london[-test_index,]
test_set <- london[test_index,]

# Remove temporary file
rm(test_index) 


## ----Baseline model----------------------------------------------------------------------------------
##################
# Baseline Model #
##################

mu<-mean(train_set$ppsf)
rmse0<-RMSE(test_set$ppsf, mu)


## ----Model 1A Linear Regression, results = FALSE-----------------------------------------------------

#######################################################
# Model 1A Linear Regression Without Demographic Data #
#######################################################

# Linear Regression Computation
lr <- lm(ppsf ~ type+area+bedroom+bathroom+reception+borough, data = train_set)
model1a <- predict(lr, newdata = test_set)

# Compute RMSE for Model 1A
rmse1a<-RMSE(test_set$ppsf, model1a)


## ----Model 1B Decision Tree, results = FALSE---------------------------------------------------------

###################################################
# Model 1B Decision Tree Without Demographic Data #
###################################################

# Decision Tree Computation
train_rpart1b <- rpart(ppsf ~ type+area+bedroom+bathroom+reception+borough, data = train_set)

# Split text display
split.fun <- function(x, labs, digits, varlen, faclen)
{
# replace commas with space
labs <- gsub(",", " ", labs)
for(i in 1:length(labs)) {
  
# split labs[i] into multiple lines
labs[i] <- paste(strwrap(labs[i], width = 15), collapse = "\n")
}
labs
}

rpart.plot(train_rpart1b, faclen = 0, type=5, yesno=T, clip.facs = TRUE, under = T, digits=-3, extra=101, cex=0.35, split.fun = split.fun)

# Prune the Tree
pfit1b<- prune(train_rpart1b, cp=train_rpart1b$cptable[which.min(train_rpart1b$cptable[,"xerror"]),"CP"])
rpart.plot(pfit1b, faclen = 0, type=5, yesno=T, clip.facs = TRUE, under = T, digits=-3, extra=101, cex=0.5, split.fun = split.fun)

model1b <- predict(pfit1b, newdata=test_set)

# Compute RMSE for Model 1B
rmse1b<-RMSE(test_set$ppsf, model1b)


## ----Model 1C Ramdon Forest--------------------------------------------------------------------------

###############################################################
# Model 1C Random Forest Without Demographic Data Computation #
###############################################################

# Random Forest Computation
rf<-randomForest(ppsf~type+area+bedroom+bathroom+reception+borough, data=train_set)
model1c <- predict(rf, newdata=test_set)
importance  <- importance(rf)
varImpPlot(rf)

# Compute RMSE for Model 1C
rmse1c<-RMSE(test_set$ppsf, model1c)


## ----Model 2A Linear Regression, results = FALSE-----------------------------------------------------

#######################################################
# Model 2A  - Linear Regression With Demographic Data #
#######################################################

# Linear Regression Computation
lr <- lm(ppsf ~ ., data = train_set)
model2a <- predict(lr, newdata = test_set)

# Compute RMSE for Model 2A
rmse2a<-RMSE(test_set$ppsf, model2a)


## ----Model 2B Decision Tree, results = FALSE---------------------------------------------------------

################################################
# Model 2B Decision Tree With Demographic Data #
################################################

# Decision Tree Computation
train_rpart2b <- rpart(ppsf ~ ., data = train_set)
rpart.plot(train_rpart2b, faclen = 0, type=5, yesno=T, clip.facs = TRUE, under = T, digits=-3, extra=101, cex=0.5, split.fun = split.fun)

# Prune the Tree
pfit2b<- prune(train_rpart2b, cp=train_rpart2b$cptable[which.min(train_rpart2b$cptable[,"xerror"]),"CP"])
rpart.plot(pfit2b, faclen = 0, type=5, yesno=T, clip.facs = TRUE, under = T, digits=-3, extra=101, cex=0.5, split.fun = split.fun)

model2b <- predict(pfit2b, newdata=test_set)

# Compute RMSE for Model 2B
rmse2b<-RMSE(test_set$ppsf, model2b)


## ----Model 2C Ramdon Forest--------------------------------------------------------------------------

############################################################
# Model 1C Random Forest With Demographic Data Computation #
############################################################

# Random Forest Computation
rf<-randomForest(ppsf~., data=train_set)
model2c <- predict(rf, newdata=test_set)
importance <- importance(rf)
varImpPlot(rf)

# Compute RMSE for Model 2C
rmse2c<-RMSE(test_set$ppsf, model2c)


## ----Results-----------------------------------------------------------------------------------------

#Compile the results into a summary table
summary <- tibble(Model = c("Baseline Model", "Model 1A Linear Regression without Demographic Data", "Model 1B Decision Tree without Demographic Data", "Model 1C Random Forest without Demographic Data", "Model 2A Linear Regression with Demographic Data", "Model 2B Decision Tree with Demographic Data", "Model 2C Random Forest with Demographic Data"), RMSE = c(rmse0, rmse1a, rmse1b, rmse1c, rmse2a, rmse2b, rmse2c), "Difference to Baseline Model" = c(rmse0 - rmse0, rmse0 - rmse1a, rmse0 - rmse1b, rmse0 - rmse1c, rmse0 - rmse2a, rmse0 - rmse2b, rmse0 - rmse2c))
summary

