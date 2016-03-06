# Clear memory
rm(list=ls())

library(plyr)
library(dplyr)
library(data.table)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

#Exploratory analysis 

air_train<- read.csv('/Users/Andi/Desktop/airbnb_train.csv')
View(air_train)

#Target variable: country_destination that we predict

#Exploratory data analysis 

na_count <-sapply(air_train, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Predictor: Age 
#Exploratory analysis on age
length(which(air_train$age > 100))
length(which(air_train$age < 18))
air_train$age <- as.factor(air_train$age)
summary(air_train$age)
ggplot(air_train) + geom_bar(aes(x = age),fill="deepskyblue")
#sz??veges elemz??s

#taking out outliers
condition1 <- air_train[,6] >=18 & air_train[,6] <=100
air3 <- air_train[condition1,]
View(air3)
air4<- na.omit(air3)
View(air4)

summary(air4)

# Heatmap on age and date of accounts dreated
require(data.table)
require(ggplot2)
require(reshape2)
require(RColorBrewer)
air_train <- as.data.table(air_train)
air_train[, age:= as.numeric(age)]
air_train$date_account_created <- as.Date(air_train$date_account_created)
heatmapData <- as.matrix(table(air_train$age,air_train$date_account_created)^.2)
p <- qplot(x=Var2, y=Var1, data=melt(heatmapData), fill=value, geom="tile", 
           xlab = "Date Account Created", ylab = "User Age", 
           main = "Number of Accounts Created Over Time Across User Ages")
p <- p + scale_x_discrete(breaks=levels(as.factor(air_train$date_account_created))[c(TRUE, rep(FALSE, 90))], 
                          labels=levels(as.factor(air_train$date_account_created))[c(TRUE, rep(FALSE, 90))])
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=0.5, size=10))
p <- p + scale_fill_gradient(low="white", high="orangered")
print(p)

# Age has a relatively large number of NA values therefore I'm going to drop the  column from the dataset

air_train <- subset(air_train, select = -c(age))

#Predictor: signup_method
factor(air_train$signup_method) #basic, facebook, google
air_train %>% group_by(country_destination, `signup_method`) %>% 
  summarize(n = n()) %>% as.data.frame

ggplot(air_train, aes(x=country_destination, fill=signup_method)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

air_train2 <- air_train %>%
  filter(country_destination != "NDF")

##By taking out the NDF country destination:
ggplot(air_train2, aes(x=country_destination, fill=signup_method)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

#Predictor: gender
air_train[,.N, by=gender] #-unknown-, FEMALE, MALE, OTHER

air_train2 %>% group_by(country_destination, `gender`) %>% 
  summarize(n = n()) %>% as.data.frame

air_train2 <- air_train2 %>%
  filter(gender != "-unknown-")

air_train2 <- air_train2 %>%
  filter(gender != "OTHER") #taking out the "other" value as it is not significant from an analysis perspective

air_train2 %>% group_by(country_destination, `gender`) %>% 
  summarize(n = n()) %>% as.data.frame

ggplot(air_train2, aes(x=country_destination, fill=gender)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

#Predictor: signup_app
#Android, Moweb, Web, IOS
air_train2[,.N, by=signup_app]

air_train2 %>% group_by(country_destination, `signup_app`) %>% 
  summarize(n = n()) %>% as.data.frame

ggplot(air_train2, aes(x=country_destination, fill=signup_app)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

##Predictor: language
air_train2[,.N, by=language]
ggplot(air_train2, aes(x=country_destination, fill=language)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

##Predictor: affiliate_provider
air_train2[,.N, by=affiliate_provider]

air_train2 %>% group_by(country_destination, `affiliate_provider`) %>% 
  summarize(n = n()) %>% as.data.frame 

ggplot(air_train2, aes(x=country_destination, fill=affiliate_provider)) +
  geom_bar(position="dodge", colour="black") +
  scale_colour_manual(values="rainboww")

#Predictor: timestamp_first_active

air_train2$timestamp_first_active <- strptime(air_train2$timestamp_first_active,format="%Y%m%d%H%M%S")
air_train2 <- data.table(air_train2)
air_train2[,diff_booking := as.Date(date_first_booking) - as.Date(date_account_created)]


air_train2[,.N, by=diff_booking]
air_train2[diff_booking ]

#Predictor: date first booking

airb2 <- airb2 %>%
  mutate(
    date_first_booking = as.date(date_first_booking))

summary(airb2$date_first_booking)

airb2 <- airb2 %>% filter(date_first_booking== "")

airb2 %>% group_by(country_destination, `date_first_booking`) %>% 
  summarize(n = n()) %>% as.data.frame #this does not tell us much

#  Target variable: Country_destination

ggplot(air_train2, aes(x = country_destination)) + geom_bar(fill='deepskyblue')


#delete blank cells
air_train2[air_train==""] <- NA
air_train2 <- na.omit(air_train)

set.seed(123)
N <- nrow(air_train2)
idx_train <- sample(1:N,N/2)
idx_valid <- sample(base::setdiff(1:N, idx_train), N/4)
idx_test <- base::setdiff(base::setdiff(1:N, idx_train),idx_valid)
d_train <- air_train2[idx_train,]
d_valid <- air_train2[idx_valid,]
d_test  <- air_train2[idx_test,]

save(air_train2,file="airbnb.csv")

library(h2o)

h2o.init(max_mem_size = "4g", nthreads = -1)   ## starts Java server (R connects via REST)

write.csv(airbnb, 'airbnb.csv', row.names = FALSE)
airbnb.hex <- h2o.uploadFile('airbnb.csv', destination_frame = 'airbnb')
View(airbnb.hex)
airbnb.hex$country_destination <- as.factor(airbnb.hex$country_destination)


dx_train <- as.h2o(d_train)  ## uploads data to H2O
dx_train$country_destination <- as.factor(dx_train$country_destination)
dx_valid <- as.h2o(d_valid)
dx_valid$country_destination <- as.factor(dx_valid$country_destination)
dx_test <- as.h2o(d_test)
dx_test$country_destination <- as.factor(dx_test$country_destination)

h2o.ls()

#K-Means

airbnb.kmeans<- h2o.kmeans(training_frame = airbnb.hex, k = 12, x = 1:4)
summary(airbnb.kmeans)




#Random Forest

airbnb.rf <- h2o.randomForest(
  x = setdiff(names(airbnb.hex), 'country_destination'),
  y = 'country_destination',
  training_frame = 'd_train',
  validation_frame = 'd_valid')

summary(airbnb.rf)

h2o.auc(airbnb.rf) 

# GBM
airbnb.gbm <- h2o.gbm(
  x = setdiff(names(airbnb.hex), 'country_destination'),
  y = 'country_destination',
  training_frame = 'd_train',
  validation_frame = 'd_valid',
  model_id = 'airbnb.gbm')

summary(airbnb.gbm)

plot(airbnb.gbm)

h2o.auc(airbnb.gbm)
        
#GBM with cross validation 

  airbnb1.gbm <- h2o.gbm(x = 2:ncol(d_train), y = 1, 
                training_frame = 'd_train', 
                max_depth = 15, ntrees = 500, learn_rate = 0.01, nbins = 100,
                nfolds = 5,
                stopping_rounds = 3, stopping_tolerance = 1e-3)


# Neural Network 

system.time({
  airbnb.nn <- h2o.deeplearning(x = 2:ncol(dx_train), y = 1, 
                         training_frame = dx_train, validation_frame = dx_valid,
                         activation = "Rectifier", hidden = c(100,100), epochs = 50,
                         stopping_rounds = 3, stopping_tolerance = 0)})

#Test set
model_perf_rf<- h2o.performance(airbnb.rf,dx_test)
model_perf_gbm<- h2o.performance(airbnb.gbm,dx_test)
model_perf_gbm1<- h2o.performance(airbnb1.gbm,dx_test)
model_perf_nn<- h2o.performance(airbnb.nn,dx_test)



## bye
h2o.shutdown()
Y




