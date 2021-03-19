sample <- read.csv("C:/Users/tbish/AppData/Local/Temp/housing_wQthBJ")
housing<-sample[1:500,]
housing$ocean_proximity<-NULL
summary(housing)

#splitting the data into training and test data

set.seed(2)
library(caTools)
split <-sample.split(housing, SplitRatio = 0.9)
split

train<-subset(housing,split="TRUE")
test<-subset(housing,split="FALSE")


# not to consider the columns with low stars so removed households 
Model<-lm(median_house_value~. -households ,data=train)
summary(Model)

#prediction
pred<-predict(Model,test)
pred

#comparing  predicted vs actual values

plot(test$median_house_value, type="l",  lty=1.8,col="blue")
lines(pred ,type="l",col="red")


#Fiding accuracy
rmse<-sqrt(mean(pred-housing$median_house_value)^2)
rmse

#1, is needed to get all columns from the first row
newuser<- housing[1,]
newuser

newpredict<-predict(Model,data.newuser)
newpredict



#getting results based on our model prediction
datafrom<- data.frame( longitude= -122.22,latitude= 37.86 ,housing_median_age =21,total_rooms =7099,total_bedrooms=1106, population=2401, households=1138,median_income=8.3104, median_house_value=358500)
datafrom

newpredict<-predict(Model,data.frame(datafrom))
newpredict
