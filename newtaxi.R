library(data.table)
train <- fread("train.csv")
test_csv <- fread("test.csv")
col_train <- colnames(train)
col_test <- colnames(test_csv)
col_train[!col_train %in% col_test]
train$dropoff_datetime <- NULL
test.trip_duration <- data.frame(test_csv[],trip_duration = rep("None",nrow(test_csv)))
datanew <- rbind(train,test.trip_duration)
str(datanew)

str(datanew)
head(datanew$pickup_datetime)
datanew$pickup_datetime <- as.character(datanew$pickup_datetime)
library(stringr) 
split_DT <- as.data.frame(str_split_fixed(datanew$pickup_datetime," ",2))
datanew1<-cbind(datanew,split_DT)
str(datanew1)
library(data.table)
setnames(datanew1,"V1","pickup_date")
setnames(datanew1,"V2","pickup_time")
datanew1$pickup_date<-as.character(datanew1$pickup_date)
datanew1$pickup_time<-as.character(datanew1$pickup_time)
str(datanew1)
datanew1$pickup_date<-as.Date(datanew1$pickup_date)
time_split <- as.data.frame(str_split_fixed(datanew1$pickup_time,":",3))

time_hr <-time_split[,c("V1")]

time_hr <- as.data.frame(time_hr)
datanew2<-cbind(datanew1,time_hr)
datanew2$time_hr<-as.factor(datanew2$time_hr)
datanew2$pickup_datetime <- NULL
#install.packages('h2o')
datanew2$passenger_count<-scale(datanew2$passenger_count)
datanew2$pickup_longitude<-scale(datanew2$pickup_longitude)
datanew2$pickup_latitude<-scale(datanew2$pickup_latitude)
datanew2$dropoff_longitude<-scale(datanew2$dropoff_longitude)
datanew2$dropoff_latitude<-scale(datanew2$dropoff_latitude)

library(h2o)
h2o.init(nthreads = 3)
datanew2$pickup_time <- NULL
datanew2$trip_duration=as.numeric(datanew2$trip_duration)
train.h20<-as.h2o(datanew2[1:1458644],)
train.h20$vendor_id<-as.factor(train.h20$vendor_id)
train.h20$store_and_fwd_flag<-as.factor(train.h20$store_and_fwd_flag)

model = h2o.deeplearning(x=c(2,3,4,5,6,7,8),y = 9,
                         training_frame = as.h2o(train.h20),
                         activation = 'Rectifier',
                         hidden = c(100,100),
                         epochs = 100,
                         )
y_pred = as.data.frame(y_pred)
y_pred = h2o.predict(model, newdata = as.h2o(test.h20))
 y_pred=cbind(test_csv$id,y_pred)
 setnames(y_pred,"test_csv$id","id")
 setnames(y_pred,"predict","trip_duration")
 mean <- mean(datanew2$trip_duration[1:1458644])
 mean<- round(mean,0)
 y_pred$trip_duration[which(y_pred$trip_duration<0)] <- mean
 write.csv(y_pred, file = "sub_first_taxi.csv", row.names =  F)



library(ggplot2)
suppressPackageStartupMessages()
a<-as.data.frame(a)
ggplot(a,aes(x=as.factor(Var1),fill=Freq))+geom_bar()+xlab("Hour")+ylab("Number of Passengers")+labs(fill="Travelled")

library(data.table)
library(h2o)


time_day <- as.data.frame(as.numeric(datanew2$time_hr))
setnames(time_day,"as.numeric(datanew2$time_hr)","time_hrs")
time_mod2 <- data.frame(time_day= rep("None",nrow(datanew2)))
time_mod2$time_day <- as.character(time_mod2$time_day)


time_mod2$time_day[which(time_day$time_hr>=8 & time_day$time_hr <=11)] <- as.character("M")
time_mod2$time_day[which(time_day$time_hr>=12 & time_day$time_hr <=15)] <- as.character("A")
time_mod2$time_day[which(time_day$time_hr>=16 & time_day$time_hr <=19)] <- as.character("E")
time_mod2$time_day[which(time_day$time_hr>=20 & time_day$time_hr <=22)] <- as.character("N")
time_mod2$time_day[which(time_day$time_hr>=23)] <- as.character("LN")
time_mod2$time_day[which(time_day$time_hr <=2)] <- as.character("LN")
time_mod2$time_day[which(time_day$time_hr>=3 & time_day$time_hr <=7)] <- as.character("EM")


datanew2<-cbind(datanew2,time_mod2)
datanew2$time_hr<-NULL


summary(datanew2)

#install.packages("geosphere")
library(geosphere)
datanew2$distance <- distHaversine(datanew1[,5:6], datanew1[,7:8])/1000

summary(datanew2$distance[which(datanew2$trip_duration<5)])
library(ggplot2)
summary(data_sm$trip_duration)
summary(data_sm$distance)

dt<-ggplot() +
  geom_point(aes(x = datanew2$trip_duration, y = datanew2$distance),
             colour = 'red') 

dt[[1]] + coord_cartesian(ylim= c(0,400))
temp<-datanew2[-(1:nrow(train))]
temp2<-datanew2[1:nrow(train)]
temp2<-temp2[which(temp2$distance<45)]
library(h2o)
h2o.init(nthreads = 3)

temp2$trip_duration=as.numeric(temp2$trip_duration)
train.h20<-as.h2o(temp2)
train.h20$vendor_id<-as.factor(train.h20$vendor_id)
train.h20$store_and_fwd_flag<-as.factor(train.h20$store_and_fwd_flag)

train.h20$time_day<-as.factor(train.h20$time_day)
train.h20$pickup_time<-as.factor(train.h20$pickup_time)
train.h20$pickup_time<-NULL

model = h2o.deeplearning(x=c(2,4,5,6,7,8,12,13),y = 9,
                         training_frame = as.h2o(train.h20),
                         activation = 'Rectifier',
                         hidden = c(60,60),
                         epochs = 100,
)

temp$trip_duration=as.numeric(temp$trip_duration)
test.h20<-as.h2o(temp)
test.h20$vendor_id<-as.factor(test.h20$vendor_id)
test.h20$store_and_fwd_flag<-as.factor(test.h20$store_and_fwd_flag)

test.h20$time_day<-as.factor(test.h20$time_day)
test.h20$pickup_time<-as.factor(test.h20$pickup_time)

y_pred = h2o.predict(model, newdata = as.h2o(test.h20))
y_pred = as.data.frame(y_pred)
y_pred=cbind(test_csv$id,y_pred)
setnames(y_pred,"test_csv$id","id")
setnames(y_pred,"predict","trip_duration")
mean <- mean(temp2$trip_duration)
mean<- round(mean,0)
y_pred$trip_duration[which(y_pred$trip_duration<0)] <- mean
write.csv(y_pred, file = "sub_fifth_taxi.csv", row.names =  F)
