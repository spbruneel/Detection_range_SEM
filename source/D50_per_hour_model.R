#Determine D50 for every hour

source("source/Libraries.R")
source("source/Functions.R")

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df$hours=hour(df$timebin)
df$perc=df$perc/100

df.temp=df[which(df$timebin==df$timebin[20] & df$signal.direction=="upstream"),]
df.temp=df.temp[-which(df.temp$station_name_submitting=="Rt6"),]
modelD50<-lm(perc~poly(distm50,5),data=df.temp)
#modelD50<-glm(perc~distm50,data=df.temp,family="binomial")
summary(modelD50)
plot(modelD50)
#plot(modelD50)
df.temp$pred=predict(modelD50)
df.temp$res=residuals(modelD50)
plot(df.temp$distm50,df.temp$perc)
plot(df.temp$distm50,df.temp$pred)
plot(df.temp$distm50,df.temp$res)