#Develop loess models for average noise and tilt angle

source("source/Libraries.R")
source("source/Functions.R")

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df$hours=hour(df$timebin)

df$timebin_station=paste(df$timebin,df$station_name)
df=df[ !duplicated(df$timebin_station), ]
df$timebin_station<-NULL
df=df[order(df$timebin),]

for (i in unique(df$station_name)){
  df.temp=df[which(df$station_name==i),]
  
  loessMod10_noise <- loess(Average_noise ~ as.numeric(timebin), data=df.temp, span=0.002)
  smoothed10 <- predict(loessMod10_noise,df.temp)
  plot(y=df.temp$Average_noise, x=df.temp$timebin, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Noise")
  lines(smoothed10, x=df.temp$timebin, col="red")
  save(loessMod10_noise,file=paste("./models/loess_noise_models/Loess_noise_model_for_",i,".Rdata",sep=""))
  
  loessMod10_tilt_angle <- loess(Tilt_angle ~ as.numeric(timebin), data=df.temp, span=0.002)
  smoothed10 <- predict(loessMod10_tilt_angle,df.temp)
  plot(y=df.temp$Tilt_angle, x=df.temp$timebin, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Tilt angle")
  lines(smoothed10, x=df.temp$timebin, col="red")
  save(loessMod10_tilt_angle,file=paste("./models/loess_tilt_angle_models/Loess_tilt_angle_model_for_",i,".Rdata",sep=""))
}