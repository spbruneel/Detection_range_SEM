#Outlier_removal
df.agg<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df.bin<-read.csv("./data/internal/telemetry/databin_zeros_added_with_env.csv")

NoCorr2<-df.agg
for (h in c("Average_noise","Tilt_angle","q.melle.abs","water.height.schoonaarde","tidal.range.HWLW.schoonaarde","Temperature","cond.schellebelle","wind.v.liedekerke","wind.r.liedekerke","turb.schellebelle")){
  NoCorr2[,h]=as.numeric(NoCorr2[,h])
    Q3<-quantile(NoCorr2[,h],0.75)
    Q1<-quantile(NoCorr2[,h],0.25)
    IQR<-(Q3-Q1)
    left<-(Q1-(1.5*IQR))
    right<-(Q3+(1.5*IQR))
    NoCorr2[,h][which(NoCorr2[,h]<= left || NoCorr2[,h]>= right)]="OL"
}
NoOutl=NoCorr2[which(apply(NoCorr2, 1, function(r) any(r %in% c("OL")))==FALSE),] #No outliers for data_agg


NoCorr2<-df.bin
for (h in c("Average_noise_receiving","Tilt_angle_receiving","q.melle.abs","water.height.schoonaarde","tidal.range.HWLW.schoonaarde","temp.schellebelle","cond.schellebelle","wind.v.liedekerke","wind.r.liedekerke","turb.schellebelle")){
  NoCorr2[,h]=as.numeric(NoCorr2[,h])
  Q3<-quantile(NoCorr2[,h],0.75)
  Q1<-quantile(NoCorr2[,h],0.25)
  IQR<-(Q3-Q1)
  left<-(Q1-(1.5*IQR))
  right<-(Q3+(1.5*IQR))
  NoCorr2[,h][which(NoCorr2[,h]<= left || NoCorr2[,h]>= right)]="OL"
}
NoOutl=NoCorr2[which(apply(NoCorr2, 1, function(r) any(r %in% c("OL")))==FALSE),] #No outliers for data_bin

which(is.na(df.bin$Average_noise_receiving)==TRUE)
which(is.na(df.bin$Tilt_angle_receiving)==TRUE)
which(is.na(df.bin$q.melle.abs)==TRUE)
which(is.na(df.bin$water.height.schoonaarde)==TRUE)
which(is.na(df.bin$tidal.range.HWLW.schoonaarde)==TRUE)
which(is.na(df.bin$temp.schellebelle)==TRUE)
which(is.na(df.bin$cond.schellebelle)==TRUE)
which(is.na(df.bin$wind.v.liedekerke)==TRUE)
which(is.na(df.bin$O2.schellebelle)==TRUE)
which(is.na(df.bin$neerslag.zele.5min)==TRUE)