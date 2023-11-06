#1b.Wavelet_analysis_and_preprocessing_data

source("source/Libraries.R")
source("source/Functions.R")

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df=df[order(df$timebin),]

for (i in unique(df$RecRec)){
  df.temp=df[which(df$RecRec==i),c("perc","timebin")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.single<-analyze.wavelet(df.temp,my.series="perc",loess=0.75,dt=1)
    save(wave.test.single,file=paste("./models/single_wavelet_data_agg/",i,".Rdata",sep=""))
  }, error=function(e){})
}

df.0m<-df[which(df$distm50==0),]
for (i in unique(df$RecRec)){
  df.temp=df.0m[which(df.0m$RecRec==i),c("Tilt_angle","timebin")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.single<-analyze.wavelet(df.temp,my.series="Tilt_angle",loess=0.75,dt=1)
    save(wave.test.single,file=paste("./models/single_wavelet_data_agg_tilt/",i,".Rdata",sep=""))
  }, error=function(e){})
}

df.0m<-df[which(df$distm50==0),]
for (i in unique(df$RecRec)){
  df.temp=df.0m[which(df.0m$RecRec==i),c("Average_noise","timebin")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.single<-analyze.wavelet(df.temp,my.series="Average_noise",loess=0.75,dt=1)
    save(wave.test.single,file=paste("./models/single_wavelet_data_agg_noise/",i,".Rdata",sep=""))
  }, error=function(e){})
}

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("water.height.schoonaarde","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="water.height.schoonaarde",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_water_height/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("temp.schellebelle","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="temp.schellebelle",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_temp/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("cond.schellebelle","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="cond.schellebelle",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_cond/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("q.melle","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="q.melle",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_q/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("q.melle.abs","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="q.melle.abs",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_q_abs/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("q.melle.magn","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="q.melle.magn",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_q_magn/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

df.temp=df[which(df$RecRec==unique(df$RecRec)[1]),c("wind.v.liedekerke","timebin")]
rownames(df.temp)=df.temp$timebin
tryCatch({
  wave.test.single<-analyze.wavelet(df.temp,my.series="wind.v.liedekerke",loess=0.75,dt=1)
  save(wave.test.single,file=paste("./models/single_wavelet_data_agg_wind/",unique(df$RecRec)[1],".Rdata",sep=""))
}, error=function(e){})

for (i in unique(df$RecRec)){
  df.temp=df[which(df$RecRec==i),c("perc","timebin","water.height.schoonaarde")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.coherence<-analyze.coherency(df.temp,my.pair=c("perc","water.height.schoonaarde"),loess=0.75,dt=1)
    save(wave.test.coherence,file=paste("./models/coherence_wavelet_data_agg_water_height/",i,".Rdata",sep=""))
  }, error=function(e){})
}


for (i in unique(df$RecRec)){
  df.temp=df[which(df$RecRec==i),c("perc","timebin","q.melle")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.coherence<-analyze.coherency(df.temp,my.pair=c("perc","q.melle"),loess=0.75,dt=1)
    save(wave.test.coherence,file=paste("./models/coherence_wavelet_data_agg_q/",i,".Rdata",sep=""))
  }, error=function(e){})
}


for (i in unique(df$RecRec)){
  df.temp=df[which(df$RecRec==i),c("perc","timebin","q.melle")]
  df.temp$q.melle.abs=abs(df.temp$q.melle)
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.coherence<-analyze.coherency(df.temp,my.pair=c("perc","q.melle.abs"),loess=0.75,dt=1)
    save(wave.test.coherence,file=paste("./models/coherence_wavelet_data_agg_q_abs/",i,".Rdata",sep=""))
  }, error=function(e){})
}



for (i in unique(df$RecRec)){
  df.temp=df[which(df$RecRec==i),c("perc","timebin","Average_noise")]
  rownames(df.temp)=df.temp$timebin
  tryCatch({
    wave.test.coherence<-analyze.coherency(df.temp,my.pair=c("perc","Average_noise"),loess=0.75,dt=1)
    save(wave.test.coherence,file=paste("./models/coherence_wavelet_data_agg_noise/",i,".Rdata",sep=""))
  }, error=function(e){})
}
