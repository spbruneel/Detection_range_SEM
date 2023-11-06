source("./source/Libraries.R")
source("./source/Functions.R")

mike11<-read_excel("./data/external/env/env/HIC/201812_MIKE11_Schoonaarde_v2.xlsx")
colnames(mike11)=c("timebin","water.height.schoonaarde.hic","q.schoonaarde")
mike11$timebin<-as.POSIXct(mike11$timebin,format='%Y-%m-%d %H:%M:%S',tz="utc")

#Waterstand Schoonaarde (per minuut: hoogste resolutie)
tij.schoonaarde<-read.table("./data/external/env/env/HIC/Schoonaarde tij_Waterpeil getij/Schoonaarde tij_Waterpeil getij.csv",sep=";",col.names = c("datetime","water.height.schoonaarde","check","abs","AVQ"))[,c(1:3)]
tij.schoonaarde$water.height.schoonaarde<-as.numeric(gsub(",",".",tij.schoonaarde$water.height.schoonaarde))
tij.schoonaarde<-time.cleanup.waterinfo(tij.schoonaarde)
mike11<-left_join(mike11,tij.schoonaarde[,c("timebin","water.height.schoonaarde")],by="timebin")

#waterhoogte Melle (per minuut: hoogste resolutie)
water.height.melle<-read.table("./data/external/env/env/HIC/Melle tij_Waterpeil getij/Melle tij_Waterpeil getij.csv",sep=";",col.names = c("datetime","water.height.melle","check","abs","AVQ"))[,c(1:3)]
water.height.melle$water.height.melle<-as.numeric(gsub(",",".",water.height.melle$water.height.melle))
water.height.melle<-time.cleanup.waterinfo(water.height.melle)
mike11<-left_join(mike11,water.height.melle[,c("timebin","water.height.melle")],by="timebin")

#Debiet Melle (per 5 minuten: hoogste resolutie)
q.melle<-read.table("./data/external/env/env/HIC/Melle tij_Afvoer/Melle tij_Afvoer.csv",sep=";",col.names = c("datetime","q.melle","check","abs","AVQ"))[,c(1:3)]
q.melle$q.melle<-as.numeric(gsub(",",".",q.melle$q.melle))
q.melle<-time.cleanup.waterinfo(q.melle)
mike11<-left_join(mike11,q.melle[,c("timebin","q.melle")],by="timebin")

plot(mike11$water.height.schoonaarde.hic,mike11$water.height.schoonaarde)
plot(mike11$q.schoonaarde,mike11$q.melle)
plot(mike11$q.schoonaarde[which(mike11$q.melle<0)],mike11$q.melle[which(mike11$q.melle<0)])
plot(mike11$q.schoonaarde[which(mike11$q.melle>0)],mike11$q.melle[which(mike11$q.melle>0)])
ggplot(data=mike11,aes(x=timebin,y=q.melle))+geom_line()
ggplot(data=mike11,aes(x=timebin,y=q.schoonaarde))+geom_line()
ggplot(data=mike11,aes(x=timebin,y=water.height.melle))+geom_line()
ggplot(data=mike11,aes(x=timebin,y=water.height.schoonaarde.hic))+geom_line()
ggplot(data=mike11,aes(x=timebin,y=water.height.schoonaarde))+geom_line()

ggplot(data=mike11,aes(x=timebin,y=q.schoonaarde))+geom_line(colour="red")+geom_line(data=mike11,aes(x=timebin,y=q.melle))
ggplot(data=mike11,aes(x=timebin,y=water.height.schoonaarde.hic))+geom_line(colour="red")+geom_line(data=mike11,aes(x=timebin,y=water.height.schoonaarde))
ggplot(data=mike11,aes(x=timebin,y=water.height.schoonaarde.hic))+geom_line(colour="red")+geom_line(data=mike11,aes(x=timebin,y=water.height.melle))
ggplot(data=mike11,aes(x=timebin,y=water.height.schoonaarde))+geom_line(colour="red")+geom_line(data=mike11,aes(x=timebin,y=q.schoonaarde))

mike11=mike11[order(mike11$timebin),]
ccf.h.s.h.m<-ccf(mike11$water.height.schoonaarde,mike11$water.height.melle) #max timelag at -4 (1hour)
ccf.q.s.q.m<-ccf(mike11$q.schoonaarde,mike11$q.melle) #max timelag at -1 (15min)
ccf.h.s.q.s<-ccf(mike11$water.height.schoonaarde,mike11$q.schoonaarde) #max timelag at 8 (2hour)
ccf.h.m.q.m<-ccf(mike11$water.height.melle,mike11$q.melle) #max timelag at 6 (1.15hour)

mike11.temp<-mike11
mike11.temp$q.schoonaarde.lag<-lag(mike11.temp$q.schoonaarde)
mike11.temp$q.melle.lag<-lag(mike11.temp$q.melle)
mike11.temp$water.height.schoonaarde.lag<-lag(mike11.temp$water.height.schoonaarde)
mike11.temp$water.height.melle.lag<-lag(mike11.temp$water.height.melle)
mike11.temp=mike11.temp[-which(is.na(mike11.temp$q.schoonaarde.lag)==TRUE),]
#mike11.temp=mike11.temp[-which(is.na(mike11.temp$q.melle.lag)==TRUE),]
cor(mike11.temp$q.schoonaarde.lag,mike11.temp$q.melle)
cor(mike11.temp$q.schoonaarde,mike11.temp$q.melle)
mike11.temp$q.melle.abs<-abs(mike11.temp$q.melle)

mike11.temp$current.direction.melle=NA
mike11.temp$current.direction.melle[which(mike11.temp$q.melle<=0)]="upstream"
mike11.temp$current.direction.melle[which(mike11.temp$q.melle>0)]="downstream"

plot(mike11.temp$q.schoonaarde,mike11.temp$q.melle)
plot(mike11.temp$q.schoonaarde.lag,mike11.temp$q.melle)
plot(mike11.temp$q.schoonaarde.lag[which(mike11.temp$q.melle<0)],mike11.temp$q.melle[which(mike11.temp$q.melle<0)])
plot(mike11.temp$q.schoonaarde.lag[which(mike11.temp$q.melle>0)],mike11.temp$q.melle[which(mike11.temp$q.melle>0)])
cor(mike11.temp$q.schoonaarde.lag[which(mike11.temp$q.melle<0)],mike11.temp$q.melle[which(mike11.temp$q.melle<0)])
cor(mike11.temp$q.schoonaarde.lag[which(mike11.temp$q.melle>0)],mike11.temp$q.melle[which(mike11.temp$q.melle>0)])

plot(mike11.temp$q.schoonaarde,mike11.temp$q.melle)
plot(mike11.temp$q.schoonaarde,mike11.temp$q.melle.lag)
plot(mike11.temp$q.schoonaarde[which(mike11.temp$q.melle.lag<0)],mike11.temp$q.melle.lag[which(mike11.temp$q.melle.lag<0)])
plot(mike11.temp$q.schoonaarde[which(mike11.temp$q.melle.lag>0)],mike11.temp$q.melle.lag[which(mike11.temp$q.melle.lag>0)])
cor(mike11.temp$q.schoonaarde[which(mike11.temp$q.melle.lag<0)],mike11.temp$q.melle.lag[which(mike11.temp$q.melle.lag<0)])
cor(mike11.temp$q.schoonaarde[which(mike11.temp$q.melle.lag>0)],mike11.temp$q.melle.lag[which(mike11.temp$q.melle.lag>0)])

mike11.temp$water.height.schoonaarde.diff=mike11.temp$water.height.schoonaarde-mike11.temp$water.height.schoonaarde.lag
mike11.temp$water.height.melle.diff=mike11.temp$water.height.melle-mike11.temp$water.height.melle.lag
mike11.temp$tidal.phase.schoonaarde=NA
mike11.temp$tidal.phase.schoonaarde[which(mike11.temp$water.height.schoonaarde.diff<=0)]="ebb"
mike11.temp$tidal.phase.schoonaarde[which(mike11.temp$water.height.schoonaarde.diff>0)]="flood"
mike11.temp$tidal.phase.melle=NA
mike11.temp$tidal.phase.melle[which(mike11.temp$water.height.melle.diff<=0)]="ebb"
mike11.temp$tidal.phase.melle[which(mike11.temp$water.height.melle.diff>0)]="flood"

mike11.temp.flood<-mike11.temp
mike11.temp.flood$q.melle[which(mike11.temp.flood$tidal.phase.schoonaarde=="ebb")]=NA
mike11.temp.flood$q.schoonaarde[which(mike11.temp.flood$tidal.phase.schoonaarde=="ebb")]=NA
mike11.temp=mike11.temp[order(mike11.temp$timebin),]
ccf.q.s.q.m.flood<-ccf(mike11.temp.flood$q.schoonaarde,mike11.temp.flood$q.melle,na.action = na.pass)

mike11.temp.ebb<-mike11.temp
mike11.temp.ebb$q.melle[which(mike11.temp.ebb$tidal.phase.schoonaarde=="flood")]=NA
mike11.temp.ebb$q.schoonaarde[which(mike11.temp.ebb$tidal.phase.schoonaarde=="flood")]=NA
mike11.temp=mike11.temp[order(mike11.temp$timebin),]
ccf.q.s.q.m.ebb<-ccf(mike11.temp.ebb$q.schoonaarde,mike11.temp.ebb$q.melle,na.action = na.pass)

mike11.temp.upstream<-mike11.temp
mike11.temp.upstream$q.melle[which(mike11.temp.upstream$current.direction.melle=="downstream")]=NA
mike11.temp.upstream$q.schoonaarde[which(mike11.temp.upstream$current.direction.melle=="downstream")]=NA
mike.temp=mike11.temp[order(mike11.temp$timebin),]
ccf.q.s.q.m.upstream<-ccf(mike11.temp.upstream$q.schoonaarde,mike11.temp.upstream$q.melle,na.action = na.pass)

mike11.temp.downstream<-mike11.temp
mike11.temp.downstream$q.melle[which(mike11.temp.downstream$current.direction.melle=="upstream")]=NA
mike11.temp.downstream$q.schoonaarde[which(mike11.temp.downstream$current.direction.melle=="upstream")]=NA
mike11.temp=mike11.temp[order(mike11.temp$timebin),]
ccf.q.s.q.m.downstream<-ccf(mike11.temp.downstream$q.schoonaarde,mike11.temp.downstream$q.melle,na.action = na.pass)

mike11.temp=as.data.frame(mike11.temp)
model_HIC_empty=lm(data=mike11.temp,q.schoonaarde.lag~1)
model_HIC=lm(data=mike11.temp,q.schoonaarde~q.melle+current.direction.melle+tidal.phase.melle+water.height.melle
             +tidal.phase.schoonaarde+water.height.schoonaarde)
summary(model_HIC)
model_HIC=stepAIC(model_HIC,direction="forward",scope=list(upper=model_HIC,lower=model_HIC_empty))
summary(model_HIC)
plot(model_HIC)
model=lm(data=mike11.temp,q.schoonaarde.lag~0+q.melle)
summary(model)
plot(model)
model=lm(data=mike11.temp,q.schoonaarde.lag~0+q.melle+tidal.phase.schoonaarde)
summary(model)
model=lm(data=mike11.temp,q.schoonaarde~q.melle*tidal.phase.schoonaarde*current.direction.melle)
summary(model)
model=lm(data=mike11.temp,q.schoonaarde~0+q.melle+tidal.phase.schoonaarde)
summary(model)
save(model_HIC,file="./models/model_HIC.Rdata")
mike11.temp <- dummy_cols(mike11.temp, select_columns = 'tidal.phase.schoonaarde')
mike11.temp <- dummy_cols(mike11.temp, select_columns = 'current.direction.melle')
mike11.temp$q.melle.tidal.phase.schoonaarde=mike11.temp$q.melle*mike11.temp$tidal.phase_ebb
mike11.temp$q.melle.current=mike11.temp$q.melle*mike11.temp$current.direction.melle_downstream
model_rf <- ranger(q.schoonaarde~q.melle+tidal.phase.schoonaarde_ebb+current.direction.melle_downstream+q.melle.tidal.phase.schoonaarde+q.melle.current+water.height.schoonaarde, # formula
                   mike11.temp, # data
                   num.trees = 500,
                   respect.unordered.factors = "order",importance = "permutation")
model_rf$variable.importance
mike11.temp$predictions <- predict(model_rf, mike11.temp)$predictions
mike11.temp$residuals<-mike11.temp$q.schoonaarde.lag-mike11.temp$predictions
plot(mike11.temp$pred,mike11.temp$residuals)
hist(mike11.temp$residuals)
acf(mike11.temp$residuals)

mike11.temp$res<-residuals(model_HIC)
mike11.temp$pred<-predict(model_HIC)
mike11.temp=mike11.temp[order(mike11.temp$timebin),]
acf(mike11.temp$res)

p <- mike11.temp[c(1:100),] %>%
  ggplot2::ggplot(aes(x=timebin, y=q.melle)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Det.Prob and noise") +
  theme_ipsum() + geom_line(aes(x=timebin, y=q.schoonaarde))
print(p)

mike11.temp.wave=mike11.temp
rownames(mike11.temp.wave)=mike11.temp.wave$timebin
mike11.temp.wave=as.data.frame(mike11.temp.wave)

mike11.temp.wave=mike11.temp.wave[order(mike11.temp.wave$timebin),]
wave.test.coherence<-analyze.coherency(mike11.temp.wave,my.pair=c("q.schoonaarde","q.melle"))
## Plot of cross-wavelet power
## (with color breakpoints according to quantiles):
wc.image(wave.test.coherence, main = "cross-wavelet power spectrum, x over y",legend.params = list(lab = "cross-wavelet power levels"), periodlab = "period (hours)")
## The same plot, now with calendar axis
## (according to date format stored in 'my.wc'):
wc.image(wave.test.coherence, main = "cross-wavelet power spectrum, x over y",legend.params = list(lab = "cross-wavelet power levels"),periodlab = "period (hours)", show.date = TRUE)
## Plot of average cross-wavelet power:
wc.avg(wave.test.coherence, siglvl = 0.05, sigcol = 'red',periodlab = "period (hours)")
## Plot of wavelet coherence
## (with color breakpoints according to quantiles):
wc.image(wave.test.coherence, which.image = "wc", main = "wavelet coherence, x over y",legend.params = list(lab = "wavelet coherence levels",lab.line = 3.5, label.digits = 3),periodlab = "period (hours)")
## plot of average coherence:
wc.avg(wave.test.coherence, which.avg = "wc",siglvl = 0.05, sigcol = 'red',legend.coords = "topleft",periodlab = "period (hours)")
## Default plot of phase differences
## (with contour lines referring to cross-wavelet power)
wc.phasediff.image(wave.test.coherence, which.contour = "wp",main = "image of phase differences, x over y",periodlab = "period (hours)")
## The same plot, but with (automatically produced) calendar axis:
wc.phasediff.image(wave.test.coherence, which.contour = "wp",main = "image of phase differences, x over y",periodlab = "period (hours)",show.date = TRUE)
## Select period 64 and compare plots of corresponding phases, including
## the phase differences (angles) in their non-smoothed (default) version:
wc.sel.phases(wave.test.coherence, sel.period = 64, show.Angle = TRUE, show.date = TRUE)
## With time elapsed in days
## (starting from 0 and proceeding in steps of 50 days)
## instead of the (default) time index:
index.ticks <- seq(1, nrow(mike11.temp.wave), by = 12*24)
index.labels <- (index.ticks-1)/24
wc.sel.phases(wave.test.coherence, sel.period = 64, show.Angle = TRUE,timelab = "time elapsed (hours)",spec.time.axis = list(at = index.ticks, labels = index.labels))
## In the following, no periods are selected.
## In this case, instead of individual phases, the plot shows
## average phases for each series:
wc.sel.phases(wave.test.coherence,show.date = TRUE)


