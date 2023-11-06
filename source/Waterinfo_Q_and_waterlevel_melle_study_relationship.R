#Waterinfo_Q_and_waterlevel_melle_study_relationship
#Onderzoek relatie debiet en getij Melle - hoge resolutie
source("./source/Libraries.R")
source("./source/Functions.R")

q.melle.5min<-read.table("./data/external/env/env/Melle tij relatie waterpeil en afvoer/Melle tij_Afvoer/Melle tij_Afvoer.csv",sep=";",col.names = c("datetime","q.melle.5min","check","abs","AVQ"))[,c(1:3)] 
q.melle.5min$q.melle.5min<-as.numeric(gsub(",",".",q.melle.5min$q.melle.5min))
q.melle.5min<-time.cleanup.waterinfo(q.melle.5min)

tij.melle.10min<-read.table("./data/external/env/env/Melle tij relatie waterpeil en afvoer/Melle tij_Waterpeil/Melle tij_Waterpeil getij.csv",sep=";",col.names = c("datetime","tij.melle.10min","check","abs","AVQ"))[,c(1:3)] 
tij.melle.10min$tij.melle.10min<-as.numeric(gsub(",",".",tij.melle.10min$tij.melle.10min))
tij.melle.10min<-time.cleanup.waterinfo(tij.melle.10min)

tij.q.melle.10min<-left_join(tij.melle.10min[,c("timebin","tij.melle.10min")],q.melle.5min[,c("timebin","q.melle.5min")],by="timebin")

tij.q.melle.10min$timebin<-round_date(tij.q.melle.10min$timebin,"hours")
tij.q.melle.10min <- tij.q.melle.10min%>%
  dplyr::group_by(timebin)%>%
  dplyr::summarise(tij.melle.10min=median(tij.melle.10min),q.melle.5min=median(q.melle.5min))

tij.q.melle.10min<-tij.q.melle.10min %>%
  arrange(timebin) %>%
  mutate(diff.vertical = tij.melle.10min - lag(tij.melle.10min),
         diff.horizontal = q.melle.5min - lag(q.melle.5min))

tij.q.melle.10min$tidal.phase<-NA
tij.q.melle.10min$tidal.phase[which(tij.q.melle.10min$diff.vertical<=0)]="ebb"
tij.q.melle.10min$tidal.phase[which(tij.q.melle.10min$diff.vertical>0)]="flood"

max(tij.q.melle.10min$tij.melle.10min)
min(tij.q.melle.10min$tij.melle.10min)

#HWLW Melle
HWLW.melle<-read.table("./data/external/env/env/Melle tij_Waterpeil getij_HWLW/Melle tij_Waterpeil getij_HWLW.csv",sep=";",col.names = c("datetime","HWLW.melle","check","abs","AVQ"))[,c(1:3)]
HWLW.melle$HWLW.melle<-as.numeric(gsub(",",".",HWLW.melle$HWLW.melle))
HWLW.melle<-time.cleanup.waterinfo(HWLW.melle)
HWLW.melle<-HWLW.melle %>%
  arrange(timebin) %>%
  mutate(tidal.range.HWLW.melle = HWLW.melle - lag(HWLW.melle))
HWLW.melle$class.HWLW.melle<-NA
HWLW.melle$class.HWLW.melle[which(HWLW.melle$tidal.range.HWLW.melle<0)]="LW"
HWLW.melle$class.HWLW.melle[which(HWLW.melle$tidal.range.HWLW.melle>0)]="HW"
HWLW.melle$timebin.HWLW=HWLW.melle$timebin

HWLW.melle<-as.data.table(HWLW.melle[,c("HWLW.melle","timebin","tidal.range.HWLW.melle","class.HWLW.melle","timebin.HWLW")])
tij.q.melle.10min.unique.timebins<-as.data.table(unique(tij.q.melle.10min$timebin))
colnames(tij.q.melle.10min.unique.timebins)="timebin"

setkey(HWLW.melle,timebin)
setkey(tij.q.melle.10min.unique.timebins,timebin)

combined<-HWLW.melle[tij.q.melle.10min.unique.timebins,roll=-Inf]

combined$timedif.HWLW.mins<-difftime(combined$timebin.HWLW,combined$timebin,units = c( "mins"),tz="utc")

tij.q.melle.10min<-left_join(tij.q.melle.10min,combined[,c("tidal.range.HWLW.melle","timebin")],by="timebin")

#tij.q.melle.10min$tidal.range.HWLW.melle<-abs(tij.q.melle.10min$tidal.range.HWLW.melle)
plot(tij.q.melle.10min$tij.melle.10min,tij.q.melle.10min$q.melle.5min)


tij.q.melle.10min<-left_join(tij.q.melle.10min,neerslag.zele[,c("timebin","neerslag.zele")],by="timebin")

tij.q.melle.10min=tij.q.melle.10min[-which(is.na(tij.q.melle.10min$tidal.range.HWLW.melle)==TRUE),]
tij.q.melle.10min=tij.q.melle.10min[-which(is.na(tij.q.melle.10min$tidal.phase)==TRUE),]
tij.q.melle.10min=tij.q.melle.10min[-which(is.na(tij.q.melle.10min$neerslag.zele)==TRUE),]
tij.q.melle.10min$tij_range=tij.q.melle.10min$tij.melle.10min*tij.q.melle.10min$tidal.range.HWLW.melle
tij.q.melle.10min$tij_diff=tij.q.melle.10min$tij.melle.10min*tij.q.melle.10min$diff.vertical
tij.q.melle.10min$diff_range=tij.q.melle.10min$diff.vertical*tij.q.melle.10min$tidal.range.HWLW.melle
tij.q.melle.10min$diff_range_tij=tij.q.melle.10min$diff.vertical*tij.q.melle.10min$tidal.range.HWLW.melle*tij.q.melle.10min$tij.melle.10min

fmla <- as.formula(q.melle.5min~tij.melle.10min+tidal.range.HWLW.melle+tij_range+tij_diff+diff_range+diff_range_tij+tidal.phase+timebin+diff.vertical+neerslag.zele)
model_rf <- ranger(fmla, # formula
                   tij.q.melle.10min, # data
                   num.trees = 500,
                   respect.unordered.factors = "order",importance = "permutation")
model_rf$variable.importance
tij.q.melle.10min$pred <- predict(model_rf, tij.q.melle.10min)$predictions
tij.q.melle.10min$residuals<-tij.q.melle.10min$q.melle.5min-tij.q.melle.10min$pred

#RMSE
tij.q.melle.10min %>%
  mutate(residual = q.melle.5min - pred)  %>%        # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2))) # calculate rmse

ggplot(tij.q.melle.10min, aes(x = pred, y = residuals)) +
  geom_point()

ggplot(tij.q.melle.10min, aes(x = q.melle.5min, y = residuals)) +
  geom_point()

ggplot(tij.q.melle.10min, aes(x = tij.melle.10min, y = residuals)) +
  geom_point()

ggplot(tij.q.melle.10min, aes(x = tidal.range.HWLW.melle, y = residuals)) +
  geom_point()

ggplot(tij.q.melle.10min, aes(x = tidal.phase, y = residuals)) +
  geom_point()

ggplot(tij.q.melle.10min, aes(x = diff.vertical, y = residuals)) +
  geom_point()