#OMES_Q measurements
source("./source/Libraries.R")
source("./source/Functions.R")

OMES<-read_excel("./data/external/env/env/OMES/export_stroming.xlsx")
OMES$DateTime<-as.POSIXct(OMES$DateTime,format='%Y-%m-%d %H:%M:%S')
OMES$Reading<-as.numeric(gsub(",",".",OMES$Reading))
unique(OMES$Parameters_ShortName)
unique(OMES$StandardName)
unique(OMES$FullTitleNChar)
unique(OMES$LODSign)
x=as.data.frame(unique(OMES$DateTime))

ggplot(data=OMES[which(OMES$Parameters_ShortName=="	
velocity X"),],aes(x=DateTime,y=Reading))+geom_point()

OMES.mean <- OMES%>%
  dplyr::group_by(DateTime,StandardName)%>%
  dplyr::summarise(Reading=mean(Reading))

OMES.mean.subset=OMES.mean[c(573:740),]
OMES.mean.subset=OMES.mean.subset[which(OMES.mean.subset$StandardName=="Velocity x-component"),]
plot(OMES.mean.subset$DateTime,OMES.mean.subset$Reading)

q.melle<-read.table("./data/external/env/env/OMES/Melle tij Zeeschelde_Afvoer.csv",sep=";",col.names = c("datetime","q.melle","check","abs","AVQ"))[,c(1:3)]
q.melle$q.melle<-as.numeric(gsub(",",".",q.melle$q.melle))
q.melle<-time.cleanup.waterinfo(q.melle)
q.melle$timebin=q.melle$timebin-30*60
q.melle=q.melle[,c("timebin","q.melle")]
OMES.mean.subset$timebin=OMES.mean.subset$DateTime

OMES.mean.subset=as.data.table(OMES.mean.subset)
q.melle=as.data.table(q.melle)
setkey(OMES.mean.subset,timebin)
setkey(q.melle,timebin)

OMES.mean.subset<-q.melle[OMES.mean.subset,roll="nearest"]

HWLW.schoonaarde<-read.table("./data/external/env/env/OMES/Schoonaarde tij Zeeschelde_Waterpeil getij.csv",sep=";",col.names = c("datetime","HWLW.schoonaarde","check","abs","AVQ"))[,c(1:3)]
HWLW.schoonaarde$HWLW.schoonaarde<-as.numeric(gsub(",",".",HWLW.schoonaarde$HWLW.schoonaarde))
HWLW.schoonaarde<-time.cleanup.waterinfo(HWLW.schoonaarde)
HWLW.schoonaarde=HWLW.schoonaarde[,c("timebin","HWLW.schoonaarde")]

HWLW.schoonaarde=as.data.table(HWLW.schoonaarde)
setkey(OMES.mean.subset,timebin)
setkey(HWLW.schoonaarde,timebin)

OMES.mean.subset<-HWLW.schoonaarde[OMES.mean.subset,roll="nearest"]

plot(OMES.mean.subset$q.melle,OMES.mean.subset$Reading)
plot(OMES.mean.subset$q.melle/OMES.mean.subset$HWLW.schoonaarde,OMES.mean.subset$Reading)
cor(OMES.mean.subset$q.melle,OMES.mean.subset$Reading)
cor(OMES.mean.subset$q.melle,lag(OMES.mean.subset$Reading),use="complete.obs")
cor(OMES.mean.subset$q.melle/(OMES.mean.subset$HWLW.schoonaarde),OMES.mean.subset$Reading)
cor(OMES.mean.subset$q.melle/(OMES.mean.subset$HWLW.schoonaarde),lag(OMES.mean.subset$Reading),use="complete.obs")

ccf_OMES<-ccf(OMES.mean.subset$Reading,OMES.mean.subset$HWLW.schoonaarde,na.action = na.pass)

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df$hours=hour(df$timebin)
df_150m<-df[which(df$RecRec=="Rt1 Rt1"),]
df_150m <- df_150m[order(df_150m$timebin),]
ccf_HIC<-ccf(df_150m$q.melle,df_150m$water.height.schoonaarde,na.action = na.pass)
cbind(ccf_HIC$lag,ccf_HIC$acf)
