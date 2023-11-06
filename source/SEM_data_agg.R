#SEM

source("source/Libraries.R")
source("source/Functions.R")
# require(devtools)
# remove.packages("piecewiseSEM")
# install_version("piecewiseSEM", version = "2.2.0", repos = "http://cran.us.r-project.org")
# remove.packages("nlme")
# install_version("nlme", version = "3.1-155", repos = "http://cran.us.r-project.org")
library(piecewiseSEM)
library(fastDummies)

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")

df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df$hours=hour(df$timebin)
df$perc=df$perc/100
df$angle.current.signal<-as.factor(df$angle.current.signal)
df$current.direction<-as.factor(df$current.direction)

df=df[-which(df$station_name=="Rt6" | df$station_name_submitting=="Rt6"),]
df=df[-which(df$distm50==0),]

#df=df[-which((df$station_name=="Rt1" | df$station_name=="Rt2" | df$station_name_submitting=="Rt1" | df$station_name_submitting=="Rt2") & (df$water.height.schoonaarde.corrected<0 | df$water.height.schoonaarde.submitting.corrected<0)),]
df=df[-which((df$water.height.schoonaarde.corrected<0 | df$water.height.schoonaarde.submitting.corrected<0)),]
df=df[-which(df$Tilt_angle>90),]
df=df[-which(df$Tilt_angle_submitting>90),]

df <- dummy_cols(df, select_columns = 'angle.current.signal')
df <- dummy_cols(df, select_columns = 'current.direction')
df <- dummy_cols(df, select_columns = 'tidal.phase')
df <- dummy_cols(df, select_columns = 'station_name')
df <- dummy_cols(df, select_columns = 'station_name_submitting')
#df <- dummy_cols(df, select_columns = 'traveldirection.boat_smallest')
#df <- dummy_cols(df, select_columns = 'shiptype.boat_smallest')

df <- df %>%
  dplyr::mutate(Tilt_angle.scaled = scale_vars(Tilt_angle),
                Tilt_angle_submitting.scaled = scale_vars(df$Tilt_angle_submitting),
                water.height.schoonaarde.scaled = scale_vars(water.height.schoonaarde.corrected),
                water.height.schoonaarde.submitting.scaled = scale_vars(water.height.schoonaarde.submitting.corrected),
                ebb.period.scaled = scale_vars(ebb.period),
                flood.period.scaled = scale_vars(flood.period),
                cond.schellebelle.scaled = scale_vars(cond.schellebelle),
                q.melle.scaled = scale_vars(q.melle),
                q.melle.abs.scaled = scale_vars(q.melle.abs),
                Average_noise.scaled = scale_vars(Average_noise),
                Average_noise_submitting.scaled = scale_vars(Average_noise_submitting),
                Temperature.scaled=scale_vars(Temperature),
                O2.schellebelle.scaled=scale_vars(O2.schellebelle),
                distm.scaled=scale_vars(distm),
                tidal.range.scaled=scale_vars(tidal.range.HWLW.schoonaarde),
                perc.scaled=scale_vars(perc),
                wind.v.scaled=scale_vars(wind.v.liedekerke),
                wind.r.scaled=scale_vars(wind.r.liedekerke),
                angle.wind.signal.cos.scaled=scale_vars(angle.wind.signal.cos),
                angle.wind.signal.sin.scaled=scale_vars(angle.wind.signal.sin),
                neerslag.scaled=scale_vars(neerslag.zele)
  )

## Order data according to each receiver's time series
setorder(df, RecRec, timebin)


# SEM for AIC model (third order interactions) ----------------------------

load("./models/GLM/glm_AIC_data_agg_3way_interaction.Rdata")
load("./models/GLM/glmer.all.intercepts.Rdata")

df$offset.tilt=(90-mean(df$Tilt_angle))/sd(df$Tilt_angle)
lm.tilt.empty<-lm(Tilt_angle.scaled~ 1,df)
lm.tilt.full<-lm(Tilt_angle.scaled ~ q.melle.abs.scaled+
                                     water.height.schoonaarde.scaled+
                                     wind.v.scaled*angle.wind.signal.cos.scaled+
                                     wind.v.scaled*angle.wind.signal.sin.scaled,df,offset = offset.tilt)
summary(lm.tilt.full)
lm.tilt.AIC1 <- stepAIC(lm.tilt.empty,trace = FALSE,direction="forward",scope=list(upper=lm.tilt.full,lower=lm.tilt.empty))
summary(lm.tilt.AIC1)

lme.tilt.full1<-lmer(Tilt_angle.scaled ~ q.melle.abs.scaled+
                        water.height.schoonaarde.scaled+
                        wind.v.scaled#+angle.wind.signal.cos.scaled+angle.wind.signal.sin.scaled
                        +(1|station_name),df,
                     na.action = "na.fail")
r.squaredGLMM(lme.tilt.full1)

df$offset.tilt=(90-mean(df$Tilt_angle))/sd(df$Tilt_angle)
lm.tilt.empty<-lm(Tilt_angle_submitting.scaled~ 1,df)
lm.tilt.full<-lm(Tilt_angle_submitting.scaled ~ q.melle.abs.scaled+
                   water.height.schoonaarde.scaled+
                   wind.v.scaled*angle.wind.signal.cos.scaled+
                   wind.v.scaled*angle.wind.signal.sin.scaled,df,offset = offset.tilt)
summary(lm.tilt.full)
lm.tilt.AIC2 <- stepAIC(lm.tilt.empty,trace = FALSE,direction="forward",scope=list(upper=lm.tilt.full,lower=lm.tilt.empty))
summary(lm.tilt.AIC2)

lme.tilt.full2<-lmer(Tilt_angle_submitting.scaled ~ q.melle.abs.scaled+
                      water.height.schoonaarde.submitting.scaled+
                   wind.v.scaled#+angle.wind.signal.cos.scaled+angle.wind.signal.sin.scaled
                   +(1|station_name_submitting),df,
                   na.action = "na.fail")
r.squaredGLMM(lme.tilt.full2)

lm.noise.empty<-lm(Average_noise.scaled ~ 1,df)
lm.noise.full<-lm(Average_noise.scaled ~ (q.melle.abs.scaled+
                                     water.height.schoonaarde.scaled+
                                     neerslag.scaled+
                                     wind.v.scaled*angle.wind.signal.cos.scaled+
                                     wind.v.scaled*angle.wind.signal.sin.scaled+
                                     cond.schellebelle.scaled
                                     ),df)
summary(lm.noise.full)
lm.noise.AIC <- stepAIC(lm.noise.empty,trace = FALSE,direction="forward",scope=list(upper=lm.noise.full,lower=lm.noise.empty))
summary(lm.noise.AIC)

lme.noise<-lmer(Average_noise.scaled ~ q.melle.abs.scaled*water.height.schoonaarde.scaled+
                                      neerslag.scaled+
                                      wind.v.scaled
                                      #+angle.wind.signal.cos.scaled#+angle.wind.signal.sin.scaled
                +Tilt_angle.scaled
                                      
               +(1|station_name),df,
               na.action = "na.fail"
               )
r.squaredGLMM(lme.noise)
summary(lme.noise)

#lme.noise.dredge<-dredge(lme.noise,rank="BIC")
#lme.tilt.dredge<-dredge(lme.tilt.full1,rank="BIC")

glmsem.AIC2 <- psem(
  #glmer((glmer.all.intercepts@call$formula),family=binomial(link="logit"),data=df),
  glm(update(glm.AIC_3way_interaction$formula,~ . +wind.v.scaled),"binomial",df),
  lme.noise,
  lme.tilt.full1,
  lme.tilt.full2,
  Tilt_angle_submitting.scaled %~~%  Tilt_angle.scaled,
  Average_noise.scaled %~~%  Tilt_angle_submitting.scaled,
  #Average_noise.scaled %~~%  Tilt_angle.scaled,
  cond.schellebelle.scaled %~~%  Tilt_angle_submitting.scaled,
  cond.schellebelle.scaled %~~%  Tilt_angle.scaled
)
glmsem.AIC2.summary=summary(glmsem.AIC2,conserve = TRUE)
glmsem.AIC2.summary
plot(glmsem.AIC2,node_attrs=list(shape="circle"))
x=glmsem.AIC2.summary$coefficients
write.csv(x,"./data/internal/SEM/pSEM.csv")