#Model approach of O'Brien (2021)

source("source/Libraries.R")
source("source/Functions.R")
library(fastDummies)

df<-read.csv("./data/internal/telemetry/data_agg_zeros_added_with_env.csv")
df$timebin<-as.POSIXct(df$timebin, tz="UTC")
df$hours=hour(df$timebin)
df$perc=df$perc/100
df$angle.current.signal<-as.factor(df$angle.current.signal)
df$current.direction<-as.factor(df$current.direction)
df$station_name=as.factor(df$station_name)
df$RecRec=as.factor(df$RecRec)
df <- dummy_cols(df, select_columns = 'angle.current.signal')
#df=df[which(df$distm50>0 & df$distm50<600),]
df=df[-which(df$distm50==0),]
df=df[-which(df$station_name=="Rt6" | df$station_name_submitting=="Rt6"),]
#df=df[which(df$station_name=="Rt5"),]
df=df[-which(df$Tilt_angle>90),]
df$time.relative.to.LW<-NA
df$time.relative.to.LW[which(df$class.HWLW.schoonaarde_prior=="LW")]=df$timedif.HWLW.mins_prior[which(df$class.HWLW.schoonaarde_prior=="LW")]
df$time.relative.to.LW[which(df$class.HWLW.schoonaarde_prior=="HW")]=df$timedif.HWLW.mins_post[which(df$class.HWLW.schoonaarde_prior=="HW")]

df$ts.start<-F
df$ts.start[which(df$timebin==min(df$timebin))]=T

df <- df %>%
  dplyr::mutate(Tilt_angle.scaled = scale_vars(Tilt_angle),
                Tilt_angle_submitting.scaled = scale_vars(df$Tilt_angle_submitting),
                water.height.schoonaarde.scaled = scale_vars(water.height.schoonaarde),
                ebb.period.scaled = scale_vars(ebb.period),
                flood.period.scaled = scale_vars(flood.period),
                cond.schellebelle.scaled = scale_vars(cond.schellebelle),
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
                neerslag.scaled=scale_vars(neerslag.zele)
  )

## Order data according to each receiver's time series
setorder(df, RecRec, timebin)

## Set seed for CV reproducibility; number reflects internal ABIT manuscript number
seed <- 2000031

form.bam=as.formula(perc ~
                      distm.scaled +
                      angle.current.signal +
                      s(station_name,bs='re') +
                      s(q.melle.abs.scaled) +
                      s(Average_noise.scaled,bs='cc') +
                      s(water.height.schoonaarde.scaled) +
                      s(tidal.range.scaled) +
                      s(Tilt_angle.scaled) +
                      s(Tilt_angle_submitting.scaled) +
                      s(cond.schellebelle.scaled)
                    )

form.bam=as.formula(perc ~
                      #angle.current.signal +
                      s(distm.scaled) +
                      #s(distm.scaled,by=angle.current.signal_180) +
                      #s(station_name,bs='re') +
                      s(q.melle.abs.scaled) +
                      s(Average_noise.scaled) +
                      s(water.height.schoonaarde.scaled) +
                      #s(tidal.range.scaled) +
                      s(Tilt_angle.scaled) +
                      s(Tilt_angle_submitting.scaled) +
                      s(cond.schellebelle.scaled) +
                      s(Average_noise.scaled,distm.scaled) +
                      s(water.height.schoonaarde.scaled,distm.scaled) +
                      s(q.melle.abs.scaled,distm.scaled) +
                      #s(Average_noise.scaled,by=angle.current.signal_180) +
                      #s(water.height.schoonaarde.scaled,by=angle.current.signal_180) +
                      #s(q.melle.abs.scaled,by=angle.current.signal_180) +
                      s(hours,bs='cc') +
                      s(time.relative.to.LW,bs='cc')
)

# form.bam=as.formula(perc ~ distm.scaled
#                         * q.melle.abs.scaled * angle.current.signal * Average_noise.scaled #+ I(Average_noise.scaled^2)
#                         * water.height.schoonaarde.scaled + Tilt_angle.scaled * Tilt_angle_submitting.scaled
#                         * tidal.range.scaled * cond.schellebelle.scaled
#                         #+ current.direction
#                         #+ Average_noise.scaled_lag
#                         + O2.schellebelle.scaled + Temperature.scaled 
#                         + wind.v.scaled * wind.r.scaled
#                         + neerslag.scaled
#                         #+ s(station_name,bs='re') + s(distm.scaled,station_name,bs='re') 
#                         + s(hours,bs='cc')
# )

m_global <- bam(form.bam,
                family = binomial(),
                data = df
                #discrete = T
                )


#gam.check(m_global)
summary(m_global)
#The ‘edf’ is the estimated degrees of freedom – essentially, the larger the number, the more wiggly the fitted model.
#plot(m_global)



rho_guess <- acf(resid(m_global)[which(df$RecRec=="Rt1 Rt2")])$acf[2]
#rho_guess <- acf(resid(m_global))$acf[2]

setorder(df, RecRec, timebin)
acf(resid(m_global))

df.model=df
df.model$res=residuals(m_global,df.model,type="response")
df.model$pred=predict(m_global,df.model,type="response")
RecRec.for.plot=unique(df.model$RecRec[which(df.model$distm50==500)])
for (i in RecRec.for.plot){
  df.model.subset<-df.model[which(df.model$RecRec==i),]
  df.model.subset=df.model.subset[order(df.model.subset$timebin),]
  
  df.model.subset <- filter(df.model.subset,timebin>=as.POSIXct("2020-03-07 01:00:00",tz="UTC"),timebin<as.POSIXct("2020-03-14 01:00:00",tz="UTC"))
  
  p <- df.model.subset %>%
    ggplot2::ggplot(aes(x=timebin, y=perc)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Det.Prob and noise") +
    theme_ipsum() + geom_line(aes(x=timebin, y=Average_noise.scaled)) + ggtitle(i)
  print(p)
  
  q <- df.model.subset %>%
    ggplot2::ggplot(aes(x=timebin, y=res)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("residuals and noise") +
    theme_ipsum() + geom_line(aes(x=timebin, y=Average_noise.scaled)) + ggtitle(i)
  print(q)
  
  r <- df.model.subset %>%
    ggplot2::ggplot(aes(x=timebin, y=perc)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Det.Prob and predictions") +
    theme_ipsum() + geom_line(aes(x=timebin, y=pred)) + ggtitle(i)
  print(r)
}

1-(logLik.gam(m_global)/logLik(glm.empty))

m_global <- bam(form.bam,
                family = binomial(),
                data = df,
                #discrete = T,
                rho=rho_guess,
                AR.start = df$ts.start
)

summary(m_global)

1-(logLik.gam(m_global)/logLik(glm.empty))
acf(resid(m_global))

plot(m_global)

