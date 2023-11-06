#Functions

time.cleanup.waterinfo<-function(df.waterinfo){
  df.waterinfo <- df.waterinfo %>% separate(datetime, c("date", "time"),sep="T")
  df.waterinfo$tz<-(substr(df.waterinfo$time,13,18))
  df.waterinfo$time<-substr(df.waterinfo$time,1,12)
  df.waterinfo$tz<-gsub(":","",df.waterinfo$tz)
  df.waterinfo$datetime=paste(df.waterinfo$date,df.waterinfo$time,df.waterinfo$tz,sep=" ")
  df.waterinfo$timebin<-as.POSIXct(df.waterinfo$datetime,format='%Y-%m-%d %H:%M:%OS %z')
  attributes(df.waterinfo$timebin)$tzone <- "UTC" 
  return(df.waterinfo)
}

scale_vars <- function(y){(y-mean(y, na.rm=TRUE))/sd(y,na.rm=TRUE)}

Assumption_bin_model<-function(model,data){
  probabilities <- predict(model, type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
  data$logit=log(probabilities/(1-probabilities))
  
  plot(data$distm,probabilities)
  plot(data$q.melle.abs.scaled,data$logit)
  
  # Select only numeric predictors
  # mydata <- data %>%
  #   dplyr::select_if(is.numeric) 
  # predictors <- colnames(mydata)
  # # Bind the logit and tidying the data for plot
  # mydata <- mydata %>%
  #   mutate(logit = log(probabilities/(1-probabilities))) %>%
  #   gather(key = "predictors", value = "predictor.value", -logit)
  # print(ggplot(mydata, aes(logit, predictor.value))+
  #         geom_point(size = 0.5, alpha = 0.5) +
  #         geom_smooth(method = "loess") + 
  #         theme_bw() +
  #         facet_wrap(~predictors, scales = "free_y"))
}

#Model evaluation
Model_evaluation<-function(model,data){
  print(summary(model))
  plot(model)
  data$pred <- predict(model, data)
  data$residuals<-data$speed-data$pred
  rmse <- data %>%
    group_by(fTransmitter) %>%
    #mutate(residual = speed - pred)  %>%        # calculate the residual
    summarize(rmse  = sqrt(mean(residuals^2)))
  rmse=mean(rmse$rmse)
  rmse
}

Single_wavelet_analysis<-function(data,response){
  df_150m.wave=data
  #df_150m.wave=df_150m[,c("timebin","non_zero","perc","water.height.schoonaarde","q.melle.abs","q.melle")]
  rownames(df_150m.wave)=df_150m.wave$timebin
  
  wave.test.single<-analyze.wavelet(df_150m.wave,my.series=response,loess=0.75,dt=1)#loess=0 means no detrending
  wave.test.single$Power.avg
  wave.test.single$Period
  wave.test.single$P
  x=cbind(wave.test.single$Power.avg,wave.test.single$Period)
  
  ## Plot of wavelet power spectrum (with equidistant color breakpoints):
  wt.image(wave.test.single, color.key = "interval", main = "wavelet power spectrum",legend.params = list(lab = "wavelet power levels"),periodlab = "period (hours)")
  ## Plot of average wavelet power:
  wt.avg(wave.test.single, siglvl = 0.05, sigcol = "red",periodlab = "period (hours)")
  ## Default image of phases:
  wt.phase.image(wave.test.single,main = "image of phases",periodlab = "period (hours)")
  ## With time elapsed in days
  ## (starting from 0 and proceeding in steps of 50 days)
  ## instead of the (default) time index:
  index.ticks <- seq(1, nrow(df_150m.wave), by = 12*24)
  index.labels <- (index.ticks-1)/24
  wt.phase.image(wave.test.single,main = "image of phases",periodlab = "period (hours)",timelab = "time elapsed (hours)",spec.time.axis = list(at = index.ticks, labels = index.labels))
  ## The same plot, but with (automatically produced) calendar axis:
  wt.phase.image(wave.test.single,main = "image of phases", periodlab = "period (hours)",show.date = TRUE)
  ## For further axis plotting options:
  ## Please see the examples in our guide booklet,
  ## URL http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf.
  ## Image plot of phases with numerals as labels of the color legend bar:
  wt.phase.image(wave.test.single,legend.params=list(pi.style = FALSE, label.digits = 2))
  return(wave.test.single)
}

Double_wavelet_analysis<-function(data,response){
  df_150m.wave=data
  #df_150m.wave=df_150m[,c("timebin","non_zero","perc","water.height.schoonaarde","q.melle.abs","q.melle")]
  rownames(df_150m.wave)=df_150m.wave$timebin
  
  wave.test.coherence<-analyze.coherency(df_150m.wave,my.pair=response)
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
  wc.sel.phases(wave.test.coherence, sel.period = 300, show.Angle = TRUE, show.date = TRUE)
  ## With time elapsed in days
  ## (starting from 0 and proceeding in steps of 50 days)
  ## instead of the (default) time index:
  index.ticks <- seq(1, nrow(df_150m.wave), by = 12*24)
  index.labels <- (index.ticks-1)/24
  wc.sel.phases(wave.test.coherence, sel.period = 300, show.Angle = TRUE,timelab = "time elapsed (hours)",spec.time.axis = list(at = index.ticks, labels = index.labels))
  ## In the following, no periods are selected.
  ## In this case, instead of individual phases, the plot shows
  ## average phases for each series:
  phase.difference=wc.sel.phases(wave.test.coherence,show.date = TRUE)
  df_150m.wave$Phase.difference<-phase.difference$Angle
  df_150m.wave$ftidal.phase=as.factor(df_150m.wave$tidal.phase)
  df_150m.wave$fcurrent.direction=as.factor(df_150m.wave$current.direction)
  plot(df_150m.wave$fcurrent.direction,df_150m.wave$Phase.difference)
  mean(df_150m.wave$Phase.difference)
  return(df_150m.wave)
}