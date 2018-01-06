## Visibility and Humidity data taken from weather underground, all other weather data taken from NOAA export
## In gender_esm, 1= male, 2= female, 3,4,5 are considered "other".
## Notes from Julia: Control for all other variables when doing regression, 
## paths controlled for the influence of all paths
## Look at box plots/arrow bars
## Suggestion for significance:
## Cluster analysis
## Moderator, a third variable explains the relationship between two variables
##install.packages("foreign")



#############################  Multiple Plot Function  #############################################
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
################################ Exploration #####################################################
library(foreign)
#setwd("~/Documents/EI Senior Project")

kids <- read.spss(file = "dataset.sav", to.data.frame = T)
esm <- kids[kids$BG_OR_ESM=="ESM",]
weather <- read.csv("weather.csv", as.is=T)
extra <- read.csv("weather_extra.csv", as.is=T)


#Use Meriden Markham Municipal Airport for Chesire Kids
#Use Sikorsky Memorial Airport for Trumbull, Bassick, and FW, Kids

# Just getting the data we need from the two airports
weather <- weather[weather$STATION_NAME %in% c("IGOR I SIKORSKY MEMORI AIRPORT CT US", 
                                               "MERIDEN MARKHAM MUNICIPAL AIRPORT CT US"),]

## Reading in METARS data from sikorsky
##s_may <- read.table("sikorsky_may.txt", fill=T)
##table(s_may$V6)


#esm2 <- split(esm,esm$Participant)
#table(esm$grade_ESM)


## Getting weather date into proper form
weather$date2 <- NA
weather <- transform(weather, date2 = as.Date(as.character(weather$DATE), "%Y%m%d"))
esm <- transform(esm, date2 = as.Date(as.character(esm$Date), format = "%m/%d/%Y"))



## Matching up dates on ESM and weather data

## Adding 2000 years to ESM dates
for(i in 1:nrow(esm)){
  d <- as.POSIXlt(esm$date2[i])
  d$year <- d$year + 2000
  esm$date2[i] <- d
}
esm$date2 <- as.Date(esm$date2)


weatherI <- weather[weather$STATION_NAME=="IGOR I SIKORSKY MEMORI AIRPORT CT US",]
weatherM <- weather[weather$STATION_NAME== "MERIDEN MARKHAM MUNICIPAL AIRPORT CT US",]

weatherI$humidity <- extra$I_Humidity
weatherM$humidity <- extra$M_Humidity
weatherI$visibility <- extra$I_Visibility
weatherM$visibility <- extra$M_Visibility

esm$airport <- NA
esm$uID <- NA
for(i in 1:nrow(esm)){
  if(esm$SCHOOL_ESM[i]=="Chesire"){
    esm$airport[i] <- 2
    esm$uID[i] <- which(weatherM$date2==esm$date2[i])
  }
  else{
    esm$airport[i] <- 1
    esm$uID[i] <- which(weatherM$date2==esm$date2[i])
  }
  print(i)
}

## Moving weather data into esm dataframe
esm$station <- NA
esm$precip <- NA
esm$snowdepth <- NA
esm$snowfall <- NA
esm$tempavg <- NA
esm$tempmax <- NA
esm$tempmin <- NA
esm$tempobs <- NA
esm$windavg <- NA
esm$peakgust <- NA
esm$blowingsnow <- NA
esm$fog <- NA
esm$fogheavy <- NA
esm$smoke <- NA
esm$thunder <- NA
esm$humidity <- NA
esm$visibility <- NA

for(i in 1:nrow(esm)){
  if(esm$airport[i]==1){
    esm$station[i] <- weatherI$STATION_NAME[esm$uID[i]]
    esm$precip[i] <- weatherI$PRCP[esm$uID[i]]
    esm$snowdepth[i] <- weatherI$SNWD[esm$uID[i]]
    esm$snowfall[i] <- weatherI$SNOW[esm$uID[i]]
    esm$tempavg[i] <- weatherI$TAVG[esm$uID[i]]
    esm$tempmax[i] <- weatherI$TMAX[esm$uID[i]]
    esm$tempmin[i] <- weatherI$TMIN[esm$uID[i]]
    esm$tempobs[i] <- weatherI$TOBS[esm$uID[i]]
    esm$windavg[i] <- weatherI$AWND[esm$uID[i]]
    esm$peakgust[i] <- weatherI$PGTM[esm$uID[i]]
    esm$blowingsnow[i] <- weatherI$WT09[esm$uID[i]]
    esm$fog[i] <- weatherI$WT01[esm$uID[i]]
    esm$fogheavy[i] <- weatherI$WT02[esm$uID[i]]
    esm$smoke[i] <- weatherI$WT08[esm$uID[i]]
    esm$thunder[i] <- weatherI$WT03[esm$uID[i]]
    esm$visibility[i] <- weatherI$visibility[esm$uID[i]]
    esm$humidity[i] <- weatherI$humidity[esm$uID[i]]
  }
  else{
    esm$station[i] <- weatherM$STATION_NAME[esm$uID[i]]
    esm$precip[i] <- weatherM$PRCP[esm$uID[i]]
    esm$snowdepth[i] <- weatherM$SNWD[esm$uID[i]]
    esm$snowfall[i] <- weatherM$SNOW[esm$uID[i]]
    esm$tempavg[i] <- weatherM$TAVG[esm$uID[i]]
    esm$tempmax[i] <- weatherM$TMAX[esm$uID[i]]
    esm$tempmin[i] <- weatherM$TMIN[esm$uID[i]]
    esm$tempobs[i] <- weatherM$TOBS[esm$uID[i]]
    esm$windavg[i] <- weatherM$AWND[esm$uID[i]]
    esm$peakgust[i] <- weatherM$PGTM[esm$uID[i]]
    esm$blowingsnow[i] <- weatherM$WT09[esm$uID[i]]
    esm$fog[i] <- weatherM$WT01[esm$uID[i]]
    esm$fogheavy[i] <- weatherM$WT02[esm$uID[i]]
    esm$smoke[i] <- weatherM$WT08[esm$uID[i]]
    esm$thunder[i] <- weatherM$WT03[esm$uID[i]]
    esm$humidity[i] <- weatherM$humidity[esm$uID[i]]
    esm$visibility[i] <- weatherM$visibility[esm$uID[i]]
  }
  print(i)
}



## Looking at means of emotions at different temperatures
cols <- 17
count <- 1
temps <- unique(esm$tempavg)
em_avg_temp <- matrix(ncol = cols,nrow=length(temps))
for(i in temps){
  em_avg_temp[count,1] <- i
  em_avg_temp[count,2] <- length(which(esm$tempavg==i))
  em_avg_temp[count,3:cols] <- colMeans(esm[esm$tempavg==i,724:738], na.rm=T)
  count <- count+1
}
em_avg_temp <- em_avg_temp[order(em_avg_temp[,1]),]
em_avg_temp_df <- as.data.frame(em_avg_temp)
colnames(em_avg_temp_df) <- c("tempavg","num_obs",names(esm)[724:738])



## Means of emotions at different visiblity levels
count <- 1
temps <- unique(esm$visibility)
em_avg_vis <- matrix(ncol = cols,nrow=length(temps))
for(i in temps){
  em_avg_vis[count,1] <- i
  em_avg_vis[count,2] <- length(which(esm$visibility==i))
  em_avg_vis[count,3:cols] <- colMeans(esm[esm$visibility==i,724:738], na.rm=T)
  count <- count+1
}
em_avg_vis <- em_avg_vis[order(em_avg_vis[,1]),]
em_avg_vis_df <- as.data.frame(em_avg_vis)
colnames(em_avg_vis_df) <- c("visiblity","num_obs",names(esm)[724:738])
em_avg_vis_df


## Means of emotions at different humidity levels
count <- 1
temps <- unique(esm$humidity)
em_avg_hum <- matrix(ncol = cols,nrow=length(temps))
for(i in temps){
  em_avg_hum[count,1] <- i
  em_avg_hum[count,2] <- length(which(esm$humidity==i))
  em_avg_hum[count,3:cols] <- colMeans(esm[esm$humidity==i,724:738], na.rm=T)
  count <- count+1
}
em_avg_hum <- em_avg_hum[order(em_avg_hum[,1]),]
em_avg_hum_df <- as.data.frame(em_avg_hum)
colnames(em_avg_hum_df) <- c("humidity","num_obs",names(esm)[724:738])
em_avg_hum_df


## Means of emotions at different rain levels
count <- 1
temps <- unique(esm$precip)
em_avg_rain <- matrix(ncol = cols,nrow=length(temps))
for(i in temps){
  em_avg_rain[count,1] <- i
  em_avg_rain[count,2] <- length(which(esm$precip==i))
  em_avg_rain[count,3:cols] <- colMeans(esm[esm$precip==i,724:738], na.rm=T)
  count <- count+1
}
em_avg_rain <- em_avg_rain[order(em_avg_rain[,1]),]
em_avg_rain_df <- as.data.frame(em_avg_rain)
colnames(em_avg_rain_df) <- c("rain","num_obs",names(esm)[724:738])
em_avg_rain_df

## Selecting the emotions we want to focus on
em_avg_temp_df <- em_avg_temp_df[,c(1,2,3,4,7,12,13,14,15)]
em_avg_vis_df <- em_avg_vis_df[,c(1,2,3,4,7,12,13,14,15)]
em_avg_rain_df <- em_avg_rain_df[,c(1,2,3,4,7,12,13,14,15)]


## MAKING MEAN TEMPERATURE/EMOTION PLOTS
library(ggplot2)

## Remove temps that had less than 100 observations
count <- NULL
for(i in 1:nrow(em_avg_temp_df)){
  if(em_avg_temp_df$num_obs[i] < 100){
    count <- c(count,i)
  }
}
if(length(count) >0){
em_avg_temp_df <- em_avg_temp_df[-count,]
}

## Column of all emotions means
means1 <- c(em_avg_temp_df$enthusiast_ESM,em_avg_temp_df$happy_ESM,em_avg_temp_df$calm_ESM,
            em_avg_temp_df$tired_ESM, em_avg_temp_df$sad_ESM, em_avg_temp_df$bored_ESM,
            em_avg_temp_df$stressed_ESM)

## Column of which column each mean was in for colors
len <- nrow(em_avg_temp_df)
Emotions <- rep(NA,len*7)
Emotions[1:len] <- "Enthusiastic"
Emotions[(len+1): (2*len)] <- "Happy"
Emotions[(2*len + 1):(3*len)] <- "Calm"
Emotions[(3*len + 1):(4*len)] <- "Tired"
Emotions[(4*len + 1 ):(5*len)] <-"Sad"
Emotions[(5*len + 1):(6*len)] <-"Bored"
Emotions[(6*len + 1):(7*len)] <- "Stressed"

## Temperature column
celsius <- rep(em_avg_temp_df$tempavg,7)

## Repitions of the num of observations for sizing of points
obs <- rep(em_avg_temp_df$num_obs,7)

## Make the data frame
df1 <- data.frame(celsius,obs,Emotions,means1)

## Making the temperature emotions plot
p1 <- ggplot(df1,aes(celsius,means1, colour = Emotions))
p1 <- p1 + geom_point(aes(size=obs)) + geom_line() + ggtitle("Mean Emotions at Different Temperature Levels") +
  xlab("Temperature (in F)") + ylab("Emotion Levels (1 to 4)") + 
  scale_size(name="Number of Observations") + ylim(c(1.5,3)) 
p1

## MAKING MEAN HUMIDITY/EMOTION PLOTS

## Remove temps that had less than 100 observations
count <- NULL
for(i in 1:nrow(em_avg_hum_df)){
  if(em_avg_hum_df$num_obs[i] < 100){
    count <- c(count,i)
  }
}
if(length(count)>0){
em_avg_hum_df <- em_avg_hum_df[-count,]
}
## Column of all emotions means
means2 <- c(em_avg_hum_df$enthusiast_ESM,em_avg_hum_df$happy_ESM,em_avg_hum_df$calm_ESM,
            em_avg_hum_df$tired_ESM, em_avg_hum_df$sad_ESM, em_avg_hum_df$bored_ESM,
            em_avg_hum_df$stressed_ESM)

## Column of which column each mean was in for colors
len <- nrow(em_avg_hum_df)
Emotions <- rep(NA,len*7)
Emotions[1:len] <- "Enthusiastic"
Emotions[(len+1): (2*len)] <- "Happy"
Emotions[(2*len + 1):(3*len)] <- "Calm"
Emotions[(3*len + 1):(4*len)] <- "Tired"
Emotions[(4*len + 1 ):(5*len)] <-"Sad"
Emotions[(5*len + 1):(6*len)] <-"Bored"
Emotions[(6*len + 1):(7*len)] <- "Stressed"

## Temperature column
hume <- rep(em_avg_hum_df$humidity,7)

## Repitions of the num of observations for sizing of points
obs <- rep(em_avg_hum_df$num_obs,7)

## Make the data frame
df1 <- data.frame(hume,obs,Emotions,means2)

## Making the humidity emotions plot
p2 <- ggplot(df1,aes(hume,means2, colour = Emotions))
p2 <- p2 + geom_point(aes(size=obs)) + geom_line() + ggtitle("Mean Emotions at Different Humidity Levels") +
  xlab("Humidity (in %)") + ylab("Emotion Levels (1 to 4)") + scale_size(name="Number of Observations")


## MAKING MEAN VISIBILITY/EMOTION PLOTS

## Remove temps that had less than 100 observations
count <- NULL
for(i in 1:nrow(em_avg_vis_df)){
  if(em_avg_vis_df$num_obs[i] < 100){
    count <- c(count,i)
  }
}
if(length(count)>0){
  em_avg_vis_df <- em_avg_vis_df[-count,]
}
## Column of all emotions means
means3 <- c(em_avg_vis_df$enthusiast_ESM,em_avg_vis_df$happy_ESM,em_avg_vis_df$calm_ESM,
            em_avg_vis_df$tired_ESM, em_avg_vis_df$sad_ESM, em_avg_vis_df$bored_ESM,
            em_avg_vis_df$stressed_ESM)

## Column of which column each mean was in for colors
len <- nrow(em_avg_vis_df)
Emotions <- rep(NA,len*7)
Emotions[1:len] <- "Enthusiastic"
Emotions[(len+1): (2*len)] <- "Happy"
Emotions[(2*len + 1):(3*len)] <- "Calm"
Emotions[(3*len + 1):(4*len)] <- "Tired"
Emotions[(4*len + 1 ):(5*len)] <-"Sad"
Emotions[(5*len + 1):(6*len)] <-"Bored"
Emotions[(6*len + 1):(7*len)] <- "Stressed"

## Temperature column
viz <- rep(em_avg_vis_df$visiblity,7)

## Repitions of the num of observations for sizing of points
obs <- rep(em_avg_vis_df$num_obs,7)

## Make the data frame
df1 <- data.frame(viz,obs,Emotions,means3)

## Making the visibility emotions plot
p3 <- ggplot(df1,aes(viz,means3, colour = Emotions))
p3 <- p3 + geom_point(aes(size=obs)) + geom_line() + ggtitle("Mean Emotions at Different Visibility Levels") +
  xlab("Visbility (in miles)") + ylab("Emotion Levels (1 to 4)") + scale_size(name="Number of Observations")


## MAKING MEAN PRECIPITATION/EMOTION PLOTS

## Remove temps that had less than 100 observations
count <- NULL
for(i in 1:nrow(em_avg_rain_df)){
  if(em_avg_rain_df$num_obs[i] < 100){
    count <- c(count,i)
  }
}
if(length(count)>0){
  em_avg_rain_df <- em_avg_rain_df[-count,]
}
## Column of all emotions means
means4 <- c(em_avg_rain_df$enthusiast_ESM,em_avg_rain_df$happy_ESM,em_avg_rain_df$calm_ESM,
            em_avg_rain_df$tired_ESM, em_avg_rain_df$sad_ESM, em_avg_rain_df$bored_ESM,
            em_avg_rain_df$stressed_ESM)

## Column of which column each mean was in for colors
len <- nrow(em_avg_rain_df)
Emotions <- rep(NA,len*7)
Emotions[1:len] <- "Enthusiastic"
Emotions[(len+1): (2*len)] <- "Happy"
Emotions[(2*len + 1):(3*len)] <- "Calm"
Emotions[(3*len + 1):(4*len)] <- "Tired"
Emotions[(4*len + 1 ):(5*len)] <-"Sad"
Emotions[(5*len + 1):(6*len)] <-"Bored"
Emotions[(6*len + 1):(7*len)] <- "Stressed"

## Temperature column
rain <- rep(em_avg_rain_df$rain,7)

## Repitions of the num of observations for sizing of points
obs <- rep(em_avg_rain_df$num_obs,7)

## Make the data frame
df1 <- data.frame(rain,obs,Emotions,means4)

## Making the rain emotions plot
p4 <- ggplot(df1,aes(rain,means4, colour = Emotions))
p4 <- p4 + geom_point(aes(size=obs)) + geom_line() + ggtitle("Mean Emotions at Different Rain Levels") +
  xlab("Rain (in inches)") + ylab("Emotion Levels (1 to 4)") + scale_size(name="Number of Observations")

## A couple interesting things to note here, these will be the things we go more in depth with.

## 1. Looking at the decrease in levels of tiredness as temperature increase
## 2. High levels of calm during rainy, cloudy days, is there any evidence of sadness?

## 1. Tired v. Temperature

## Could the difference in levels of tiredness v. temperature be coming from a different source?
## First examine if tiredness v. date (closer to end of school) could be causing this

dates <- unique(esm$date2)
tired_scores <- rep(NA,length(dates))
observations <- rep(NA,length(dates))
tempers <-  rep(NA,length(dates))
for(i in 1:length(dates)){
  find <- which(esm$date2==dates[i])
  tired_scores[i] <- mean(esm$tired_ESM[find], na.rm = T)
  observations[i] <- length(esm$tired_ESM[find]) - length(which(is.na(esm$tired_ESM[find])))
  tempers[i] <- mean(esm$tempavg[find])
}

## Remove row if number of observations is below 100 (run this chunk together)
tired_df <- data.frame(dates,tired_scores,observations, tempers)
count <- NULL
for(i in 1:nrow(tired_df)){
  if(tired_df$observations[i]< 100){
    count <- c(count,i)
  }
}
tired_df <- tired_df[-count,]

## Compare how tiredness changes with date
t1 <- ggplot(tired_df,aes(dates,tired_scores))
t1 <- t1 + geom_point(size=3) + 
  geom_line() + ylim(c(2,3)) + ggtitle("Mean Tired Scores at Different Dates") +
  xlab("Date") + ylab("Mean Tired Score (1 to 4)")


tempa <- em_avg_temp_df$tempavg
tempb <- em_avg_temp_df$tired_ESM
temp_df <- data.frame(tempa,tempb)
p_tired <- ggplot(temp_df,aes(tempa,tempb))
p_tired <- p_tired + geom_point(size=3) + geom_line() + ggtitle("Mean Tired at Different Temperature Levels") +
  xlab("Temperature (in F)") + ylab("Emotion Levels (1 to 4)") + 
  scale_size(name="Number of Observations") + ylim(c(2,3)) 


dt_fit <- lm(tempavg ~ date2, data = esm)
summary(dt_fit)
dt1 <- ggplot(esm,aes(x =date2,y = tempavg)) + geom_point() + ggtitle("Temperature Predicted By Date") + xlab("Dates") + 
  ylab("Temperature (in F)") +
  stat_smooth(method = "lm", col = "green") 

multiplot(p_tired,t1, dt1, cols=2)

## T-test at low temp v. high temp (70)
t.test(esm$tired_ESM[esm$tempavg<70],esm$tired_ESM[esm$tempavg>70])

## Regressions of temp and date trying to predict avg level of tired per day
fit_temp <- lm(tired_ESM ~ tempavg, data = em_avg_temp_df)
summary(fit_temp)
fit1 <- ggplot(em_avg_temp_df, aes(x=tempavg, y = tired_ESM)) + geom_point() + 
  stat_smooth(method = "lm", col = "red")

fit_date <- lm(tired_scores ~ dates + tempers , data = tired_df)
fit_date2 <- lm(tired_scores ~ dates, data = tired_df)
summary(fit_date2)
summary(fit_date)
fit2 <- ggplot(tired_df, aes(x=dates, y = tired_scores)) + geom_point() + 
  stat_smooth(method = "lm", col = "blue")

anova(fit_date,fit_date2)



## Permutation test, looking at correlation between tired_levels and temp
## If this is nothing more than noise, then we should see similar results if we randomize scores across temps
library(psych)
a <- esm$tired_ESM
b <- esm$tempavg
c <- esm$Participant
alpha <- data.frame(a,b,c)
pack <- statsBy(alpha,group = "c")
s_star <- pack$rwg[2]
reps <- 1000
s_perm <- rep(NA,reps)
for(i in 1:reps){
  t_tired <- sample(esm$tired_ESM,replace = F)
  participant <- esm$Participant
  t_temps <- esm$tempavg
  t_df <- data.frame(t_tired,t_temps, participant)
  s_perm[i] <- cor(t_tired, t_temps, use = "complete.obs")
}

## Plot the permutation test histogram
s_perm <- as.data.frame(s_perm)
names(s_perm) <- "perms"
library(RColorBrewer)
myColors <- brewer.pal(8,"Set2")
h1 <- ggplot(s_perm, aes(perms)) + geom_histogram(binwidth = .0006, fill = myColors[8]) +
  geom_vline(aes(xintercept = s_star), colour = myColors[1], size = 3) + theme(legend.position="none") +
  xlab("Correlation between Temperature and Tiredness (From 53 degrees to 74 degrees)") +
  ggtitle("Permutation Test Results")

## 2. "Rainy Day" Emotions

## Another interesting find manifests itself when examining students emotions during a "rainy day"

## Show that low visibility and precip occured on same day
table(esm$visibility,esm$precip)

## t-test comparing rainy day emotions to non-rainy day emotions

#Significant
t.test(esm$calm_ESM[esm$visibility==5],esm$calm_ESM[esm$visibility!=5])

#Not significant
t.test(esm$enthusiast_ESM[esm$visibility==5],esm$enthusiast_ESM[esm$visibility!=5])

# Significant
t.test(esm$bored_ESM[esm$visibility==5],esm$bored_ESM[esm$visibility!=5])

#Significant
t.test(esm$stressed_ESM[esm$visibility==5],esm$stressed_ESM[esm$visibility!=5])

table(esm$SCHOOL_ESM[esm$precip>1],esm$grade_ESM[esm$precip>1])




## Was this CALM finding consistent across schools?
calm_means <- rep(NA, 8)
skool <- unique(esm$SCHOOL_ESM)
esm$rain_b <- rep(0,nrow(esm))
for(i in 1:nrow(esm)){
  if(esm$precip[i]>1){
    esm$rain_b[i] <- 1
  }
}
drip <- factor(unique(esm$rain_b))
for(j in 0:(length(skool)-1)){
  for(k in 1:length(drip)){
    calm_means[(2*j) + k] <- mean(esm$calm_ESM[esm$SCHOOL_ESM==skool[j+1] & esm$rain_b==drip[k]], na.rm=T)
  }
}
skool2 <- factor(c("Bassick (n=10)", "Bassick (n=10)", "Trumbull (n=608)", "Trumbull (n=608)",
                   "Cheshire (n=143)", "Cheshire (n=143)", "Fairchild (n=73)", "Fairchild (n=73)"), levels = c("Bassick (n=10)","Trumbull (n=608)", "Cheshire (n=143)","Fairchild (n=73)"))

drip2 <- rep(drip,4)
calms <- data.frame(calm_means,skool2,drip2)
calms

## Bar graph time, mean levels across different schools for RAIN/NOTRAIN
bc <- ggplot(data=calms,aes(x=skool2,y = calm_means, fill=drip2)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=round(calm_means,3)), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Calm Means Across Schools in No Rain v. Rain") +
  scale_fill_manual(values = c(myColors[8],myColors[1]),name="Weather",
                    breaks=c("0", "1"),
                    labels=c("No Rain", "Rain")) + scale_y_continuous(breaks = seq(0,3,by=0.5)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) + ylab("Mean Calm Levels") + xlab("Schools")
bc

## Was this CALM finding consistent across schools?
calm_means <- rep(NA, 8)
skool <- unique(esm$SCHOOL_ESM)
esm$rain_b <- rep(0,nrow(esm))
for(i in 1:nrow(esm)){
  if(esm$precip[i]>1){
    esm$rain_b[i] <- 1
  }
}
drip <- unique(esm$rain_b)
  for(j in 0:(length(skool)-1)){
    for(k in 1:length(drip)){
      calm_means[(2*j) + k] <- mean(esm$calm_ESM[esm$SCHOOL_ESM==skool[j+1] & esm$rain_b==drip[k]], na.rm=T)
    }
  }
skool2 <- c("Bassick (n=10)", "Bassick (n=10)", "Trumbull (n=608)", "Trumbull (n=608)",
            "Cheshire (n = 143)", "Cheshire (n = 143)", "Fairchild  (n=73)", "Fairchild  (n=73)")
drip2 <- rep(drip,4)
calms <- data.frame(calm_means,skool2,drip2)

## Was this TIRED finding consistent across schools?
tired_means <- rep(NA, 8)
skool <- unique(esm$SCHOOL_ESM)
drip <- unique(esm$rain_b)
for(j in 0:(length(skool)-1)){
  for(k in 1:length(drip)){
    tired_means[(2*j) + k] <- mean(esm$tired_ESM[esm$SCHOOL_ESM==skool[j+1] & esm$rain_b==drip[k]], na.rm=T)
  }
}
tireds<- data.frame(tired_means,skool2,drip2)

## Was this BORED finding consistent across schools?
bored_means <- rep(NA, 8)
skool <- unique(esm$SCHOOL_ESM)
drip <- unique(esm$rain_b)
for(j in 0:(length(skool)-1)){
  for(k in 1:length(drip)){
    bored_means[(2*j) + k] <- mean(esm$bored_ESM[esm$SCHOOL_ESM==skool[j+1] & esm$rain_b==drip[k]], na.rm=T)
  }
}
boreds<- data.frame(bored_means,skool2,drip2)


## Was this STRESSED finding consistent across schools?
stressed_means <- rep(NA, 8)
skool <- unique(esm$SCHOOL_ESM)
drip <- unique(esm$rain_b)
for(j in 0:(length(skool)-1)){
  for(k in 1:length(drip)){
    stressed_means[(2*j) + k] <- mean(esm$stressed_ESM[esm$SCHOOL_ESM==skool[j+1] & esm$rain_b==drip[k]], na.rm=T)
  }
}
stresseds <- data.frame(stressed_means,skool2,drip2)


## Bar graph time, mean levels across different schools for RAIN/NOTRAIN
bc <- ggplot(data=calms,aes(x=skool2,y = calm_means, fill=drip2)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label=round(calm_means,3)), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Calm Means Across Schools in No Rain v. Rain") +
  scale_fill_manual(values = c(myColors[8],myColors[1]),name="Weather",
                      breaks=c("0", "1"),
                      labels=c("No Rain", "Rain")) + scale_y_continuous(breaks = seq(0,3,by=0.5)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) + ylab("Mean Calm Levels") + xlab("Schools")

bc

bt <- ggplot(data=tireds,aes(x=skool2,y = tired_means, fill=drip2)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Tired Means Across Schools in No Rain v. Rain") +
  scale_fill_manual(values = c(myColors[8],myColors[1]),name="Weather",
                    breaks=c("0", "1"),
                    labels=c("No Rain", "Rain")) + scale_y_continuous(breaks = seq(0,3,by=0.5)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) + ylab("Mean Calm Levels") + xlab("Schools") +
  geom_text(aes(label=round(tired_means,3)), position=position_dodge(width=0.9), vjust=-0.25) 

bt

bb <- ggplot(data=boreds,aes(x=skool2,y = bored_means, fill=drip2)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Bored Means Across Schools in No Rain v. Rain") +
  scale_fill_manual(values = c(myColors[8],myColors[1]),name="Weather",
                    breaks=c("0", "1"),
                    labels=c("No Rain", "Rain")) + scale_y_continuous(breaks = seq(0,3,by=0.5)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) + ylab("Mean Calm Levels") + xlab("Schools") +
  geom_text(aes(label=round(bored_means,3)), position=position_dodge(width=0.9), vjust=-0.25) 

bb

bs <- ggplot(data=stresseds,aes(x=skool2,y = stressed_means, fill=drip2)) +
  geom_bar(stat="identity", position=position_dodge()) + ggtitle("Stressed Means Across Schools In Rain v. No Rain") +
  scale_fill_manual(values = c(myColors[8],myColors[1]),name="Weather",
                    breaks=c("0", "1"),
                    labels=c("No Rain", "Rain")) + scale_y_continuous(breaks = seq(0,3,by=0.5)) +  
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold")) + ylab("Mean Calm Levels") + xlab("Schools") +
  geom_text(aes(label=round(stressed_means,3)), position=position_dodge(width=0.9), vjust=-0.25) 

bs

## Why were students more calm, less tired, less bored, less stressed? Were they more engaged w/material b/c
## no distraction of nice weather outside?  Were they experiencing different levels of skill, challenge? Let's look:

## MAKING MEAN RAIN/INTEREST PLOTS

## Remove temps that had less than 100 observations
count <- NULL
for(i in 1:nrow(em_avg_vis_df)){
  if(em_avg_rain_df$num_obs[i] < 100){
    count <- c(count,i)
  }
}
if(length(count)>0){
  em_avg_rain_df <- em_avg_rain_df[-count,]
}
## Column of all emotions means
means5 <- c(em_avg_rain_df$calm_ESM,
            em_avg_rain_df$tired_ESM, em_avg_rain_df$bored_ESM,
            em_avg_rain_df$stressed_ESM, em_avg_rain_df$challenge_ESM,em_avg_rain_df$interested_ESM)

## Column of which column each mean was in for colors
len <- nrow(em_avg_rain_df)
Emotions <- rep(NA,len*6)
Emotions[1:len] <- "Calm"
Emotions[(len+1): (2*len)] <- "Tired"
Emotions[(2*len + 1):(3*len)] <- "Bored"
Emotions[(3*len + 1):(4*len)] <- "Stressed"
Emotions[(4*len + 1):(5*len)] <- "Challenged"
Emotions[(5*len + 1):(6*len)] <- "Interested"
## Temperature column
rain <- rep(em_avg_rain_df$rain,6)

## Repitions of the num of observations for sizing of points
obs <- rep(em_avg_rain_df$num_obs,6)

## Make the data frame
df1 <- data.frame(rain,obs,Emotions,means5)

## Making the visibility emotions plot
p5 <- ggplot(df1,aes(rain,means5, colour = Emotions))
p5 <- p5 + geom_point(aes(size=obs)) + geom_line() + ggtitle("Mean Emotions/Interest at Different Visibility Levels") +
  xlab("Rain (in inches)") + ylab("Emotion/Interest Levels (1 to 4)") + scale_size(name="Number of Observations")


## T-test comparing challenge levels (Significant)
t.test(esm$challenge_ESM[esm$precip>1],esm$challenge_ESM[esm$precip<1])


## T-test comparing interest levels (Not Significant)
t.test(esm$interested_ESM[esm$precip>1], esm$interested_ESM[esm$precip<1])




rainp <- unique(esm$Participant[esm$precip>1])

## Are calm scores of rain respondents the same as regular respondents when weather is nice
rainpT_calm <- esm$calm_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Trumbull"]
norainpT_calm <- esm$calm_ESM[esm$precip<1  & esm$SCHOOL_ESM=="Trumbull"]
rainpC_calm <- esm$calm_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Cheshire"]
norainpC_calm <- esm$calm_ESM[esm$precip<1 &  esm$SCHOOL_ESM=="Cheshire"]
rainpF_calm <- esm$calm_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Fairchild"]
norainpF_calm <- esm$calm_ESM[esm$precip<1 & esm$SCHOOL_ESM=="Fairchild"]

testCT <- t.test(rainpT_calm,norainpT_calm)
testCC <- t.test(rainpC_calm, norainpC_calm)
testCF <- t.test(rainpF_calm,norainpF_calm)


## Are bored scores of rain respondents the same as regular respondents when weather is nice
rainpT_bored <- esm$bored_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Trumbull"]
norainpT_bored <- esm$bored_ESM[esm$precip<1  & esm$SCHOOL_ESM=="Trumbull"]
rainpC_bored <- esm$bored_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Cheshire"]
norainpC_bored <- esm$bored_ESM[esm$precip<1 &  esm$SCHOOL_ESM=="Cheshire"]
rainpF_bored<- esm$bored_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Fairchild"]
norainpF_bored<- esm$bored_ESM[esm$precip<1 & esm$SCHOOL_ESM=="Fairchild"]

testBT <- t.test(rainpT_bored,norainpT_bored)
testBC <- t.test(rainpC_bored,norainpC_bored)
testBF <- t.test(rainpF_bored,norainpF_bored)

## Are stress scores of rain respondents the same as regular respondents when weather is nice
rainpT_stressed <- esm$stressed_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Trumbull"]
norainpT_stressed <- esm$stressed_ESM[esm$precip<1  & esm$SCHOOL_ESM=="Trumbull"]
rainpC_stressed <- esm$stressed_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Cheshire"]
norainpC_stressed <- esm$stressed_ESM[esm$precip<1 &  esm$SCHOOL_ESM=="Cheshire"]
rainpF_stressed <- esm$stressed_ESM[esm$precip<1 & esm$Participant %in% rainp & esm$SCHOOL_ESM=="Fairchild"]
norainpF_stressed <- esm$stressed_ESM[esm$precip<1 & esm$SCHOOL_ESM=="Fairchild"]

testST <- t.test(rainpT_stressed,norainpT_stressed)
testSC <- t.test(rainpC_stressed,norainpC_stressed)
testSF <- t.test(rainpF_stressed,norainpF_stressed)


fitsky <- lm(tired_ESM ~ tempavg, data=esm)
summary(fitsky)
fitsky2 <- lm(tired_ESM ~ date2, data=esm)
summary(fitsky2)
anova(fitsky,fitsky2)
