# How people were recruited: Went to schools, talked to principals, put the word out throught
# emails and speaker systems and other channels.  Parent permission was given (online).
# Incentive: $40 Gifts cards 90 % threshold in theory, link was given 90% of the way through the test
# Difficult thing to do: Get variables next to each other
# Six schools: Trumbull, Chesire, Bessick, Fairchild-Wheeler
# Evening beeps, restrospective beeps, don't include eveening surveys
# ethusiast_ESM through choice
# Check to make sure the schedules are further apart than an hour
# Call Julia Moeller: GET HER NUMBER
# USE Particpant for ESM
# Linking ESM/BG use Code_Merge_Final
# Imputed answers in the BG survey, can we predict, use Regression approach, markov chain to maintain
# similar distribution. 

#install.packages("foreign")
library(foreign)

x <- read.spss(file = "dataset.sav", to.data.frame = T)
y <- x[x$BG_OR_ESM=="ESM",]
#weather <- read.csv("weather.csv", as.is=T)

#Use Meriden Markham Municipal Airport for Chesire Kids
# Use Sikorsky Memorial Airport for Trumbull Kids


#write.table(x, file = "dataset.csv", quote = FALSE, sep = ",")

a
table(x$SCHOOLS_ALL)
table(x$Session_Name)
table(y$SCHOOLS_ESM)
table(x$BG_OR_ESM)
table(x$Code_Merge_Final)


## Only selecting the columns we care about for the analysis
beep <- y[,709:740]

## These are the new columns we are creating
beep$date_coded <- NA
beep$session_char <- as.character(beep$Session_Name)
beep$A2 <- NA
beep$B2 <- NA
beep$C2 <- NA
beep$D2 <- NA
beep$E2 <- NA
beep$F2 <- NA
beep$G2 <- NA
beep$H2 <- NA
beep$I2 <- NA
beep$J2 <- NA
beep$K2 <- NA
beep$L2 <- NA
beep$M2 <- NA
beep$N2 <- NA
beep$O2 <- NA
beep$P2 <- NA

## Renaming old columns
library(plyr)
beep <- rename(beep, c("enthusiast_ESM" = "A",
            "happy_ESM" = "B",
            "interested_ESM" = "C",
            "curious_ESM" = "D",
            "calm_ESM" = "E",
            "relaxed_ESM" = "F",
            "frustrated_ESM" = "G",
            "anxious_ESM" = "H",
            "afraid_ESM" = "I",
            "tired_ESM" = "J",
            "sad_ESM" = "K",
            "bored_ESM" = "L",
            "stressed_ESM" = "M",
            "challenge_ESM" = "N",
            "skills_ESM" = "O",
            "choice" = "P"))

## Removing Evening Entries
rem <- c()
for(i in 1: nrow(beep)){
  if(beep$session_char[i] == unique(beep$session_char)[2] | 
     beep$session_char[i]== unique(beep$session_char)[4]) {
    rem <- c(rem,i)
  }
}
beep <- beep[-rem,]

## Breaking the beeps down by participant
beep2 <- split(beep,beep$Participant)


## Coding dates
M <- length(beep2)
for(i in 1:M){
  N <- dim(beep2[[i]])[1]
  temp <- 1
  init <- as.numeric(beep2[[i]]$Date[1])
  for(j in 1:N){
    tempdate <- as.numeric(beep2[[i]]$Date[j])
    if(tempdate > init){
      init <- tempdate
      temp <- temp + 1
    }
    beep2[[i]]$date_coded[j] <- temp
  }
}

## Loop for filling in emotions at time t+1
for(i in 1:M){ # Number of participants
  N <- dim(beep2[[i]])[1] # Number of responses from participant 'i'
  if(N > 1){ #
    for(j in 1:(N-1)){
      if(beep2[[i]]$date_coded[j]==beep2[[i]]$date_coded[j+1]){
        beep2[[i]][j,35:50] <- beep2[[i]][(j+1),16:31]
      }
    }
  }
}

## Turning the list back into a data frame so correlations can be run
df2 <- do.call(rbind.data.frame, beep2)

## Selecting only the emotions at time t and emotions at time t+1
df_final <- df2[,c(3,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,35,
                   36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)]
df_export <- df2[,c(3,4,5,6,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,35,
                    +                    36,37,38,39,40,41,42,43,44,45,46,47,48,49,50)]

df_export <- df_export[,c(1,2,3,4,5,21,6,22,7,23,8,24,9,25,10,26,11,27,12,28,13,29,14,30,15,31,16,32,17,33,18,
                         34,19,35,20,36)]


## Sanity check to make sure df_final is only emotions
names(df_final)
names(df_export)

## Partial Correlations/Regression Coefficients
##install.packages("psych")
library(psych)
t_val <- matrix(nrow=16,ncol=16)
regression_final <- matrix(nrow=16,ncol=16)
len <- 16
participant <- df_final$Participant
for(i in 1:len){
  temp1 <- df_final[,i+1]
  for(j in 1:len){
    temp2 <- df_final[,j+17]
    tempdf <- data.frame(temp1,temp2,participant)
    sb <- lm(temp2 ~ temp1, data = tempdf)
    regression_final[i,j] <- sb$coefficients[2]
    sb2 <- summary(sb)[4]
    t_val[i,j] <- sb2[[1]][2,4]
  }
  print(i)
}
not_sig <- which(abs(t_val)>0.05)
regression_final[not_sig] <- NA
rownames(regression_final) <- c("Enthusiastic", "Happy", "Interested", "Curious", "Calm", "Relaxed", "Frustrated",
                                "Anxious", "Afraid","Tired", "Sad", "Bored", "Stressed", "Challenged","Skilled",
                                "Choice")
colnames(regression_final) <- c("Enthusiastic2", "Happy2", "Interested2", "Curious2", "Calm2", "Relaxed2", "Frustrated2",
                                "Anxious2", "Afraid2","Tired2", "Sad2", "Bored2", "Stressed2", "Challenged2","Skilled2",
                                "Choice2")

write.csv(df_final, file = "ESM_network_stuff.csv", row.names = F)
write.csv(correlation_final,file = "ESM_correlations.csv")

## Writing the SPSS file to do multi-level correlations
write.foreign(df_export, "ESM_export.txt", "ESM_export.sps",   package="SPSS")


bfi <- read.csv("bfi.csv",as.is=T)
