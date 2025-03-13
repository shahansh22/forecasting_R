Spotify = read.csv("Spotify_Chart(1).csv")
head(Spotify)
summary(Spotify)
Spotify_2017 <- Spotify %>% filter(Year == 2017)
Spotify_Surv <- Surv(Spotify_2017$time, Spotify_2017$delta)
Spotify_Survfit <- survfit(Spotify_Surv ~ 1)
plot(Spotify_Survfit, conf.int="none", mark.time=TRUE, xlab="Time (days)", ylab="Proportion
Survival")
Spotify_Survfit
head(summary(Spotify_Survfit))

compare <- survfit(Surv(time = time, event = delta) ~ Year, data = Spotify)
plot(compare, xlab="Time (days)", ylab="Proportion Survival",col= c('red','blue') , mark.time=TRUE)
legend("topright", c("2017", "2018"), lty = 1 ,col= c('red','blue'))

survdiff(Surv(time = time, event = delta) ~ Year, data = Spotify)

Spotify_2017_Sheeran <- Spotify %>% filter(Artist == 'Ed Sheeran' & Year == 2017)
Spotify_Surv_Sheeran2017 <- Surv(Spotify_2017_Sheeran$time, Spotify_2017_Sheeran$delta)
Spotify_Surv_Sheeran2017

Spotify_Survfit_Sheeran2017 <- survfit(Spotify_Surv_Sheeran2017 ~ 1)
plot(Spotify_Survfit_Sheeran2017,conf.int="none",mark.time=TRUE,xlab="Time(days)",
     ylab="Proportion Survival")
title("A study of ‘incredible longevity’ of a generic pop music in chart.")

Spotify_Drake <- Spotify %>% filter(Artist == 'Drake')
compare <- survfit(Surv(time = time, event = delta) ~ Year, data = Spotify_Drake)
plot(compare, xlab="Time (days)", ylab="Proportion Survival",col= c('red','blue') , mark.time=TRUE)
title("Drake Fight V17/18")
legend("topright", c("2017", "2018"), lty = 1 ,col= c('red','blue'))

Spotify_Drake <- Spotify %>% filter(Artist == 'Kanye West')
compare <- survfit(Surv(time = time, event = delta) ~ Year, data = Spotify_Drake)
plot(compare, xlab="Time (days)", ylab="Proportion Survival",col= c('red','blue') , mark.time=TRUE)
title("Kanye Fight V17/18")
legend("topright", c("2017", "2018"), lty = 1 ,col= c('red','blue'))

Spotify_Drake <- Spotify %>% filter(Artist == 'Twenty One Pilots')
compare <- survfit(Surv(time = time, event = delta) ~ Year, data = Spotify_Drake)
plot(compare, xlab="Time (days)", ylab="Proportion Survival",col= c('red','blue') , mark.time=TRUE)
title("TOP Fight V17/18")
legend("topright", c("2017", "2018"), lty = 1 ,col= c('red','blue'))
