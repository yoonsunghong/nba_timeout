rm(list=ls())
library(dplyr)
library(plyr)
nbaPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/data/"                  # Where NBA data is
dat <- read.csv(paste0(nbaPath, "2017-pbp.csv"),stringsAsFactors = F)
chance <- read.csv(paste0(nbaPath, "2017-chances.csv"), stringsAsFactors = F)
rootPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/timeout/"
genPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/timeout/report/general/"
aggPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/timeout/report/aggregate/"
dat$call <- substr(dat$text, 2, 4)
vec <- grep("Timeout", dat$text)
cvec <- dat$chance_id[vec]
dat_who <- dat[vec,]
dat_who$opp <- rep(0, nrow(dat_who))
for(i in 1:nrow(dat_who)) {
  if(dat_who$call[i] == dat_who$home_abbrev[i]) {
    dat_who$opp[i] <- dat_who$away_abbrev[i]
  }
  else {
    dat_who$opp[i] <- dat_who$home_abbrev[i]
  }
}
timeout <- chance[match(cvec,chance$chance_id),]
timeout <- timeout %>%
  select(end_game_clock,period,game_code)
timeout$call <- dat_who$call
timeout$opponents <- dat_who$opp
timeout$minute <- floor(timeout$end_game_clock/60)
timeout$end_game_clock <- round(timeout$end_game_clock, 0)
timeout$second <- timeout$end_game_clock - (timeout$minute*60)
timeout <- na.omit(timeout)
timeout$qtrtime <- paste(timeout$period, "quarter,", timeout$minute, "minute(s)", timeout$second, "seconds left", sep = " ")
#simplifying the game number
timeout$game_code <- gsub('.{2}$', '', timeout$game_code)
#new dataframe for avg values
timeout2 <- timeout
#creating variables for each periods
timeout2$q1_12 <- rep(0, nrow(timeout2))
timeout2$q1_9 <- rep(0, nrow(timeout2))
timeout2$q1_6 <- rep(0, nrow(timeout2))
timeout2$q1_3 <- rep(0, nrow(timeout2))
timeout2$q2_12 <- rep(0, nrow(timeout2))
timeout2$q2_9 <- rep(0, nrow(timeout2))
timeout2$q2_6 <- rep(0, nrow(timeout2))
timeout2$q2_3 <- rep(0, nrow(timeout2))
timeout2$q3_12 <- rep(0, nrow(timeout2))
timeout2$q3_9 <- rep(0, nrow(timeout2))
timeout2$q3_6 <- rep(0, nrow(timeout2))
timeout2$q3_3 <- rep(0, nrow(timeout2))
timeout2$q4_12 <- rep(0, nrow(timeout2))
timeout2$q4_9 <- rep(0, nrow(timeout2))
timeout2$q4_6 <- rep(0, nrow(timeout2))
timeout2$q4_3 <- rep(0, nrow(timeout2))
timeout2$q4_2half <- rep(0,nrow(timeout2))
timeout2$q4_2 <- rep(0,nrow(timeout2))
timeout2$q4_1half <- rep(0,nrow(timeout2))
timeout2$q4_1 <- rep(0,nrow(timeout2))
timeout2$q4_half <- rep(0,nrow(timeout2))
for(i in 1:nrow(timeout2)) {
  if(timeout2$period[i] == 1 & timeout2$end_game_clock[i] <=720 & timeout2$end_game_clock[i] >= 540) {
    timeout2$q1_12[i] <- 1
  }
  else if(timeout2$period[i] == 1 & timeout2$end_game_clock[i] <540 & timeout2$end_game_clock[i] >= 360) {
    timeout2$q1_9[i] <- 1
  }
  else if(timeout2$period[i] == 1 & timeout2$end_game_clock[i] <360 & timeout2$end_game_clock[i] >= 180) {
    timeout2$q1_6[i] <- 1
  }
  else if(timeout2$period[i] == 1 & timeout2$end_game_clock[i] <180) {
    timeout2$q1_3[i] <- 1
  }
  else if(timeout2$period[i] == 2 & timeout2$end_game_clock[i] <=720 & timeout2$end_game_clock[i] >= 540) {
    timeout2$q2_12[i] <- 1
  }
  else if(timeout2$period[i] == 2 & timeout2$end_game_clock[i] <540 & timeout2$end_game_clock[i] >= 360) {
    timeout2$q2_9[i] <- 1
  }
  else if(timeout2$period[i] == 2 & timeout2$end_game_clock[i] <360 & timeout2$end_game_clock[i] >= 180) {
    timeout2$q2_6[i] <- 1
  }
  else if(timeout2$period[i] == 2 & timeout2$end_game_clock[i] <180) {
    timeout2$q2_3[i] <- 1
  }
  else if(timeout2$period[i] == 3 & timeout2$end_game_clock[i] <=720 & timeout2$end_game_clock[i] >= 540) {
    timeout2$q3_12[i] <- 1
  }
  else if(timeout2$period[i] == 3 & timeout2$end_game_clock[i] <540 & timeout2$end_game_clock[i] >= 360) {
    timeout2$q3_9[i] <- 1
  }
  else if(timeout2$period[i] == 3 & timeout2$end_game_clock[i] <360 & timeout2$end_game_clock[i] >= 180) {
    timeout2$q3_6[i] <- 1
  }
  else if(timeout2$period[i] == 3 & timeout2$end_game_clock[i] <180) {
    timeout2$q3_3[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <=720 & timeout2$end_game_clock[i] >= 540) {
    timeout2$q4_12[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <540 & timeout2$end_game_clock[i] >= 360) {
    timeout2$q4_9[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <360 & timeout2$end_game_clock[i] >= 180) {
    timeout2$q4_6[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <180 & timeout2$end_game_clock[i] >= 150) {
    timeout2$q4_3[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <150 & timeout2$end_game_clock[i] >= 120) {
    timeout2$q4_2half[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <120 & timeout2$end_game_clock[i] >= 90) {
    timeout2$q4_2[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <90 & timeout2$end_game_clock[i] >= 60) {
    timeout2$q4_1half[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <60 & timeout2$end_game_clock[i] >= 30) {
    timeout2$q4_1[i] <- 1
  }
  else if(timeout2$period[i] == 4 & timeout2$end_game_clock[i] <30 ) {
    timeout2$q4_half[i] <- 1
  }
}
#splitting into different teams
teamlist <- split(timeout, timeout$call)
timeout2 <- timeout2 %>%
  select(call, q1_12,q1_9,q1_6,q1_3,q2_12,q2_9,q2_6,q2_3,q3_12,q3_9,q3_6,q3_3,q4_12,q4_9,q4_6,q4_3, q4_2half, q4_2, q4_1half, q4_1, q4_half)
teamlist2 <- split(timeout2, timeout$call)

#for loops for export
for(i in 1:30) {
  team <- as.data.frame(teamlist[i])
  team <- team[,c(3,8,5)]
  colnames(team) <- c("Game Number", "Time in the quarter", "Opponents")
  write.table(team, file = paste0(genPath,names(teamlist[i]),'.csv'), sep=',', row.names = FALSE)
}
#for avg
for(i in 1:30) {
  team2 <- as.data.frame(teamlist2[i])
  q1_12 <- sum(team2[,2])
  q1_9 <- sum(team2[,3])
  q1_6 <- sum(team2[,4])
  q1_3 <- sum(team2[,5])
  q2_12 <- sum(team2[,6])
  q2_9 <- sum(team2[,7])
  q2_6 <- sum(team2[,8])
  q2_3 <- sum(team2[,9])
  q3_12 <- sum(team2[,10])
  q3_9 <- sum(team2[,11])
  q3_6 <- sum(team2[,12])
  q3_3 <- sum(team2[,13])
  q4_12 <- sum(team2[,14])
  q4_9 <- sum(team2[,15])
  q4_6 <- sum(team2[,16])
  q4_3 <- sum(team2[,17])
  q4_2half <- sum(team2[,18])
  q4_2 <- sum(team2[,19])
  q4_1half <- sum(team2[,20])
  q4_1 <- sum(team2[,21])
  q4_half <- sum(team2[,22])
  here <- c(q1_12,q1_9,q1_6,q1_3,q2_12,q2_9,q2_6,q2_3,q3_12,q3_9,q3_6,q3_3,q4_12,q4_9,q4_6,q4_3, q4_2half, q4_2, q4_1half, q4_1, q4_half)
  name <- c("1st quarter, 9 minutes remaining", "1st quarter, 6 minutes remaining","1st quarter, 3 minutes remaining", "1st quarter, end period",
            "2nd quarter, 9 minutes remaining", "2nd quarter, 6 minutes remaining","2nd quarter, 3 minutes remaining", "2nd quarter, end period",
            "3rd quarter, 9 minutes remaining", "3rd quarter, 6 minutes remaining","3rd quarter, 3 minutes remaining", "3rd quarter, end period",
            "4th quarter, 9 minutes remaining", "4th quarter, 6 minutes remaining","4th quarter, 3 minutes remaining", "4th quarter, 2 and 1/2 minutes remaining",
            "4th quarter, 2 minutes remaining", "4th quarter, 1 and 1/2 minutes remaining",
            "4th quarter, 1 minutes remaining", "4th quarter, 1/2 minutes remaining", "4th quarter, end period")
  table <- rbind(name,here)
  write.table(table, file = paste0(aggPath,names(teamlist[i]),'.csv'), sep=',', row.names = FALSE, col.names = FALSE)
}