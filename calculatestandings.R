standings <- data.frame()

#Calculate current standings
for (team in teams) {
      
      temp <- get(team)
      
      #get team's stats
      team_R <- sum(temp$R[1:15], na.rm=TRUE)
      team_HR <- sum(temp$HR[1:15], na.rm=TRUE)
      team_RBI <- sum(temp$RBI[1:15], na.rm=TRUE)
      team_SB <- sum(temp$SB[1:15], na.rm=TRUE)
      team_AVG <- sum(temp$AVG*temp$AB[1:15], na.rm=TRUE) / sum(temp$AB, na.rm =TRUE)
      
      team_ERA <- sum(temp$ERA[16:25] * temp$IP[16:25], na.rm=TRUE) /sum(temp$IP[16:25], na.rm=TRUE) 
      team_WHIP <- sum(temp$WHIP[16:25] * temp$IP[16:25], na.rm=TRUE) /sum(temp$IP[16:25], na.rm=TRUE) 
      team_K <- sum(temp$K[16:25], na.rm=TRUE)
      team_SV <- sum(temp$SV[16:25], na.rm=TRUE)
      team_W <- sum(temp$W[16:25], na.rm=TRUE)
      
      team_salary <- sum(temp$salary, na.rm=TRUE)
      salary_left <- 260-sum(temp$salary, na.rm=TRUE)
      
      picks_left <- sum(temp$Name[1:25]=="")
      dollars_per_pick <- salary_left/picks_left
      
      max_bid <- salary_left + 1 - picks_left
      
      #merge team's stats in to standings
      standings <- data.frame(
                  team_name = c(standings$team_name, team),
                  R =  c(standings$R, team_R),
                  HR = c(standings$HR,team_HR),
                  RBI = c(standings$RBI,team_RBI),
                  SB = c(standings$SB, team_SB),
                  AVG = round(c(standings$AVG,team_AVG),3),
                  ERA = round(c(standings$ERA, team_ERA),2),
                  WHIP = round(c(standings$WHIP, team_WHIP),2),
                  K = c(standings$K, team_K),
                  SV = c(standings$SV, team_SV),
                  W = c(standings$W, team_W),
                  spent = c(standings$spent, team_salary), 
                  left = c(standings$left, salary_left),
                  picks_left <- c(standings$picks_left, picks_left),
                  dollars_per_pick <- c(standings$dollars_per_pick, dollars_per_pick),                  
                  max_bid <- c(standings$max_bid, max_bid),
                  stringsAsFactors = FALSE
            )
}

names(standings)[14] <- "picks_left"
names(standings)[15] <- "dollars_per_pick"
names(standings)[16] <- "max_bid"

#calculate points
standings$R_pts <- rank(standings$R)
standings$HR_pts <- rank(standings$HR)
standings$RBI_pts <- rank(standings$RBI)
standings$SB_pts <- rank(standings$SB)
standings$AVG_pts <- rank(standings$AVG)

standings$ERA_pts <- 19-rank(standings$ERA)
standings$WHIP_pts <- 19-rank(standings$ERA)
standings$K_pts <- rank(standings$K)
standings$SV_pts <- rank(standings$SV)
standings$W_pts <- rank(standings$W)


standings$total_points <-     standings$ERA_pts +
                              standings$WHIP_pts + 
                              standings$K_pts +
                              standings$SV_pts +
                              standings$W_pts +
                              standings$R_pts +
                              standings$HR_pts +
                              standings$RBI_pts +      
                              standings$SB_pts +
                              standings$AVG_pts 

standings$total_points <- round(standings$total_points, 0)

#Rownames
rownames(standings) <- standings$team_name

#Sort by total points
standings <- standings[order(-standings$total_points),]


