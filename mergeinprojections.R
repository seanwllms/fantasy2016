batter_positions <- c("C1","C2","1B","2B","SS","3B","CI","MI","OF1","OF2","OF3","OF4","OF5","OF6","DH")
pitcher_positions <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")

#read in replacement level hitters
replacement_hitters <- read.csv("replacement_hitters.csv")
rownames(replacement_hitters) <- c("C","1B","2B","SS","3B","MI","CI","OF","DH")
replacement_hitters$AB <- c(400)
replacement_hitters[c("C1","C2"),] <- replacement_hitters["C",]
replacement_hitters[c("OF1","OF2","OF3","OF4","OF5","OF6"),] <- replacement_hitters["OF",]

#read in replacement level pitchers
replacement_pitcher <- c(150,4.47,1.4,4,1,102)


#####################################################################
#############MERGE IN PROJECTIONS FOR EACH PLAYER AND TEAM###########
#####################################################################

for (team in teams) {
      
      temp <- get(team)
            
      #separate hitters and pitchers
      hitters <- temp[batter_positions,]
      pitchers <- temp[pitcher_positions,]
      
      #merge in projections
      hitters <- left_join(hitters, hitter_projections, by = "name")
      pitchers <- left_join(pitchers, pitcher_projections, by = "name")
      
      #add columns for stats to team
      temp[c("playerid","roster_spot", "AB","R","HR","RBI","SB","AVG","IP","ERA","WHIP","SV","W","K")] <- NA
      
      #reassign rownames to merged projections
      rownames(hitters) <- hitters$roster_spot
      rownames(pitchers) <- pitchers$roster_spot
      

      #add hitter projections to team 
      for (position in batter_positions) {
            temp[position, c("playerid","AB","R","HR","RBI","SB","AVG")] <- 
                  hitters[position, c("playerid","AB","R","HR","RBI","SB","AVG")]
      }
      
      #add pitcher projections to team
      for (position in pitcher_positions) {
            temp[position, c("playerid","IP","ERA","WHIP","SV","W","K")] <- 
                  pitchers[position, c("playerid","IP","ERA","WHIP","SV","W","K")]
      }
      
      
      #add replacement level stats for empty hitter roster spots
      for (position in batter_positions) {
            if (temp[position,"name"] == "") {
                  temp[position,c("AB","R","HR","RBI","SB","AVG")] <- replacement_hitters[position,c("AB","Runs","HR","RBI","SB","AVG")]
            }
      }
      
      #add replacement level stats for pitcher spots
      for (position in pitcher_positions) {
            if (temp[position,"name"] == "") {
                  temp[position,c("IP","ERA","WHIP","SV","W","K")] <- replacement_pitcher
            }
      }
      
      
      
      assign(team, temp)
      
      remove(hitters)
      remove(pitchers)
      remove(temp)
      
}


#reorder rows.
for (team in teams) {
      
      temp <- get(team)
      
      temp <- temp[positions,]
      
      assign(team,temp)
}