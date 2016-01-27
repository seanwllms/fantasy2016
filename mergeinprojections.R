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

      #add replacement level stats for hitters
      undrafted.hitters <- filter(hitters, is.na(playerid))
      undrafted.pitchers <- filter(pitchers, is.na(playerid))
      
      #pull in replacement level stats to undrafted df
      undrafted.hitters[,c("AB","R","HR","RBI","SB","AVG")] <- replacement_hitters[undrafted.hitters$roster_spot,c(7,2:6)]
      undrafted.pitchers[,c("IP","ERA","WHIP","SV","W","K")] <- sapply(replacement_pitcher, rep, nrow(undrafted.pitchers))
      
      #insert replacement level stats in hitters and pitchers df
      hitters[hitters$roster_spot %in% undrafted.hitters$roster_spot, 7:12] <- undrafted.hitters[,7:12]
      pitchers[pitchers$roster_spot %in% undrafted.pitchers$roster_spot, 7:12] <- undrafted.pitchers[,7:12]
      
      #merge hitters and pitchers
      temp <- rbind_list(hitters, pitchers)
      
      assign(team, temp)
      
      remove(hitters, pitchers, temp, undrafted.pitchers, undrafted.hitters)
      
}

