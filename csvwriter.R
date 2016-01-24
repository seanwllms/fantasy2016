hitter_projections$status <- ""
pitcher_projections$status <- ""

#Mark drafted hitters as drafted.
for (player in hitter_projections$Name) {
      if (player %in% draftpicks$player) {
            hitter_projections[which(hitter_projections$Name == player),"status"] <- "drafted"
      }
}

#Mark drafted pitchers as drafted.
for (player in pitcher_projections$Name) {
      if (player %in% draftpicks$player) {
            pitcher_projections[which(pitcher_projections$Name == player),"status"] <- "drafted"
      }
}

#Merge hitter and pitcher projections
player_projections <- rbind_list(hitter_projections, pitcher_projections) %>% arrange(desc(dollar.value))


#write player projections to csv
write.csv(player_projections, file = "combined_projections.csv")

#write standings output to file
standings.output <- select(standings,
                           team_name, spent, left, total_points, R_points, HR_points, RBI_points, 
                           SB_points, AVG_points, ERA_points, WHIP_points, K_points, SV_points, W_points)

write.csv(standings.output, file="standings.csv")

