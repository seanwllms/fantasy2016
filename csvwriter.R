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

#create file for best remaining players
hitterpitcher <- bind_rows(hitter_projections, pitcher_projections) %>%
      arrange(desc(dollar.value)) %>%
      select(name, Team, position, marginal.total.points, dollar.value, status) %>%
      filter(status != "drafted") %>%
      filter(dollar.value > -5)

write.csv(hitterpitcher, "bestremaining.csv")


#write out draft errors to csv
write.csv(drafterrors, "drafterrors.csv")

#write standings output to file
standings.output <- select(standings,
                           team_name, spent, left, total_points, R_points, HR_points, RBI_points, 
                           SB_points, AVG_points, ERA_points, WHIP_points, K_points, SV_points, W_points)

write.csv(standings.output, file="standings.csv")

