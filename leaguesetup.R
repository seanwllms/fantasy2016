################################################################
#################LEAGUE STUFF LIVES HERE########################
################################################################

#list of teams
teams <- c("marmaduke",
           "pk dodgers",
           "ottawa",
           "isotopes",
           "jobu",
           "d&s",
           "hermanos",
           "deano",
           "dembums",
           "bellevegas",
           "baycity",
           "balco",
           "sturgeon",
           "rippe",
           "pasadena",
           "deener",
           "counsel",
           "bears")

positions <- c("C1","C2","1B","2B","SS","3B","CI","MI","OF1","OF2","OF3","OF4","OF5","OF6","DH",
               "P1","P2","P3","P4","P5","P6","P7","P8","P9","P10",
               "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10")


#create data frame for each team.
for (team in teams) {
      
      assign(team,
             data.frame(roster_spot = positions,
                        salary = 0,
                        name = "",
                        row.names = positions,
                        stringsAsFactors = FALSE
             ),
             env = .GlobalEnv
      )
}
