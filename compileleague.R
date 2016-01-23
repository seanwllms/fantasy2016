setwd("/home/sean/Documents/rstuff/fantasy")
library(dplyr)

#load in coefficients file
if (!file.exists("coefs.rda")) {
  source("historyanalysis.R")
}

#load hitter and pitcher projections
if (!file.exists("hitter_projections.csv") & !file.exists("pitcher_projections.csv")) {
  source("calculatevalue.R")
} else{
  load("projections.rda")
}

#Build league
source("leaguesetup.R")

#run draft
source("draftpicks.R")

#merge in projections
source("mergeinprojections.R")

#calculate standings
source("calculatestandings.R")

#write to .csv
source("csvwriter.R")