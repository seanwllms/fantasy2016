#set up file
setwd("C:/Users/Sean/Documents/Fantasy/fantasybaseball2016")
library(dplyr)

###Load the coefficients data frame
load("coefs.rda")

###############################################################
################HITTER STUFF LIVES HERE#########################
################################################################

#Import and clean data on replacement levels

#read in league wide csv
replacement_hitters <- read.csv("replacement_hitters.csv", stringsAsFactors = FALSE)

replacement_hitters$Position <- c("catcher",
                                  "first_base",
                                  "second_base",
                                  "shortstop",
                                  "third_base",
                                  "middle_infield",
                                  "corner_infield",
                                  "outfield",
                                  "dh"
                                  )

names(replacement_hitters)[2:6] <- sapply(names(replacement_hitters[c(2:6)]), paste, ".repl", sep="")

#rename columns
names(replacement_hitters) <- c("position",
                                "runs",
                                "hr",
                                "rbi",
                                "sb",
                                "avg")

#list of file names
filelocs <- sapply("./steamer/", paste, list.files("./steamer"), sep="")

#read in hitterdata
hitterdata <- lapply(filelocs, read.csv, header=TRUE, stringsAsFactors = FALSE)

#keep only variables I care about
hitterdata <- lapply(hitterdata, select, ï..Name, Team, PA, R,HR, RBI, SB, AVG, OBP, playerid)

#rename columns
hitterdata <- lapply(hitterdata, function(x) {colnames(x)[1] <- "name" 
                                              return(x)})

#create projection dataframes for each position
grab.repl <- function(pos) {
      temp <- filter(replacement_hitters, position==pos)
      names(temp)[2:6] <- sapply(names(temp)[2:6], paste, ".repl", sep="")
      return(temp)
}

      #1b
      first.base.proj <- cbind(hitterdata[[1]], grab.repl("first_base"))
      
      #2b
      second.base.proj <- cbind(hitterdata[[2]], grab.repl("second_base"))
      
      #3b
      third.base.proj <- cbind(hitterdata[[3]], grab.repl("third_base"))
      
      #C
      catcher.proj <- cbind(hitterdata[[4]], grab.repl("catcher"))
      
      #dh
      dh.proj <- cbind(hitterdata[[5]], grab.repl("dh"))
      
      #of
      of.proj <- cbind(hitterdata[[6]], grab.repl("outfield"))
      
      #SS
      shortstop.proj <- cbind(hitterdata[[7]], grab.repl("shortstop"))
      
#build all positional projections into a list
projections <- list(first.base.proj,
                    second.base.proj,
                    third.base.proj,
                    catcher.proj,
                    dh.proj,
                    of.proj,
                    shortstop.proj)

#convert coefficients frame to a normal data frame
coefs.for.calc <- as.numeric(coefs$estimate)

names(coefs.for.calc) <- coefs$Category

#create function to calculate value for a position
calculate.value <- function(df) {
      mutate(df, 
             marginal.hr = HR - hr.repl, 
             marginal.runs = R - runs.repl,
             marginal.rbi = RBI - rbi.repl,
             marginal.sb = SB - sb.repl,
             marginal.avg = AVG - avg.repl,
             marginal.runs.points = marginal.runs * coefs.for.calc[["r"]],
             marginal.hr.points = marginal.hr * coefs.for.calc[["hr"]],
             marginal.rbi.points = marginal.rbi * coefs.for.calc[["rbi"]],
             marginal.sb.points = marginal.sb * coefs.for.calc[["sb"]],
             marginal.avg.points = marginal.avg * coefs.for.calc[["avg"]]/15,
             marginal.total.points = (marginal.runs.points +
                                    marginal.hr.points +
                                    marginal.rbi.points +
                                    marginal.avg.points +
                                    marginal.sb.points)*1.2,#this is an adjustment upwards;
                                                            #in 2016 calculation, top 270 players
                                                            #result in only 735.5 marginal points.
                                                            #this means we adjust upwards to make 855
                                                            #marginal points (sum(1:18)*5)
             
             #total of 4680 dollars exist in the league. 1700 marginal points exist. Therefore, marginal
             #point is worth 4680/1700
             dollar.value = marginal.total.points*(4680/1700)
      )      
}

#calculate values for all of the positions
projections <- lapply(projections, calculate.value)

#merge projections for different positions together.
projections <- do.call(rbind, projections)

#get player's strongest position
projections <- projections %>%
      group_by(playerid) %>%
      mutate(times.appears = n(), max.points = max(dollar.value)) %>%
      filter(position != "dh" | times.appears==1) %>%
      filter(dollar.value==max.points) %>%
      ungroup() %>%
      arrange(desc(dollar.value)) %>%
      select(name, Team, position, playerid, PA, R, HR, RBI, SB, AVG, marginal.total.points, dollar.value) %>%
      mutate( marginal.total.points = round(marginal.total.points, 2),
              dollar.value = round(dollar.value, 2)) %>%
      filter(PA > 1)


################################################################
################PITCHER STUFF LIVES HERE########################
################################################################

#read in projections
pitchers <- read.csv("pitchers.csv", stringsAsFactors=FALSE)

#keep only relevant columns
pitchers <- select(pitchers,ï..Name,Team,W,ERA,SV,IP,SO,WHIP,playerid) %>%
      mutate(position = "pitcher")

names(pitchers)[c(1, 7)] <- c("name", "K")

#create replacement pitcher values
#these are the mean projections for the 170th through 190th best players
replacement.pitcher <- c(3.761429,1.284286,5.523810,2.952381,88.714286)
names(replacement.pitcher) <- c("ERA.repl","WHIP.repl","W.repl","SV.repl","K.repl")

#calculate marginal values and points
pitchers <- pitchers %>%
      mutate(
            marginal.ERA = ERA - replacement.pitcher["ERA.repl"],
            marginal.WHIP = WHIP - replacement.pitcher["WHIP.repl"],
            marginal.W = W - replacement.pitcher["W.repl"],
            marginal.SV = SV - replacement.pitcher["SV.repl"],
            marginal.K = K - replacement.pitcher["K.repl"],
            ERA.points = (marginal.ERA *coefs.for.calc[["era"]])*(IP/1464),
            WHIP.points = (marginal.WHIP*coefs.for.calc[["whip"]])*(IP/1464),
            W.points = marginal.W*coefs.for.calc[["w"]],
            SV.points = marginal.SV*coefs.for.calc[["sv"]],
            K.points = marginal.K*coefs.for.calc[["k"]],
            marginal.total.points =  1.16*(ERA.points + WHIP.points + W.points + SV.points + K.points),
            dollar.value = marginal.total.points*(4680/1700)
      ) %>%
      
      #sort by dollar value
      arrange(desc(dollar.value)) %>%
      
      #select relevant columns
      select(name,Team,position,playerid,IP,ERA,WHIP,SV,W,K,marginal.total.points,dollar.value) %>%
      
      #round points and dollars columns
      mutate(marginal.total.points = round(marginal.total.points, 2), dollar.value = round(dollar.value, 2)) %>%
      
      #select only pithcers with at least 1 IP
      filter(IP > 1)

#create file for player projections
hitterpitcher <- bind_rows(projections, pitchers) %>%
      arrange(desc(dollar.value)) %>%
      select(name, Team, position, marginal.total.points, dollar.value)

#write both files out to csv files
write.csv(pitchers, file = "pitcher_projections.csv")
write.csv(projections, file = "hitter_projections.csv")
write.csv(hitterpitcher, file = "player_projections.csv")


