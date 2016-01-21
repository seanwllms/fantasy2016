#read csv of draft picks
draftpicks <- read.csv("draftpicks.csv", stringsAsFactors = FALSE)

#draft everything in the draft csv
for (pick in 1:nrow(draftpicks)) {
      
      #pick out a row
      a <- draftpicks[pick,]
      
      #run draft function on that row
      draft(a[,1],a[,2],a[,3],a[,4])
      
      #remove leftover variable that isn't needed
      remove(a)
}
