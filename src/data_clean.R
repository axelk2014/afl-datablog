# Rscript file created for data cleanup
# created 27/12/16


# using xlsx downloaded from: 
# http://www.aussportsbetting.com/data/historical-afl-results-and-odds-data/
# http://www.aussportsbetting.com/historical_data/afl.xlsx
# convert to csv

## code to download and convert - usually commented out
#download.file("http://www.aussportsbetting.com/historical_data/afl.xlsx","data/raw/afl.xlsx",method="curl")
#print("Finish downloading afl.xlsx")
system("in2csv data/raw/afl.xlsx > data/raw/afl.csv")
print("Finish converting afl.xlsx -> afl.csv")

## AFL fixtures
tmp.afl <- read.csv("data/raw/afl.csv", skip=1,header = TRUE)
# convert date from numeric to Date (in2csv bug)
tmp.afl$Date <- as.Date(tmp.afl$Date, origin="1899-12-30")

# subset dataframe to remove betting odds columns
tmp.afl <- tmp.afl[,1:12]

# create vector of all teams and replace spaces with periods (R friendly)
teams <- unique(tmp.afl$Away.Team)
teams <- gsub(" ",".", teams)

# data frame to sum goals and behinds by team
teams.goals <- paste(teams, ".Goals", sep="")
teams.behinds <- paste(teams, ".Behinds", sep="")

tmp.afl[,teams.goals] <- NA
tmp.afl[,teams.behinds] <- NA

# convert date field to Date data type
tmp.afl$Date <- as.Date(tmp.afl$Date,"%d-%b-%y")

# the following two for loops populate the teamname.Goals and teamname.Behinds
# columns for plotting. the first loop parses the Home games stats, 
# the second loop parses the Away games stats

for ( teamname in unique(tmp.afl$Home.Team) ) {
  
  RowIndex = which(tmp.afl$Home.Team == teamname)
  # find ncol index of appropriate columns
  ColIndex = which( substr(names(tmp.afl),1,nchar(teamname)) %in% gsub(" ",".", teamname))
  
  # Home Goals
  tmp.afl[RowIndex, ColIndex[1]] <- tmp.afl[RowIndex,9 ]
  # Home Behinds
  tmp.afl[RowIndex, ColIndex[2]] <- tmp.afl[RowIndex,10 ]
  
}

for ( awayteamname in unique(tmp.afl$Away.Team )) {
  
  RowIndex = which(tmp.afl$Away.Team == awayteamname)
  # find ncol index of appropriate columns
  ColIndex = which( substr(names(tmp.afl),1,nchar(awayteamname)) %in% gsub(" ",".", awayteamname))
  
  # Home Goals
  tmp.afl[RowIndex, ColIndex[1]] <- tmp.afl[RowIndex,11 ]
  # Home Behinds
  tmp.afl[RowIndex, ColIndex[2]] <- tmp.afl[RowIndex,12 ]
  
}

# STADIUM names
# df.Stadii is dataframe with different names for each oval and state
# as extracted from Wikipedia
df.Stadium <- as.data.frame(unique(tmp.afl$Venue))
df.Stadii <- read.csv("data/stadii", header=FALSE)

# merge home/away goals/behind dataframe with stadium dataframe
df.games.all <- merge(tmp.afl,df.Stadii, by.x="Venue", by.y="V1")

# remove Date col and rename columns sensibly
df.games.all <- df.games.all[,c(2,1,3:51)]
names(df.games.all)[names(df.games.all) %in% c("V2","V3","V4")] <- c("Oval.name","City","State")
# names(tmp.stadium)[names(tmp.stadium)==c("Format.Date")] <- c("Date")
names(df.games.all)[names(df.games.all)==c("Kick.Off..local.")] <- "Kick.off.localtime"

# remove any whitespace
df.games.all$Oval.name <- trimws(df.games.all$Oval.name)

df.games.all$Winner <- ifelse(df.games.all$Home.Score > df.games.all$Away.Score, "Home","Away")


# colours manually transcribed from a message on this thread:
# https://www.bigfooty.com/forum/threads/afl-colours-guide.810014/page-2
# afl.colours <- read.csv("aflcolourRGBHEX.csv", header=TRUE)

# read csv from Google Drive published sheet - afl colours
tmpGS <- getURL("https://docs.google.com/spreadsheets/d/1tJKEJj4cfXjdgVQvXgkMKZaPzhosNkeJ2GtrgYIINuE/pub?output=csv")
afl.colours <- read.csv(textConnection(tmpGS))

# create a weather dataframe with relevant data points for each date of play
lstofCSVs <- as.list(list.files("data/raw/bomdata"))
tmpcsv <- ldply(paste("data/raw/bomdata/",lstofCSVs,sep=""), read.csv)

# create date field
tmpcsv$Date <- as.Date(paste(tmpcsv$Year, "-", sprintf("%02d", tmpcsv$Month),"-",sprintf("%02d",tmpcsv$Day), sep=""))

# get list of dates of games played - 2016 only (to match collected weather records)
lstDatesofGamesPlayed <- unique(tmp.afl[which(tmp.afl$Date < "2017-01-01"),]$Date)

# subset temp df
df.temp <- subset(tmpcsv, tmpcsv$Date %in% lstDatesofGamesPlayed)

# get rid of unused columns
df.temp <- df.temp[,c(13,2:6,9,11)]

# rename for ease of use
names(df.temp)[2] <-  c("station.number")

# add city
df.temp.stations <- read.csv("data/bomnotes/stations.csv", colClasses= c(station.number="numeric",name="character",City="character",priority="integer"))
df.temp <- merge(df.temp, df.temp.stations[,c(1,3)], by = "station.number")

# there are some missing temperature records by city, this next section
# fills up those missing records by collecting secondary and tertiary stations records

# List all game dates -> [subset of df.temp]
lstofdftemp.dates <- unique(df.temp[,c(2,9)])

# create frequency table of all city climate records
vecCities <- sapply(split(lstofdftemp.dates, lstofdftemp.dates$City),nrow)

# loop through cities with not enough date records
for (n in names(vecCities[which(vecCities < length(unique(lstofdftemp.dates$Date)))])) {
  
  # detail missing dates
  lstofMissingDates <- lstDatesofGamesPlayed[!(lstDatesofGamesPlayed %in% unique(df.temp[which(df.temp$City == n),c("Date")]) )]
  
  # df.temp.stations with priority > 1 (ie secondary stations)
  stationNum <- df.temp.stations[which(df.temp.stations$priority > 1 & df.temp.stations$City == n),c("station.number")]
  
  df.temp.missing <- tmpcsv[which(tmpcsv$Date %in% lstofMissingDates & tmpcsv[,2] == stationNum),c(2,13,3:6,9,11)]
  df.temp.missing$City = n
  
  # collapse df.temp.missing to get rid of NAs
  df.temp.missing <- setDT(df.temp.missing)[,lapply(.SD, na.omit), by=Date]
  
  # remove duplicated rows and resets column order 
  df.temp.missing <- df.temp.missing[!duplicated(df.temp.missing),c(2,1,3:9)] 
  
  # truncate df.temp.missing with only the number of missing dates
  df.temp.missing2 <- subset(df.temp.missing, FALSE)
  
  for (i in 1:length(lstofMissingDates)) {
    
    df.temp.missing2 <- rbind (df.temp.missing2, df.temp.missing[i,])
    
  }
  
  
  df.temp <- rbind(df.temp, setNames(df.temp.missing, names(df.temp)))
  
  print(n)
  print(nrow(df.temp.missing))
  
  stationNum <- NULL
  df.temp.missing <- NULL
  df.temp.missing2 <- NULL
  lstofMissingDates <- NULL
  
}


# set up blank player data frame
dfPlayers <- data.frame(player=character(),
                       dob=character(),
                       height=character(),
                       weight=character(),
                       year=character(),
                       team=character())

lstPlayers <- list.files(path="data/raw/afltables.com/",pattern = "*.html")

for (player in lstPlayers) {
  
  doc <- htmlTreeParse(paste("data/raw/afltables.com/", player, sep=""), useInternalNodes  = TRUE)
  plain.text <- xpathSApply(doc, "/html/body/center/text()", xmlValue)
  tblteam <- as.data.frame(readHTMLTable(doc)[1])
  
   # DOB
  dob <- plain.text[5]
  # height
  hght <- plain.text[8]
  # weight
  wght <- plain.text[9]
  
  print(player)
  tmpdf <- cbind(name=as.character(player),d.o.b = as.character(dob),weight = as.character(wght)
                 , height=as.character(hght),year=as.character(tblteam[,1]), team=as.character(tblteam[,2]))
  
  dfPlayers <- rbind(dfPlayers, tmpdf)
  
  dob <- NULL
  height <- NULL
  weight <- NULL
  
}

# cleanup

# remove '.html', 'kg', 'cm'
dfPlayers$name <- gsub(".html","",dfPlayers$name)
dfPlayers$weight <- gsub(" kg","",dfPlayers$weight)
dfPlayers$weight <- as.numeric(dfPlayers$weight)
dfPlayers$height <- gsub(" cm ","",dfPlayers$height)
dfPlayers$height <- as.numeric(dfPlayers$height)

# remove parentheses and convert to date format
dfPlayers$d.o.b <- gsub(pattern=" \\(",replacement="",dfPlayers$d.o.b)
dfPlayers$d.o.b <- as.Date(dfPlayers$d.o.b, format = "%d-%B-%Y")

dfPlayers.AFL <- dfPlayers[ !duplicated(dfPlayers$name),]

# NBA players - 2013 roster
# downloaded from https://www.besttickets.com/blog/unofficial-2013-nba-census/
# http://www.besttickets.com/blog/wp-content/uploads/2013/12/NBA-Census-10.14.2013.csv
# 26/1/17 10:07am

dfPlayers.NBA <- read.csv("data/NBA/NBA-Census-10.14.2013.csv", header=TRUE)

# convert weight (pound to kg = 1 : 0.453592)
dfPlayers.NBA$WT <- round(dfPlayers.NBA$WT*0.453592,0)

# convert height (inches to cm = 1 : 2.54)
dfPlayers.NBA$Ht..In.. <- round(dfPlayers.NBA$Ht..In..*2.54)

names(dfPlayers.NBA)[7:8] <- c("height","weight")

# convert DOB to YYYY-MM-DD
dfPlayers.NBA$DOB <- as.Date(as.character(dfPlayers.NBA$DOB), format="%m/%d/%Y")

# remove bad data - Taylor, Jermaine
dfPlayers.NBA <- dfPlayers.NBA[which(dfPlayers.NBA$Name != "Taylor, Jermaine"),]

# NFL Roster - 2017
dfPlayers.NFL <- read.csv("data/NFLroster.csv", header=TRUE)

# convert weight kg
dfPlayers.NFL$Weight <- round(dfPlayers.NFL$Weight*0.453592,0)

# convert height
dfPlayers.NFL$Height <- gsub("\"","", dfPlayers.NFL$Height)
dfPlayers.NFL$Height <- gsub("'",".", dfPlayers.NFL$Height)

# split the height measure into whole.decimal 
dfPlayers.NFL <- cbind(dfPlayers.NFL,colsplit(as.character(dfPlayers.NFL$Height),"\\.", names=c("Height.integer","Height.decimal")))

# multiply the decimal by 0.0833333 to convert to feet/inches decimal form
dfPlayers.NFL$Height.decimal <- dfPlayers.NFL$Height.decimal*0.0833333

# concatenate for proper decimal feet.inches number
dfPlayers.NFL$Height <- dfPlayers.NFL$Height.decimal+dfPlayers.NFL$Height.integer

# then multiply by 0.3048
dfPlayers.NFL$Height <- round((as.numeric(dfPlayers.NFL$Height)*0.3048)*100,0)

names(dfPlayers.NFL)[5:6] <- c("height","weight")

# dob
dfPlayers.NFL$Birthdate <- as.Date(dfPlayers.NFL$Birthdate,"%m/%d/%Y")

# NRL Players - 2017 roster

dfPlayers.NRL <- data.frame(player=character(),
                        dob=character(),
                        height=character(),
                        weight=character(),
                        year=character(),
                        team=character())

for ( f in list.files(path = "data/NRLplayers/", pattern = "index*")) {
  
  print(f)
  
  doc <- htmlTreeParse(paste("data/NRLplayers/",f,sep=""), useInternalNodes  = TRUE)
  player.name <- xpathSApply(doc, "//*[@id=\"td-outer-wrap\"]/div[2]/div/div/div/h1/text()", xmlValue)
  tblteam <- as.data.frame(readHTMLTable(doc)[1])
  
  tmpdf <- cbind(player=player.name, 
                 dob=as.character(tblteam[4,2]), 
                 height=as.character(tblteam[3,2]), 
                 weight=as.character(tblteam[2,2]),
                 year="2017", 
                 team=as.character(tblteam[11,1]))
  
  dfPlayers.NRL <- rbind(dfPlayers.NRL,tmpdf)
  
  tmpdf <- NULL
  player.name <- NULL
  doc <- NULL
  tblteam <- NULL
  
}


# clean up dob
dfPlayers.NRL$dob <- gsub("\\s*\\([^\\)]+\\)","", dfPlayers.NRL$dob)
dfPlayers.NRL$dob <- as.Date(as.character(dfPlayers.NRL$dob),"%d/%m/%Y")

# remove rows where dob is NA as those html pages have incomplete stat tables
# on those pages - total number of rows missed = 11
# should not be an issue for this exercise.
dfPlayers.NRL <- dfPlayers.NRL[which(!is.na(dfPlayers.NRL$dob)),]

# clean up height 
dfPlayers.NRL$height <- as.numeric(gsub(" cm\\s*\\([^\\)]+\\)","", dfPlayers.NRL$height))

# clean up weight
dfPlayers.NRL$weight <- as.numeric(gsub(" kg","", dfPlayers.NRL$weight))


# SOCCER - connect to the sqlite file
setwd("data/soccer/")
con = dbConnect(RSQLite::SQLite(), dbname="database.sqlite")
# get a list of all tables
alltables = dbListTables(con)
dfPlayers.soccer = dbGetQuery( con,'select * from Player' )

dfPlayers.soccer$weight <- round(dfPlayers.soccer$weight*0.453592,2)






# SCRAPBOOK

# # input player csv
# playertmpcsv <- read.csv("data/raw/aflwiki/z3players.txt", header=FALSE, sep="*" )
# # truncate (no useful info in $V9 to $V13)
# playertmpcsv <- playertmpcsv[,1:8]
# # convert to character to allow for string manipulations
# playertmpcsv[,1:8] <- sapply(playertmpcsv[,1:8], function(z) as.character(z))
# 
# # remove V1 columns that have more than 100 characters (ie not names)
# rowIndexRemove <- which(nchar(playertmpcsv$V1) < 68)
# playertmpcsv.sub <- subset(playertmpcsv, nchar(playertmpcsv$V1) < 68 )
# 
# # clean up names column
# playertmpcsv.sub$V1 <- gsub("|.name.*=", "", playertmpcsv.sub$V1)
# 
# # set up empty data.frame - names not important as columns will be 
# # added on 
# dfplayer.raw <- data.frame(stub=character())
# 
# pattern = paste("name","birth date","height.","years.*=","clubs",sep="|")
# for (playerfile in list.files(path="data/raw/aflwiki/", pattern="*.xml")) {
#   
#   pathtofile = paste("data/raw/aflwiki/",playerfile, sep="")
#   
#   xtmp <- xmlParse(pathtofile)
#   xtop <- xmlRoot(xtmp)
#   
#   dftmp <- data.frame(do.call(cbind, as.list(grep(pattern,strsplit(xmlValue(xtop[[2]]), "\n")[[1]], value=TRUE))))
#   
#   print(length(dftmp))
#   
#   dfplayer.raw <- rbind.fill(dfplayer.raw,dftmp)
#   
#   dftmp <-NULL
#   
# }
# # TODO
# # drop unneeded columns
# dfplayer.abridged <- dfplayer.raw[,2:9]
# 
# # 2. remove "| fullname" cells
# # 3. remove "| nickname =" cells
# # 4. function separate out yyyy mm dd and returne YYYY-MM-DD
# # 5. separate out height / weight
# 
# # Name | Birthdate | height | weight | clubs | years
# 
# # SCRAPBOOK
# dfplayer.raw <- data.frame(Name=character(),
#                            Full.name=character(),
#                            Nickname=character(),
#                            Birthdate=character(),
#                            Heightweight=character(),
#                            Years=character(),
#                            Clubs=character(),
#                            Other=character())
# 
# xtmp <- xmlParse("data/raw/aflwiki/Alastair_Lynch.xml")
# xtmp2 <- xmlParse("data/raw/aflwiki/Albert_Barker.xml")
# xtop = xmlRoot(xtmp)
# xtop2 = xmlRoot(xtmp2)
# xtop[[2]] # show everything in <page>
# 
# vtmplst <- strsplit(xmlValue(xtop[[2]]), "\n")
# vtmplst2 <- strsplit(xmlValue(xtop2[[2]]), "\n")
# pattern = paste("name","birth date","height.","years.*=","clubs",sep="|")
# dftmp <- data.frame(do.call(cbind, as.list(grep(pattern,strsplit(xmlValue(xtop2[[2]]), "\n")[[1]], value=TRUE))))
# 
# str(grep(pattern,vtmplst2[[1]],value=TRUE))
