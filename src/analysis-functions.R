# analysis-functions R script created for AFL dataset analysis
# forked from aflRscript.R on 28/12/16

library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(reshape)
library(data.table)
library(XML)
library(rvest)
library(gridExtra)
library(RSQLite)
library(DBI)
library(grid)
library(formattable)
library(plotly)
library(RColorBrewer)
library(htmltools)
library(webshot)
library(fitdistrplus)
library(logspline)

a1 <- Sys.time()
setwd("/Users/alex/Documents/datasets/afl")
source("src/data_clean.R")

setwd("/Users/alex/Documents/datasets/afl")
source("src/functions.R")


a2 <- Sys.time()
print(a2 -a1)
print("functions.R & data_clean.R sourcing complete") 

# PLOT 1
df.kicks <- as.data.frame(colSums(df.games.all[,13:48], na.rm=TRUE))
df.kicks <- add_rownames(df.kicks, "Teams")
names(df.kicks)[2] <- "value"
#why?? who knows but only way to get it to behave like a df
df.kicks <- as.data.frame(df.kicks)

df.teams <- as.data.frame(gsub(" ",".",unique(tmp$Home.Team)))
names(df.teams) <- "Teams"

df.kicks[grep("Goals", df.kicks$Teams),3] <- substr(grep("Goals", df.kicks$Teams, value=TRUE), 1, nchar(df.kicks$Teams)-6)
df.kicks[grep("Behinds", df.kicks$Teams),3] <- substr(grep("Behinds", df.kicks$Teams, value=TRUE), 1, nchar(df.kicks$Teams)-6)

# add 4th column for behinds
df.kicks[match(df.kicks$V3, df.kicks[grep("Behinds", df.kicks$Teams),3]),4] <-df.kicks[grep("Behinds", df.kicks$Teams),2]
df.kicks <- df.kicks[1:18,]
df.kicks <- df.kicks[,c(3,2,4)]

names(df.kicks) <- c("Team","Goals","Behinds")

df.kicks.m <- melt(df.kicks, id.vars="Team")

df.kicks.m$Team <- factor(df.kicks.m$Team, levels=na.omit(df.kicks.m[which(df.kicks.m$variable == "Goals"),1][ order(df.kicks.m$value, decreasing = FALSE)]))

plot.title = paste("Number of goals & behinds by AFL team") 
plot.subtitle <- paste("From 2009-06-19 to", max(tmp.afl$Date))

p1 <- ggplot(df.kicks.m, aes(Team,as.numeric(value),fill=variable)) + geom_bar(position = position_dodge(width = NULL),stat="identity")+
  geom_text(data=df.kicks.m, aes(label=as.numeric(value), fontface=.5,hjust=1.1), position = position_dodge(width=.95),colour= "white") + 
  theme(axis.text.x=element_text(angle=-90)) + scale_fill_manual(values=c("#0000ff", "#ff4040")) +
  labs(fill="") + scale_y_continuous(breaks=seq(0,2800, 400)) + ylab(NULL) + coord_flip() + 
  theme(panel.grid.major.y = element_blank()) + 
  ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))

ggsave(filename="figs/NumberofGoalsandBehinds_byTeam.png", p1, width=12, height=10)

# PLOT 1a - ratio of goals to behinds
df.kicks$Ratio <- df.kicks$Goals/df.kicks$Behinds

grid.draw(tableGrob(df.kicks[order(df.kicks$Ratio, decreasing=TRUE),], rows=NULL))

# PLOT 2 - Home vs Away

# remove draws by subsetting
tmp.subset <- df.games.all[-which(df.games.all$Home.Score == df.games.all$Away.Score),]
tmp.subset.homewins <- df.games.all[which(df.games.all$Home.Score > df.games.all$Away.Score),c(3:5,6:7)] 
tmp.subset.homewins$Winner <- "Home"
tmp.subset.awaywins <- df.games.all[which(df.games.all$Home.Score < df.games.all$Away.Score),c(3:5,6:7)] 
tmp.subset.awaywins$Winner <- "Away"

# create frequency table of Home wins and Away wins
df.tmp.home <- count(tmp.subset.homewins, vars="Home.Team")
names(df.tmp.home)[2] <- "Home.wins"
df.tmp.away <- count(tmp.subset.awaywins, vars="Away.Team")
names(df.tmp.away)[2] <- "Away.wins"

# combine the subsetted dataframes
df.plot.homevsaway <- cbind(df.tmp.away, df.tmp.home)
# dataframe clean up
df.plot.homevsaway <- df.plot.homevsaway[,2:4]
df.plot.homevsaway <- merge(df.plot.homevsaway,afl.colours)

title.plot = "Home Wins vs Away wins"
subtitle.plot = paste("(from 2009-06-19 to", max(tmp.afl$Date),")",sep="") 

p2 <- ggplot(df.plot.homevsaway, aes(x=Home.Team, y=Home.wins, fill=Home.Team)) + geom_bar(stat = "identity") +
  geom_bar(aes(x=Home.Team, y=-Away.wins), stat="identity") +
  geom_hline(yintercept = -0,colour = "white") + 
  #geom_text(data=df.plot.homevsaway, aes(x=Home.Team, y=Home.wins, label=Home.wins, size=1, hjust=1.2, fontface=2),colour=df.plot.homevsaway$hex2) +
  geom_text(data=df.plot.homevsaway, aes(x=Home.Team, y=-Away.wins, label=Away.wins, size=1, hjust=-0.4, fontface=2),colour=df.plot.homevsaway$hex2) +
  scale_fill_manual(breaks=c("Adelaide", "Brisbane", "Carlton", "Collingwood", 
                             "Essendon", "Freemantle", "Geelong", "GWS", "GCS", "Hawthorn", 
                             "Melbourne", "North.Melbourne", "Port.Adelaide", "Richmond", 
                             "St.Kilda", "Sydney", "West.Coast", "Western.Bulldogs"),
                    values = c("#0F1D42", "#9B0033", "#011931", "#000000", "#000000", "#2D0054", 
                               "#05173F", "#F78F1E", "#FC1921", "#361500", "#011931", "#0E2B8D", 
                               "#000000", "#000000", "#000000", "#F20017", "#05173F", "#0D3692")) + 
  scale_y_continuous(limits=c(-85,85),breaks=c(-80,-40,0,40,80), labels=c("80","40", "0","40","80")) +
  coord_flip() + labs(x="", y="Away wins    Home wins" + theme(legend.position="none"))
  #coord_flip() + labs(x="", y=paste("Away wins    Home wins\n",plot.title)) + theme(legend.position="none")

p2
ggsave(filename="figs/Away-Homewins_byTeam_colours.png", p2, width=12, height=10)

# PLOT 3 - uses the same data as p2

title.plot = "Away Wins vs Home wins"
subtitle.plot = paste("(from 2009-06-19 to", max(tmp.afl$Date),")") 

p3 <- ggplot(df.plot.homevsaway, aes(x=Home.Team, y=Home.wins)) + geom_bar(stat = "identity", fill= "#0C2340") +
  geom_bar(aes(x=Home.Team, y=-Away.wins ),  fill= "#0C2340", stat="identity") +
  geom_hline(yintercept = -0,colour = "white") + 
  geom_text(data=df.plot.homevsaway, aes(x=Home.Team, y=Home.wins, label=Home.wins, size=1, hjust=1.2, fontface=2), colour="white") +
  geom_text(data=df.plot.homevsaway, aes(x=Home.Team, y=-Away.wins, label=Away.wins, size=1, hjust=-0.4, fontface=2), colour="white") +
  scale_y_continuous(limits=c(-85,85),breaks=c(-80,-40,0,40,80), labels=c("80","40", "0","40","80")) +
  coord_flip() + labs(x="", y="Away wins    Home wins", title=title.plot, subtitle=subtitle.plot) + theme_bw() + 
  theme(legend.position="none",plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

p3
ggsave(filename="figs/Away-Homewins_byTeam.png", p3, width=12, height=10)

# PLOT 4 - percentage home wins vs percentage away wins horizontal
df.plot.percentage <- cbind(as.data.frame(table(df.games.all$Away.Team)),as.data.frame(table(df.games.all$Home.Team))[,2])
names(df.plot.percentage) = c("Team","Away.games","Home.games")

# had to spot check the data to ensure the cbind worked correclty
df.plot.percentage <- cbind(df.plot.percentage, df.plot.homevsaway[,2:3])

df.plot.percentage$Awaywins.percentage <- df.plot.percentage$Away.wins/df.plot.percentage$Away.games
df.plot.percentage$Homewins.percentage <- df.plot.percentage$Home.wins/df.plot.percentage$Home.games

title.plot = "% Home wins vs % Away wins"
subtitle.plot = paste("(from 2009-06-19 to ", max(tmp.afl$Date),")", sep="") 

p4 <- ggplot(df.plot.percentage, aes(x=Team, y=Homewins.percentage)) + geom_bar(stat = "identity", fill= "#0C2340") +
  geom_bar(aes(x=Team, y=-Awaywins.percentage ),  fill= "#0C2340", stat="identity") +
  geom_hline(yintercept = -0,colour = "white") + 
  geom_text(data=df.plot.percentage, aes(x=Team, y=Homewins.percentage, label=paste(round(df.plot.percentage$Homewins.percentage,2) * 100, "%", sep=""), size=1, hjust=1.2, fontface=2), colour="white") +
  geom_text(data=df.plot.percentage, aes(x=Team, y=-Awaywins.percentage, label=paste(round(df.plot.percentage$Awaywins.percentage,2) * 100, "%", sep=""), size=1, hjust=-0.2, fontface=2), colour="white") +
  scale_y_continuous(limits=c(-1,1),breaks=c(-1,-0.5,0,0.5,1), labels=c("100%","50%", "0","50%","100%")) +
  coord_flip() + labs(x="", y="% wins Away games   % wins Home games", title=title.plot, subtitle=subtitle.plot) + theme_bw() + 
  theme(legend.position="none",plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

p4
ggsave(filename="figs/Away-Homewins-percentage_byTeam.png", p3, width=12, height=10)

# PLOT 5 - percentage home wins vs percentage away wins - vertical
df.plot.homevsaway.percentage <- melt(df.plot.percentage[,c(1,6:7)],id.vars = "Team")

title.plot = "% Home wins vs % Away wins"
subtitle.plot = paste("(from 2009-06-19 to", max(tmp.afl$Date),")") 

#df.plot.homevsaway.percentage$Team <- factor(df.plot.homevsaway.percentage$Team , levels=na.omit(df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),1][order(df.plot.homevsaway.percentage$value, decreasing=TRUE)])) 

p5 <- ggplot(df.plot.homevsaway.percentage, aes(x=Team, y=value, fill=variable)) + geom_bar(stat="identity", position=position_dodge()) +
  geom_text(data=df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Homewins.percentage"),], aes(x=Team, y=value, label=round(df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Homewins.percentage"),]$value,3) * 100),position="identity", size=3, hjust=-.05, vjust=1.5, colour="white",show.legend = F) +
  geom_text(data=df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),], aes(x=Team, y=value, label=round(df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),]$value,3) * 100),position="identity", size=3, hjust=1.05, vjust=1.5, colour="white",show.legend = F) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0","25","50","75","100")) + 
  scale_fill_manual(labels=c("% Away game wins","% Home game wins"), values=c("#FF0000","#222DFF")) + #coord_cartesian(ylim = (0, 1)) +
  labs(y="%", title=title.plot, subtitle=subtitle.plot) + 
  theme(legend.title=element_blank(),axis.text.x=element_text(angle=-90),axis.title.y = element_text(angle=0),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  

p5
ggsave(filename="figs/Away-Homewins-percentage_byTeam_vertical.png", p5, width=14, height=10)

# % Away wins
# subset first to remove potential confusion with results and make
# code human readable
df.plot.percentage.awaywins <- df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),]
df.plot.percentage.awaywins$Team <- factor(df.plot.percentage.awaywins$Team, levels=df.plot.percentage.awaywins[order(df.plot.percentage.awaywins$value, decreasing=TRUE),1]) 

title.plot = "% Away wins"

p5a <-ggplot(df.plot.percentage.awaywins, aes(x=Team, y=value)) + geom_bar(stat="identity",fill="#FF0000") +
  #geom_text(data=df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Homewins.percentage"),], aes(x=Team, y=value, label=round(df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Homewins.percentage"),]$value,3) * 100),position="identity", size=3, hjust=-.05, vjust=1.5, colour="white",show.legend = F) +
  geom_text(data=df.plot.percentage.awaywins, aes(x=Team, y=value, label=round(df.plot.percentage.awaywins$value,3) * 100),position="identity", size=3, vjust=1.5, colour="white",show.legend = F) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0","25","50","75","100")) + 
  scale_fill_manual(labels=c("% Away game wins","% Home game wins"), values=c("#FF0000","#222DFF")) + #coord_cartesian(ylim = (0, 1)) +
  labs(y="%", title=title.plot, subtitle=subtitle.plot) + 
  theme(legend.position="none",axis.text.x=element_text(angle=-90),axis.title.y = element_text(angle=0),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  

# % Home wins
# subset first to remove potential confusion with results and make
# code human readable
df.plot.percentage.homewins <- df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Homewins.percentage"),]
df.plot.percentage.homewins$Team <- factor(df.plot.percentage.homewins$Team , levels=df.plot.percentage.homewins[order(df.plot.percentage.homewins$value, decreasing=TRUE),1]) 

title.plot = "% Home wins"

p5h <-ggplot(df.plot.percentage.homewins, aes(x=Team, y=value)) + geom_bar(stat="identity", fill="#222DFF") +
  geom_text(data=df.plot.percentage.homewins, aes(x=Team, y=value, label=round(df.plot.percentage.homewins$value,3) * 100),position="identity", size=3, vjust=1.5, colour="white",show.legend = F) +
  #geom_text(data=df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),], aes(x=Team, y=value, label=round(df.plot.homevsaway.percentage[which(df.plot.homevsaway.percentage$variable == "Awaywins.percentage"),]$value,3) * 100),position="identity", size=3, vjust=1.5, colour="white",show.legend = F) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0","25","50","75","100")) + 
  scale_fill_manual(labels=c("% Home game wins"), values=c("#222DFF")) + #coord_cartesian(ylim = (0, 1)) +
  labs(y="%", title=title.plot, subtitle=subtitle.plot) + 
  theme(legend.title=element_blank(),axis.text.x=element_text(angle=-90),axis.title.y = element_text(angle=0),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  

p.double <- grid.arrange(p5h,p5a,ncol=2)

ggsave(filename="figs/HomevsAwaywins_percentage_h.png", p.double, width=14, height=8)

# PLOT 8 - percentage wins by venue

# work out number of games played by team by stadium
tmp.df.awaygames <- as.data.frame(df.games.all %>% group_by(Venue, Away.Team) %>% summarise(count=n()))
tmp.df.homegomes <- as.data.frame(df.games.all %>% group_by(Venue, Home.Team) %>% summarise(count=n()))

# work out winners
tmp.afl$Winner <- "winner"
tmp.afl[which(tmp.afl$Home.Score > tmp.afl$Away.Score),]$Winner <- "Home"
tmp.afl[which(tmp.afl$Home.Score < tmp.afl$Away.Score),]$Winner <- "Away"
tmp.afl[which(tmp.afl$Home.Score == tmp.afl$Away.Score),]$Winner <- "Draw"

# create df with winners: Away.Team
tmp.df.awaywinners <- tmp.afl[which(tmp.afl$Winner == "Away"),c(5,4,49)]
tmp.df.awaywinners <- as.data.frame(tmp.df.awaywinners %>% group_by(Venue, Away.Team) %>% summarise(count=n()))
names(tmp.df.awaywinners)[2] <- "Team"

# create df with winners: Home.Team
tmp.df.homewinners <- tmp.afl[which(tmp.afl$Winner == "Home"),c(5,3,49)]
tmp.df.homewinners <- as.data.frame(tmp.df.homewinners %>% group_by(Venue, Home.Team) %>% summarise(count=n()))
names(tmp.df.homewinners)[2] <- "Team"

# merge and group
tmp.df.total <- rbind(tmp.df.homewinners,tmp.df.awaywinners)
tmp.df.total <- ddply(tmp.df.total, c("Venue", "Team"), summarise, wins=sum(count))

# calculate number of games played by that team in that Stadium.
tmp.df.totalgames.hometeam <- as.data.frame(tmp.afl[,c(3:5)] %>% group_by(Venue, Home.Team) %>% summarise(count=n()))
names(tmp.df.totalgames.hometeam)[2] <- "Team"
tmp.df.totalgames.awayteam <- as.data.frame(tmp.afl[,c(3:5)] %>% group_by(Venue, Away.Team) %>% summarise(count=n()))
names(tmp.df.totalgames.awayteam)[2] <- "Team"

tmp.df.totalgames <- rbind(tmp.df.totalgames.awayteam,tmp.df.totalgames.hometeam)
tmp.df.totalgames <- ddply(tmp.df.totalgames, c("Venue", "Team"), summarise, games=sum(count))

# create plot dataframe
df.plot.percentagewins <- merge(tmp.df.totalgames, tmp.df.total, by=c("Venue","Team"), all.x=TRUE)

# add column with percentage
df.plot.percentagewins$percentage.wins <- df.plot.percentagewins$wins/df.plot.percentagewins$games 

# replace NA % with 0
df.plot.percentagewins[is.na(df.plot.percentagewins$percentage.wins),]$percentage.wins <- 0

# add team colours
df.plot.percentagewins <- merge(df.plot.percentagewins, afl.colours[,c("Home.Team","rgb1")], by.x="Team", by.y="Home.Team")

# create plots by stadium
for (v in unique(df.plot.percentagewins$Venue)) {
  
  fnPlotPercentageWins(v)
  print(v)
}


# create plots by team and stadium
for (t in unique(df.plot.percentagewins$Team)) {
  
  fnPlotPercentageWins.Venue(t)
  print(t)
}

# mega plot
title.plot = "Percentage of games won by team by venue"
p8.mega <- ggplot(data=df.plot.percentagewins, aes(x=Venue, y=percentage.wins, fill=Team)) + geom_bar(stat="identity", position=position_dodge()) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%","100%")) + 
  labs(y="Percentage games won", title=title.plot, subtitle=subtitle.plot) + 
  colScale + theme(axis.text.x=element_text(angle=-90),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

#ggsave(filename=paste("figs/percentage_wins_byTeam-byVenue.png", sep=""), p8.mega, width=30, height=8)

             #titlefont = tf,
         #plot_bgcolor = "#d3d3d3",
         #xaxis = x, 
         #yaxis = y, 
         #legend=list(traceorder='reversed'),
         #annotations=a)

# PLOT 9 - Wins by year

tmp.afl.year <- df.games.all
tmp.afl.year$Year <- as.numeric(format(tmp.afl.year$Date, format = "%Y"))
tmp.afl.year$win <- ifelse(tmp.afl.year$Home.Score > tmp.afl.year$Away.Score, "Home","Away")

tmp.afl.year.homewins <- as.data.frame(tmp.afl.year[which(tmp.afl.year$win == "Home"),] %>% group_by(Year, Home.Team, win) %>% dplyr::summarise(count=n()))
tmp.afl.year.awaywins <- as.data.frame(tmp.afl.year[which(tmp.afl.year$win == "Away"),] %>% group_by(Year, Home.Team, win) %>% dplyr::summarise(count=n())) 
tmp.afl.year.totalwins <- rbind(tmp.afl.year.homewins, tmp.afl.year.awaywins)

# variable to fix size of graph to keep it common across all teams
maxCount = max(as.data.frame(tmp.afl.year.totalwins %>% group_by(Home.Team,Year) %>% dplyr::summarise(wins=sum(count)))$win)

for (t in unique(tmp.afl.year.totalwins$Home.Team)) {
  
  fnPlotNumberofWins(t)
  print(t)
  
}

# PLOT 10 - wins/losses by temperature

df.games.2016 <- df.games.all[which(df.games.all$Date < "2017-01-01"),]
# keep relevant columns only
df.team.total <- df.games.2016[,c(1,2,4:7,49:52)]

# Team | Date | Venue | City | Max.temp | Min.temp | rainfall 
df.winlose.bytemp <- merge(df.team.total, df.temp, by=c("Date","City"))

# replace NAs with values from other rows
df.winlose.bytemp <- setDT(df.winlose.bytemp)[,lapply(.SD, na.omit), by=Date]

# removes duplicated rows
df.winlose.bytemp <- df.winlose.bytemp[!duplicated(df.winlose.bytemp),] 
# truncates df by number of unique games 
df.winlose.bytemp <- ddply(df.winlose.bytemp, .(Date,City,Venue),function(x) head(x,1))

df.winlose.bytemp$mean.temp <- (df.winlose.bytemp$Maximum.temperature..Degree.C.+df.winlose.bytemp$Minimum.temperature..Degree.C.)/2

# plot all points
title.plot = paste("Temperature vs Score")
subtitle.plot = "(from 2009-06-19 to 2016-10-01)"


p10 <- ggplot(data=df.winlose.bytemp, aes(x=mean.temp,y=Home.Score)) + geom_point(aes(colour=Winner), alpha=6/10) +
  scale_y_continuous(limits=c(0,max(df.winlose.bytemp$Home.Score))) + 
  scale_colour_manual(labels=c("Lose","Win"), values=c("#FF0000","#000000"),guide=guide_legend(reverse=TRUE)) + 
  scale_x_continuous(limits=c(0,30),breaks=c(-5,0,5,10,15,20,25,30)) + 
  annotate("text", x=max(df.winlose.bytemp$mean.temp, na.rm=TRUE)-0.3, y=max(df.winlose.bytemp$Home.Score, na.rm=TRUE)-10, label=paste("(n=",nrow(df.winlose.bytemp),")", sep="")) +
  theme(legend.title=element_blank(),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  labs(y="Score", x=expression("mean temperature ("*degree*C*")"), title=title.plot, subtitle=subtitle.plot)

ggsave(filename="TempvsScore.png", p10, width=14, height=8)

###############
# ggplotly syntax

#title.plot = paste("Temperature vs Score\n(from 2009-06-19 to 2016-10-01)")
#g10 <- ggplot(data=df.winlose.bytemp, aes(x=mean.temp,y=Home.Score)) + geom_point(aes(colour=factor(Winner,labels=c("Lose","Win"))), alpha=6/10) + guides(colour=FALSE) +
#    scale_y_continuous(limits=c(0,max(df.winlose.bytemp$Home.Score))) + 
#    scale_colour_manual(labels=c("Lose","Win"), values=c("#FF0000","#000000"),guide=guide_legend(reverse=TRUE)) + 
#    scale_x_continuous(limits=c(0,30),breaks=c(-5,0,5,10,15,20,25,30)) + 
#    annotate("text", x=max(df.winlose.bytemp$mean.temp, na.rm=TRUE)-0.3, y=max(df.winlose.bytemp$Home.Score, na.rm=TRUE)-10, label=paste("(n=",nrow(df.winlose.bytemp),")", sep="")) +
#    labs(y="Score", x="mean temperature in Celsius", title=title.plot)

#ggplotly(g10)
#p10.ly <- plotly_build(g10)
#p10.ly$x$data[[2]]$text <- ~paste("Mean.temp:", mean.temp,"</br>Team: ",Home.Team)
# p10.ly$filename <- 'TempvsScore'
# plotly_POST(p10.ly)
###################

# 

par(mfrow = c(1,2))
descdist(c(na.omit(df.winlose.bytemp$mean.temp)), discrete=FALSE, boot=100,boot.col="red", graph=TRUE)
title(sub="$mean.temp")
descdist(c(na.omit(df.winlose.bytemp$Home.Score)), discrete=FALSE, boot=100,boot.col="red", graph=TRUE)
title(sub="$Home.Score")


# PLOT 11 - wins by team by temperature
setwd("/Users/alex/Documents/datasets/afl")

subtitle.plot = paste("(from 2009-06-19 to ", max(df.winlose.bytemp$Date),")",sep="") 
maxscore = ifelse(max(df.winlose.bytemp$Home.Score) > max(df.winlose.bytemp$Away.Score), max(df.winlose.bytemp$Home.Score), max(df.winlose.bytemp$Away.Score))

for (t in unique(df.winlose.bytemp$Home.Team)) {
  
  fnPlotbyTemp(t,maxscore)
  print(t)
  
}


# Plot different codes
# create mega df with all players

dfPlayers.all <- rbind(cbind(dfPlayers.AFL[,c("weight","height")],"code"="AFL"),
                       cbind(dfPlayers.NBA[,c("weight","height")],"code"="NBA"),
                       cbind(dfPlayers.NRL[,c("weight","height")],"code"="NRL"),
                       cbind(dfPlayers.NFL[,c("weight","height")],"code"="NFL"),
                       cbind(dfPlayers.soccer[,c("weight","height")],"code"="SOCCER"))


notation <- data.frame(dfPlayers.all %>% group_by(code) %>% dplyr::summarize(count=n()))

plot.hw <- ggplot(data=dfPlayers.all, aes(x=weight,y=height)) + geom_point(aes(alpha=0.4, showguides=F)) +
  scale_y_continuous(limits=c(min(dfPlayers.all$height),max(dfPlayers.all$height))) +
  labs(x="weight (kg)", y="height (cm)") +
  facet_grid(code ~ .) + geom_text(x=160, y=165, data=notation, label=paste("n=",notation$count))+
  theme(legend.position="none") 

dfPlayers.averages <- data.frame(dfPlayers.all %>% group_by(code) %>% 
  summarize("average weight"=round(mean(weight),2),"average height"=round(mean(height),2)))

# bmi is calculated by dividing weight in kg by the squared heignt in metres
dfPlayers.averages$bmi <- round(dfPlayers.averages$average.weight/(dfPlayers.averages$average.height/100)^2,2)

grid.table(dfPlayers.averages)

plot.box <- ggplot(data=dfPlayers.all, aes(x=weight,y=height),position_dodge()) + geom_boxplot() +
  facet_grid(.~ code) + labs(x="weight (kg)", y="height (cm)")

# create list of player dataframes
vecdfPlayers <- c("dfPlayers.AFL","dfPlayers.NBA","dfPlayers.NRL","dfPlayers.NFL","dfPlayers.soccer")

dfPlayers.summaries <- adply(vecdfPlayers, 1,function(x) summary(get(x)[,c("height","weight")]),.id="code")

# split out into nicely formatted weight and height summary tables
dfPlayers.summaries[,c("stat.w","weight.value")] <- colsplit(dfPlayers.summaries[,3],":",names="weight.value")
dfPlayers.summaries[,c("height.value")] <- colsplit(dfPlayers.summaries[,2],":",names="height.value")[2]

dfPlayers.summaries.height <- dcast(dfPlayers.summaries,code ~ stat.w,value.var="height.value")
dfPlayers.summaries.height$code <- toupper(substr(vecdfPlayers[dfPlayers.summaries.height$code],11,nchar(vecdfPlayers[dfPlayers.summaries.height$code])))

dfPlayers.summaries.weight <- dcast(dfPlayers.summaries,code ~ stat.w,value.var="weight.value")
dfPlayers.summaries.weight$code <- toupper(substr(vecdfPlayers[dfPlayers.summaries.weight$code],11,nchar(vecdfPlayers[dfPlayers.summaries.weight$code])))

# re-order columns
dfPlayers.summaries.height <- dfPlayers.summaries.height[,c(1,7,2,5:6,3:4)]
dfPlayers.summaries.weight <- dfPlayers.summaries.weight[,c(1,7,2,5:6,3:4)]

# plot height+weight summary table

tbl.height <- formattable(dfPlayers.summaries.height, list(
  code = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A", 
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(`Max.   `)) ~ normalize_bar("lightblue",0.4)
))

tbl.weight <- formattable(dfPlayers.summaries.weight, list(
  code = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A", 
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(`Max.   `)) ~ normalize_bar("lightblue",0.4)
))

export_formattable(tbl.weight,"weight.png")
export_formattable(tbl.height,"height.png")


# Plot / table stats changing over 2009-2016
plot.yr <- ggplot(data=dfPlayers.AFL, aes(x=weight,y=height,colour=year)) + geom_point() +
  scale_y_continuous(limits=c(min(dfPlayers.AFL$height),max(dfPlayers.AFL$height))) +
  labs(x="weight (kg)", y="height (cm)") 

# Plot average stats by team
dfPlayers.AFL.byteams <- data.frame(dfPlayers.AFL %>% group_by(team) %>% 
                                      summarize("average weight"=round(mean(weight),2),"average height"=round(mean(height),2)))

# TODO
# 1. Plot score vs average height
# 2. Plot score vs birthday
# 3. Plot score vs weight (total weight of team and average weight)





