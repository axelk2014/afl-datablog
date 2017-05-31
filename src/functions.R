# Rscript file for custom functions for afl analysis
# created 27/12/16

teamColours <- c("#6814f9", "#9B0033", "#00119b", "#000000", "#CC0C00", "#2D0054", 
                 "#09c143", "#F78F1E", "#361500", "#e75a00", "#008de8", "#0E2B8D", 
                 "#000000", "#FFD600", "#000000", "#F20017", "#05173F", "#0D3692")

names(teamColours) <- c("Adelaide", "Brisbane", "Carlton", "Collingwood", 
                        "Essendon", "Fremantle", "Geelong", "GWS Giants", "Gold Coast", "Hawthorn", 
                        "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", 
                        "St Kilda", "Sydney", "West Coast", "Western Bulldogs")

colScale <- scale_fill_manual(name="Team", values=teamColours)

'%nin%' <- Negate('%in%')

# function to return list of dates and cities for games played
# to enable subsetting of df.temp

fnDateCity <- function(team, datesplayed) {
  
  # clear previous values
  df.DC <- NULL
  df.DC <- subset(tmp.afl, tmp.afl$Away.Team == team & tmp.afl$Date %in% datesplayed )
  df.DC <- rbind(df.DC, subset(tmp.afl, tmp.afl$Home.Team == team & tmp.afl$Date %in% datesplayed ) )
  
  df.DC <- df.DC[,c("Date","Venue")]
  df.DC <- merge(df.DC,df.Stadii, by.x="Venue", by.y="V1")
  # re-order and truncate
  df.DC <- df.DC[,c(2,4,1,5)]
  names(df.DC)[c(2,4)] <- c("City","State")
  return(df.DC)
  
}

fnBuildTeamTempDF <- function(dfDC) {
  
  df.team.temp <- NULL
  df.team.temp <- select(df.temp, Date, City, Maximum.temperature..Degree.C.) %>% 
    filter(Date %in% dfDC$Date, City %in% dfDC$City, !is.na(Maximum.temperature..Degree.C.))
  
  df.team.temp.min <- select(df.temp, Date, City, Minimum.temperature..Degree.C.) %>% 
    filter(Date %in% dfDC$Date, City %in% dfDC$City, !is.na(Minimum.temperature..Degree.C.))
  
  df.team.temp <- left_join(df.team.temp,df.team.temp.min)

  df.team.rain <- select(df.temp, Date, City, Rainfall.amount..millimetres.) %>% 
    filter(Date %in% dfDC$Date, City %in% dfDC$City, !is.na(Rainfall.amount..millimetres.))

  df.team.temp <- left_join(df.team.temp,df.team.rain)
  
  df.team.temp <- df.team.temp[which(!duplicated(df.team.temp)),]
  
  # remove NAs
  df.team.temp <- df.team.temp[which(!is.na(df.team.temp$Rainfall.amount..millimetres.)),]
  df.team.temp <- df.team.temp[which(!is.na(df.team.temp$Maximum.temperature..Degree.C.)),]
  df.team.temp <- df.team.temp[which(!is.na(df.team.temp$Minimum.temperature..Degree.C.)),]
  
  # truncate - need to do it by date and city
  df.tt <- subset(df.team.temp, FALSE)
  i = NULL
  for (i in 1:nrow(dfDC)) {
    print(dfDC$Date[i],i)

    df.tt <- rbind(df.tt, df.team.temp[which(df.team.temp$Date == dfDC$Date[i] & df.team.temp$City == dfDC$City[i]),])

    
  }
  
  df.team.temp <- df.tt
  
  return(df.team.temp)
  
}


fnPlotPercentageWins <- function(p) {
  
  df.plot.tmp <- df.plot.percentagewins[which(df.plot.percentagewins$Venue == p), ]
  
  # reverse the order of the bars for alphabetical list going from the top
  df.plot.tmp <- within(df.plot.tmp,Team <- factor(df.plot.tmp$Team,levels=rev(levels(df.plot.tmp$Team))))
  
  # add number of games played to Team variable for labels
  df.plot.tmp$TeamLabel <- paste(df.plot.tmp$Team,"(",df.plot.tmp$games,")", sep="") 
  
  
  title.plot=paste("% wins vs games played, by team at Venue - ", p, sep="")
  
  p8 <- ggplot(data=df.plot.tmp, aes(x=Team, y=percentage.wins, fill=Team)) + geom_bar(stat="identity", position=position_dodge(), width=.8)  + 
    geom_text(data=df.plot.tmp, aes(x=Team, y=percentage.wins, label=paste(round(df.plot.tmp$percentage.wins,2) * 100, "%", sep=""), size=4, hjust=1.2, fontface=1), colour=ifelse(df.plot.tmp$percentage.wins > 0, "white","slategray"),show.legend = F) +
    colScale +   
    scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%","100%")) + 
    scale_x_discrete(labels=sort(df.plot.tmp$TeamLabel, decreasing=TRUE)) +
    labs(y="Percentage games won", title=title.plot, subtitle=subtitle.plot) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    theme(axis.text=element_text(size=10),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + coord_flip() 
  
  ggsave(filename=paste("figs/percentage_wins-",p,".png", sep=""), p8, width=12, height=8)
  
}

fnPlotPercentageWins.Venue <- function(v) {
  
  df.plot.tmp <- df.plot.percentagewins[which(df.plot.percentagewins$Team == v),]
  
  df.plot.tmp$VenueLabel <- paste(df.plot.tmp$Venue,"(",df.plot.tmp$games,")", sep="") 
  
  title.plot=paste("% wins for ",v, sep="")
  subtitle.plot = paste("(from 2009-06-19 to", max(tmp.afl$Date),")") 
  
  colourCount = length(unique(df.plot.tmp$Venue))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  
  p8.byteam <- ggplot(data=df.plot.tmp, aes(x=Venue, y=percentage.wins, fill=Venue)) + geom_bar(stat="identity", position=position_dodge(), width=.8)  + 
    geom_text(data=df.plot.tmp, aes(x=Venue, y=percentage.wins, label=paste(round(df.plot.tmp$percentage.wins,2) * 100, "%", sep=""), size=4, hjust=1.2, fontface=1), colour=ifelse(df.plot.tmp$percentage.wins > 0, "white","slategray"),show.legend = F) +
    scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.5,0.75,1), labels=c("0%","25%","50%","75%","100%")) + 
    scale_x_discrete(labels=sort(df.plot.tmp$VenueLabel, decreasing=TRUE)) +
    scale_fill_manual(values = getPalette(colourCount)) +
    labs(y="Percentage games won", title=title.plot, subtitle=subtitle.plot) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    theme(axis.text=element_text(size=10),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + coord_flip() 

  ggsave(filename=paste("figs/percentage_wins_venue-",v,".png", sep=""), p8.byteam, width=12, height=8)
  
}


fnPlotNumberofWins <- function (z) {
  
  plot.title = paste("Number of wins - ", z)
  
  p9 <- ggplot(data=subset(tmp.afl.year.totalwins, tmp.afl.year.totalwins$Home.Team == z), aes(x=Year, y=count, fill=win)) + geom_bar(stat="identity") +
    geom_text(data=subset(tmp.afl.year.totalwins, tmp.afl.year.totalwins$Home.Team == z), aes(x=Year, y=count, label=count), size=4, vjust=1.6, fontface=1, colour="white",position="stack", show.legend = F) +
    scale_y_continuous(limits=c(0,maxCount)) + 
    scale_x_continuous(breaks=seq(2009,2017)) +
    scale_fill_manual(values=c("#FF0000","#222DFF")) + 
    labs(y="wins", title=plot.title, subtitle=subtitle.plot) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(filename=paste("figs/numberof_wins-",z,".png", sep=""), p9, width=10, height=8)
  
}

fnPlotbyTemp <- function(team, max.score) {
  
  title.plot = team
  nameOffile = paste("figs/tempvsscore-",team,".png", sep="")
  
  # work out dates game played & venue
  lstDatesofGamesPlayed.byteam <- tmp.afl[which(tmp.afl$Date < "2017-01-01" & tmp.afl$Home.Team == team | tmp.afl$Away.Team == team ),]$Date
  df.games.dateandcities <- fnDateCity(team,lstDatesofGamesPlayed.byteam)

  # subset df.temp
  df.games.team.temp <- fnBuildTeamTempDF(df.games.dateandcities)
  #df.games.team.temp <- ddply(df.games.team.temp, .(Date,City),function(x) head(x,1))
  
  df.winlose.bytemp.byteam <- df.winlose.bytemp[which(df.winlose.bytemp$Home.Team == team | df.winlose.bytemp$Away.Team == team),]
  df.winlose.bytemp.byteam <- merge(df.winlose.bytemp.byteam,df.games.team.temp, all.x = TRUE)
  
  # mean temp annotation
  total.meantemp = round(mean(df.winlose.bytemp.byteam$mean.temp, na.rm=TRUE),1)
  meantemp.lbl = paste("(mean temp. = ",total.meantemp,"C )")
  
  # R squared value
  tempvsscore.lm = lm(mean.temp ~ Home.Score, data=df.winlose.bytemp.byteam)
  rsquared = round(summary(tempvsscore.lm)$r.squared,3)
  
  p11 <- ggplot(data=df.winlose.bytemp.byteam[which(df.winlose.bytemp.byteam$Home.Team == team | df.winlose.bytemp.byteam$Away.Team == team),], aes(x=mean.temp,y=Home.Score)) + geom_point(aes(colour=Winner)) +
    scale_y_continuous(limits=c(0,max.score)) + 
    scale_x_continuous(limits=c(0,30),breaks=c(0,5,10,15,20,25,30)) +
    annotate("text", x=26, y=225, label=paste("(n = ",nrow(df.winlose.bytemp.byteam),")", sep=""),hjust=0) + 
    annotate("text", x=26, y=215, label=meantemp.lbl, hjust=0) + 
    annotate("text", x=26, y=205, label=paste("(R^2 ==",rsquared,")"), parse=TRUE,hjust=0) + 
    scale_colour_manual(labels=c("Lose","Win"), values=c("#FF0000","#000000"),guide=guide_legend(reverse=TRUE)) + 
    theme(legend.title=element_blank(),plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(y="Score", x=expression("mean temperature ("*degree*C*")"), title=title.plot, subtitle=subtitle.plot)
  
  ggsave(filename=nameOffile, p11, width=14, height=8)
  
}


# copied from http://stackoverflow.com/questions/38833219/command-for-exporting-saving-table-made-with-formattable-package-in-r
    
# also had to run webshot::install_phantomjs()

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


fnWhatIsThisDist <- function(dst) {
  
  dst <- na.omit(dst)
  descdist(c(dst), discrete=FALSE, boot=100,boot.col="cornflowerblue",graph=TRUE)
  
  fit.weibull <- fitdist(c(dst),"weibull")
  fit.norm <- fitdist(c(dst),"norm")
  fit.lnorm <- fitdist(c(dst),"lnorm")
  fit.logis <- fitdist(c(dst),"logis")
  fit.cauchy <- fitdist(c(dst),"cauchy")
  fit.gamma <- fitdist(c(dst),"gamma")
  
  c.Title <- c('AIC','BIC')
  c.weibull <- c(round(fit.weibull$aic,4),round(fit.weibull$bic,4))
  c.norm <- c(round(fit.norm$aic,4),round(fit.norm$bic,4))
  c.logis <- c(round(fit.logis$aic,4),round(fit.logis$bic,4))
  c.cauchy <- c(round(fit.cauchy$aic,4),round(fit.cauchy$bic,4))
  
  df.statcompare <- data.frame(c.Title,c.norm,c.weibull,c.logis,c.cauchy)
  names(df.statcompare) <- c('Test','norm','weibull','logis','cauchy')
  print(df.statcompare)
  print("Lower AIC / BIC score is closer to stated distribution")
  
  
  png(file="figs/_denscomp.png",width=800,height=700,res=72)
  par(mfrow = c(1, 1))
  plot.legend <- c("Weibull", "normal", "lognormal", "logis","cauchy","gamma")
  denscomp(list(fit.weibull,fit.norm,fit.lnorm, fit.logis,fit.cauchy),legendtext = plot.legend)
  dev.off()
  
  png(file="figs/_qqplot.png",width=800,height=700,res=72)
  par(mfrow = c(2, 2))
  plot.legend2 <- c("Weibull","normal")
  plot.legend3 <- c("lnorm","logis","cauchy")
  qqcomp(list(fit.weibull,fit.norm),legendtext = plot.legend2)
  qqcomp(list(fit.lnorm, fit.logis,fit.cauchy),legendtext = plot.legend3)
  cdfcomp(list(fit.weibull,fit.norm),legendtext = plot.legend2)
  cdfcomp(list(fit.lnorm,fit.logis,fit.cauchy),legendtext = plot.legend3)
  dev.off()
  
  png(file="figs/_pplot.png",width=800,height=700,res=72)
  par(mfrow = c(1, 2))
  ppcomp(list(fit.weibull,fit.norm),legendtext = plot.legend2)
  ppcomp(list(fit.lnorm, fit.logis,fit.cauchy),legendtext = plot.legend3)
  dev.off()
  
  #plot(fit.norm)
  #plot(fit.weibull)
  #plot(fit.logis)
  #plot(fit.cauchy)
  
}

# gets list of dates not included in df.games.team.temp
lstofEXdates <- lstDatesofGamesPlayed.byteam[lstDatesofGamesPlayed.byteam %nin% df.games.team.temp$Date]
lstofEXdates <- lstofEXdates[which(lstofEXdates<"2017-01-01")]

dfDC.tmp <- dfDC[which(dfDC$Date %in% lstofEXdates),]
tmp4 <- df.temp[which(df.temp$Date %in% dfDC.tmp$Date & df.temp$City %in% dfDC.tmp$City),]
##