## PLOTLY mega plot teams by venue

#df.plot.percentagewins$percentage.wins <- df.plot.percentagewins$percentage.wins*100

title.plot = "Percentage Wins by venue - All"

teams1 <- list(
  showactive = FALSE,
  type= 'buttons',
  direction = "right",
  xanchor = 'center',
  yanchor = "bottom",
  x = 0.5,
  y = -1,
  buttons = list(
    
    list(
      label = "Adelaide",
      method = "update",
      args = list(list(visible = c(TRUE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Adelaide</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Brisbane",
      method = "update",
      args = list(list(visible = c(FALSE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Brisbane</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Carlton",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Carlton</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Collingwood",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Collingwood</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Essendon",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Essendon</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Fremantle",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Fremantle</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "North Melbourne",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - North Melbourne</br><i>",subtitle.plot,"</i>"))))

  ))

teams2 <- list(
  showactive = FALSE,
  type= 'buttons',
  direction = "right",
  xanchor = 'center',
  yanchor = "bottom",
  x = 0.5,
  y = -1.2,
  buttons = list(
    
    list(
      label = "Geelong",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Geelong</br><i>",subtitle.plot,"</i>")))),
    
    list(
      label = "Gold Coast",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Gold Coast</br><i>",subtitle.plot,"</i>")))),
    
    list(
      label = "GWS Giants",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - GWS Giants</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Hawthorn",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Hawthorn</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Melbourne",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Melbourne</br><i>",subtitle.plot,"</i>")))),
    
    list(
      label = "North Melbourne",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - North Melbourne</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Port Adelaide",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Port Adelaide</br><i>",subtitle.plot,"</i>"))))

  ))

teams3 <- list(
  showactive = FALSE,
  type= 'buttons',
  direction = "right",
  xanchor = 'center',
  yanchor = "bottom",
  x = 0.5,
  y = -1.4,
  buttons = list(
   
     list(
      label = "Richmond",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Richmond</br><i>",subtitle.plot,"</i>")))),  
   list(
      label = "St Kilda",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - St Kilda</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Sydney",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)),
                  list(title = paste("Percentage Wins by venue - Sydney</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "West Coast",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
                  list(title = paste("Percentage Wins by venue - West Coast</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "Western Bulldogs",
      method = "update",
      args = list(list(visible = c(FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)),
              list(title = paste("Percentage Wins by venue - Western Bulldogs</br><i>",subtitle.plot,"</i>")))),
    list(
      label = "All teams",
      method = "update",
      args = list(list(visible = c(TRUE, TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
             list(title = paste("Percentage Wins by venue - All teams</br><i>",subtitle.plot,"</i>"))))

  ))

m <- list(
  l = 150,
  r = 100,
  b = 150,
  t = 150,
  pad=10
)


p <- plot_ly(df.plot.percentagewins, x=~Venue,
        hoverinfo = 'text',
        text=~paste(round(percentage.wins,2),"%", Team,
                    "</br>",Venue),
        width = 800,
        height = 600) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Adelaide"),],
                 y=~percentage.wins, name="Adelaide", marker = list(color = 'rgb(15,29,66)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Brisbane"),],
                 x=~Venue, y=~percentage.wins, name="Brisbane", marker = list(color = 'rgb(155,0,51)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Carlton"),],
                 x=~Venue, y=~percentage.wins, name="Carlton", marker = list(color = 'rgb(1,25,49)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Collingwood"),],
                 x=~Venue, y=~percentage.wins, name="Collingwood", marker = list(color = 'rgb(0,0,0)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Essendon"),],
                 x=~Venue, y=~percentage.wins, name="Essendon",marker = list(color= 'rgb(204,12,0)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Fremantle"),],
                 x=~Venue, y=~percentage.wins, name="Fremantle",marker = list(color = 'rgb(45,0,84)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Geelong"),],
                 x=~Venue, y=~percentage.wins, name="Geelong",marker = list(color = 'rgb(12,35,64)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Gold Coast"),],
                 x=~Venue, y=~percentage.wins, name="Gold Coast",marker = list(color ='rgb(252,25,33)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "GWS Giants"),],
                 x=~Venue, y=~percentage.wins, name="GWS Giants",marker = list(color = 'rgb(247,143,30)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Hawthorn"),],
                 x=~Venue, y=~percentage.wins, name="Hawthorn",marker = list(color = 'rgb(54,21,0))')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Melbourne"),],
                 x=~Venue, y=~percentage.wins, name="Melbourne",marker = list(color = 'rgb(1,25,49)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "North Melbourne"),],
                 x=~Venue, y=~percentage.wins, name="North Melbourne",marker = list(color = 'rgb(14,43,141)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Port Adelaide"),],
                 x=~Venue, y=~percentage.wins, name="Port Adelaide",marker = list(color = 'rgb(0,142,143)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Richmond"),],
                 x=~Venue, y=~percentage.wins, name="Richmond",marker = list(color = 'rgb(255,214,0)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "St Kilda"),],
                 x=~Venue, y=~percentage.wins, name="St Kilda",marker = list(color = 'rgb(252,25,33)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Sydney"),],
                 x=~Venue, y=~percentage.wins, name="Sydney",marker = list(color = 'rgb(242,0,23)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "West Coast"),],
                 x=~Venue, y=~percentage.wins, name="West Coast",marker = list(color = 'rgb(5,23,63)')) %>%
        add_bars(data=df.plot.percentagewins[which(df.plot.percentagewins$Team == "Western Bulldogs"),],
                 x=~Venue, y=~percentage.wins, name="Western Bulldogs",marker = list(color = 'rgb(13,54,146)')) %>%
        layout(title = paste(title.plot,"</br><i>",subtitle.plot,"</i>"),
               showlegend=FALSE,
               autosize=F,
               margin = m,
               yaxis = list(title = "Percentage Wins",range=list(1,100)),
               xaxis = list(title = "",
                            range=list(0,18),
                            showticklabels = TRUE,
                            tickangle = 90),
               updatemenus=list(teams1,teams2,teams3),
               hovermode = "closest") 

plotly_POST(p, filename = "percentage-wins-by-venue-all")


# PLOT 10 plotly
# setup display variables
cs.winlose <- c("red","black")
tf <- list(
  family = "arial",
  size = 18
)
f <- list(
  family = "arial",
  size = 14,
  color = "#7f7f7f"
)
x <- list(
  title = "Mean temperature in Celsius",
  titlefont = f,
  range=c(0,35),
  zeroline=FALSE
)
y <- list(
  title = "Score",
  titlefont = f
)

# annotations
# mean temp 
total.meantemp = round(mean(df.winlose.bytemp$mean.temp, na.rm=TRUE),1)
meantemp.lbl = paste("(mean temp. = ",total.meantemp,"C )")

# R squared value
tempvsscore.lm = lm(mean.temp ~ Home.Score, data=df.winlose.bytemp)
rsquared = round(summary(tempvsscore.lm)$r.squared,3)
rsquared.lbl = paste("(R<sup>2</sup> =",rsquared,")")

a <- list(
  x = 32,
  y = 200,
  text = paste("(n =",nrow(df.winlose.bytemp),")</br>",meantemp.lbl,"</br>",rsquared.lbl),
  xref = "x",
  yref = "y",
  showarrow = FALSE
)

# add column for proper detailed names in legend
df.winlose.bytemp.col <- df.winlose.bytemp %>% mutate(col = ifelse(Winner == "Home", "Win", "Lose"))

p10.plotly <- plot_ly(df.winlose.bytemp.col,x=~mean.temp,y=~Home.Score,
                      type="scatter",mode="markers",
                      color = ~col,
                      colors = cs.winlose,
                      opacity = 0.7,
                      hoverinfo = 'text',
                      text=~paste('Mean temp: ',mean.temp,
                                  '</br>Match: ',Home.Team, ' vs ', Away.Team,
                                  '</br>Score: ',Home.Score, ' to ', Away.Score,
                                  '</br>Venue: ',Venue)) %>%
  layout(title = paste("Temperature vs Score</br><i>",subtitle.plot,"</i>"),
         titlefont = tf,
         xaxis = x, 
         yaxis = y, 
         legend=list(traceorder='reversed'),
         annotations=a)

plotly_POST(p10.plotly, filename = "TemperatureVSScore")
