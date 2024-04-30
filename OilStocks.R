install.packages('ggplot2')
install.packages('quantmod')
install.packages('tseries')

library(ggplot2)
library(quantmod)
library(tseries)
library(lubridate)

#Closing Stock data for Exxon from 2021-2022
exxon <- get.hist.quote("xom", start="2021-1-1", end="2022-1-1", quote=c("Cl")) #pulling and saving closing price
exxon <- as.data.frame(exxon) #creating data as dataframe
name <- row.names(exxon) #pulling dates for each row
dates <- ymd(row.names(exxon)) #identifying specific dates from name
exxon <- transform(exxon, date=dates) #adding new column with dates

#Closing Stock data for Chevron from 2021-2022
chevron <- get.hist.quote("cvx", start="2021-1-1", end="2022-1-1", quote=c("Cl")) #pulling and saving closing price
chevron <- as.data.frame(chevron) #creating data as dataframe
name <- row.names(chevron) #pulling dates for each row
dates <- ymd(row.names(chevron)) #identifying specific dates from name
chevron <- transform(chevron, date=dates) #adding new column with dates

#Closing Stock data for Shell from 2021-2022
shellplc <- get.hist.quote("shel", start="2021-1-1", end="2022-1-1", quote=c("Cl")) #pulling and saving closing price
shellplc <- as.data.frame(shellplc) #creating data as dataframe
name <- row.names(shellplc) #pulling dates for each row
dates <- ymd(row.names(shellplc)) #identifying specific dates from name
shellplc <- transform(shellplc, date=dates) #adding new column with dates


#Creating color pallete --> will be used to generate Legend manually
#colors = c('exxon' = 'blue', 'chevron' = 'red', 'shellplc' = 'purple')

#Creating plot
myplot <- ggplot()

  #Adding plot for Exxon
myplot <- myplot + 
  geom_line(data=exxon, aes(x=date, y=Close, color = 'Exxon')) + #generating lineplot
  geom_point(data=exxon, aes(x=date[which.max(Close)], y=max(Close)), size = 3, color = 'green') + #generating Max point
  geom_point(data=exxon, aes(x=date[which.min(Close)], y=min(Close)), size = 3, color = 'red') + #generating Min point
  geom_label(data = exxon, aes(x=date[which.max(Close)], y=max(Close)+2, label = paste("Max Price: $",max(Close))), size = 3) + #Max point label
  geom_label(data = exxon, aes(x=date[which.min(Close)]+13, y=min(Close)+3, label = paste("Min Price: $",min(Close))), size = 3) #Min point label

  #Adding plot for Chevron
myplot <- myplot + 
  geom_line(data=chevron, aes(x=date, y=Close, color = 'Chevron')) + #generating lineplot
  geom_point(data=chevron, aes(x=date[which.max(Close)], y=max(Close)), size = 3, color = 'green') + #generating Max point
  geom_point(data=chevron, aes(x=date[which.min(Close)], y=min(Close)), size = 3, color = 'red') + #generating Min point
  geom_label(data = chevron, aes(x=date[which.max(Close)]-5, y=max(Close)-2, label = paste("Max Price: $",max(Close))), size = 3) + #Max point label
  geom_label(data = chevron, aes(x=date[which.min(Close)]+13, y=min(Close)-2, label = paste("Min Price: $",min(Close))), size = 3) #Min point label


  #Adding plot for Shell
myplot <- myplot + 
  geom_line(data=shellplc, aes(x=date, y=Close, color = 'Shell')) + #generating lineplot
  geom_point(data=shellplc, aes(x=date[which.max(Close)], y=max(Close)), size = 3, color = 'green') + #generating Max point
  geom_point(data=shellplc, aes(x=date[which.min(Close)], y=min(Close)), size = 3, color = 'red') + #generating Min point
  geom_label(data = shellplc, aes(x=date[which.max(Close)], y=max(Close)+2, label = paste("Max Price: $",max(Close))), size = 3) + #Max point label
  geom_label(data = shellplc, aes(x=date[which.min(Close)]+13, y=min(Close)-2, label = paste("Min Price: $",min(Close))), size = 3) #Min point label

#Changing legend and axis title
myplot <- myplot + labs(color = "Legend", title = "Chevron, Exxon, & Shell closing stock prices for 2021", x = 'Date', y = 'Closing Price')

myplot
