library(XML)
library(dplyr)
library(stringr)
library(ggplot2)
setwd("C:/Users/Neil/Desktop/fantasy football")
url <- "http://games.espn.com/ffl/leaders?&slotCategoryId=2&scoringPeriodId=%s&seasonId=2017&startIndex=%s"

#rb <- readHTMLTable(url, stringsAsFactors = FALSE, header = TRUE, skip.rows = 1)$playertable_0

#Read in data for one week for the top 100 scoring running backs in a week
weeks <- seq(1:10)
index <- c(0,50)
rb <- data.frame()

#Loop through each week for the first 10 weeks of the season for RBs
for( w in weeks){
  print( paste("Downloading Week", w, "Data") )
    # Load the url twice to get all of the top 100 ranked players in a position
    for( i in index ) {
      url2 <- sprintf(url,w,i)
      df <- readHTMLTable(url2, stringsAsFactors = FALSE, header = TRUE, skip.rows = 1)$playertable_0
      df$week <- w
      rb <- rbind(rb, df)
    }
}

# Separate Player and Team into two different columns
splt <- str_split_fixed( rb$`PLAYER, TEAM POS`, ",", 2)
rb$Name <- splt[,1]
rb$Team <- splt[,2]
# Drop blank columns
rb <- rb[,-c(2,5,10,14,19,23)]
# Drop rows without any fantasy points scored
rb <- rb[!(rb$PTS == 0),]
# turn relevant columns into numeric
rb$PTS <- as.numeric(rb$PTS)

# Get the average fantasy points scored by each player
avgFP <- aggregate(rb$PTS, by = list(Name = rb$Name), FUN = mean)
varFP <- aggregate(rb$PTS, by = list(Name = rb$Name), FUN = sd)
fpByPlayer <- merge(avgFP,varFP, by = "Name" , suffixes = c("Average_Points", "Std_Dev"))

# Subset the the data for the top 15 averages and then boxplot
pts <- rb[, c("Name", "PTS")]
topPlayers <- fpByPlayer[order(-fpByPlayer$xAverage_Points),]
top15 <- topPlayers[1:15,1]
ptsFiltered <- pts[pts$Name %in% top15,]
boxplot( PTS ~ Name, ptsFiltered )
ggplot(ptsFiltered, aes(x = factor(Name), y = PTS, fill = Name)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Top 15 RBs 2017 by Average Fantasy Points") +
  ylab("Fantasy Points (First Ten Weeks)") + xlab("Player") + labs(fill = "Player")
ggsave("topRBs.png")

# Create a avg points / std. dev ratio 
fpByPlayer$pvRatio <- fpByPlayer$xAverage_Points / fpByPlayer$xStd_Dev
totalFP <- aggregate(rb$PTS, by = list(Name = rb$Name), FUN = sum)
fpByPlayer <- merge(fpByPlayer, totalFP, by = "Name" , suffixes = c("Average_Points", "Std_Dev", "Total Points"))

# See if there is a relationship between total points and the ratio of points to std. dev
ggplot(fpByPlayer, aes(x = fpByPlayer$pvRatio, y = fpByPlayer$x)) + geom_point()
ggplot(fpByPlayer, aes(x = fpByPlayer$xStd_Dev, y = fpByPlayer$x)) + geom_point()
ggplot(fpByPlayer, aes(x = fpByPlayer$xAverage_Points, y = fpByPlayer$x)) + geom_point()

# WR statistics -----------------------------------------------------------

url <- "http://games.espn.com/ffl/leaders?&slotCategoryId=4&scoringPeriodId=%s&seasonId=2017&startIndex=%s"

#wr <- readHTMLTable(url, stringsAsFactors = FALSE, header = TRUE, skip.rows = 1)$playertable_0

#Read in data for one week for the top 100 scoring running backs in a week
weeks <- seq(1:10)
index <- c(0,50)
wr <- data.frame()

#Loop through each week for the first 10 weeks of the season for wrs
for( w in weeks){
  print( paste("Downloading Week", w, "Data") )
  # Load the url twice to get all of the top 100 ranked players in a position
  for( i in index ) {
    url2 <- sprintf(url,w,i)
    df <- readHTMLTable(url2, stringsAsFactors = FALSE, header = TRUE, skip.rows = 1)$playertable_0
    df$week <- w
    wr <- rbind(wr, df)
  }
}

# Separate Player and Team into two different columns
splt <- str_split_fixed( wr$`PLAYER, TEAM POS`, ",", 2)
wr$Name <- splt[,1]
wr$Team <- splt[,2]
# Drop blank columns
wr <- wr[,-c(2,5,10,14,19,23)]
# Drop rows without any fantasy points scored
wr <- wr[!(wr$PTS == 0),]
# turn relevant columns into numeric
wr$PTS <- as.numeric(wr$PTS)

# Get the average fantasy points scored by each player
avgFP <- aggregate(wr$PTS, by = list(Name = wr$Name), FUN = mean)
varFP <- aggregate(wr$PTS, by = list(Name = wr$Name), FUN = sd)
fpByPlayer <- merge(avgFP,varFP, by = "Name" , suffixes = c("Average_Points", "Std_Dev"))

# Subset the the data for the top 15 averages and then boxplot
pts <- wr[, c("Name", "PTS")]
topPlayers <- fpByPlayer[order(-fpByPlayer$xAverage_Points),]
top15 <- topPlayers[1:15,1]
ptsFiltered <- pts[pts$Name %in% top15,]
boxplot( PTS ~ Name, ptsFiltered )
ggplot(ptsFiltered, aes(x = factor(Name), y = PTS, fill = Name)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Top 15 WRs 2017 by Average Fantasy Points") +
  ylab("Fantasy Points (First Ten Weeks)") + xlab("Player") + labs(fill = "Player")
ggsave("topWRs.png")

# Create a avg points / std. dev ratio 
fpByPlayer$pvRatio <- fpByPlayer$xAverage_Points / fpByPlayer$xStd_Dev
totalFP <- aggregate(wr$PTS, by = list(Name = wr$Name), FUN = sum)
fpByPlayer <- merge(fpByPlayer, totalFP, by = "Name" , suffixes = c("Average_Points", "Std_Dev", "Total Points"))

# See if there is a relationship between total points and the ratio of points to std. dev
ggplot(fpByPlayer, aes(x = fpByPlayer$pvRatio, y = fpByPlayer$x)) + geom_point()
ggplot(fpByPlayer, aes(x = fpByPlayer$xStd_Dev, y = fpByPlayer$x)) + geom_point()
ggplot(fpByPlayer, aes(x = fpByPlayer$xAverage_Points, y = fpByPlayer$x)) + geom_point()
