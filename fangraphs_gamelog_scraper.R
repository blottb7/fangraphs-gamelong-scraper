library(rvest)
library(tidyverse)

#user defined variables
low_stat <- 2.00
high_stat <- 4.40
steamer_projection <- 4.56

#input player game log data from fangraphs
#url <- 'https://www.fangraphs.com/statsd.aspx?playerid=14168&position=P&gds=&gde='  #jose berrios
url <- 'https://www.fangraphs.com/statsd.aspx?playerid=12049&position=P&gds=&gde='  #kyle hendricks
#read player game log data webpage
webpage <- read_html(url)

#get all data
webpage1 <- html_nodes(webpage, '.grid_line_break , .grid_line_regular , .rgHeader')
webpage2 <- html_text(webpage1)
#save data as dataframe
game_log <- data.frame(matrix(unlist(webpage2), ncol = 27, byrow = TRUE), stringsAsFactors = FALSE)

#the first row are column names; get that and save as names
game_log_names <- game_log[1,]

#name columns
names(game_log) <- game_log_names

#remove first column
game_log <- game_log[-1,]

#invert row order
game_log <- game_log[order(nrow(game_log):1),]

#save totals for later use
totals <- game_log[nrow(game_log),]

#remove totals from game log
game_log <- game_log[-nrow(game_log),]

#create ytd totals
copy <- game_log

#convert classes
copy$IP <- as.numeric(copy$IP)
copy$ER <- as.numeric(copy$ER)

#create cumulative totals
copy$IP_ytd <- cumsum(copy$IP)
copy$ER_ytd <- cumsum(copy$ER)
copy$ERA_ytd <- round(9 * copy$ER_ytd / copy$IP_ytd, 2)

#graph
ggplot(copy, aes(Date, ERA)) + geom_point()
ggplot(copy, aes(Date, ERA_ytd, group = 1)) + 
  geom_point() + 
  geom_line() +
  geom_hline(yintercept = low_stat, linetype = "dashed", color = "orange", size = 2) +
  geom_hline(yintercept = high_stat, linetype = "dashed", color = "orange", size = 2) + 
  geom_hline(yintercept = steamer_projection, color = "red", size = 4)

# ggplot(copy, aes(x = Date, y = ERA_ytd, ERA, group = 1)) + 
#   geom_point() + 
#   geom_line() +
#   geom_hline(yintercept = low_stat, linetype = "dashed", color = "orange", size = 2) +
#   geom_hline(yintercept = high_stat, linetype = "dashed", color = "orange", size = 2) + 
#   geom_hline(yintercept = steamer_projection, color = "red", size = 4)

#copy["GS":"SO"] <- as.numeric(copy["GS":"SO"])
#
# raw_stats <- copy[,4:16]
# raw_stats <- as.numeric(unlist(raw_stats))
# # copy[,4:16] <- as.numeric(copy[,4:16])
# copy[,4] <- as.numeric(copy[,4])


#roll apply type function
# i <- 1
# for(1:nrow(copy)) {
#   copy
# }