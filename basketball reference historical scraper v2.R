library(XML)
library(RSQLite)
library(sqldf)
library(lubridate)
library(stringr)
require(dplyr)

history <- c(2000:2015)

setwd("C:/Users/rdelvicario/Desktop/Projects/150427 Quantifan/Basektball Reference Scraping")
url_base <- "http://www.basketball-reference.com/boxscores/"
today <- Sys.Date()

# Load Crosswalk ----------------------------------------------------------
teams <- read.csv("team name crosswalk.csv")


# Load Schedule -----------------------------------------------------------
for(i in history){
  url <- paste0("http://www.basketball-reference.com/leagues/NBA_", i, "_games.html")
  tbl <- readHTMLTable(url)
  sched <- tbl$games
  colnames(sched) <- c("date", "box_score", "visitor", "v_pts", "home", "h_pts", "ot", "notes")
  
  # fix team names to create our reference URL
  sched <- sqldf("Select
                 sched.*,
                 teams.team_short as home_short
                 from
                 sched
                 join
                 teams
                 on
                 sched.home = teams.team_long")
  
  sched <- sqldf("Select
                 sched.*,
                 teams.team_short as visitor_short
                 from
                 sched
                 join
                 teams
                 on
                 sched.visitor = teams.team_long")
  
  sched$date <- as.Date(sched$date, format = "%a, %b %d, %Y")
  sched$url <- paste0(url_base, format(sched$date, format = "%Y%m%d"), "0",
                      sched$home_short, ".html")
  
  sched <- sched[sched$date < (today - 1), ]
  
  
  # loop through teams & dates ----------------------------------------------
  
  cnames <- c("name", "minutes", "true_shooting", "efg", "tpar", "ftar", "orb_perc", "drb_perc",
              "trb_perc", "ast_perc", "stl_perc", "blk_perc", "tov_perc", "usg_perc",
              "off_rtg", "def_rtg", "starters", "mp", "fg", "fga", "fg_perc", "tp",
              "tpa", "tp_perc", "ft", "fta", "ft_perc", "orb", "drb", "trb", "ast",
              "stl", "blk", "tov", "pf", "pts", "plus_minus", "venue", "team", 
              "start", "opp", "date")
  
  data <- data.frame()
  for(i in 1:nrow(sched)){
      print(sched$date[i])
      print(sched$url[i])
      
      tbl <- readHTMLTable(sched$url[i])
      home <- cbind(eval(parse(text = paste0("tbl$", sched$home_short[i], "_advanced"))),
                    eval(parse(text = paste0("tbl$", sched$home_short[i], "_basic"))))
      home$venue <- "H"
      home$team <- sched$home_short[i]
      home$start <- "No"
      home$start[1:5] <- "Yes"
      home$opp <- sched$visitor_short[i]
      
      visitor <- cbind(eval(parse(text = paste0("tbl$", sched$visitor_short[i], "_advanced"))),
                       eval(parse(text = paste0("tbl$", sched$visitor_short[i], "_basic"))))
      visitor$venue <- "A"
      visitor$team <- sched$visitor_short[i]
      visitor$start <- "No"
      visitor$start[1:5] <- "Yes"
      visitor$opp <- sched$home_short[i]
      
      df <- rbind(home, visitor)
      df$date <- sched$date[i]
      colnames(df) <- cnames
      
      # team level stats
      fact <- tbl$four_factors
      colnames(fact) <- c("team", "pace", "efg_perc", "tov_perc", "orb_perc", "fg_fga",
                          "off_rtg")
      df <- sqldf("select
                  df.*,
                  fact.pace,
                  fact.efg_perc as team_efg_perc,
                  fact.tov_perc as team_tov_perc,
                  fact.orb_perc as team_orb_perc,
                  fact.fg_fga as team_fg_fga,
                  fact.off_rtg as team_off_rtg
                  from
                  df
                  join
                  fact
                  on df.team = fact.team")
    data <- rbind(data, df)
  
    rm(fact, visitor, home, df)
  }

  # Remove irrelevant rows and columns
  data <- data %>%
    select(-starters, - mp)
  data <- data[data$name != "Reserves", ]
  
  # Fix minutes
  data$suspended <- ifelse(data$minutes == "Player Suspended", 1, 0)
  data$dnp <- ifelse(data$minutes == "Did Not Play", 1, 0)
  data$minutes[data$minutes == "Did Not Play"] <- NA
  data$minutes[data$minutes == "Player Suspended"] <- NA
  data$minutes <- as.character(data$minutes)
  data$minutes[is.na(data$minutes)] <- "0:0"
  minutes <- str_split(data$minutes, pattern = ":")
  min <- c()
  for(i in 1:length(minutes)){
    min_temp <- as.numeric(minutes[[i]][1]) + as.numeric(minutes[[i]][2]) / 60
    min <- c(min, min_temp)
  }
  data$minutes <- min
  
  # remove all the NA values
  data_txt <- data %>%
    select(name, venue, team, date, start, opp)
  
  # convert player names from factor to text
  data_txt$name <- as.character(data_txt$name)
  
  # Fix numeric values
  data_numeric <- data %>%
    select(-name, -venue, -team, -date, -start, -opp)
  
  # from text to numeric
  for(i in 1:ncol(data_numeric)){
    # convert everything to character first because pace came in as a factor
    data_numeric[, i] <- as.numeric(as.character(data_numeric[, i]))
  }
  
  # recombine data
  data <- cbind(data_txt, data_numeric)
  data$year <- 2015
  
  write.csv(data, paste0(i, " br data.csv"), row.names = F)
}


