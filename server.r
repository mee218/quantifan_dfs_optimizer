library(data.table)
library(xtable)
library(RColorBrewer)
library(twitteR)
library(stringr)
library(lpSolve)
library(shiny)
library(shinyBS) #supports alerts and tooltips
library(changepoint)
library(gridExtra)
require(reshape2)
library(ggthemes)
library(ggplot2)
library(rhandsontable)
library(plyr)
library(dplyr)

twit_cred <- read.csv("C:/Users/rdelvicario/Desktop/Projects/150427 Quantifan/twitter credentials.csv", stringsAsFactors=FALSE)

#setup twitter credentials
setup_twitter_oauth(consumer_key = twit_cred[1,2],
                    consumer_secret = twit_cred[2,2],
                    access_token = twit_cred[3,2],
                    access_secret = twit_cred[4,2])
rm(twit_red)


  
#load data
pdata <- data.table(readRDS("player_data.rds"))
dk <- data.table(readRDS("projections.rds"))
fd <- data.table(readRDS("projections.rds"))

# Setup data --------------------------------------------------------------

#this data will be setup prior
dk <- cbind(start = rep(T, nrow(dk)), lock = rep(F, nrow(dk)), exclude = rep(F, nrow(dk)), dk)
dk <- dk %>% select(name, ps, start, lock, exclude, salary, game_info, dk_fp_start, dk_fp_bench)
dk$fp <- ifelse(dk$start == T, dk$dk_fp_start, dk$dk_fp_bench)


fd <- cbind(start = rep(T, nrow(fd)), lock = rep(F, nrow(fd)), exclude = rep(F, nrow(fd)), fd)
fd <- fd %>% select(name, ps, start, lock, exclude, salary, game_info, dk_fp_start, dk_fp_bench)
fd$fp <- ifelse(dk$start == T, fd$dk_fp_start, fd$dk_fp_bench)

# UDF ---------------------------------------------------------------------

#source optimization functions
source("iterative_solve.r")

getTweets <- function(x) {
  out <- ldply(searchTwitter(x, lang = 'en', resultType = "recent", n = 10), function(x) {
    Tweet <- as.character(x$text)
    Tweet <- str_replace_all(Tweet, "[^[:alnum:]]", " ")
    Screename <- x$screenName
    x <- data.frame(Screename, Tweet)
  })
  out
}

tweetToDT <- function(x, num){
  tweets <- x()
  tweets <- tweets[tweets$player_num == num, ]
  player <- unique(tweets$player)
  tweets$player_num <- NULL
  tweets$player <- NULL
  rownames(tweets) <- NULL
  print(tweets)
  #     tweets <- print(xtable(tweets, caption = player),
  #                    type="html",
  #                    html.table.attributes='class="data table table-bordered table-condensed"',
  #                    caption.placement="top")
  #     print(tweets)
  print(is.data.frame(tweets))
  tweets <- DT::datatable(tweets,
                          caption = player,
                          escape = T,
                          selection = "none",
                          style = "bootstrap",
                          rownames = F,
                          filter = "none")
  tweets
}

createCptEst <- function(est, cpts) {
  idx <- 0
  out <- c()
  est <- unlist(est)
  cpts <- unlist(cpts)
  
  for (i in 1:length(cpts)) {
    out <- c(out, rep(est[i], cpts[i] - idx))
    idx <- cpts[i]
  }
  out
}

calcStartFp <- function(x) {
  x$start_fp <- ifelse(x$start == T, x$dk_fp_start, x$dk_fp_bench)
  x
}

setHotNames <- function(x){
  setnames(x, colnames(x), c("Player", "Position", "Start", "Lock", "Exclude", 
                             "Salary", "Game Info", "Start FP", "Bench FP", "FP"))
  x
}

setDFNames <- function(x){
  setnames(x, colnames(x), c("name", "ps", "start", "lock", "exclude", "salary", "game_info", "fp_start", "fp_bench", "fp"))
  x
}

setFp <- function(x){
  x$fp <- ifelse(x$start == T, x$fp_start, x$fp_bench)
  x
}

hotUpdate <- function(df){
  df <- setDFNames(df)
  df <- setFp(df)
  df <- setHotNames(df)
  hot <- rhandsontable(df, readOnly = T, rowHeaders = F, columnSorting = T, exportToCsv = T) %>%
    hot_table(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_col("Salary", format = "$0,0") %>%
    hot_col(c("Start", "Lock", "Exclude"), readOnly = F) 
  hot
}

dfInit <- function(df){
  hot <- rhandsontable(df, readOnly = T, rowHeaders = F, columnSorting = T, exportToCsv = T) %>%
    hot_table(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_col("Salary", format = "$0,0") %>%
    hot_col(c("Start", "Lock", "Exclude"), readOnly = F) 
  hot
}

tableSplit <- function(df, i){
  out <- df[df$sol_num == i, ]
  out
}

tabelize <- function(variables, arg2) {
  
  tables <- list() # create a list to hold all tables
  
  for (i in 1:length(variables)) { # go through all possible values of variables
    table <- tableSplit(arg2, i)
    proj_fp <- unique(table$proj_fp)
    table$proj_fp <- NULL
    table$sol_num <- NULL
    setnames(table, colnames(table), c("Player", "Pos.", "Salary", "Game Info", "Proj. FP"))
    rownames(table) <- NULL
    # save table into slot in created list
    # print table as HTML with additional formatting options
    cap <- paste0("Solution Number: ", i, " Projected Fantasy Points: ", round(proj_fp, 1))
    table <- print(xtable(table, caption= cap),
                type="html",
                html.table.attributes='class="data table table-bordered table-condensed"',
                caption.placement="top")
    tables[[as.character(i)]] <- table

  }
  return(lapply(tables, paste)) # return HTML tables pasted together
}

simBetPath <- function(opt_bet_size, win, win_prob, trials){
  trial_num <- c(0:trials)
  br <- rep(NA, trials + 1)
  binom <- runif(trials)
  
  #set starting bankroll
  br[1] <- 100
  for(i in 2:length(br)){
    if(binom[i-1] < win_prob){
      br[i] <- br[i-1] + ((br[i-1] * opt_bet_size) * win)
    } else {
      br[i] <- br[i-1] - (br[i-1] * opt_bet_size)
    }
  }
  br
}

simMultPaths <- function(opt_bet_size, win, win_prob, trials, paths){
  
  df <- data.frame(step_num = c(0:trials))
  for(i in 1:paths){
    x <- simBetPath(opt_bet_size, win, win_prob, trials)
    df <- cbind(df, x)
  }
  colnames(df) <- c("step_num", paste0("x", c(1:paths)))
  df <- melt(df, id.vars = "step_num")
  df
}




# Shiny Server  -----------------------------------------------------------
shinyServer(function(input, output, session) {
# Player Projections and Upload -------------------------------------------

  values = reactiveValues()
  values[["draftkings"]] <- dk
  values[["fanduel"]] <- fd
  
  observe({   
    if (!is.null(input$projections)) {  
      temp <- hot_to_r(input$projections)
      temp <- setDFNames(temp)
      if (isolate(input$site) == "DraftKings") {       
        values[["draftkings"]] <- temp
      } else {
        values[["fanduel"]] <- temp
      }
    }
  })
  
  df <- reactive({
    if (input$site == "DraftKings") {
      df <- values[["draftkings"]]
    } else {
      df <- values[["fanduel"]]
    }
    df
  })

  #table tools to export to csv, xls, etc exist
  #https://rstudio.github.io/DT/003-tabletools-buttons.html
  output$projections <- renderRHandsontable({
    hot <- hotUpdate(df())
    hot
  })
  
# Lineup Optimization -----------------------------------------------------
  
  #optimization
    solutions <- reactive({
      opt <- df()
      opt <- setDFNames(opt)
      dummy_vars <- solverSetupData(opt)
      out <- loopSolver(opt, dummy_vars, input$num_lineup)
      out
    })
  
  output$tables <- renderUI({
    out <- solutions()
    out <- tabelize(unique(out$sol_num), out) 
    # additional options to make rendering possible
    return(div(HTML(out),class="shiny-html-output"))
  })
  
# High Value Players ------------------------------------------------------
  output$value_players <- renderPlot({
    #setup plot data
    plot_data <- df()
    plot_data$df_differential <- plot_data$fp_start - plot_data$fp_bench
    plot_data$rank <- rank(plot_data$df_differential) 
    plot_data <- plot_data[plot_data$rank > (nrow(plot_data) - 26), ]
    plot_data$fp_1000 <- plot_data$fp_start / (plot_data$salar / 1000)
    
    #setup plot
    p1 <- ggplot(data = plot_data, aes(x = df_differential, y = reorder(name, rank))) +
      geom_point(size = 3) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90, hjust = 1),
            axis.text.y = element_text(hjust = 1)) +
      xlab("FP Start Projection less FP Bench Projection") +
      ylab("") +
      ggtitle("Bench players with the highest start FP less bench FP differential")
    
    p2 <- ggplot(data = plot_data, aes(x = fp_1000, y = reorder(name, fp_1000))) +
      geom_point(size = 3) + 
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90, hjust = 1),
            axis.text.y = element_text(hjust = 1)) +
      xlab("FP Start Projection less FP Bench Projection") +
      ylab("") +
      ggtitle("Bench players with the highest Start FP / $1,000")
    
    #write graphs for output
    p <- grid.arrange(p1, p2, ncol = 2)
    
  })
  
# Player Analysis ---------------------------------------------------------
  output$change_point_graph <- renderPlot({
    plot_data <- pdata[pdata$name == input$player,]
    plot_data$fp_min <- switch(
      input$fp_analysis,
      'DraftKings' = plot_data$dk_fp_min,
      'FanDuel' = plot_data$fd_fp_min
    )
    plot_data$metric <- switch(
      input$stat,
      'Fantasy Points per Min.' = plot_data$fp_min,
      'Minutes per Game' = plot_data$minutes,
      'Points per Min.' = plot_data$pts_min,
      'Rebounds per Min.' = plot_data$trb_min,
      'Assists per Min.' = plot_data$ast_min
    )
    
    #remove NA Values for change point analysis
    plot_data <- plot_data[!is.na(plot_data$metric),]
    plot_data$num <- c(1:nrow(plot_data))
    
    #create change point data
    mdl <- cpt.mean(plot_data$metric, method = "PELT")
    plot_data$est <- createCptEst(mdl@param.est, mdl@cpts)
    
    #graph change points
    p1 <- ggplot(data = plot_data) +
      geom_point(aes(x = num, y = metric)) +
      geom_line(aes(x = num, y = est), col = "red") +
      ggtitle(paste0("Changepoint Analysis: ", input$stat,"\n")) +
      scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90)) +
      xlab("Game Number") +
      ylab("")
    
    #graph trends
    p2 <- ggplot(data = plot_data) +
      geom_point(aes(x = num, y = metric)) +
      geom_smooth(aes(x = num, y = metric), col = "red") +
      ggtitle(paste0("Trend Analysis: ", input$stat,"\n")) +
      scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90)) +
      xlab("Game Number") +
      ylab("")
    
    #write graphs for output
    p <- grid.arrange(p1, p2, ncol = 2)
    
  })
  
  output$player_minutes_fp <- renderPlot({
    plot_data <- pdata[pdata$name == input$player,]
    plot_data <- plot_data[plot_data$minutes > 0,]
    plot_data$num <- c(1:nrow(plot_data))
    
    p1 <- ggplot(data = plot_data) +
      geom_point(aes(x = num, y = minutes)) +
      geom_smooth(aes(x = num, y = minutes), col = "red") +
      ggtitle("Trend Analysis: Minutes Played") +
      scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90)) +
      xlab("Game Number") +
      ylab("")
    
    p2 <- ggplot(data = plot_data) +
      geom_point(aes(x = num, y = fp)) +
      geom_smooth(aes(x = num, y = fp), col = "red") +
      ggtitle("Trend Analysis: Fantasy Points") +
      scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(),
            axis.title.y = element_text(angle = 90)) +
      xlab("Game Number") +
      ylab("")
    
    #write graphs for output
    p <- grid.arrange(p1, p2, ncol = 2)
    
  })
  
# Twitter Search ----------------------------------------------------------
  
  twit_out <- reactive({
    players <- solutions()
    players <- players$name[players$sol_num == 1]
    tweets <- data.frame()
    for(i in 1:length(players)){
      tmp <- getTweets(players[i])
      tmp$player_num <- i
      tmp$player <- players[i]
      tweets <- rbind(tweets, tmp)
    }
    tweets
  })
  
  output$twitter_1 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 1)
    tweets
  })
  
  output$twitter_2 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 2)
    tweets
  })
  
  output$twitter_3 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 3)
    tweets
  })
  
  output$twitter_4 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 4)
    tweets
  })
  
  output$twitter_5 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 5)
    tweets
  })
  
  output$twitter_6 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 6)
    tweets
  })
  
  output$twitter_7 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 7)
    tweets
  })
  
  output$twitter_8 <- DT::renderDataTable({
    tweets <- tweetToDT(twit_out, 8)
    tweets
  })
  
# Bet Sizing --------------------------------------------------------------
  
  output$opt_bet_size <- renderText({
    lose_norm <- input$bet / input$win
    win_norm <- 1
    win_prob <- input$win_prob
    opt_bet_size <- ((win_norm * win_prob) - ((1 - win_prob) * lose_norm)) * (win_norm * lose_norm)
    opt_bet_size <- as.character(round(opt_bet_size / 2 * 100, 1))
    paste0("Half Kelly Criterion indicates that you should bet ", opt_bet_size, "% of your bankroll per night to maximize your returns.")
  })
  
  output$bet_graph <- renderPlot({
    
    #calculate optimal bet size
    lose_norm <- input$bet / input$win
    win_norm <- 1
    win_prob <- input$win_prob
    opt_bet_size <- (((win_norm * win_prob) - ((1 - win_prob) * lose_norm)) * (win_norm * lose_norm)) / 2
    win_perc_loss <- input$win / input$bet
    
    if(opt_bet_size > 0){
      #simulate trials
      paths <- simMultPaths(opt_bet_size, win_perc_loss, win_prob, input$trials, 100)

      p1 <- ggplot(data = paths, aes(x = step_num, y = value)) + stat_density2d(aes(fill = log(..level..)), geom="polygon") + 
        scale_fill_gradient(low = "#bdbdbd", high = "#252525", guide = FALSE) +
        scale_color_fivethirtyeight() +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(),
              axis.title.y = element_text(angle = 90)) +
        geom_hline(aes(yintercept = 100), col = "red") +
        ylab("Bankroll Value") +
        xlab("Bet Number\n(100 Bankroll Path Simulations)") +
        ggtitle("Bankroll Path Density Using 1/2 Kelly Criterion")
      p1
      
      p2 <- ggplot(data = paths[paths$step_num == 100, ]) + geom_histogram(aes(x = value), fill = "gray", col = "black") +
        ylab("Frequency") + 
        xlab("Ending Bankroll Value\n(100 Bankroll Path Simulations)") + 
        ggtitle("Ending Bankroll Value Using 1/2 Kelly Criterion") +
        geom_vline(aes(xintercept = 100), col = "red") +
        scale_color_fivethirtyeight() +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(),
              axis.title.y = element_text(angle = 90))
      
      #write graphs for output
      p <- grid.arrange(p1, p2, ncol = 2)

    } else {
      df <- data.frame(x = rep(0, 10), y = rep(0,10))
      p <- ggplot(data = df) + geom_point(aes(x = x, y = y), alpha = 0) +
        scale_color_fivethirtyeight() +
        theme_fivethirtyeight() +
        theme(axis.title = element_text(),
              axis.title.y = element_text(angle = 90)) +
        ylab("") +
        xlab("") +
        ggtitle("Please make sure optimal bet size is greater than 0")
      # p <- NULL
      
    }
    p
  })
  
})
