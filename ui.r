library(data.table)
library(rhandsontable)
library(shinythemes)
library(shiny)
library(shinyBS) #supports alerts and tooltips0

#load data
pdata <- readRDS("player_data.rds")
proj <- readRDS("projections.rds")

#setup UI

shinyUI(
  navbarPage(theme = "united.css",
             "Quantifan DFS Optimizer",
# Player Projections & Upload ---------------------------------------------
             tabPanel("Player Projections",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          #fantasy site selection input
                          selectInput('site', 'DFS site projections:', 
                                      choices = c('DraftKings', 'FanDuel'),
                                      selected = 'DraftKings'),
#                           hr(),
#                           #fantasy projections download
#                           downloadButton('download_projections', 'Download projections'),
#                           #fantsy projections upload
                          hr(),
                          fileInput('projection_upload', 'Upload projections (.csv)',
                                    accept = '.csv')
                        ),
                        mainPanel(rHandsontableOutput("projections"))
                      )),
             
# Lineup Optimizer Controls -----------------------------------------------
             tabPanel("Lineup Optimization",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
#                           selectInput("site", 
#                                       "DFS site projections:",
#                                       choices = c('DraftKings', 'FanDuel'),
#                                       selected = 'DraftKings'),
#                           hr(),
                          sliderInput("num_lineup", 
                                      "Number of lineups to create:",
                                      min = 1,
                                      max = 20,
                                      value = 1)),
                        mainPanel(uiOutput("tables"))
                      )
             ),
#              
# High Value Players ------------------------------------------------------
             tabPanel("Potential Value Plays",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          selectInput('site_value', 'DFS site projections:', 
                                      choices = c('FanDuel', 'DraftKings'),
                                      selected = 'DraftKings')
                        ),
                        mainPanel(plotOutput("value_players"))
                      )),
#              
# Player analysis controls ------------------------------------------------
             tabPanel("Player Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          selectInput('fp_analysis', 'DFS site projections:', 
                                      choices = c('DraftKings', 'FanDuel'),
                                      selected = 'DraftKings'),
                          hr(),
                          selectInput("player",
                                      "Select player for analysis:",
                                      choices = unique(pdata$name)[order(unique(pdata$name))],
                                      selected = "Carmelo Anthony"),
                          hr(),
                          selectInput("stat", 
                                      "Select statistic for analysis:",
                                      choices = c("Fantasy Points per Min.",
                                                  "Points per Min.",
                                                  "Rebounds per Min.",
                                                  "Assists per Min."),
                                      selected = "Fantasy Points per Min.")),
                        #placeholder for the mainpanel
                        mainPanel(plotOutput('player_minutes_fp'),
                          plotOutput('change_point_graph'))
                      )
             ),
             
# Twitter search controls -------------------------------------------------
             tabPanel("Twitter Search",
                        mainPanel(DT::dataTableOutput("twitter_1"),
                                  DT::dataTableOutput("twitter_2"),
                                  DT::dataTableOutput("twitter_3"),
                                  DT::dataTableOutput("twitter_4"),
                                  DT::dataTableOutput("twitter_5"),
                                  DT::dataTableOutput("twitter_6"),
                                  DT::dataTableOutput("twitter_7"),
                                  DT::dataTableOutput("twitter_8"))
                      ),
             
# Bet sizing estimator controls -------------------------------------------
             tabPanel("Bet Sizing Estimator",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          numericInput("bet", "Bet:", 1, min = 0),
                          numericInput("win", "To win:", .8, min = 0),
                          sliderInput("win_prob", "Your win probability:", .6, min = 0, max = 1, step = .01),
                          sliderInput("trials", "Number of trials:", 5, 365, 100, 5)),
                        mainPanel(textOutput("opt_bet_size"),
                                  plotOutput("bet_graph"))
                        )
                      )
  )
)



