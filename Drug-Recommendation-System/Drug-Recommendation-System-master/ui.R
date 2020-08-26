#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinysky)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(tidyverse)
library(tidytext)
library(data.table)
library(tm)
library(caret)
library(dplyr)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(lubridate)
#install.packages("proxy")
library(proxy)
library(openssl)

data = read.csv('D:/ADS/IST 707 Data Analytics/IST707-Project/drugsComTrain_raw.csv', header = TRUE,  stringsAsFactors = TRUE)
data$date <- dmy(data$date)
data['Year'] <- year(data$date)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Drug Recommendation",
                   
                   
                   tabPanel("About",
                            includeMarkdown("Intro.Rmd")
                            ),
                   tabPanel(p(icon("table"), "Explore Data"),
                            sidebarPanel( actionButton(inputId = "clearAll", 
                                                       label = "Clear selection", 
                                                       icon = icon("square-o")),
                                          actionButton(inputId = "selectAll",
                                                       label = "Select all",
                                                       icon = icon("check-square-o")),
                                          uiOutput("drugControl"),
                                          sliderInput("year", "Year:",
                                                      min = min(data$Year), max = max(data$Year), value = 2015, animate = TRUE)),
                            mainPanel(
                              tabsetPanel(
                                # Data 
                                tabPanel(p(icon("table"), "Dataset"),
                                         dataTableOutput(outputId="dTable")
                                ),
                                tabPanel(p(icon("line-chart"), "Visualize the Data"),
                                         h4('Reviews', align = "center"),
                                         plotOutput("plotbydrug"),
                                         h4('Distribution of Top 9 Conditions', align = "center"),
                                         plotOutput("wordhist"),
                                         h4('Distribution of Rating', align = "center"),
                                         plotOutput("rating"),
                                         h4('Average Rating per Drug', align = "center"),
                                         plotOutput("avgdrugrating"),
                                         h4('Number of Useful Reviews by Rating', align = "center"),
                                         plotOutput("countvrating")
                                )
                                ))),
                   tabPanel(p(icon("search"),"Find your Condition"),
                            h5('Let us know what is wrong?', align = "center"),
                            textInput("text", label = "Enter here..."),
                            #selectInput('text'),
                            actionButton("go", "Go"),
                            h5('Prediction by KNN'),
                            verbatimTextOutput('knn'),
                            h5('Prediction by Naive Bayes'),
                            verbatimTextOutput('nb'),
                            h5('Prediction by Tree Bagging'),
                            verbatimTextOutput('tbag')
                            ),
                   tabPanel(p(icon("plus", lib = "glyphicon"),"Conditions"),
                            sidebarPanel(
                              #h5('Select the number of clusters:', align = "center"),
                              radioButtons("k", "Choose of the number of clusters:",
                                           choices = c("2" = 2,
                                                       "3" = 3,
                                                       "4" = 4,
                                                       "5" = 5,
                                                       "6" = 6,
                                                       "7" = 7,
                                                       "8" = 8),
                                           selected = "2")
                            ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("K-means Clustering",
                                  h4('K-means Clustering', align = "center"),
                                  plotOutput("cluster",height = "500px",width = "2000px"),
                                  dataTableOutput(outputId="clustertable")),
                                  tabPanel("Hierarchical Clustering",
                                           h4('Clustering', align = "center"),
                                           plotOutput("hcluster",height = "500px",width = "1000px"),
                                           dataTableOutput(outputId="hclustertable"))
                                  
                                  ))
                              
                                
                                 ),
                   tabPanel(p(icon("search"),"Find your drugs"),
                            
                            fluidRow(
                              
                                     #verbatimTextOutput('out6'),
                                     selectInput('in6', 'Conditions', unique(data$condition) , multiple=TRUE, selectize=TRUE),
                                     dataTableOutput("ls")
                            )
                          
                          
                            )
                            ))
