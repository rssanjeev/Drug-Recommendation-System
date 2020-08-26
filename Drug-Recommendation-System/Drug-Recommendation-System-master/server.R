#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
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

source("data_processing.R")
drugs <- sort(unique(data$drugName))

data = read.csv('D:/ADS/IST 707 Data Analytics/IST707-Project/drugsComTrain_raw.csv', header = TRUE,  stringsAsFactors = TRUE)

df <- data[-c(1)]
df<- df[!(df$condition %like% '</span>'),]
df<- df[!(df$condition == ""),]
df %>%
  group_by(drugName) %>%
  filter(n() >= 1000) -> data
data <- droplevels(data)
drugs <- unique(data$drugName)
drugs <- drugs[! drugs %in%  c( "Levonorgestrel","Nexplanon","Ethinyl estradiol / levonorgestrel", "Ethinyl estradiol / norethindrone","Ethinyl estradiol / norgestimate","Ethinyl estradiol / norethindrone")]
data <- data[data$drugName %in% drugs,]
data <- droplevels(data) 
data <- data %>% group_by(drugName)
data$review <- gsub('&#039;', "'", data$review)
data$date <- dmy(data$date)
data['Year'] <- year(data$date)
#nrow(data)

#Find your drugs

drug_table <- function (data, conditions) {
  
  df <- data[-c(5,6)]
  df <- as.data.frame(df[ (df$condition %in% conditions),])
  df <- unique(df)
  df <- df %>%
    group_by(drugName) %>%
    dplyr::summarize(Average_Rating = mean(rating, na.rm=TRUE))
  df <- df[order(-df$Average_Rating),]
  df
  
}

#Wordcloud

groupbyDrug <- function(data) {
  drugViz <- data %>% 
    unnest_tokens(word, review) %>% 
    anti_join(stop_words)
  res <- drugViz %>% 
    dplyr::group_by(Year,drugName,word) %>% 
    dplyr::summarise(Freq=n())  
  
  return(res)
}

groupbyDrughist <- function(data) {
  res <- data %>% 
    dplyr::group_by(condition) %>% 
    dplyr::summarise(Freq=n())  
  #res <- res[!res$word=='2']
  #res <- res[!res$word=='3']
  
  return(res)
}

groupbyRating <- function(data) {
  res <- data %>% 
    dplyr::group_by(rating) %>% 
    dplyr::summarise(Freq=n())  
  #res <- res[!res$word=='2']
  #res <- res[!res$word=='3']
  
  return(res)
}


groupbyDrugYear <- function(data) {
  drugViz <- data %>% 
    unnest_tokens(word, review) %>% 
    anti_join(stop_words)
  
  #drugViz$Year <- year(drugViz$date)
  
  res <- drugViz %>% 
    dplyr::group_by(Year, drugName,word) %>% 
    dplyr::summarise(Freq=n())  
  
  return(res)
  
}

load(file = "model_knn2.rda")
load(file = "model_nb.rda")
load(file = "model_bag.rda")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$out6 <- renderPrint(input$in6)
  
  datatable <- reactive({
    drug_table(data, input$in6)
  })
  
  output$ls <- renderDataTable( {
    datatable()
    })

  # Prepare dataset
  dataTable <- reactive({
    result <- data %>% filter(drugName %in% input$drugs) 
    return(result)
      })
    
  # Render data table
  output$dTable <- renderDataTable({
    dataTable()
  } #, options = list(bFilter = FALSE, iDisplayLength = 50)
  )
  
  # Initialize reactive values
  values <- reactiveValues()
  values$drugs <- drugs
  
  observe({
    if(input$selectAll > 0) {
      updateCheckboxGroupInput(session=session, inputId="drugs", 
                               choices=drugs, selected=drugs)
      values$drugs <- drugs
    }
  })
  
  observe({
    if(input$clearAll > 0) {
      updateCheckboxGroupInput(session=session, inputId="drugs", 
                               choices=drugs, selected=NULL)
      values$drugs <- c()
    }
  })
  
  # Create event type checkbox
  output$drugControl <- renderUI({
    checkboxGroupInput('drugs', 'Drugs:', 
                       drugs, selected = data$drugName)
  })
  

  #Wordcloud for drugs
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  drug_wordplot <- reactive({
    result <- data %>% filter(drugName %in% input$drugs)
    result <- result %>% filter(Year == input$year)
    groupbyDrug(result)
  })
  
  drug_histplot <- reactive({
    result <- data %>% filter(drugName %in% input$drugs)
    result <- result %>% filter(Year == input$year)
    groupbyDrughist(result)
  })
  
  getdata <- reactive({
    result <- data %>% filter(drugName %in% input$drugs)
    result <- result %>% filter(Year == input$year)
    #groupbyRating(result)
  })
  
  getk <- reactive({
    k <- intput$k
    k
  })
  
  getmastercluster <- reactive({
    
    temp <- as.matrix(drug_dtm)
    temp = proxy::dist(temp, method = "cosine") 
    temp <- kmeans(temp, input$k) 
    master.cluster <- temp$cluster 
  })
  
  getslave.hierarchical <- reactive({
    
    drug_dtm_matrix <- as.matrix(drug_dtm)
    dist.matrix = proxy::dist(drug_dtm_matrix, method = "cosine")
    clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
    slave.hierarchical <- cutree(clustering.hierarchical, k = input$k) 
    
  })
  
  getpoints <- reactive({
    temp <- as.matrix(drug_dtm)
    dist.matrix = proxy::dist(temp, method = "cosine") 
    points <- cmdscale(dist.matrix, k = 2) 
  })
  
  getclustertable <- reactive({
    
    temp <- as.matrix(drug_dtm)
    temp = proxy::dist(temp, method = "cosine") 
    clustering.kmeans <- kmeans(temp, input$k) 
  })
  
  getclusters_hac <- reactive({
    
    drug_dtm_matrix <- as.matrix(drug_dtm)
    dist.matrix = proxy::dist(drug_dtm_matrix, method = "cosine") 
    clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
    clusters_hac<-as.data.frame(cutree(clustering.hierarchical, k = input$k))
    colnames(clusters_hac)<-c("cluster")
    clusters_hac <- cbind(rownames(clusters_hac), clusters_hac)
    
  })
  
  gettextdf <- eventReactive(input$go, {
    x <- input$text
    t <- as.data.frame(x)
    colnames(t)<-c("review")
    t$review <- as.character(t$review)
    t2 <- t %>% unnest_tokens(word, review) %>% filter(!str_detect(word, "^[0-9]*$")) %>% anti_join(stop_words) %>% mutate(word = SnowballC::wordStem(word))
  })
  
  output$plotbydrug <- renderPlot({
    p <- drug_wordplot()
    wordcloud_rep(words = p$word, freq = p$Freq , min.freq = 3000,max.words=100,scale=c(3.5,0.25), rot.per=0.35,colors=brewer.pal(8, "Dark2"))
  })
  
  output$wordhist <- renderPlot({
    p <- drug_histplot()
    p<-p[with(p, order(-Freq)),]
    p <- head(p, 9)
    #ggplot(p, aes(word)) +
    #  geom_bar(fill = "#0073C2FF")
    ggplot(p, aes(x=condition, y=Freq)) + geom_point() + theme(axis.text.x = element_text(angle = 25, hjust = 1))+xlab("Conditions")+ylab("Count")
  })
  
  output$rating <- renderPlot({
    p <- getdata()
    #p<-p[with(p, order(-Freq)),]
    ggplot(p, aes(x=rating)) + geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666")+xlab("Rating")+ylab("Density")  # Overlay with transparent density plot
  })

  output$avgdrugrating <- renderPlot({
    p <- getdata()
    temp <-aggregate(p[, 4], list(p$drugName), mean)
    temp$rating <- round(temp$rating, digits = 2)
    ggplot(data=temp, aes(x=Group.1, y=rating)) +
      geom_bar(stat="identity", fill="steelblue")+xlab("Drugs")+ylab("Average Rating")+
      geom_text(aes(label=rating), vjust=1.6, color="white", size=3.5)+
      theme_minimal()
  })
  
  output$countvrating <- renderPlot({
    p <- getdata()
    #temp <-aggregate(p[, 4], list(p$rating), sum)
    ggplot(p, aes(x=rating, y=usefulCount)) + geom_point() + theme(axis.text.x = element_text(angle = 25, hjust = 1))+xlab("Rating")+ylab("Useful Reviews")
    })
  
  output$cluster <- renderPlot({
    master.cluster <- getmastercluster()
    points <-getpoints()
    #palette <- colorspace::diverge_hcl(input$k)
    previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4))
    plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
         mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
    })
    
    output$clustertable <- renderDataTable({
      clusters <- getclustertable()
      clusters<-as.data.frame(clusters$cluster)
      })
    
    output$hcluster <- renderPlot({
      points <-getpoints()
      slave.hierarchical <- getslave.hierarchical()
      plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
           mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
           xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
          })
    
    output$hclustertable <- renderDataTable({
      
      clusters_hac<-getclusters_hac()
      
    })
    
    output$knn <- renderText({
      userip <- gettextdf()
      userip <- as.character(unique(predict(model_knn2, newdata = userip, na.action=na.pass)))
      
    })
    output$nb <- renderText({
      userip <- gettextdf()
      userip <- as.character(unique(predict(model_nb, newdata = userip, na.action=na.pass)))
      
    })
    output$tbag <- renderText({
      userip <- gettextdf()
      userip <- as.character(unique(predict(model_bag, newdata = userip, na.action=na.pass)))
      
    })
    
})

