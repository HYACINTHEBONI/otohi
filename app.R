# Packages ----
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("bs4Dash")
install.packages("DT")
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("sentimentr")
install.packages("tm")
install.packages("ggplot2")
install.packages("plotly")
install.packages("echarts4r")
install.packages("reshape2")
install.packages("rtweet")
install.packages("wordcloud")

# Libraries ----
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(DT)
library(fresh)
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(dplyr)
library(sentimentr)
library(tm)
library(ggplot2)
library(plotly)
library(echarts4r)
library(reshape2)
library(rtweet)
library(wordcloud)

# Load data ----
countries <- read_csv("countries_07_03_23.csv") # Database of countries tweets for reputation analysis (50 countries)

# UI & Server ----

ui <- dashboardPage(
  
  dashboardHeader(title = "POLL"),
  dashboardSidebar(
    
    sidebarMenu(menuItem("Twitter", tabName = "twitter", selected = TRUE, icon = shiny::icon("twitter")),
                menuItem("News", tabName = "news", icon = shiny::icon("rss")))),
  dashboardBody(
    # style = "background-color: white;",
    tabItems(
      tabItem(tabName = "twitter",
              fluidRow(column(width = 2, selectizeInput("region_1", label = NULL, choices = unique(countries$region))),
                       column(width = 2, selectizeInput("country_1", label = NULL, choices = unique(countries$country)))),
              fluidRow(bs4InfoBoxOutput(outputId = "tweets", width = 4),
                       bs4InfoBoxOutput(outputId = "retweets", width = 4),
                       bs4InfoBoxOutput(outputId = "likes", width = 4)),
              fluidRow(box(echarts4rOutput(outputId = "timeline"), width = 12, collapsible = F))),

      tabItem(tabName = "news")
    )
  )
)

server <- function(input, output) {
  
  set.seed(122)
  # Output infobox "tweets" ----
  output$tweets <- renderbs4InfoBox({
    req(input$region_1)
    req(input$country_1)
    
    tw <- countries %>% 
      # filter(region %in% input$region_1, date == input$date_1, country %in% input$country_1) %>%
      filter(region %in% input$region_1, country %in% input$country_1) %>%
      summarise(nb_tweets=n()) %>%
      select(nb_tweets) %>%
      as.numeric()
    
    bs4InfoBox(
      title = "Number of tweets",
      value = tw,
      subtitle = NULL,
      icon = shiny::icon("telegram"),
      color = "lightblue",
      width = 4,
      href = NULL,
      fill = FALSE,
      gradient = FALSE,
      elevation = NULL,
      iconElevation = NULL,
      tabName = NULL
    )
  })
  
  # Output infobox "retweets" ----
  output$retweets <- renderbs4InfoBox({
    req(input$region_1)
    req(input$country_1)
    
    rtw <- countries %>% 
      # filter(region %in% input$region_1, date == input$date_1, country %in% input$country_1) %>%
      filter(region %in% input$region_1, country %in% input$country_1) %>%
      summarise(nb_retweets = sum(retweet_count)) %>%
      select(nb_retweets) %>%
      as.numeric()
    
    bs4InfoBox(
      title = "Number of retweets",
      value = rtw,
      subtitle = NULL,
      icon = shiny::icon("retweet"),
      color = "gray",
      width = 4,
      href = NULL,
      fill = FALSE,
      gradient = FALSE,
      elevation = NULL,
      iconElevation = NULL,
      tabName = NULL
    )
  })
  
  # output infobox "likes" ----
  output$likes <- renderbs4InfoBox({
    req(input$region_1)
    req(input$country_1)
    
    lk <- countries %>% 
      # filter(region %in% input$region_1, date == input$date_1, country %in% input$country_1) %>%
      filter(region %in% input$region_1, country %in% input$country_1) %>%
      summarise(nb_likes = sum(favorite_count)) %>%
      select(nb_likes) %>%
      as.numeric()
    
    bs4InfoBox(
      title = "Number of likes",
      value = lk,
      subtitle = NULL,
      icon = shiny::icon("heart"),
      color = "purple",
      width = 4,
      href = NULL,
      fill = FALSE,
      gradient = FALSE,
      elevation = NULL,
      iconElevation = NULL,
      tabName = NULL
    )
  })
  
  # Countries timeline *  ----
  output$timeline <- renderEcharts4r({
    req(input$region_1)
    req(input$country_1)

    a <- countries %>%
      filter(region %in% input$region_1, country %in% input$country_1s) %>%
      count(date)
    
    ts_base <- a %>% 
      e_charts(x = date) %>% 
      e_datazoom(
        type = "slider", 
        toolbox = FALSE,
        bottom = -5
      ) %>% 
      e_tooltip() %>% 
      e_title("Frequence de tweets par jour") %>% 
      e_x_axis(date, axisPointer = list(show = TRUE))
    
    # ts_base %>% e_line(n)
    
    ts_base %>% e_area(n, stack = "grp")
    
  })
  
}


shinyApp(ui, server)
