library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)

df <- read.csv("data/shiny_1213.csv", header = T, fileEncoding = "euc-kr")


ui <- dashboardPage(
  dashboardHeader(title = "대용량자료관리및시각화", titleWidth = 240),
  dashboardSidebar(width = 240,
                   sidebarMenu(
                     menuItem("어린이집별 통계량", tabName="viz1", icon=icon("home")),
                     menuItem("데이터 테이블", tabName="datatable", icon=icon("table"))
                   )),
  dashboardBody(
    # css style 적용
    includeCSS("www/style.css"),
    # tabItems
    tabItems(
      tabItem(tabName="viz1",
              valueBoxOutput("value1", width=6),
              valueBoxOutput("value_score", width=3),
              valueBoxOutput("value2", width=3),
              valueBoxOutput("value3", width=3),
              valueBoxOutput("value4", width=3),
              valueBoxOutput("value5", width=3),
              valueBoxOutput("value6", width=3),
              leafletOutput(outputId="mymap")),
      tabItem(tabName="datatable", DTOutput("table"))
    )
  )
)

server <- function(input, output) {
  output$mymap <- renderLeaflet({
    leaflet(df) %>%
      setView(lng=126.97, lat=37.56, zoom=12) %>%
      addTiles() %>%
      addCircles(data=df[df$기존365여부==1,], lat=~lat, lng=~lng,
                 radius=50, color="red", opacity=1, group="기존365 - O") %>%
      addCircles(data=df[df$기존365여부==0,], lat=~lat, lng=~lng,
                 radius=30, color="#427D9D", opacity=0.8, group="기존365 - X") %>%
      addLayersControl(overlayGroups=c("기존365 - O", "기존365 - X"),
                       options=layersControlOptions(collapsed=FALSE),
                       position="bottomleft")
  })
  
  coordinates <- reactiveValues(lat = 0, lng = 0)
  clicked <- reactiveValues(index = 0, val1="어린이집명", 
                            val2=0, val3=0, val4=0)
  dist = NULL
  
  observeEvent(input$mymap_click, {
    click <- input$mymap_click
    if (!is.null(click)) {
      coordinates$lat <- click$lat
      coordinates$lng <- click$lng
      # 클릭지점의 좌표를 coordinates 변수에 저장하고
      
      dist <- sqrt((df$lat-coordinates$lat)^2+(df$lng-coordinates$lng)^2)
      clicked$index <- which(min(dist) == dist)
      # 선택된 어린이집의 인덱스 번호
      
      clicked$val1 <- df[clicked$index, "어린이집명"]
      clicked$val_score <- df[clicked$index, "평가지표_scaled"]
      clicked$val2 <- df[clicked$index, "보육교직원수"]
      clicked$val3 <- df[clicked$index, "반수총계"]
      clicked$val4 <- df[clicked$index, "아동수총계"]
      clicked$val5 <- df[clicked$index, "CCTV총설치수"]
      clicked$val6 <- round(df[clicked$index, "지하철역최단거리"], 1)
    }
  })
  
  output$value1 <- renderValueBox({
    valueBox(
      value = clicked$val1,
      subtitle = "어린이집명",
      color = "olive",
      icon = icon("heart")
    )
  })
  
  output$value_score <- renderValueBox({
    valueBox(
      value = paste0(as.numeric(clicked$val_score), "점"),
      subtitle = "365 어린이집 지수",
      color = "orange",
      icon = tags$i(class="fa-solid fa-child-reaching", style="font-size= : 15px")
    )
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      value = paste0(clicked$val2, "명"),
      subtitle = "보육교직원 수",
      color = "green",
      icon = tags$i(class="fa-solid fa-face-smile", style="font-size= : 15px")
    )
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      value = paste0(clicked$val3, "개"),
      subtitle = "교실 수",
      color = "lime",
      icon = tags$i(class="fa-solid fa-school", style="font-size= : 15px")
    )
  })
    
  output$value4 <- renderValueBox({
    valueBox(
      value = paste0(clicked$val4, "명"),
      subtitle = "아동 수 총계",
      color = "teal",
      icon = tags$i(class="fa-solid fa-children", style="font-size= : 15px")
    )
  })
  
  output$value5 <- renderValueBox({
    valueBox(
      value = paste0(clicked$val5, "대"),
      subtitle = "CCTV총설치수",
      color = "maroon",
      icon = tags$i(class="fa-solid fa-camera", style="font-size= : 15px")
    )
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      value = paste0(clicked$val6, "m"),
      subtitle = "지하철역최단거리",
      color = "purple",
      icon = tags$i(class="fa-solid fa-subway", style="font-size= : 15px")
    )
  })
  output$table <- DT::renderDataTable(df,
                                      extensions = "FixedColumns",
                                      options = list(
                                        autoWidth = TRUE,
                                        scrollX = TRUE
                                      ))
}

runApp(shinyApp(ui=ui, server=server))