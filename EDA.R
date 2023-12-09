#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinythemes")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("treemapify")
#install.packages("readr")
#install.packages("RColorBrewer")
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(treemapify)
library(readr)
library(RColorBrewer)

data_path <- "C:/bdata/raw/preprocessed_data.csv"
your_data <- read.csv(data_path)

ui <- dashboardPage(
  dashboardHeader(
    title = "1인여성가구 범죄율 파악"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Home', tabName = 'home', icon = icon('home')),
      menuItem('여성가구수', tabName = 'female', icon = icon('female')),
      menuItem('자치구별', tabName = 'gu', icon = icon('globe')),
      menuItem('연도별', tabName = 'time', icon = icon('clock'))
    )
  ),
  dashboardBody(
    #홈 화면
    tabItems(
      
      tabItem(
        tabName = 'home',
        fluidPage(
          titlePanel(" 서울시 1인 여성가구 범죄율 파악"),
          # 내용을 표시하는 주 패널
          mainPanel(
            # 첫 번째 박스 - 프로젝트 설명
            box(
              title = "Description",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              HTML("<br><p>'서울시 1인 여성가구 범죄율 파악' 프로젝트는 성별, 지역별에 따른 범죄율을 비교하고 범죄 발생 다발 지역을 분석해 범죄 다발 지역 예방책을 세우는 것을 목표로 합니다.</p>")
            ),
            
            # 두 번째 박스 - 분석 가치
            box(
              title = "분석 가치",
              status = "success",
              solidHeader = TRUE,
              width = 6,
              HTML("<br>
             <p>1. 1인 가구 여성들에게 유용한 정보를 제공하여 안전한 생활을 도모합니다.</p>
             <p>2. 범죄 데이터를 기반으로 정책을 개발하고 범죄 예방이 가능한 방안을 모색합니다.</p>
             <p>3. 사회 범죄 분야에 적용하여 지역사회의 안전을 향상시킵니다.</p>")
            )
          )
        )
      ),
     tabItem(tabName = 'female',
             
     ),
     tabItem(tabName = 'gu',
             
     ),
     tabItem(
       tabName = 'time',
       fluidPage(
         titlePanel("연도별"),
         tabsetPanel(
           type = "tabs",
           id = "timeTabs",
           tabPanel(
             "1인 여성 가구수",
             box(
               title = "1인 여성 가구수 통계",
               status = "info",
               solidHeader = TRUE,
               width = 1000,
               selectInput("selected_year", "연도를 선택해주세요", choices = unique(your_data$연도)),
               plotOutput("femaleYearPlot")
             )
           ),
           tabPanel(
             "범죄 발생 건수",
             box(
               title = "범죄 건수",
               status = "info",
               solidHeader = TRUE,
               width = 500,
               selectInput("selected_year", "연도를 선택해주세요", choices = unique(your_data$연도)),
               plotOutput("crimeCountByYearPlot")
             )
           ),
           tabPanel(
             "범죄율",
             box(
               title = "연도별 5대 범죄율",
               status = "info",
               solidHeader = TRUE,
               width = 500,
               selectInput("selected_year", "연도를 선택해주세요", choices = unique(your_data$연도)),
               plotOutput("crimeRateByYearPlot")
             )
           )
         )
       )
     )
    )
   )
  )

server <- function(input, output){
 #'time'탭--------------------------------------------
    #1인여성가구수 그래프
    output$femaleYearPlot <- renderPlot({
    # 선택한 연도에 해당하는 데이터 필터링
    selected_year_data <- subset(your_data, 연도 == input$selected_year)
    
    # NA 값이 있는 경우 0으로 대체
    selected_year_data$여자[is.na(selected_year_data$여자)] <- 0
    
    # 막대 그래프 생성
    ggplot(selected_year_data, aes(x = 자치구명, y = 여자, fill = 자치구명)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste(input$selected_year, "년도 1인 여성 가구수"),
           x = "자치구",
           y = "1인 여성 가구수",
           fill = "자치구") +
      theme(legend.position = "none")
  })
    # 연도별 범죄 건수 그래프
    output$crimeCountByYearPlot <- renderPlot({
      # 선택한 연도에 해당하는 데이터 필터링
      selected_year_data <- subset(your_data, 연도 == input$selected_year)
      
      # 연도별 5대 범죄 건수 계산
      crime_counts <- selected_year_data %>%
        summarise(
          murder_count = sum(살인),
          robbery_count = sum(강도),
          rape_count = sum(강간),
          theft_count = sum(절도),
          violence_count = sum(폭력)
        )
      
      # 연도별 범죄 건수를 데이터프레임으로 변환
      crime_counts_df <- data.frame(
        범죄유형 = c("살인", "강도", "강간", "절도", "폭력"),
        건수 = c(
          crime_counts$murder_count,
          crime_counts$robbery_count,
          crime_counts$rape_count,
          crime_counts$theft_count,
          crime_counts$violence_count
        )
      )
      
      # 막대 그래프 생성
      ggplot(crime_counts_df, aes(x = 범죄유형, y = 건수, fill = 범죄유형)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste(input$selected_year, "년도 5대 범죄 발생 건수"),
             x = "범죄 유형",
             y = "범죄 발생 건수",
             fill = "범죄 유형") +
        theme(legend.position = "none")
    })
    #범죄율 선 그래프
    output$crimeRateByYearPlot <- renderPlot({
      # 선택한 연도에 해당하는 데이터 필터링
      selected_year_data <- subset(your_data, 연도 == input$selected_year)
      # NA 값이 있는 경우 0으로 대체
      selected_year_data$살인[is.na(selected_year_data$살인)] <- 0
      selected_year_data$강도[is.na(selected_year_data$강도)] <- 0
      selected_year_data$강간[is.na(selected_year_data$강간)] <- 0
      selected_year_data$절도[is.na(selected_year_data$절도)] <- 0
      selected_year_data$폭력[is.na(selected_year_data$폭력)] <- 0
       # 연도별 5대 범죄율 계산
      crime_rates <- selected_year_data %>%
        summarise(
          murder_rate = sum(살인) / sum(여자) * 1000,  # 살인율
          robbery_rate = sum(강도) / sum(여자) * 1000,  # 강도율
          rape_rate = sum(강간) / sum(여자) * 1000,  # 강간율
          theft_rate = sum(절도) / sum(여자) * 1000,  # 절도율
          violence_rate = sum(폭력) / sum(여자) * 1000  # 폭력율
        )
      
      # 연도별 범죄율을 데이터프레임으로 변환
      crime_rates_df <- data.frame(
        범죄율 = c("살인율", "강도율", "강간율", "절도율", "폭력율"),
        비율 = c(
          crime_rates$murder_rate,
          crime_rates$robbery_rate,
          crime_rates$rape_rate,
          crime_rates$theft_rate,
          crime_rates$violence_rate
        )
      )
      
      # 선 그래프 생성
      ggplot(crime_rates_df, aes(x = 범죄율, y = 비율, group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        theme_minimal() +
        labs(title = paste(input$selected_year, "년도 5대 범죄율"),
             x = "범죄 유형",
             y = "범죄율") +
        theme(legend.position = "none")
    })  
}


shinyApp(ui, server)
