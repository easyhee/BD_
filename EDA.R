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

ui <- dashboardPage(
  dashboardHeader(
    title = "여성 가구 범죄율 파악"
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
             fluidPage(
               titlePanel("Crime Analysis for Single-Person Female Households"),
               mainPanel(
                 plotOutput("crimePlot"),
                 dataTableOutput("crimeTable")
               )
             )
     ),
     tabItem(tabName = 'gu'
     ),
     tabItem(tabName = 'time'
     )
    )
   )
  )

server <- function(input, output){
  output$crimePlot <- renderPlot({
    # 데이터를 기반으로 플롯을 생성하는 코드를 추가하세요 (여기서는 예시로 임의의 데이터 사용)
    ggplot(data = data.frame(x = 2015:2022, y = c(3, 1, 4, 1, 5, 3, 2, 3)), aes(x = x, y = y )) +
      geom_bar(stat = "identity") +
      labs(title = "서울시 1인 여성 가구수 파악")
  })
  
  output$crimeTable <- renderDataTable({
    # 데이터를 기반으로 데이터 테이블을 생성하는 코드를 추가하세요 (여기서는 예시로 임의의 데이터 사용)
    datatable(data.frame(ID = 1:5, Value = c(3, 1, 4, 1, 5)), options = list(pageLength = 10))
  })
}


shinyApp(ui, server)
