library(readxl)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)
library(shiny)

setwd("C:/danışmanlık/metro istanbul/kesit hacimleri/kesit_sonuçlar/14.02.2019/3-son veri")
data<-read_excel("Kesit_20180409_V3.xlsx", col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess","numeric"))
data1<-data%>%
    mutate(
        LineName=as.factor(LineName),
        TimeTableTime=parse_hms(substr(TimeTableTime, 1, 8)),
        Direction=ifelse(is.na(FirstDirectionCount),1,2),
        Count=ifelse(is.na(FirstDirectionCount),SecondDirectionCount,FirstDirectionCount)
    )%>%
    select(LineName,LineId,FromStationOrder,ToStationOrder ,TimeTableTime,TimeTableOrderNo,Direction,Count)%>%
    arrange(LineName,Direction,TimeTableTime,FromStationOrder)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Passenger Volumes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("direction", label = h3("Select Direction:"),
                         choices = list("Direction 1" = 1,"Direction 2" = 2), 
                         selected = 2),
            
            selectInput("line", label = h3("Select Line"), 
                        choices = list("F1"=138,"M1A"=129,"M1B"=130,"M2"=149,"M2B"=150,"M3"=164,"M3B"=165,"M4"=163,"M5"=171,"M6"=169,"MRY"=168,"T1"=132,"T4"=137
), 
                        selected = 163),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # input$bins 

        data2<-data1%>%
            filter(LineId==as.numeric(input$line) &
                       Direction==as.numeric(input$direction) &
                       #TimeTableTime==as_hms("08:33:32")&
                       TimeTableOrderNo==20
                       )
        ggplot(data2,aes(x=FromStationOrder,y=Count))+geom_line()
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
