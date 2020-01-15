
library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
library(markdown)
library(shinythemes)

#UI
shinyUI(fluidPage(
  theme=shinytheme("flatly"),
  navbarPage("Validation tool",
             tabPanel("Mortality visualization",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Choose desired variables to filter health data regarding mortality in the Canadian population. 
                                   This visually compares control data with three different microsimulation models. 
                                   Data selected can be viewed as a mortality count or rate. "),
                          sliderInput("yearInput",
                                      "Year",
                                      min=2000,
                                      max=2017,
                                      value=c(2000,2018),
                                      sep=""),
                          selectInput("ageInput",
                                      "Age",
                                      selected="All",
                                      choices=c("All","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44",
                                                "45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74",
                                                "75 to 79","80 to 84","85 to 89"),
                                      multiple=FALSE),
                          radioButtons("sexInput",
                                       "Sex",
                                       choices=c("B","F","M")),
                          checkboxGroupInput("sourceInput",
                                             "Source",
                                             choices=c("Control","Model1","Model2","Model3"),
                                             selected="Control"),
                          selectInput("regionInput",
                                      "Region",
                                      choices=c("AB","BC","CANADA","MB","NB","NL",
                                                "NS","ON","P.E.I.","QC","SK"),
                                      selected="CANADA")
                          
                          ),
                        mainPanel(
                          tabsetPanel(type="tabs",
                                      tabPanel("Number of deaths",plotOutput("plotNumber")),
                                      tabPanel("Rate of mortality ",plotOutput("plotRate"))),
                          br(),
                          br(),
                          dataTableOutput("Health_model")
                        )
                     )
  )
  )
)
)

