library(shiny)
library(shinydashboard)
htg <- 350
wd <- 480

ui <- dashboardPage(
  dashboardHeader(title = tags$img(src='logo.svg', height = '60', width ='100')
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sex", tabName = "sex", icon = icon("person-half-dress")),
      menuItem("Employment Time", tabName = "employment", icon = icon("clipboard")),
      menuItem("Age", tabName = "age", icon = icon("person-cane"))
    )
  ),
  dashboardBody(
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "age",
                h2("Age"),
                fluidRow(
                  box(plotOutput("ageall", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("ageover", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("ageunder", height = htg, width = wd))
                )
        ),
        
        # Second tab content
        tabItem(tabName = "employment",
                h2("Time Employed"),
                fluidRow(
                  box(plotOutput("empall", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("empover", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("empunder", height = htg, width = wd))
                )
        ),
        tabItem(tabName = "sex",
                h2("Sex"),
                fluidRow(
                  box(plotOutput("sexall", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("female", height = htg, width = wd))
                ),
                fluidRow(
                  box(plotOutput("male", height = htg, width = wd))
                )
                
        )
      )
    )
  )
)

server <- function(input, output) { 
source("main.R")
hgt <- 250
teams_selection <- names(x_survey)[22:24]
all_teams <- list()
team_select <- list()
for(team in teams_selection) {
  all_teams <- append(all_teams,c("all",c(unique(x_survey[[team]]))))
  team_select <- append(team_select,team)
}

the_title <- "RDS Survey"

output$empall <- renderPlot({liekart_calc(x_survey, team_select[[1]],all_teams[[1]], c(sections,start,finish,Ext))})
output$empover <- renderPlot({liekart_calc(x_survey, team_select[[1]],all_teams[[2]], c(sections,start,finish,Ext))})
output$empunder <- renderPlot({liekart_calc(x_survey, team_select[[1]],all_teams[[3]], c(sections,start,finish,Ext))})

output$sexall <- renderPlot({liekart_calc(x_survey, team_select[[2]],all_teams[[4]], c(sections,start,finish,Ext))})
output$female <- renderPlot({liekart_calc(x_survey, team_select[[2]],all_teams[[5]], c(sections,start,finish,Ext))})
output$male <- renderPlot({liekart_calc(x_survey, team_select[[2]],all_teams[[6]], c(sections,start,finish,Ext))})

#liekart_calc(x_survey, team_select,all_teams[[8]], the_title,c(sections,start,finish,Ext))

output$ageall <- renderPlot({liekart_calc(x_survey, team_select[[3]],all_teams[[7]], c(sections,start,finish,Ext))})
output$ageover <- renderPlot({liekart_calc(x_survey, team_select[[3]],all_teams[[8]], c(sections,start,finish,Ext))})
output$ageunder <- renderPlot({liekart_calc(x_survey, team_select[[3]],all_teams[[9]], c(sections,start,finish,Ext))})
}
shinyApp(ui, server)
