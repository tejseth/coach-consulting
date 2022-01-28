library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ranger)
library(vip)
library(shinythemes)

final_grid <- read_csv(url("https://raw.githubusercontent.com/tejseth/coach-consulting/master/final_grid.csv"))

options(shiny.usecairo=T)

game_seconds_params <- c(3600, 2700, 1800, 1350, 900, 720, 420, 240, 120, 60, 30)

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  titlePanel("Coaching Consulting"),
  
  mainPanel(
    navbarPage("By Michael Venit",
      tabPanel("4th Down Decisions",
        fluidRow(
          column(4, align = "center",
                sliderInput("yards_to_goal_select", "Yards to Goal", value = 10, min = 5, max = 65, sep = "", step = 5),
            ),
          column(7, align = "center",
                sliderInput("distance_select", "Yards to Sticks", value = 2, min = 1, max = 6),
           ),
          column(4, align = "center",
                 selectInput("game_secs_select",
                             "Game Seconds Remaining",
                             c(sort(unique(game_seconds_params))), selected = 420)
          ),
          column(7, align = "center",
                 sliderInput("score_diff_select", "Score Diff.", value = 0, min = -14, max = 11),
          ),
          mainPanel(
            plotOutput(outputId = "decision_table",
                       width = "100%",
                       height = "50%"),
            br(),
            tableOutput("decision_table"),
            br()
          ),
        ),
      )
    )
  )
)

server <- function(input, output) { 
  
  output$decision_table({
    
    row <- final_grid %>% 
      filter(yardline_100 == input$yards_to_goal_select) %>%
      filter(ydstogo == input$distance_select) %>%
      filter(game_seconds_remaining = input$game_seconds_remaining) %>%
      filter(score_differential == input$score_differential)
    
    row %>% gt()
    
  }, width = 850)

  
}

shinyApp(ui = ui, server = server)

