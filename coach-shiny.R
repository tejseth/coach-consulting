library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ranger)
library(vip)

final_grid <- read_csv(url("https://raw.githubusercontent.com/tejseth/coach-consulting/master/final_grid.csv"))

options(shiny.usecairo=T)

game_seconds_params <- c(3600, 2700, 1800, 1350, 900, 720, 420, 240, 120, 60, 30)

ui <- fluidPage(
  
  titlePanel("Coaching Consulting"),
  
  mainPanel(
    navbarPage("By Michael V",
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
        )
      )
    )
  )
)

server <- function(input, output) { 
  
}

shinyApp(ui = ui, server = server)

