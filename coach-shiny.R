library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ranger)
library(vip)
library(shinythemes)
library(gtExtras)

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
            tableOutput(outputId = "decision_table"),
          ),
        ),
      )
    )
  )
)

server <- function(input, output) { 
  
  output$decision_table <- render_gt({
    
    row <- final_grid %>% 
      filter(yardline_100 == input$yards_to_goal_select) %>%
      filter(ydstogo == input$distance_select) %>%
      filter(game_seconds_remaining == input$game_secs_select) %>%
      filter(score_differential == input$score_diff_select)
    
    up_or_down = ifelse(input$score_diff_select >= 0, ", Up ", ", Down ")
    string = paste0("4th & ",  input$distance_select, ", ", input$yards_to_goal_select,
                    " yards to goal", up_or_down, input$score_diff_select, ", ", 
                    input$game_secs_select, " seconds left")
    
    # row %>% gt() %>% tab_header(string)
  
    
    df <- data.frame(Label  = c("Go For It", "Kick a FG", "Punt"),
                Success = c(row$wp1, row$wp3, NA),
                Failure = c(row$wp2, row$wp4, NA),
                conv_rate = c(row$c1, row$c2, NA))
      
    df <- df %>% mutate(exp_wpa = (Success*conv_rate + Failure*(1-conv_rate))) 
    
    df2 <- mutate_if(df, is.numeric, ~ . * 100) %>% 
    mutate_if(is.numeric, round, digits = 1)
    
    df2 %>% 
      distinct() %>% 
      gt() %>% 
      tab_header(string) %>%
      gtExtras::gt_theme_538() %>%
      cols_label(conv_rate = "Conversion Rate",
                 exp_wpa = "Expected WPA") %>%
      cols_align(align = "center")
      
    
  }, width = 850)

  
}

shinyApp(ui = ui, server = server)

