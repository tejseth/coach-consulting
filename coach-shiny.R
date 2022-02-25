library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ranger)
library(vip)
library(shinythemes)
library(gtExtras)

# Have explanations for each column

final_grid <- read_csv(url("https://raw.githubusercontent.com/tejseth/coach-consulting/master/final_grid.csv"))
two_point_grid <- read_csv(url("https://raw.githubusercontent.com/tejseth/coach-consulting/master/two_point_grid.csv"))

options(shiny.usecairo=T)

game_seconds_params <- c(3600, 2700, 1800, 1350, 900, 720, 420, 240, 120, 60, 30)
game_secs_params <- c(420, 240, 120, 60, 30)

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  titlePanel("playcalleR"),
  
  mainPanel(
    navbarPage("By Michael Venit",
      tabPanel("4th Down Decisions",
        fluidRow(
          column(4, align = "center",
                sliderInput("yards_to_goal_select", "Yards to Goal", value = 35, min = 5, max = 65, sep = "", step = 5),
            ),
          column(7, align = "center",
                sliderInput("distance_select", "Yards to Sticks", value = 2, min = 1, max = 6),
           ),
          column(4, align = "center",
                 selectInput("game_secs_select",
                             "Game Seconds Remaining",
                             c(sort(unique(game_seconds_params))), selected = 60)
          ),
          column(7, align = "center",
                 sliderInput("score_diff_select", "Score Diff.", value = -4, min = -14, max = 11),
          ),
          mainPanel(
            tableOutput(outputId = "decision_table"),
          ),
          br(),
          br(),
          br(),
          br(),
          column(10, align = "left",
          textOutput("wpa_success"),
          textOutput("wpa_fail"),
          textOutput("conv_rate"),
          textOutput("exp_wpa"),),
        ),
      ),
      tabPanel("2 Point Conversion",
        fluidRow(
          column(4, align = "center",
                 sliderInput("off_epa_select", "Offensive EPA/Play", value = 0, min = -0.7, max = 0.7, sep = "", step = 0.1),
          ),
          column(7, align = "center",
                 sliderInput("def_epa_select", "Defensive EPA/Play", value = 0, min = -0.7, max = 0.7, sep = "", step = 0.1),
          ),
          column(4, align = "center",
                 sliderInput("xp_make_rate_select", "Kicker XP Make Rate", value = 0.89, min = 0.8, max = 1.0, sep = "", step = 0.03),
          ),
          column(7, align = "center",
                 selectInput("game_seconds_select",
                             "Game Seconds Remaining",
                             c(sort(unique(game_secs_params))), selected = 60),
          ),
          column(11, align = "center",
                 sliderInput("diff_score_select", "Score Diff.", value = -2, min = -10, max = 10, sep = "", step = 1),
          ),
          mainPanel(
            tableOutput(outputId = "two_point_table"),
          ),
          br(),
          br(),
          br(),
          br(),
          # column(10, align = "left",
          #        textOutput("wpa_success"),
          #        textOutput("wpa_fail"),
          #        textOutput("conv_rate"),
          #        textOutput("exp_wpa"),),
        )
      )
    )
  )
)

server <- function(input, output) { 
  
  output$wpa_success <- renderText("WPA if Success: The estimated win probability added if the play is successful")
  output$wpa_fail <- renderText("WPA if Fail: The estimated win probability added if the play fails")
  output$conv_rate <- renderText("Exp. Conversion Rate: The expected conversion rate for that decision")
  output$exp_wpa <- renderText("Expected WPA: The aggregate of success and failure - the projected WPA if that decision is chosen")
  
  output$decision_table <- render_gt({
    
    row <- final_grid %>% 
      filter(yardline_100 == input$yards_to_goal_select) %>%
      filter(ydstogo == input$distance_select) %>%
      filter(game_seconds_remaining == input$game_secs_select) %>%
      filter(score_differential == input$score_diff_select) %>%
      mutate(wp5 = ifelse(input$yards_to_goal_select < 45, NA, wp5)) %>%
      mutate(rec = case_when(
        a < -0.02 ~ "Kick",
        between(a, -0.02, 0.02) ~ "Toss-Up",
        a > 0.02 ~ "Go For It",
        TRUE ~ "Toss-Up"
      ),
      rec = ifelse((rec == "Kick" & k_sup != k), "Punt", rec))
      # rec = case_when(
      #   a >= 0 ~ paste0(rec, " (+", 100*round(a, 3), ")")))
    
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
    
    df2$exp_wpa[3] = round(100*row$wp5, 1)
    
    df2 %>% 
      distinct() %>% 
      gt() %>% 
      tab_header(title = md(row$rec),
                 subtitle = md(string)) %>%
      gtExtras::gt_theme_espn() %>%
      cols_label(Success = "WPA if Success",
                 Failure = "WPA if Failure",
                 conv_rate = "Exp. Conversion Rate",
                 exp_wpa = "Expected WPA") %>%
      cols_align(align = "center") %>%
      opt_align_table_header(align = "center") %>%
      tab_style(
        style = cell_text(color = "red", weight = "bold"),
        locations = cells_body(
          columns = vars(exp_wpa),
          rows = exp_wpa < 0
        )
      ) %>% 
      tab_style(
        style = cell_text(color = "darkgreen", weight = "bold"),
        locations = cells_body(
          columns = vars(exp_wpa),
          rows = exp_wpa >= 0
        )
      ) %>%
      tab_style(
        style = cell_text(color = "red", weight = "bold", align = "center"),
        locations = cells_title("title")
      )
    
    
  }, width = 950)
  
  output$two_point_table <- render_gt({
    
    two_row <- two_point_grid %>%
      filter(off_epa_in_5 == input$off_epa_select,
             def_epa_in_5 == input$def_epa_select,
             xp_make_rate == input$xp_make_rate_select,
             game_seconds_remaining == input$game_seconds_select,
             score_differential == input$diff_score_select) %>%
      mutate(rec = case_when(
        a < 0.03 ~ "Kick XP",
        between(a, 0.03, 0.08) ~ "Toss-Up",
        a > 0.08 ~ "Go For 2",
        TRUE ~ "Toss-Up"
      ))
    
    two_up_or_down = ifelse(input$diff_score_select >= 0, "Up ", "Down ")
    two_string = paste0(two_up_or_down, input$diff_score_select, ", ", 
                    input$game_seconds_select, " seconds left")
    
    two_df <- data.frame(Label  = c("Go For 2", "Kick XP"),
                     Success = c(two_row$wp1, two_row$wp3),
                     Failure = c(two_row$wp2, two_row$wp4),
                     conv_rate = c(two_row$c1, two_row$c2))
    
    two_df <- two_df %>% mutate(exp_wpa = (Success*conv_rate + Failure*(1-conv_rate))) 
    
    two_df2 <- mutate_if(two_df, is.numeric, ~ . * 100) %>% 
      mutate_if(is.numeric, round, digits = 1)
    
    two_df2 %>% 
      distinct() %>% 
      gt() %>% 
      tab_header(title = md(two_row$rec),
                 subtitle = md(two_string)) %>%
      gtExtras::gt_theme_espn() %>%
      cols_label(Success = "WPA if Success",
                 Failure = "WPA if Failure",
                 conv_rate = "Exp. Conversion Rate",
                 exp_wpa = "Expected WPA") %>%
      cols_align(align = "center") %>%
      opt_align_table_header(align = "center") %>%
      tab_style(
        style = cell_text(color = "red", weight = "bold"),
        locations = cells_body(
          columns = vars(exp_wpa),
          rows = exp_wpa < 0
        )
      ) %>% 
      tab_style(
        style = cell_text(color = "darkgreen", weight = "bold"),
        locations = cells_body(
          columns = vars(exp_wpa),
          rows = exp_wpa >= 0
        )
      ) %>%
      tab_style(
        style = cell_text(color = "red", weight = "bold", align = "center"),
        locations = cells_title("title")
      )
    
  }, width = 950)

  
}

shinyApp(ui = ui, server = server)

