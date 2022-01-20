library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

pbp <- load_pbp(2015:2021)

pbp_4th <- pbp %>%
  filter(down == 4)

pbp_wentforit <- pbp_4th %>%
  filter(pass == 1 | rush == 1) %>%
  mutate(converted = ifelse(yards_gained >= ydstogo, 1, 0)) %>%
  filter(!is.na(converted)) %>%
  mutate(yardline_bucket = case_when(
    between(yardline_100, 91, 100) ~ "91 to 100 Yds to go",
    between(yardline_100, 81, 90) ~ "81 to 90 Yds to go",
    between(yardline_100, 71, 80) ~ "71 to 80 Yds to go",
    between(yardline_100, 61, 70) ~ "61 to 70 Yds to go",
    between(yardline_100, 51, 60) ~ "51 to 60 Yds to go",
    between(yardline_100, 41, 50) ~ "41 to 50 Yds to go",
    between(yardline_100, 31, 40) ~ "31 to 40 Yds to go",
    between(yardline_100, 21, 30) ~ "21 to 30 Yds to go",
    between(yardline_100, 11, 20) ~ "11 to 20 Yds to go",
    between(yardline_100, 1, 10) ~ "1 to 10 Yds to go",
  ))

convert_rate <- pbp_wentforit %>%
  group_by(yardline_bucket, ydstogo) %>%
  summarize(plays = n(),
            convert_rate = mean(converted)) %>%
  filter(plays >= 25, ydstogo <= 7)

convert_rate %>%
  mutate(`Yards to Go` = as.factor(ydstogo)) %>%
  ggplot(aes(x = yardline_bucket, y = convert_rate, group = `Yards to Go`)) +
  geom_smooth(aes(color = `Yards to Go`), se = FALSE, size = 1.5) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Yardline Bucket",
       y = "Conversion Rate",
       title = "4th Down Conversion Rates by Yards to Go and Yardline Bucket") +
  theme(axis.text.x = element_text(size = 12, angle = -25),
        plot.title = element_text(size = 20, face = "bold"))


  
