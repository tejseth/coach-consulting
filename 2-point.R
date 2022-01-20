pbp_2pt <- pbp %>%
  filter(two_point_attempt == 1) %>%
  mutate(success = ifelse(two_point_conv_result == "success", 1, 0))

team_2pt <- pbp_2pt %>%
  group_by(posteam) %>%
  summarize(plays_2pt = n(),
            success_rate = mean(success)) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

team_2pt %>%
  ggplot(aes(x = plays_2pt, y = success_rate)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_minimal() +
  labs(x = "Amount of 2pt. Conversion Attempts",
       y = "2pt. Conversion Success Rate",
       title = "Each NFL Team's 2 Point Conversion Breakdown Since 2015") +
  theme(plot.title = element_text(size = 20, face = "bold"))
