games <- read_csv(url("https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"))

games_select <- games %>%
  mutate(home_team_won = ifelse(result > 0, 1, 0),
         winner = ifelse(home_team_won == 1, home_team, away_team)) %>%
  select(game_id, winner)

timeouts <- pbp %>%
  filter(qtr == 4, half_seconds_remaining == 900) %>%
  select(game_id, home_team, away_team, home_timeouts_remaining, 
         away_timeouts_remaining, home_coach, away_coach) %>%
  left_join(games_select, by = "game_id")

home_timeouts <- timeouts %>%
  select(game_id, team = home_team, timeouts_rem = home_timeouts_remaining, winner, coach = home_coach)
away_timeouts <- timeouts %>%
  select(game_id, team = away_team, timeouts_rem = away_timeouts_remaining, winner, coach = away_coach)

timeouts_final <- rbind(home_timeouts, away_timeouts)

timeouts_final <- timeouts_final %>%
  mutate(did_win = ifelse(team == winner, 1, 0))

timeouts_final %>%
  group_by(timeouts_rem) %>%
  summarize(instances = n(),
            win_perc = 100*round(mean(did_win), 3)) %>%
  gt() %>%
  cols_label(timeouts_rem = "Timeouts Remaining",
             instances = "Count",
             win_perc = "Winning %") %>%
  cols_align(align = "center") %>%
  data_color(
    columns = vars(win_perc),
    colors = scales::col_numeric(
      palette = c(
        "white", "orange", "red"),
      domain = NULL)
  ) %>%
  tab_header(
    title = "Timeouts Remaining and Win Percentage Since 2015"
  ) %>% 
  tab_style(
    style = cell_text(color = "black", weight = "bold", align = "left"),
    locations = cells_title("title")
  )

avg_coach_timeouts <- timeouts_final %>%
  group_by(coach) %>%
  summarize(games = n(),
            avg_timeouts_rem = mean(timeouts_rem)) %>%
  filter(games >= 32) %>%
  ungroup() %>%
  arrange(-avg_timeouts_rem) %>%
  mutate(rank = row_number())

avg_coach_timeouts %>%
  select(rank, coach, avg_timeouts_rem) %>%
  gt()

avg_coach_timeouts %>%
  ggplot(aes(x = avg_timeouts_rem, y = fct_reorder(coach, avg_timeouts_rem))) +
  geom_bar(aes(fill = avg_timeouts_rem), stat = "identity") +
  scale_fill_viridis_c() +
  theme_minimal()



