library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ranger)
library(vip)

pbp <- load_pbp(2015:2021)

pbp_4th <- pbp %>%
  filter(down == 4)

pbp_went <- pbp_4th %>%
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

######################## Making Conversion Model ###########################

pbp_4th <- pbp %>%
  filter(down == 4)

pbp_went <- pbp_4th %>%
  filter(pass == 1 | rush == 1) %>%
  mutate(converted = ifelse(yards_gained >= ydstogo, 1, 0)) %>%
  filter(!is.na(converted), !is.na(ydstogo), !is.na(yardline_100)) 

init_logit <- glm(converted ~ (ydstogo + yardline_100 + ydstogo:yardline_100),
                  data = pbp_went, family = "binomial")

summary(init_logit)

pred_outcome <- ifelse(init_logit$fitted.values > 0.5, "1", "0")

pbp_went <- pbp_went %>%
  mutate(pred = init_logit$fitted.values)

pbp_went_select <- pbp_went %>%
  select(down, ydstogo, yardline_100, pred, converted)

pbp_went_select %>%
  filter(between(ydstogo, -15, 15)) %>%
  ggplot(aes(x = ydstogo)) +
  geom_smooth(aes(y = pred), color = "darkorange") +
  geom_point(aes(y = converted), alpha = 0.4, color = "darkblue") +
  theme_minimal() +
  labs(y = "Conversion Rate",
       x = "Distance from Sticks") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

################## Making Field Goal Probability #######################

field_goals <- pbp %>% filter(field_goal_attempt == 1)

field_goals <- field_goals %>%
  mutate(fg_made = ifelse(field_goal_result == "made", 1, 0))

fg_per_game <- field_goals %>%
  select(game_id, kicker_player_id, kicker_player_name, game_seconds_remaining, fg_made)

fg_missed <- fg_per_game %>%
  filter(fg_made == 0) %>%
  select(game_id, kicker_player_id, kicker_player_name, time_missed = game_seconds_remaining) %>%
  group_by(game_id, kicker_player_id, kicker_player_name) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  distinct() %>%
  filter(count == 1)

fg_per_game <- fg_per_game %>%
  left_join(fg_missed, by = c("game_id", "kicker_player_id", "kicker_player_name"))

fg_per_game <- fg_per_game %>%
  mutate(prev_missed = ifelse(game_seconds_remaining < time_missed, 1, 0),
         prev_missed = ifelse(is.na(prev_missed), 0, prev_missed)) %>%
  select(game_id, kicker_player_id, game_seconds_remaining, prev_missed)

field_goals <- field_goals %>%
  left_join(fg_per_game, by = c("game_id", "kicker_player_id", "game_seconds_remaining"))

fg_logit <- glm(fg_made ~ yardline_100, 
                data = field_goals, family = "binomial")

summary(fg_logit)

field_goals %>%
  mutate(pred_prob = fg_logit$fitted.values) %>%
  ggplot(aes(x = yardline_100)) +
  geom_line(aes(y = pred_prob), color = "darkorange", size = 2) +
  geom_point(aes(y = fg_made), alpha = 0.4, color = "darkblue") +
  theme_minimal()

###################### wp1: if go and succeed ##########################

success_4th <- pbp %>%
  filter(down == 4) %>%
  filter(rush == 1 | pass == 1) %>%
  filter(yards_gained >= ydstogo) %>%
  filter(!is.na(wpa))

success_4th_rf <- ranger(wpa ~ game_seconds_remaining + ydstogo + yardline_100 + score_differential, 
                         data = success_4th, num.trees = 100, importance = "impurity")

vip(success_4th_rf)

###################### wp2: if go and fail ##########################

fail_4th <- pbp %>%
  filter(down == 4) %>%
  filter(rush == 1 | pass == 1) %>%
  filter(yards_gained < ydstogo) %>%
  filter(!is.na(wpa))

fail_4th_rf <- ranger(wpa ~ game_seconds_remaining + ydstogo + yardline_100 + score_differential, 
                         data = fail_4th, num.trees = 100, importance = "impurity")

vip(fail_4th_rf)

###################### wp3: if make field goal ##########################

made_fg <- pbp %>%
  filter(field_goal_result == "made") %>%
  filter(!is.na(wpa)) %>%
  mutate(wpa = wpa + 0.075)

mean(made_fg$wpa + 0.1)

made_fg_rf <- ranger(wpa ~ game_seconds_remaining + ydstogo + yardline_100 + score_differential,
                     data = made_fg, num.trees = 100, importance = "impurity")

vip(made_fg_rf)

###################### wp4: if missed field goal ##########################

miss_fg <- pbp %>%
  filter(field_goal_result != "made") %>%
  filter(!is.na(wpa)) 

mean(miss_fg$wpa)

miss_fg_rf <- ranger(wpa ~ game_seconds_remaining + ydstogo + yardline_100 + score_differential,
                     data = miss_fg, num.trees = 100, importance = "impurity")

vip(miss_fg_rf)

###################### wp5: punt ##########################

punts <- pbp %>%
  filter(punt_attempt == 1) %>%
  filter(!is.na(wpa))

punt_rf <- ranger(wpa ~ game_seconds_remaining + ydstogo + yardline_100 + score_differential,
                  data = punts, num.trees = 100, importance = "impurity")

vip(punt_rf)

##############################################################################

yards_to_goal_params <- seq(5, 65, 5)
distance_params <- seq(1, 6, 1)
game_seconds_params <- c(3600, 2700, 1800, 1350, 900, 720, 420, 240, 120, 60, 30)
score_diff_params <- seq(-14, 11, 1)

hyper_grid <- expand.grid(
  yardline_100 = yards_to_goal_params,
  ydstogo = distance_params,
  game_seconds_remaining = game_seconds_params,
  score_differential = score_diff_params
)

success_preds <- data.frame(predict(success_4th_rf, data.frame(hyper_grid))$predictions) %>% rename(wp1 = predict.success_4th_rf..data.frame.hyper_grid...predictions)
fail_preds <- data.frame(predict(fail_4th_rf, data.frame(hyper_grid))$predictions) %>% rename(wp2 = predict.fail_4th_rf..data.frame.hyper_grid...predictions)
made_fg_preds <- data.frame(predict(made_fg_rf, data.frame(hyper_grid))$predictions) %>% rename(wp3 = predict.made_fg_rf..data.frame.hyper_grid...predictions)
miss_fg_preds <- data.frame(predict(miss_fg_rf, data.frame(hyper_grid))$predictions) %>% rename(wp4 = predict.miss_fg_rf..data.frame.hyper_grid...predictions)
punt_preds <- data.frame(predict(punt_rf, data.frame(hyper_grid))$predictions) %>% rename(wp5 = predict.punt_rf..data.frame.hyper_grid...predictions)
conver_preds <- data.frame(predict.glm(init_logit, newdata = hyper_grid, type = "response")) %>% rename(c1 = predict.glm.init_logit..newdata...hyper_grid..type....response..)
fg_preds <- data.frame(predict.glm(fg_logit, newdata = hyper_grid, type = "response")) %>% rename(c2 = predict.glm.fg_logit..newdata...hyper_grid..type....response..)

hyper_grid_preds <- cbind(hyper_grid, success_preds, fail_preds, made_fg_preds, miss_fg_preds, punt_preds, conver_preds, fg_preds)

final_grid <- hyper_grid_preds %>%
  ungroup() %>%
  mutate(k_sup = (wp3*c2 + wp4*(1-c2)),
         k = ifelse(k_sup > wp5, k_sup, wp5),
         a = wp1*c1 + wp2*(1-c1) - k)

write_csv(final_grid, "final_grid.csv")


