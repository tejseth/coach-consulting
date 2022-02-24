pbp_2pt <- load_pbp(2011:2021)

pbp_just_2pt <- pbp_2pt %>%
  filter(two_point_attempt == 1) %>%
  mutate(success = ifelse(two_point_conv_result == "success", 1, 0))

# team_2pt <- pbp_2pt %>%
#   group_by(posteam) %>%
#   summarize(plays_2pt = n(),
#             success_rate = mean(success)) %>%
#   left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# team_2pt %>%
#   ggplot(aes(x = plays_2pt, y = success_rate)) +
#   geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
#   theme_minimal() +
#   labs(x = "Amount of 2pt. Conversion Attempts",
#        y = "2pt. Conversion Success Rate",
#        title = "Each NFL Team's 2 Point Conversion Breakdown Since 2015") +
#   theme(plot.title = element_text(size = 20, face = "bold"))

####################### Making 2pt conversion rate #############################

off_epa_5 <- pbp_2pt %>%
  filter(yardline_100 <= 5, !is.na(epa)) %>%
  group_by(posteam, season) %>%
  summarize(off_plays_in_5 = n(),
            off_epa_in_5 = mean(epa))

def_epa_5 <- pbp_2pt %>%
  filter(yardline_100 <= 5, !is.na(epa)) %>%
  group_by(defteam, season) %>% 
  summarize(def_plays_in_5 = n(),
            def_epa_in_5 = mean(epa))

pbp_just_2pt <- pbp_just_2pt %>%
  left_join(off_epa_5, by = c("posteam")) %>%
  left_join(def_epa_5, by = c("defteam"))

log_2pt <- glm(success ~ off_epa_in_5 + def_epa_in_5 + game_seconds_remaining,
                  data = pbp_just_2pt, family = "binomial")

pred_2pt<- ifelse(log_2pt$fitted.values > 0.5, "1", "0")

pbp_just_2pt<- pbp_just_2pt %>%
  mutate(pred = log_2pt$fitted.values)

model_2pt_data <- pbp_just_2pt %>%
  select(label = success, off_epa_in_5, def_epa_in_5, game_seconds_remaining, score_differential) 

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.025,
    gamma = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 6,
    base_score = mean(model_2pt_data$label)
  )

full_train_2pt <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = model_2pt_data %>% select(-label)),
                                       label = model_2pt_data$label)
model_2pt <- xgboost::xgboost(params = params, data = full_train_2pt, nrounds = nrounds, verbose = 2)
vip(model_2pt, num_features = 5) 


####################### Making Extra Point conversion rate #############################

pbp_xp <- pbp_2pt %>%
  dplyr::filter(extra_point_attempt == 1) %>%
  dplyr::mutate(xp_made = ifelse(extra_point_result == "good", 1, 0))

team_xp_rate <- pbp_xp %>%
  group_by(posteam, season) %>%
  summarize(xps = n(),
            xp_make_rate = mean(xp_made))

pbp_xp <- pbp_xp %>%
  left_join(team_xp_rate, by = c("posteam", "season")) %>%
  # select(xp_made, xp_make_rate, game_seconds_remaining) %>%
  filter(!is.na(xp_made), !is.na(xp_make_rate), !is.na(game_seconds_remaining))

log_xp <- glm(xp_made ~ xp_make_rate + game_seconds_remaining, 
              data = pbp_xp, family = "binomial")

summary(log_xp)

model_xp_data<- pbp_xp %>%
  select(label = xp_made, xp_make_rate, score_differential, game_seconds_remaining)

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.025,
    gamma = 5,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 4,
    min_child_weight = 6,
    base_score = mean(model_xp_data$label)
  )

full_train_xp <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = model_xp_data %>% select(-label)),
                                      label = model_xp_data$label)
model_xp <- xgboost::xgboost(params = params, data = full_train_xp, nrounds = nrounds, verbose = 2)

vip(model_xp, num_features = 5)

############################# Random Forests ################################

success_2pt<- pbp_just_2pt %>%
  filter(!is.na(wpa)) %>%
  filter(success == 1) %>%
  mutate(wpa = ifelse(wpa < 0, -wpa, wpa))

success_2pt_rf<- ranger(wpa ~ game_seconds_remaining + score_differential + off_epa_in_5 + def_epa_in_5, 
                        data = success_2pt, num.trees = 100, importance = "impurity")

vip(success_2pt_rf)

success_2pt_rf$r.squared 

fail_2pt<- pbp_just_2pt %>%
  filter(!is.na(wpa)) %>%
  filter(success != 1) %>%
  mutate(wpa = ifelse(wpa < 0, -wpa, wpa))

fail_2pt_rf<- ranger(wpa ~ game_seconds_remaining + score_differential + off_epa_in_5 + def_epa_in_5, 
                     data = fail_2pt, num.trees = 100, importance = "impurity")

vip(fail_2pt_rf)

fail_2pt_rf$r.squared 

made_xp <- pbp_xp %>%
  dplyr::filter(xp_made == 1) %>%
  dplyr::filter(!is.na(wpa)) 

made_xp_rf <- ranger(wpa ~ game_seconds_remaining + score_differential, 
                     data = pbp_xp, num.trees = 100, importance = "impurity")

vip(made_xp_rf)

made_xp_rf$r.squared

miss_xp<- pbp_xp %>%
  filter(xp_made != 1) %>%
  filter(!is.na(wpa))

miss_xp_rf <- ranger(wpa ~ game_seconds_remaining + score_differential + xp_make_rate, 
                     data = miss_xp, num.trees = 100, importance = "impurity")

vip(miss_xp_rf)

miss_xp_rf$r.squared

############################ Hyper Grid #################################

off_epa = seq(-0.7, 0.7, 0.1)
def_epa = seq(-0.7, 0.7, 0.1)
xp_make_rate_params = seq(0.8, 1.0, 0.03)
game_seconds_params <- c(420, 240, 120, 60, 30)
score_diff_params <- seq(-10, 10, 1)

two_pt_grid <- expand.grid(
  off_epa_in_5 = off_epa,
  def_epa_in_5 = def_epa,
  xp_make_rate = xp_make_rate_params,
  game_seconds_remaining = game_seconds_params,
  score_differential = score_diff_params
)

two_success_preds <- data.frame(predict(success_2pt_rf, data.frame(two_pt_grid))$predictions) %>% rename(wp1 = predict.success_2pt_rf..data.frame.two_pt_grid...predictions)
two_fail_preds <- data.frame(predict(fail_2pt_rf, data.frame(two_pt_grid))$predictions) %>% rename(wp2 = predict.fail_2pt_rf..data.frame.two_pt_grid...predictions)
two_made_xp_preds <- data.frame(predict(made_xp_rf, data.frame(two_pt_grid))$predictions) %>% rename(wp3 = predict.made_xp_rf..data.frame.two_pt_grid...predictions)
two_miss_xp_preds <- data.frame(predict(miss_xp_rf, data.frame(two_pt_grid))$predictions) %>% rename(wp4 = predict.miss_xp_rf..data.frame.two_pt_grid...predictions)
two_conver_preds <- data.frame(predict.glm(log_2pt, newdata = two_pt_grid, type = "response")) %>% rename(c1 = predict.glm.log_2pt..newdata...two_pt_grid..type....response..)
xp_preds <- data.frame(predict.glm(log_xp, newdata = two_pt_grid, type = "response")) %>% rename(c2 = predict.glm.log_xp..newdata...two_pt_grid..type....response..)

two_point_preds <- cbind(two_pt_grid, two_success_preds, two_fail_preds, two_made_xp_preds, two_miss_xp_preds, two_conver_preds, xp_preds)

two_point_grid <- two_point_preds %>%
  ungroup() %>%
  mutate(k = wp3*c2 + wp4*(1-c2),
         a = wp1*c1 + wp2*(1-c1) - k)

two_point_grid <- two_point_grid %>%
  mutate(off_epa_in_5 = round(off_epa_in_5, 2),
         def_epa_in_5 = round(def_epa_in_5, 2))

write_csv(two_point_grid, "two_point_grid.csv")


