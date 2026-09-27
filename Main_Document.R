# Importing Librarys
library(readr)
library(tidyverse)
library(gt)
library(GGally)
library(cowplot)
library(MVN)
library(ggcorrplot)
library(mvtnorm)
library(knitr)
library(ggbiplot)
# Importing Data
nfl_team_statistics <- read_csv("nfl-team-statistics.csv")
attach(nfl_team_statistics)
# Cleaning data and adding win and loss percentages
nfl_team_statistics <- na.omit(nfl_team_statistics)
nfl_team_statistics$n_games <- (nfl_team_statistics$wins +
                                  nfl_team_statistics$losses +
                                  nfl_team_statistics$ties)
nfl_team_statistics$win_percentage <- (nfl_team_statistics$wins / nfl_team_statistics$n_games) * 100 # nolint: line_length_linter.
nfl_team_statistics$loss_percentage <- (nfl_team_statistics$losses / nfl_team_statistics$n_games) * 100 # nolint: line_length_linter.
nfl_team_statistics %>% # nolint
  count(team, sort = TRUE) %>% # nolint
  View()
View(nfl_team_statistics)
# Model containing traditional NFL stats
traditional_mod <- lm(data = nfl_team_statistics, win_percentage ~ 1 +
                        offense_total_yards_gained_run +
                        offense_total_yards_gained_pass +
                        offense_success_rate_run +
                        offense_success_rate_pass +
                        offense_n_plays_run +
                        offense_n_plays_pass +
                        offense_n_interceptions +
                        offense_n_fumbles_lost_run +
                        offense_n_fumbles_lost_pass +
                        offense_completion_percentage +
                        offense_ave_yards_gained_run +
                        offense_ave_yards_gained_pass)
anova(traditional_mod)
summary(traditional_mod)
# Model containing advanced or new NFL stats
advanced_mod <- lm(data = nfl_team_statistics, win_percentage ~ 1 +
                     offense_ave_air_yards +
                     offense_ave_epa_pass +
                     offense_ave_epa_run +
                     offense_ave_wpa_pass +
                     offense_ave_wpa_run +
                     offense_ave_yac +
                     offense_total_epa_pass +
                     offense_total_epa_run +
                     offense_total_wpa_pass +
                     offense_total_wpa_run +
                     offense_total_yac)
anova(advanced_mod)
summary(advanced_mod)

# AIC and stepwise comparisons of model and variables
AIC(advanced_mod, traditional_mod)

step(traditional_mod, direction = "both", k = log(nrow(nfl_team_statistics)))
step(advanced_mod, direction = "both", k = log(nrow(nfl_team_statistics)))

# Final Advanced model from step
final_advanced_mod <- lm(formula = win_percentage ~ offense_ave_wpa_pass +
                           offense_ave_wpa_run +
                           offense_ave_yac +
                           offense_total_epa_pass +
                           offense_total_epa_run +
                           offense_total_yac,
                         data = nfl_team_statistics)
# Final tradtional model from step
final_traditional_mod <- lm(formula = win_percentage ~
                              offense_total_yards_gained_pass +
                                offense_success_rate_pass +
                                offense_n_plays_run +
                                offense_n_plays_pass +
                                offense_n_interceptions +
                                offense_n_fumbles_lost_pass,
                            data = nfl_team_statistics)
# AIC and BIC comparisons
AIC(final_advanced_mod, final_traditional_mod)
BIC(final_advanced_mod, final_traditional_mod)
# summary and nova of models
summary(final_advanced_mod)
anova(final_advanced_mod)
summary(final_traditional_mod)
anova(final_traditional_mod)


top_teams <- nfl_team_statistics %>% # nolint: pipe_consistency_linter.
  group_by(season) %>% # nolint: pipe_consistency_linter.
  slice_max(win_percentage, n = 1, with_ties = FALSE)
View(top_teams)

ggplot(top_teams, aes(x = points_scored, y = points_allowed)) +
  annotate("rect",
    xmin = 475, xmax = Inf, ymin = -Inf, ymax = 325,
    fill = "lightgreen",
    alpha = 0.2
  ) +
  geom_text(aes(label = paste(team, season)),
    check_overlap = TRUE,
    size = 4, show.legend = FALSE
  ) +
  labs(
    title = "Best Regular Season Teams' Points Scored and Points Allowed",
    x = "Points Scored",
    y = "Points Allowed"
  ) +
  theme_minimal()


ggplot(top_teams, aes(x = offense_ave_wpa_run, y = offense_ave_wpa_pass)) +
  geom_text(aes(label = paste(team, season)),
    check_overlap = TRUE,
    size = 4, show.legend = FALSE
  ) +
  labs(
    title = "Best Regular Season Teams' by offense average wpa run and pass",
    x = "offense average wpa run",
    y = "offense average wpa pass"
  ) +
  theme_minimal()

ggplot(top_teams, aes(x = offense_ave_epa_run, y = offense_ave_epa_pass)) +
  geom_text(aes(label = paste(team, season)),
    check_overlap = TRUE,
    size = 4, show.legend = FALSE
  ) +
  labs(
    title = "Best Regular Season Teams' by offense average epa run and pass",
    x = "offense average epa run",
    y = "offense average epa pass"
  ) +
  theme_minimal()
