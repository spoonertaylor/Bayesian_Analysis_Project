# Script to run mixture model
library(data.table)
#library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = 9)#parallel::detectCores())

data = fread("Data/west_data_1516.csv")
G = nrow(data)
Tms = length(unique(data$home_team))
home_teams = data$home_team_idx
away_teams = data$away_team_idx
y_home = data$H_pts
y_away = data$A_pts

stan_data = list(G, Tms, home_teams, away_teams, y_home, y_away)
mod <- stan(file = 'Code/mix_mod_non_inform.stan', data = stan_data, 
            iter = 2000, chains = 1, 
            control = list(adapt_delta = .95))

saveRDS(mod, file = "Models/mix_model.rds")