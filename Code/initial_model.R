# Init model
library(data.table)
library(ggplot2)
library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
setwd('C:/Users/Taylor/Documents/UMich/STATS_551/STATS_551_Project/')
data = fread("Data/stats551data_1516_updated.csv")
reg_season = data[reg_season == TRUE,,]


teams = fread('Data/teamsdf.csv')
teams = teams[order(team_idx)]
team_abbs = fread("Data/team_abbrs.txt", header=FALSE)
# Get data ready
G = nrow(reg_season)
Tms = length(unique(reg_season$home_team))
home_teams = reg_season$home_team_idx
away_teams = reg_season$away_team_idx
y_home = reg_season$H_pts
y_away = reg_season$A_pts

stan_data = list(G, Tms, home_teams, away_teams, y_home, y_away)
mod <- stan(file = 'Code/init_model.stan', data = stan_data, 
            iter = 500, chains = 2, 
           control = list(max_treedepth = 15))

print(mod, pars=c("home", "off_star", "def_star"))

traceplot(mod, pars=c("home", "off", "def"))

effects = summary(mod, par=c("off", "def"))[[1]]
offense = effects[1:30]
defense = effects[31:60]

team_effects = data.table(team = teams$index,
                          team_abbv = team_abbs$V1,
                                team_idx = teams$team_idx,
                                off = offense,
                                def = defense)

theme_set(theme_gray())
ggplot(team_effects, aes(x=off, y=def)) + 
  geom_text(aes(label=team_abbv)) +
  geom_hline(yintercept = 0) + geom_vline(xintercept=0)