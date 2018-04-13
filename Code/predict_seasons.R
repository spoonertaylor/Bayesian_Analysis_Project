library(stringr)
library(data.table)
library(ggplot2)
# sim_season simulates one season using the posterior draws given.
# post_draws should be the extracted samples from stan fit model
## should have draws for "home", "intercept", "off" and "def"
## post_draws = as.data.frame(mod, pars = c("home", "intercept","off", "def"))
# season is a season of data. 
## should contain the home team, index and points (same for away teams)
## season = as.data.frame(reg_season[, c("home_team", "away_team", 
##                    "home_team_idx", "away_team_idx", "H_pts", "A_pts")])

# Outputs a data frame the same number of rows as the season but with extra columns
# for predicted scores and who won
sim_season = function(post_draws, season) {
  season = copy(season)
  nsamps = nrow(post_draws)
  draw = sample(1:nsamps, 1) # Randomly get which draw to use
  # Extract just that draw
  home_draw = post_draws$home[draw]
  intercept_draw =post_draws$intercept[draw]
  # Get offense and defense score,
  # Melt so one team per row
  # Drop the melted column.
  off_cols = str_which(colnames(post_draws), "off")
  off_draws = data.frame(team_index = 1:15,
                         reshape2::melt(post_draws[draw, off_cols]))
  off_draws = off_draws[,-2]
  def_cols = str_which(colnames(post_draws), "def")
  def_draws = data.frame(team_index = 1:15, 
                         reshape2::melt(post_draws[draw, def_cols]))
  def_draws = def_draws[,-2]
  # Merge team scores to each game.
  # Home offense
  season = merge(season, off_draws, by.x="home_team_idx", by.y = "team_index", all.x=TRUE)
  names(season)[names(season) == 'value'] = 'home_off'
  # Away offense
  season = merge(season, off_draws, by.x="away_team_idx", by.y = "team_index", all.x=TRUE)
  names(season)[names(season) == 'value'] = 'away_off'
  # Home defense
  season = merge(season, def_draws, by.x="home_team_idx", by.y = "team_index", all.x=TRUE)
  names(season)[names(season) == 'value'] = 'home_def'
  # Away defense
  season = merge(season, def_draws, by.x="away_team_idx", by.y = "team_index", all.x=TRUE)
  names(season)[names(season) == 'value'] = 'away_def'
  
  # Obtain theta parameter for each team for each game
  season['home_theta'] = exp(intercept_draw+home_draw + season$home_off + season$away_def)
  season['away_theta'] = exp(intercept_draw+season$away_off + season$home_def)
  
  # Predictive points scored
  season['pred_home_score'] = rpois(nrow(season), season$home_theta)
  season['pred_away_score'] = rpois(nrow(season), season$away_theta)
  
  # Add outcomes (did the home team win?)
  season['outcome'] = ifelse(season$H_pts > season$A_pts, 1, 0)
  season['pred_outcome'] = ifelse(season$pred_home_score > season$pred_away_score, 1, 0)
  
  cols_del = c(7:12)
  season[, cols_del] = NULL # Delete unneeded columns
  return(season)
}

# summarize_season takes the output of the sim_season
# outputs a row for each team and columns for the aggregated totals
summarize_season = function(season) {
  season = as.data.table(season)
  teams = unique(season$home_team_idx)
  num_teams = length(teams)
  sum_season = data.table(team = 1:num_teams, team_name = character(num_teams),
                          pnts_for = integer(num_teams), pred_pnts_for = integer(num_teams),
                          avg_pts_for = double(num_teams), avg_pred_pts_for = double(num_teams),
                          pts_ag = integer(num_teams), pred_pts_ag = integer(num_teams),
                          avg_pts_ag = double(num_teams), avg_pred_pts_ag = double(num_teams),
                          outcome = integer(num_teams), pred_outcome = integer(num_teams))
  
  for(t in teams) {
    # Subset data for each team
    team_dt = season[home_team_idx == t | away_team_idx == t ,,]
    n_games = nrow(team_dt)
    # Get the team name. It would be the one that is in the maximum number of times
    # in the home team column
    tab = table(team_dt$home_team)
    team_name = names(tab)[which.max(tab)]
    # Points for
    pfor = ifelse(team_dt$home_team_idx == t, team_dt$H_pts, team_dt$A_pts)
    # Pred points for
    pred_for = ifelse(team_dt$home_team_idx == t, team_dt$pred_home_score, team_dt$pred_away_score)
    # Points against
    pagainst = ifelse(team_dt$home_team_idx == t, team_dt$A_pts, team_dt$H_pts)
    # Pred point against
    pred_ag = ifelse(team_dt$home_team_idx == t, team_dt$pred_away_score, team_dt$pred_home_score)
    # Outcome
    out = ifelse(team_dt$home_team_idx == t, team_dt$outcome, 1-team_dt$outcome)
    pred_out = ifelse(team_dt$home_team_idx == t, team_dt$pred_outcome, 1-team_dt$pred_outcome)
    # Update values
    vals = as.list(c(t, team_name,
                     sum(pfor), sum(pred_for), sum(pfor)/n_games, sum(pred_for)/n_games,
                     sum(pagainst), sum(pred_ag), sum(pagainst)/n_games, sum(pred_ag)/n_games,
                     sum(out), sum(pred_out)))
    # Do not worry about the warnings
    set(sum_season, t, names(sum_season), vals)
  }
  
  return(sum_season)
  
}

# Simulate multiple seasons
# n = number of season to simulate
## season = as.data.frame(reg_season[, c("home_team", "away_team", 
##                    "home_team_idx", "away_team_idx", "H_pts", "A_pts")])
# summarize_season: should we return the summary of each season (aggregated values),
## or keep each game seperatly

sim_mult_seasons = function(n = 100, post_draws, season, sum_season = TRUE) {
  # If we want to return the summarized version
  if(sum_season) {
    # Create giant matrix to fill
    seasons = as.data.frame(matrix(rep(NA, n*15*13), nrow=n*15))
    row_idx = 1
    for(i in 1:n) {
      # Simulate season
      simmed_season = sim_season(post_draws, season)
      # Summarize season
      summed_season = summarize_season(simmed_season)
      # Iteration number
      seasons[row_idx:(row_idx+15-1),1] = i
      seasons[row_idx:(row_idx+15-1),2:ncol(seasons)] = summed_season
      row_idx = row_idx + 15 # Update counter of which row we are on
    }
    colnames(seasons) = c("season_num",colnames(summed_season))
  }
  # If sum_season is false we want to just keep all games
  else {
    seasons = as.data.frame(matrix(rep(NA, n*nrow(season)*11),nrow=n*nrow(season)))
    row_index = 1
    for(i in 1:n) {
      simmed_season = sim_season(post_draws, season)
      seasons[row_index:(row_index+nrow(season)-1),1] = i
      seasons[row_index:(row_index+nrow(season)-1),2:ncol(seasons)] = simmed_season
      row_index = row_index+nrow(season)
    }
    colnames(seasons) = c("season_num", colnames(simmed_season))
  }
  return(seasons)
}

# Plots the distribution of the simulated season
# summed_seasons is the output from sim_mult_seasons with argument true
# var_name is the predicted variable that you want to see the distribution of
## "pred_pnts_for" --> predicted points for
## "avg_pred_pts_for" --> predicted average points per game
## "pred_pts_ag" -> predicted points against
## "avg_pred_pts_ag" --> predicted average points per game
## "pred_outcome" --> predicted number of wins
# teams --> vector of team names to plot distribution of. If null, plots all
plot_distribution = function(summed_seasons, var_name, teams=NULL) {
  summed_seasons = as.data.table(summed_seasons)
  potential_cols = c("pred_pnts_for", "avg_pred_pts_for", "pred_pts_ag",
                     "avg_pred_pts_ag", "pred_outcome")
  if(var_name %in% potential_cols) {
    col_num = which(var_name == colnames(summed_seasons))
    act_col = col_num - 1 # the non-predicted column is always to the left
  }
  else {
    stop("Enter a valid statistic to plot!")
  }
  # Subset teams
  if(length(teams) > 0) {
    teams_int = summed_seasons[team_name %in% teams,,]
    if(nrow(teams_int) == 0) {
      stop("Enter at least one valid team")
    }
  }
  else {
    teams_int = summed_seasons
  }
  plt = ggplot(teams_int, aes_string(x=var_name)) + geom_histogram(bins=30) +
    facet_wrap(~team_name)
  # Add the real outcome
  actual_name = colnames(teams_int)[act_col]
  actual_stats = data.frame(unique(teams_int[,get(actual_name),by=team_name]))
  plt = plt + geom_vline(data=actual_stats, mapping=aes(xintercept=V1), color="red")
  return(plt)
}

# Rank teams by their number of wins
# by_pred = TRUE is rank them by the average wins we predicted
# FALSE is by the actual number of wins
rank_teams = function(summed_season, by_pred =TRUE) {
  summed_season = as.data.table(summed_season)
  summed_season = summed_season[,-1]
  summed_season_r = summed_season[, lapply(.SD, mean), by=.(team, team_name)]
  if(by_pred) {
    return(summed_season_r[order(-pred_outcome)])
  }
  else {
    return(summed_season_r[order(-outcome)])
  }
}