// Stan file for initial model.
// Use non-informative priors.
data {
	// Number of games
	int<lower=1> G;
	// Number of teams
	int<lower=1> Tms;

	// List of home and away teams for each game
	// Will be indexes for each team
	// EX: For 22nd game if Atlanta (idx 1) was the home team, home_team[22] = 1
	int home_teams[G];
	int away_teams[G];

	// Home and away point totals for each game
	int y_home[G];
	int y_away[G];

	// PREDICTIVE DISTRIBUTION????

}

parameters {
	// home effect
	real home; 
	real away;
	// Offense and defense params that we first sample from
	// Not centered
	vector[Tms] off_star;
	vector[Tms] def_star;

	// Hyperparams
	real mu_off;
	real mu_def;
	real<lower=0> tau_off;
	real<lower=0> tau_def;

}

transformed parameters {
	// Centered offense and defense
	vector[Tms] off;
	vector[Tms] def;
	real<lower=0> sigma_off;
	real<lower=0> sigma_def;
	// real l_theta_temp[2];
	// matrix[G, 2] theta;

	for(t in 1:Tms) {
		off[t] = (off_star[t] - mean(off_star[]))/sd(off_star[]);
		def[t] = (def_star[t] - mean(def_star[]))/sd(def_star[]);
	}


	// Variance instead of precision
	sigma_off = 1/tau_off;
	sigma_def = 1/tau_def;

}

model {
	// double team_off_t = 0 - sum(off_star)
	// double team_def_t = 0 - sum(def_star)
	real l_theta_temp[2];
	//matrix[G, 2] theta;
	real theta[2];
	// STAN USES VARIANCE!!!!!
	// Home court-- flat prior
	home ~ normal(0, 20);
	away ~ normal(0, 20);
	// Hyper params
	mu_off ~ normal(0, 150);
	mu_def ~ normal(0, 150);
	tau_off ~ gamma(.1, .1);
	tau_def ~ gamma(.1, .1);

	for(t in 1:Tms) {
		off_star[t] ~ normal(mu_off, sigma_off);
		def_star[t] ~ normal(mu_def, sigma_def);
	}

	for(g in 1:G) {
		l_theta_temp[1] = home + off[home_teams[g]] + def[away_teams[g]];
		l_theta_temp[2] = away + off[away_teams[g]] + def[home_teams[g]];
		//l_theta_temp[1] = home + off_star[home_teams[g]] + def_star[away_teams[g]]; // Using off_star and def_star makes the home court advantange really really small
		// Still off_star, def_star and home don't converge.
		// But also has the opposite effect-- offense is negative, defense is positive.
		// But team positions stay the same.
		//l_theta_temp[2] = off_star[away_teams[g]] + def_star[home_teams[g]];
		//theta[g, 1] = exp(l_theta_temp[1]);
		//theta[g, 2] = exp(l_theta_temp[2]);
		theta[1] = exp(l_theta_temp[1]);
		theta[2] = exp(l_theta_temp[2]);
		y_home[g] ~ poisson(theta[1]);
		y_away[g] ~ poisson(theta[2]);
	}

}
