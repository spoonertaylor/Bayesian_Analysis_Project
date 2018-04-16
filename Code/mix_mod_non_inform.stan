// Stan model for the non-informative mixture model
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

}

parameters {
	// home effect
	real home; 
	real intercept;
	// Offense and defense params that we first sample from
	// Not centered
	vector[Tms] off_star;
	vector[Tms] def_star;

	// FOR THE MIXTURE MODEL
	// The pi probabilites that each team is in each group
	simplex[2] pi_off[Tms];
	simplex[2] pi_def[Tms];

	// All of our hyper parameters
	ordered[2] mu_off;
	ordered[2] mu_def;
	vector<lower=0>[2] tau_off;
	vector<lower=0>[2] tau_def;

}

transformed parameters {
	// Centered offense and defense
	vector[Tms] off;
	vector[Tms] def;
	vector<lower=0>[2] sigma_off;
	vector<lower=0>[2] sigma_def;

	for(t in 1:Tms) {
		off[t] = off_star[t] - mean(off_star[]);
		def[t] = def_star[t] - mean(def_star[]);
	}


	// Variance instead of precision
	sigma_off[1] = 1/tau_off[1];
	sigma_off[2] = 1/tau_off[2];
	//sigma_off[3] = 1/tau_off[3];

	sigma_def[1] = 1/tau_def[1];
	sigma_def[2] = 1/tau_def[2];
	//sigma_def[3] = 1/tau_def[3];
}

model {
	real l_theta_temp[2];
	real theta[2];
	vector[2] contributions_off;
	vector[2] contributions_def;
	// STAN USES VARIANCE!!!!!
	// Home court-- flat prior
	home ~ normal(0, 1000);
	intercept ~ normal(0, 1000);
	// Hyper params
	// First the lowest group
	mu_off[1] ~ normal(0,100);
	mu_off[2] ~ normal(0,100);
//	mu_off[3] ~ normal(48,100) T[47,];

	mu_def[1] ~ normal(0,100);
	mu_def[2] ~ normal(0,100);
//	mu_def[3] ~ normal(-123,100) T[-123,];

	tau_off[1] ~ gamma(1, 1);
	tau_off[2] ~ gamma(1, 1);
//	tau_off[3] ~ gamma(1, 1);

	tau_def[1] ~ gamma(1, 1);
	tau_def[2] ~ gamma(1, 1);
//	tau_def[3] ~ gamma(1, 1);

	for(t in 1:Tms) {
		// Probability vectors
		pi_off[t,] ~ dirichlet(rep_vector(1, 2));
		pi_def[t,] ~ dirichlet(rep_vector(1, 2));
		// Going to want to use 
		// This is a mixture model
		// Do this for each team.
		// Just have to define the pi parameter!!
		// target += log_mix(pi_off, student_t_lpdf(off_star[t] | 4, mu_off[1], sigma_off[1]),
		// 					student_t_lpdf(off_star[t] | 4, mu_off[2], sigma_off[2]),
		// 					student_t_lpdf(off_star[t] | 4, mu_off[3], sigma_off[3]));

		// target += log_mix(pi_def, student_t_lpdf(def_star[t] | 4, mu_def[1], sigma_def[1]),
		// 			student_t_lpdf(def_star[t] | 4, mu_def[2], sigma_def[2]),
		// 			student_t_lpdf(def_star[t] | 4, mu_def[3], sigma_def[3]));
		for(k in 1:2) {
		      contributions_off[k] = log(pi_off[t,k]) + student_t_lpdf(off_star[t] | 4, mu_off[k], sigma_off[k]);
		      contributions_def[k] = log(pi_def[t,k]) + student_t_lpdf(def_star[t] | 4, mu_def[k], sigma_def[k]);
		}
		target += log_sum_exp(contributions_off);
		target += log_sum_exp(contributions_def);
	}

	for(g in 1:G) {
		l_theta_temp[1] = intercept + home + off[home_teams[g]] + def[away_teams[g]];
		l_theta_temp[2] = intercept + off[away_teams[g]] + def[home_teams[g]];
		theta[1] = exp(l_theta_temp[1]);
		theta[2] = exp(l_theta_temp[2]);
		y_home[g] ~ poisson(theta[1]);
		y_away[g] ~ poisson(theta[2]);
	}

}

