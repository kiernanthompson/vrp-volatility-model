
data {
  int<lower=1> T;
  vector[T] y;
}

parameters {
  ordered[2] mu;
  real<lower=0, upper=1> phi;
  vector<lower=0>[2] sigma;
  simplex[2] P[2];
  simplex[2] pi0;
  vector[T] h;
  real<lower=2> nu;
}

model {
  vector[2] log_alpha_prev;
  vector[2] log_alpha;

  mu[1] ~ normal(-10, 3);
  mu[2] ~ normal(-8, 3);

  phi ~ beta(20, 1.5);
  sigma ~ normal(0, 0.5);

  P[1] ~ dirichlet(to_vector({20, 2}));
  P[2] ~ dirichlet(to_vector({2, 20}));

  pi0 ~ dirichlet(to_vector({1, 1}));
  nu ~ gamma(2, 0.1);

  h[1] ~ normal(mu[1], 2);

  log_alpha_prev = log(pi0);

  for (t in 1:T) {
    for (j in 1:2) {
      vector[2] lp;

      for (i in 1:2) {
        lp[i] = log_alpha_prev[i] + log(P[i, j]);
      }

      if (t == 1) {
        target += normal_lpdf(h[t] | mu[j], sigma[j]);
      } else {
        target += normal_lpdf(
          h[t] |
          mu[j] + phi * (h[t - 1] - mu[j]),
          sigma[j]
        );
      }

      log_alpha[j] =
        log_sum_exp(lp) +
        student_t_lpdf(y[t] | nu, 0, exp(h[t] / 2));
    }

    log_alpha = log_alpha - log_sum_exp(log_alpha);
    log_alpha_prev = log_alpha;
  }
}

generated quantities {
  vector[T] vol;
  matrix[T, 2] filtered_prob;

  {
    vector[2] log_alpha_prev;
    vector[2] log_alpha;

    log_alpha_prev = log(pi0);

    for (t in 1:T) {
      for (j in 1:2) {
        vector[2] lp;

        for (i in 1:2) {
          lp[i] = log_alpha_prev[i] + log(P[i, j]);
        }

        log_alpha[j] =
          log_sum_exp(lp) +
          student_t_lpdf(y[t] | nu, 0, exp(h[t] / 2));
      }

      log_alpha = log_alpha - log_sum_exp(log_alpha);

      filtered_prob[t, 1] = exp(log_alpha[1]);
      filtered_prob[t, 2] = exp(log_alpha[2]);

      log_alpha_prev = log_alpha;

      vol[t] = exp(h[t] / 2);
    }
  }
}

