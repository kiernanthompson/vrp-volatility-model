# 04_model_bayesian.R
## (1) SINGLE-REGIME BAYESIAN SV (baseline)
## (2) SV with Student-t errors (heavy tails)
## (3) SV with leverage (asymmetry)
## (4) SV with regime switching
## (5) Hierarchical regime-switching SV (primary contribution)

library(rugarch)
library(xts)
library (zoo)
library(MSwM)
library(stochvol)

# Load data
SPY_returns <- readRDS("data/processed/SPY_returns.rds")

# ============================================
# ABLATION STEP 1: SINGLE-REGIME BAYESIAN SV (Baseline)
# ============================================
library(stochvol)

# Load returns
SPY_returns <- readRDS("data/processed/SPY_returns.rds")
returns_numeric <- as.numeric(SPY_returns)
returns_clean <- returns_numeric[!is.na(returns_numeric)]
returns_demeaned <- returns_clean - mean(returns_clean)

# Fit Bayesian stochastic volatility model
# Default specification: log-volatility follows AR(1) with Normal innovations
# Priors: standard weakly informative defaults
set.seed(42)
sv_fit <- svsample(returns_demeaned, 
                   draws = 5000,
                   burnin = 1000,
                   quiet = FALSE)

# Inspect posterior summary
summary(sv_fit)

# Save
saveRDS(sv_fit, "data/processed/Bayesian_SV_fit.rds")

# COMMENT: SV BASELINE
# POSTERIOR ESTIMATES:
# mu = -9.457 (95% CI: -9.66 to -9.26)
#   Long-run log-volatility mean
#   Implied annualised vol: ~14.1%
#   Consistent with historical averages
#
# phi = 0.976 (95% CI: 0.970 to 0.982)
#   Log-volatility AR(1) persistence
#   Near-unit-root behaviour confirmed
#   Slow mean reversion consistent with SV literature
#
# sigma = 0.226 (95% CI: 0.204 to 0.249)
#   Volatility of log-volatility
#   Stochastic innovation in latent vol process
#   First direct evidence of genuine volatility
#   variation beyond deterministic GARCH structure
#
# CONVERGENCE:
# ESS: mu = 2454, phi = 176, sigma = 84
# No divergences or warnings
# Lower ESS for phi and sigma typical of SV models
# Substantive inference sound despite lower ESS
#
# METHODOLOGICAL CONTRIBUTION OVER GARCH:
# Treats volatility as latent stochastic process
# rather than deterministic function of returns.
# Produces posterior distribution over latent
# volatility at each time point rather than
# point estimate.
#
# LIMITATION: Single-regime specification.
# Cannot accommodate regime non-stationarity
# documented in Nyblom tests. Will produce
# posterior credible intervals that are too
# tight in some periods and too wide in others
# because the single distributional structure
# averages across qualitatively different regimes.

# ============================================
# ABLATION STEP 2: SV WITH STUDENT-T ERRORS
# ============================================
# Isolates contribution of heavy tails
# Fit with Student-t innovations
set.seed(42)
sv_t_fit <- svsample(returns_demeaned,
                     draws = 5000,
                     burnin = 1000,
                     priorspec = specify_priors(
                       nu = sv_exponential(0.1)  # prior on degrees of freedom
                     ),
                     quiet = FALSE)

# Inspect
summary(sv_t_fit)

# Save (will be gitignored)
saveRDS(sv_t_fit, "data/processed/SV_t_fit.rds")

# COMMENT: ABLATION STEP 2 (SV WITH STUDENT-T)
# POSTERIOR ESTIMATES:
# mu    = -9.43  (essentially unchanged from Step 1)
# phi   =  0.981 (slight increase in persistence)
# sigma =  0.197 (decrease from 0.223)
# nu    = 14.85  (95% CI: 11.4 to 18.9)
#
# KEY FINDING - DEGREES OF FREEDOM
# SV Student-t nu estimate is 14.85
# GJR-GARCH Student-t shape estimate was 6.79
# SV framework attributes much less to fat tails
# than GARCH framework attributes to same data.
#
# Stochastic volatility structure absorbs tail
# behaviour that GARCH misclassifies as heavy-
# tailed innovations. When volatility is modelled
# as having its own stochastic error term rather
# than as deterministic function of returns,
# apparent fat tails reduce substantially.
# # Heavy-tail findings in GARCH-based literature
# are partly artefactual product of model misspecification.
#
# ABLATION CONTRIBUTION:
# Adding Student-t to SV is justified but adds
# less than the GARCH equivalent comparison
# implies. Genuine Student-t contribution in SV
# framework is modest; most of the heavy-tail
# accommodation was already happening through
# the stochastic volatility structure itself.
#
# ESS NOTE: nu has ESS = 23, consistent with
# known sampling difficulty for degrees of
# freedom parameters. Substantive finding robust
# despite autocorrelation in chain.

# ============================================
# ABLATION STEP 3: SV WITH LEVERAGE (SVL specification)
# ============================================
# rho no longer constrained to zero (isolates asymmetry contribution)
library(stochvol)

# Load returns
SPY_returns <- readRDS("data/processed/SPY_returns.rds")
returns_numeric <- as.numeric(SPY_returns)
returns_clean <- returns_numeric[!is.na(returns_numeric)]
returns_demeaned <- returns_clean - mean(returns_clean)

# Fit with Student-t innovations AND leverage
set.seed(42)
sv_tl_fit <- svsample(returns_demeaned,
                      draws = 5000,
                      burnin = 1000,
                      priorspec = specify_priors(
                        nu = sv_exponential(0.1),
                        rho = sv_beta(4, 4)
                      ),
                      quiet = FALSE)

# Inspect
summary(sv_tl_fit)

# Save
saveRDS(sv_tl_fit, "data/processed/SV_tl_fit.rds")

# Re-run with more iterations/ thinning (ESS deterioration)
set.seed(42)
sv_tl_fit <- svsample(returns_demeaned,
                      draws = 20000,
                      burnin = 5000,
                      thinpara = 4,
                      priorspec = specify_priors(
                        nu = sv_exponential(0.1),
                        rho = sv_beta(4, 4)
                      ),
                      quiet = FALSE)

# Inspect
summary(sv_tl_fit)

# COMMENT: SV with leverage (extended run)
# 20000 draws, 5000 burnin, thinpara = 4
#
# POSTERIOR ESTIMATES:
# mu    = -9.41  (95% CI: -9.53 to -9.28)
# phi   =  0.969 (95% CI: 0.964 to 0.974)
# sigma =  0.257 (95% CI: 0.232 to 0.279)
# nu    = 14.77  (95% CI: 11.2 to 19.8)
# rho   = -0.733 (95% CI: -0.767 to -0.694)
#
# ESS adequate across all parameters after extended run.
# Substantive findings match shorter initial run,
# confirming earlier interpretation was correct
# despite insufficient ESS in initial fit.
#
# KEY FINDINGS:
# 1. Strong leverage effect within SV framework
#    (rho ≈ -0.73, consistent with literature)
# 2. Heavy-tail estimate stable under leverage
#    (nu ≈ 14.8, substantially higher than GARCH 6.8)
# 3. High persistence maintained (phi ≈ 0.97)
#
# ABLATION CONTRIBUTION:
# SV with leverage adds strong asymmetric component
# while maintaining thinner-tail estimates than
# GARCH-based analyses suggest. Supports conclusion
# that correct SV specification captures tail
# behaviour through volatility dynamics rather
# than requiring extreme innovation tails.

# ============================================
# ABLATION STEP 4A: SV WITH REGIME SWITCHING
# ============================================
library(rstan)
library(bayesplot)
library(posterior)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

SPY_returns <- readRDS("data/processed/SPY_returns.rds")

returns_numeric <- as.numeric(SPY_returns)
returns_clean <- returns_numeric[!is.na(returns_numeric)]
returns_demeaned <- returns_clean - mean(returns_clean)

## input block
y <- returns_demeaned * 100   # percentage returns; helps Stan scale
T <- length(y)

stan_data <- list(
  T = T,
  y = y
)

## stan file
dir.create("models/stan", recursive = TRUE, showWarnings = FALSE)

stan_code <- '
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
'

writeLines(stan_code, "models/stan/SV_ms.stan")

## smaller validation sample (first test took >20mins)
y_test <- tail(y, 2000)

stan_data_test <- list(
  T = length(y_test),
  y = y_test
)

## rerun test
SV_ms_fit_test <- stan(
  file = "models/stan/SV_ms.stan",
  data = stan_data_test,
  chains = 2,
  iter = 1000,
  warmup = 500,
  seed = 123,
  control = list(
    adapt_delta = 0.9,
    max_treedepth = 10
  )
)

## check retest parameters
print(
  SV_ms_fit_test,
  pars = c("mu", "phi", "sigma", "P", "nu"),
  probs = c(0.1, 0.5, 0.9)
)

saveRDS(SV_ms_fit_test, "data/processed/SV_ms_failed_fit.rds")

# COMMENTS: Two-regime stochastic volatility model 
  # with common phi, regime-specific mu and sigma, 
  # Markov transition matrix, and Student-t observation errors. 
  # Test run used tail(y, 2000), 2 chains, 1000 iterations, 500 warmup,

# RESULTS: model compiled and sampled, but the posterior output unusable for inference.
# Diagnostics:
#   - 180 divergent transitions after warmup.
#   - Low BFMI in both chains.
#   - Largest R-hat = 2.89 from warning summary, with printed parameter R-hats far worse for key parameters.
#   - Effective sample sizes are essentially 1 for most structural parameters.
#   - Chains did not mix.
#
# Parameter output:
#   - mu[1] and mu[2] both collapsed around -0.01, despite ordered regime means.
#   - phi estimated around 0.14, implausibly low for daily equity volatility persistence.
#   - sigma[1] and sigma[2] collapsed toward 0.
#   - P[1,1] ≈ 0.99 and P[2,2] ≈ 0.87 suggest high persistence, but estimates are not trustworthy.
#   - nu ≈ 39.5 with extreme uncertainty, indicating unstable identification of tail thickness.
#
# Interpretation: This is not a substantive empirical result. 
# Student-t regime-switching SV specification is overparameterised for 
# current sampler geometry and is producing pathological posterior behaviour.
# The model appears to be collapsing latent volatility dynamics 
# rather than identifying meaningful regimes. Do not scale this model to the full sample. 
# Treat as failed feasibility test of the Student-t MS-SV specification.

# Simplify Step 4 to Gaussian regime-switching SV by removing nu and replacing 
# the Student-t observation density with a normal likelihood. 
# This preserves the ablation logic by isolating the value of regime structure before reintroducing heavy-tailed innovations.

## ============================================================
## ABLATION STEP 4B: GAUSSIAN REGIME-SWITCHING SV
## ============================================================
library(rstan)
library(tidyverse)
library(bayesplot)
library(posterior)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Fit
SV_ms_fit <- stan(
  file = "models/stan/SV_ms_gaussian.stan",
  data = stan_data,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 12
  )
)

## Save
saveRDS(SV_ms_fit, "data/processed/SV_ms_fit.rds")

## Inspect
print(
  SV_ms_fit,
  pars = c("mu", "phi", "sigma", "P"),
  probs = c(0.025, 0.5, 0.975)
)

## Diagnostics summary
SV_ms_summary <- summary(
  SV_ms_fit,
  pars = c("mu", "phi", "sigma", "P")
)$summary

saveRDS(SV_ms_summary, "data/processed/SV_ms_summary.rds")

## Trace plots
mcmc_trace(
  SV_ms_fit,
  pars = c("mu[1]", "mu[2]", "phi", "sigma[1]", "sigma[2]", "P[1,1]", "P[2,2]")
)

## Extract regime probabilities
post <- rstan::extract(SV_ms_fit)

prob_high_vol <- apply(post$filtered_prob[, , 2], 2, mean)
vol_est <- apply(post$vol, 2, mean)

SV_ms_regime_probs <- tibble::tibble(
  t = seq_along(y),
  return = y,
  vol = vol_est,
  prob_high_vol = prob_high_vol
)

saveRDS(SV_ms_regime_probs, "data/processed/SV_ms_regime_probs.rds")

## Plot regime probabilities
p_regime <- ggplot(SV_ms_regime_probs, aes(x = t, y = prob_high_vol)) +
  geom_line() +
  labs(
    title = "Gaussian MS-SV: high-volatility regime probability",
    x = "Time",
    y = "P(high-vol regime)"
  )

print(p_regime)

ggsave(
  "outputs/figures/SV_ms_regime_probability.png",
  p_regime,
  width = 9,
  height = 4
)

## Plot latent volatility
p_vol <- ggplot(SV_ms_regime_probs, aes(x = t, y = vol)) +
  geom_line() +
  labs(
    title = "Gaussian MS-SV: estimated latent volatility",
    x = "Time",
    y = "Volatility"
  )

print(p_vol)

ggsave(
  "outputs/figures/SV_ms_latent_volatility.png",
  p_vol,
  width = 9,
  height = 4
)
