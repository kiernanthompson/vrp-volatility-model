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
# ABLATION STEP 4: SV WITH REGIME SWITCHING
# ============================================

