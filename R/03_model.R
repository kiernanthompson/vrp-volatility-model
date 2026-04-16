# 03_model.R
# Baseline GARCH model — benchmark for Bayesian comparison

library(rugarch)
library(xts)

# Load data
SPY_returns <- readRDS("data/processed/SPY_returns.rds")

# Specify GARCH(1,1) with normal errors — the Basel standard
spec_basic <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Fit
fit_basic <- ugarchfit(spec = spec_basic, data = SPY_returns)

# Inspect
show(fit_basic)

# Extract conditional volatility
vol_basic <- sigma(fit_basic)

# Plot against VIX
VIX <- readRDS("data/raw/VIX_raw.rds")
VIX_subset <- VIX$VIX.Close[index(vol_basic)]
VIX_scaled <- VIX_subset / 100 * sqrt(252)

plot(vol_basic, main = "GARCH(1,1) Conditional Volatility vs VIX",
     col = "darkblue", ylab = "Volatility")
lines(VIX_scaled, col = "red", lty = 2)
legend("topright", legend = c("GARCH", "VIX scaled"),
       col = c("darkblue", "red"), lty = c(1,2))

# Better aligned plot
vol_basic_numeric <- as.numeric(vol_basic)
VIX_scaled_numeric <- as.numeric(VIX_scaled)

# Trim to same length
min_len <- min(length(vol_basic_numeric), length(VIX_scaled_numeric))
vol_plot <- tail(vol_basic_numeric, min_len)
vix_plot <- tail(VIX_scaled_numeric, min_len)

# Plot on same scale
plot(vix_plot, 
     type = "l",
     col = "red",
     lty = 2,
     main = "GARCH(1,1) Conditional Volatility vs VIX",
     ylab = "Annualised Volatility",
     xlab = "Trading Days",
     ylim = c(0, max(c(vol_plot, vix_plot), na.rm = TRUE)))

lines(vol_plot, col = "darkblue")

legend("topright", 
       legend = c("GARCH conditional vol", "VIX scaled"),
       col = c("darkblue", "red"), 
       lty = c(1, 2))

# Convert GARCH vol to percentage points to match VIX scale
vol_basic_numeric <- as.numeric(vol_basic) * sqrt(252) * 100
VIX_scaled_numeric <- as.numeric(VIX$VIX.Close[index(vol_basic)])

# Trim to same length
min_len <- min(length(vol_basic_numeric), length(VIX_scaled_numeric))
vol_plot <- tail(vol_basic_numeric, min_len)
vix_plot <- tail(VIX_scaled_numeric, min_len)

# Plot
plot(vix_plot,
     type = "l",
     col = "red",
     lty = 2,
     main = "GARCH(1,1) Conditional Volatility vs VIX (% points)",
     ylab = "Volatility (%)",
     xlab = "Trading Days",
     ylim = c(0, max(c(vol_plot, vix_plot), na.rm = TRUE)))

lines(vol_plot, col = "darkblue")

legend("topright",
       legend = c("GARCH conditional vol", "VIX"),
       col = c("darkblue", "red"),
       lty = c(1, 2))

# Split into two panels for clear lag observation
par(mfrow = c(2, 1), mar = c(2, 4, 2, 1))

# Top panel — VIX
plot(vix_plot,
     type = "l",
     col = "red",
     main = "VIX (Market Implied Volatility)",
     ylab = "Volatility (%)",
     xlab = "",
     ylim = c(0, max(c(vol_plot, vix_plot), na.rm = TRUE)))

# Bottom panel — GARCH
plot(vol_plot,
     type = "l",
     col = "darkblue",
     main = "GARCH(1,1) Conditional Volatility",
     ylab = "Volatility (%)",
     xlab = "Trading Days",
     ylim = c(0, max(c(vol_plot, vix_plot), na.rm = TRUE)))

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Averages
cat("Average GARCH vol (%):", round(mean(vol_plot, na.rm=TRUE), 2), "\n")
cat("Average VIX (%):", round(mean(vix_plot, na.rm=TRUE), 2), "\n")
cat("Average difference:", round(mean(vix_plot - vol_plot, na.rm=TRUE), 2), "\n")

# Save
saveRDS(fit_basic, "data/processed/GARCH_baseline_fit.rds")
saveRDS(vol_basic, "data/processed/GARCH_vol_baseline.rds")

# GARCH(1,1) BASELINE DIAGNOSTICS - KEY FINDINGS
# Parameters: alpha1 = 0.1247, beta1 = 0.8563
# Persistence = 0.981 - slow mean reversion confirmed
# High beta1 relative to alpha1 - model reacts slowly
# to new shocks, produces lagged volatility estimates
#
# FAILURES:
# 1. Sign Bias Test: p = 1.245e-04 (Sign Bias)
#                   p = 2.772e-09 (Joint Effect)
#    Leverage effect confirmed statistically - negative
#    returns produce asymmetric volatility response not
#    captured by symmetric GARCH specification.
#    Consistent with LOESS exploratory finding.
#
# 2. Nyblom Stability: Joint = 15.794 vs CV = 1.6 (1%)
#    Parameter non-stationarity confirmed - model
#    parameters shift across sample period.
#    Omega individual = 2.098 - unconditional variance
#    level itself is unstable. Confirms pre-post 2008
#    distributional shift identified descriptively.
#    Direct empirical justification for regime-switching.
#
# 3. Pearson GOF: p ≈ 0 across all group sizes
#    Normal distribution assumption rejected.
#    Fat tails not captured - consistent with kurtosis
#    finding (post-2008 Kurt = 11.97).
#    Student-t or skew-t errors required.
#
# 4. Ljung-Box Residuals: Lag[1] p = 0.007
#    Residual serial correlation remains.
#    Mean model misspecified — ARFIMA or HAR structure
#    would improve fit.
#
# IMPLICATION: 
# Standard GARCH(1,1) with normal errors
# fails on all dimensions relevant to this project.
# Each failure mode corresponds directly to a feature
# of the Bayesian hierarchical model:
# Asymmetry -> GJR extension or leverage prior
# Non-stationarity -> regime-switching structure
# Fat tails -> Student-t innovations
# Serial correlation -> richer mean specification
#
# This output constitutes the empirical motivation
# for the full model; not just theoretical preference
# but formal statistical rejection of the baseline.

# GARCH vs VIX
# Average GARCH vol: 16.76%
# Average VIX: 19.85%
# Average VRP (VIX - GARCH): 3.09 percentage points
# Consistent with Bollerslev, Tauchen and Zhou (2009)
# canonical VRP documentation.
#
# VISUAL:
# 1. GARCH lags VIX at crisis peaks - slow reaction
#    confirmed visually. Beta1 = 0.856 persistence
#    anchors estimates to recent history, preventing
#    rapid regime transition response.
#
# 2. GARCH underreacts to moderate stress in calm
#    periods (trading days ~3500-4500). VIX shows
#    episodic spikes GARCH fails to capture.
#    Regime insensitivity of single-regime model
#    made visually explicit.
#
# 3. Pre-2000 GARCH > VIX - dot-com realised vol
#    exceeded implied vol, suggesting different
#    volatility regime character versus GFC/ COVID.
#    Fear premium less pronounced in dot-com era.
#
# 4. Crisis peak magnitudes broadly comparable but
#    GARCH peaks rounder and more sustained...
#    slower rise, slower decay versus VIX sharp
#    spike and rapid mean reversion.
#
# IMPLICATION: 
# Single-regime GARCH with high
# persistence is structurally unable to:
# (i) React rapidly to regime transitions
# (ii) Capture moderate stress in calm regimes
# (iii) Produce calibrated tail uncertainty
# All three failures motivate hierarchical
# Bayesian extension with regime switching.
