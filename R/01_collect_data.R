# 01_collect_data.R
# VIX and SPY data collection

library(quantmod)
library(moments)

# --- 1. Data pull ---
# A. Pull VIX
getSymbols("^VIX", from = "2000-01-01", to = Sys.Date())
# B. Pull SPY
getSymbols("SPY", from = "2000-01-01", to = Sys.Date())

# Save raw data
saveRDS(VIX, "data/raw/VIX_raw.rds")
saveRDS(SPY, "data/raw/SPY_raw.rds")

# Check
cat("Data pulled successfully\n")
file.exists("data/raw/VIX_raw.rds")
file.exists("data/raw/SPY_raw.rds")

# --- 2. Data check and structure ---
# Load and inspect
VIX <- readRDS("data/raw/VIX_raw.rds")
SPY <- readRDS("data/raw/SPY_raw.rds")

# Structure
head(VIX)
head(SPY)

# How many trading days?
nrow(VIX) ## ~6000

# Basic VIX plot
plot(VIX$VIX.Close, main = "VIX 2000-present", ylab = "VIX", col = "darkblue")

# Basic SPY returns
SPY_returns <- dailyReturn(SPY$SPY.Adjusted)
plot(SPY_returns, main = "SPY Daily Returns", col = "darkred")

# Save returns to processed
saveRDS(SPY_returns, "data/processed/SPY_returns.rds")

# Merge VIX and SPY returns
combined <- merge(VIX$VIX.Close, SPY_returns)
colnames(combined) <- c("VIX", "SPY_return")

# Plot relationship
plot(as.numeric(combined$SPY_return), 
     as.numeric(combined$VIX),
     xlab = "SPY Daily Return",
     ylab = "VIX Level",
     main = "SPY Returns vs VIX Level",
     pch = 16, cex = 0.3, col = "darkblue")

# Add LOESS smoother - asymmetric shape of SPY-VIX relationship
## Convert to numeric vectors first
spy_ret <- as.numeric(combined$SPY_return)
vix_level <- as.numeric(combined$VIX)

## Remove any NAs
complete_idx <- complete.cases(spy_ret, vix_level)
spy_ret <- spy_ret[complete_idx]
vix_level <- vix_level[complete_idx]

## Plot with LOESS smoother
plot(spy_ret, vix_level,
     xlab = "SPY Daily Return",
     ylab = "VIX Level",
     main = "SPY Returns vs VIX — LOESS Trend",
     pch = 16, cex = 0.3, col = "grey60")

## Add LOESS line
loess_fit <- loess(vix_level ~ spy_ret, span = 0.3)
spy_seq <- seq(min(spy_ret), max(spy_ret), length.out = 300)
loess_pred <- predict(loess_fit, newdata = data.frame(spy_ret = spy_seq))
lines(spy_seq, loess_pred, col = "darkred", lwd = 2)

# Add time dimension
## Colour by year to see regime variation
years <- as.numeric(format(index(combined)[complete_idx], "%Y"))
year_colours <- colorRampPalette(c("blue", "red"))(length(unique(years)))
point_colours <- year_colours[match(years, sort(unique(years)))]

plot(spy_ret, vix_level,
     xlab = "SPY Daily Return",
     ylab = "VIX Level",
     main = "SPY Returns vs VIX — by Year",
     pch = 16, cex = 0.3, col = point_colours)

legend("topright", legend = c("2000", "2012", "2024"),
       fill = c("blue", "purple", "red"), cex = 0.7)

# Split data
pre_crisis <- combined[index(combined) < "2008-01-01"]
crisis_and_after <- combined[index(combined) >= "2008-01-01"]

# Pre 2008 clean
spy_pre <- as.numeric(pre_crisis$SPY_return)
vix_pre <- as.numeric(pre_crisis$VIX)
complete_pre <- complete.cases(spy_pre, vix_pre)
spy_pre_clean <- spy_pre[complete_pre]
vix_pre_clean <- vix_pre[complete_pre]

# Post 2008 clean
spy_post <- as.numeric(crisis_and_after$SPY_return)
vix_post <- as.numeric(crisis_and_after$VIX)
complete_post <- complete.cases(spy_post, vix_post)
spy_post_clean <- spy_post[complete_post]
vix_post_clean <- vix_post[complete_post]

# Plot side by side
par(mfrow = c(1, 2))

# Pre 2008 plot
plot(spy_pre_clean, vix_pre_clean,
     xlab = "SPY Return", ylab = "VIX",
     main = "Pre-2008",
     pch = 16, cex = 0.3, col = "grey60")
loess_pre <- loess(vix_pre_clean ~ spy_pre_clean, span = 0.3)
seq_pre <- seq(min(spy_pre_clean), max(spy_pre_clean), length.out = 200)
pred_pre <- predict(loess_pre, newdata = data.frame(spy_pre_clean = seq_pre))
lines(seq_pre, pred_pre, col = "darkblue", lwd = 2)

# Post 2008 plot
plot(spy_post_clean, vix_post_clean,
     xlab = "SPY Return", ylab = "VIX",
     main = "2008-Present",
     pch = 16, cex = 0.3, col = "grey60")
loess_post <- loess(vix_post_clean ~ spy_post_clean, span = 0.3)
seq_post <- seq(min(spy_post_clean), max(spy_post_clean), length.out = 200)
pred_post <- predict(loess_post, newdata = data.frame(spy_post_clean = seq_post))
lines(seq_post, pred_post, col = "darkred", lwd = 2)

par(mfrow = c(1, 1))

par(mfrow = c(1, 2))

hist(vix_pre_clean, 
     breaks = 50, 
     main = "VIX Distribution Pre-2008",
     xlab = "VIX Level",
     col = "steelblue",
     border = "white")

hist(vix_post_clean,
     breaks = 50,
     main = "VIX Distribution Post-2008", 
     xlab = "VIX Level",
     col = "darkred",
     border = "white")

par(mfrow = c(1, 1))

# Summary statistics by period
cat("=== PRE-2008 VIX STATISTICS ===\n")
cat("Mean:", round(mean(vix_pre_clean), 2), "\n")
cat("Median:", round(median(vix_pre_clean), 2), "\n")
cat("SD:", round(sd(vix_pre_clean), 2), "\n")
cat("Skewness:", round(moments::skewness(vix_pre_clean), 3), "\n")
cat("Kurtosis:", round(moments::kurtosis(vix_pre_clean), 3), "\n")
cat("90th percentile:", round(quantile(vix_pre_clean, 0.90), 2), "\n")
cat("99th percentile:", round(quantile(vix_pre_clean, 0.99), 2), "\n")

cat("\n=== POST-2008 VIX STATISTICS ===\n")
cat("Mean:", round(mean(vix_post_clean), 2), "\n")
cat("Median:", round(median(vix_post_clean), 2), "\n")
cat("SD:", round(sd(vix_post_clean), 2), "\n")
cat("Skewness:", round(moments::skewness(vix_post_clean), 3), "\n")
cat("Kurtosis:", round(moments::kurtosis(vix_post_clean), 3), "\n")
cat("90th percentile:", round(quantile(vix_post_clean, 0.90), 2), "\n")
cat("99th percentile:", round(quantile(vix_post_clean, 0.99), 2), "\n")

# =============================================================
# KEY DESCRIPTIVE FINDINGS — EXPLORATORY ANALYSIS
# VIX Distributional Properties: Pre vs Post 2008
# =============================================================

# Pre-2008: Mean = 19.61, Median = 18.66, SD = 6.88, 
#           Skew = 0.783, Kurt = 3.146,
#           90th pct = 29.18, 99th pct = 38.97
#
# Post-2008: Mean = 19.95, Median = 17.54, SD = 8.89, 
#            Skew = 2.472, Kurt = 11.974,
#            90th pct = 29.77, 99th pct = 57.09

# ---------------------------------------------------------
# INTERPRETATION
# ---------------------------------------------------------

# MEAN (19.61 vs 19.95): Near-identical means across periods
# mask fundamentally different distributional structures.
# The mean is not a sufficient statistic for VIX behaviour
# and should not be used as a standalone risk benchmark.

# MEDIAN (18.66 vs 17.54): Lower post-2008 median confirms
# that the typical trading day is actually calmer post-GFC
# than pre-GFC. Conventional narrative of uniformly elevated
# post-2008 fear is not supported by the median.

# MEAN-MEDIAN GAP: Pre-2008 gap = 0.95pts. 
#                 Post-2008 gap = 2.41pts.
# The widening gap directly reflects greater right tail
# influence on the mean post-2008. Extreme events are pulling
# the mean away from the typical experience more strongly.

# SD (6.88 vs 8.89): 29% increase in dispersion post-2008.
# The distribution is substantially more stretched — greater
# distance between calm baseline and crisis peaks. Reflects
# the bimodal character of post-2008 volatility regimes.

# SKEWNESS (0.783 vs 2.472): 3x increase in right tail
# asymmetry. Pre-2008 distribution was approximately
# symmetric with mild positive skew — consistent with
# near-Gaussian behaviour that standard models assume.
# Post-2008 skewness of 2.472 is severely non-Gaussian.
# Symmetric models — GARCH with normal errors, standard
# Black-Scholes — are structurally misspecified for the
# post-2008 regime. Student-t or skew-t errors required.

# KURTOSIS (3.146 vs 11.974): Pre-2008 kurtosis of 3.146
# is near-Gaussian (normal = 3.0) — fat tails were modest
# and standard models were reasonable approximations.
# Post-2008 kurtosis of 11.974 represents extreme
# leptokurtosis — tail events are approximately 4x more
# probable than a Gaussian distribution would predict.
# This is the statistical signature of crisis regimes
# that are rare but catastrophic when they occur.
# Direct empirical justification for regime-switching
# structure — a single distributional assumption cannot
# accommodate kurtosis this elevated alongside near-normal
# pre-2008 behaviour.

# 90th PERCENTILE (29.18 vs 29.77): Near-identical.
# The upper range of normal market stress is unchanged
# across periods. Moderate tail events are no more extreme
# post-2008 than before.

# 99th PERCENTILE (38.97 vs 57.09): 46% increase in
# extreme tail outcomes. The worst 1% of days post-2008
# are dramatically more severe than the worst 1% pre-2008.
# Combined with near-identical 90th percentiles, this
# confirms the tail risk is concentrated in the extreme
# quantiles — not a uniform shift but a specific fattening
# of the far right tail. Risk models calibrated to 95th or
# 99th VaR using pre-2008 data will systematically
# underestimate true tail exposure by approximately 46%.

# ---------------------------------------------------------
# CORE FINDING
# ---------------------------------------------------------

# Post-2008 VIX exhibits a bimodal regime structure absent
# pre-2008: long periods of sub-median calm (lower median)
# punctuated by dramatically more extreme spikes (99th pct
# +46%, kurtosis +280%). The same mean masks a fundamental
# distributional shift from near-Gaussian (Kurt = 3.15) to
# severely leptokurtic (Kurt = 11.97).
#
# Implication 1 (Model): Non-Gaussian, non-stationary
# distribution empirically necessitates regime-switching
# model with Student-t errors. GARCH with normal errors
# is structurally misspecified for post-2008 data.
#
# Implication 2 (Policy): Risk frameworks calibrated on
# pre-2008 VIX data underestimate 99th percentile outcomes
# by ~46%. Sophisticated volatility instruments encode
# pre-GFC tail behaviour that is no longer representative.
# Stability of the calibration period is a poor guide to
# tail risk in the subsequent period — consistent with
# Minsky (1986) and Beck (1992) risk society arguments.
#
# Implication 3 (VRP): The variance risk premium is not
# stable across regimes. Near-identical 90th percentiles
# but divergent 99th percentiles suggest the premium is
# specifically concentrated in extreme quantiles —
# options sellers demand compensation primarily for
# catastrophic tail exposure, not moderate stress.
# This regime-dependence of the VRP is the central
# estimand of the modelling phase.

# =============================================================
# Pull OVX (Oil Volatility Index - Middle East risk proxy)
getSymbols("^OVX", from = "2008-01-01", to = Sys.Date())
saveRDS(OVX, "data/raw/OVX_raw.rds")

# Quick comparison plot
par(mfrow = c(2, 1))
plot(VIX$VIX.Close, main = "VIX 2008-Present", col = "darkblue", ylab = "VIX")
plot(OVX$OVX.Close, main = "OVX 2008-Present", col = "darkorange", ylab = "OVX")
par(mfrow = c(1, 1))

# Realised Variance - first steps
# Step 1: Compute squared daily returns as simple RV proxy
SPY_returns <- readRDS("data/processed/SPY_returns.rds")
RV_daily <- SPY_returns^2

# Step 2: 22-day rolling realised variance (approx 1 month)
library(zoo)
RV_22day <- rollapply(RV_daily, width = 22, FUN = sum, align = "right", fill = NA)

# Step 3: Annualise
RV_22day_ann <- RV_22day * 252

# Step 4: VRP = VIX implied variance minus realised variance
VIX_subset <- VIX$VIX.Close["2008-01-01/"]
VIX_variance <- (VIX_subset/100)^2 * 252

# Align dates
combined_vrp <- merge(VIX_variance, RV_22day_ann)
colnames(combined_vrp) <- c("VIX_implied_var", "Realised_var")

# Step 5: Compute VRP
combined_vrp$VRP <- combined_vrp$VIX_implied_var - combined_vrp$Realised_var

# Save
saveRDS(combined_vrp, "data/processed/VRP_estimates.rds")

# Plot VRP over time
plot(combined_vrp$VRP, 
     main = "Variance Risk Premium 2008-Present",
     ylab = "VRP (Implied - Realised Variance)",
     col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

cat("OVX and VRP pipeline complete\n")

# Check for negative VRP
cat("Min VRP:", min(combined_vrp$VRP, na.rm = TRUE), "\n")
cat("Days negative:", sum(combined_vrp$VRP < 0, na.rm = TRUE), "\n")

# Forward-aligned RV - what actually happened 
# over the NEXT 22 days after each VIX observation
RV_forward <- rollapply(RV_daily, width = 22, 
                        FUN = sum, 
                        align = "left",  # changed from "right" to "left"
                        fill = NA)

RV_forward_ann <- RV_forward * 252

# Rebuild VRP with forward-aligned RV
VIX_subset2 <- VIX$VIX.Close["2008-01-01/"]
VIX_variance2 <- (VIX_subset2/100)^2 * 252

combined_vrp2 <- merge(VIX_variance2, RV_forward_ann)
colnames(combined_vrp2) <- c("VIX_implied_var", "Realised_var_forward")
combined_vrp2$VRP_forward <- combined_vrp2$VIX_implied_var - combined_vrp2$Realised_var_forward

# Check
cat("Min VRP forward:", min(combined_vrp2$VRP_forward, na.rm = TRUE), "\n")
cat("Days negative:", sum(combined_vrp2$VRP_forward < 0, na.rm = TRUE), "\n")

# Plot comparison
par(mfrow = c(2, 1))
plot(combined_vrp$VRP,
     main = "VRP - Backward Aligned (original)",
     col = "darkgreen", ylab = "VRP")
abline(h = 0, col = "red", lty = 2)

plot(combined_vrp2$VRP_forward,
     main = "VRP - Forward Aligned (theoretically correct)",
     col = "darkblue", ylab = "VRP")
abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1, 1))

saveRDS(combined_vrp2, "data/processed/VRP_forward_estimates.rds")

# NOTE: Backward-aligned RV (align = "right") produces 
# structurally positive VRP due to measurement lag —
# VIX leads realised volatility during crisis onset.
# Minimum backward VRP = 2.03, never crosses zero,
# confirming the lag prevents true sign-switching.
#
# Forward-aligned RV (align = "left") is theoretically 
# correct — compares VIX implied variance on day T to 
# realised variance over subsequent 22 trading days.
# Forward VRP approaches and crosses zero briefly
# in the post-COVID recovery period spanning late 
# 2020 into 2021 — realised volatility temporarily 
# exceeded VIX implied variance as the recovery 
# was more turbulent than the market anticipated
# at each forward observation point.
#
# Key observation: Both series share broadly similar
# shape and crisis timing — same dominant spikes,
# same calm baseline periods. The 2020 spike magnitudes
# are near-identical across both series. The 2008 spike
# appears slightly larger in the forward-aligned series,
# consistent with realised volatility continuing to 
# accumulate beyond the initial VIX peak during the
# prolonged GFC stress period.
#
# Notable: 2008 and 2020 dominate both series as the
# two principal crisis regimes in the sample period,
# with all other stress episodes producing materially
# smaller VRP spikes — consistent with the bimodal
# regime structure identified in the descriptive analysis.
#
# Current (2024-2025) VRP elevated above post-2016 
# calm baseline in both series — consistent with 
# elevated macro uncertainty around tariffs, Middle 
# East risk, and Fed policy path.
#
# Limitation: forward alignment introduces look-ahead 
# bias in any real-time trading application — 
# model-implied posterior predictive variance is the 
# correct forward-looking comparison in the full model,
# replacing the naive rolling RV measure entirely.




