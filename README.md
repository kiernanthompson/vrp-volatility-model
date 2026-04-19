# vrp-volatility-model
## Bayesian Volatility Modelling and the Variance Risk Premium
A bayesian hierarchical framework for uncertainty quantification in equity volatility markets. 
This project develops and evaluates a regime-shifting stochastic volatility model as a correctly specified alternative to single-regime GARCH-class models, evalauted on calibration rather than point forecast accuracy.
## Application
Standard volatility models produce joint point estimates with limited uncertainty characterisation. For risk management, regulatory capital and policy applications, the full predictive distribution over future realised variance matters more than central tendency. This project examines whether Bayesian hierarchical models with regime-switching structure produce better "calibrated" posterior predictive distributions than existing benchmarks, while characterising the variance risk as a time-varying posterior discrepancy between market-implied and model-implied uncertainty.
## Structure
- 'data/raw/' original data pulls (e.g., VIX, SPY, OVX)
- 'data/processed/' cleaned data and model outputs
- 'R/01_collect_data.R' data pipeline
- 'R/03_model.R' benchmark model ladder
- 'R/04_simulate.R' Bayesian hierarchical model (in development)
- 'outputs/' figures and diagnostic output
## Current status
Benchmark model ladder established:
- GARCH(1,1) with normal errors
- GJR-GARCH(1,1) with normal errors
- GJR-GARCH(1,1) with Student-t errors
- HAR-RV (daily returns RV proxy; WRDs intraday)
- Markov-switching GARCH (in development)
- Single-regime Bayesian stochastic volatility (in development)
- Bayesian hierarchical regime-switching (in development)
## Data sources
- CBOE VIX and OVX via quantmod
- SPY returns via quantmod
- Realised variance (TBD)
- WRDS TAQ intraday data
- FRED macro covariates (TBD)
## Methods
Bayesian estimation via Stan/brms. Hamiltonian Monte Carlo for posterior inference. Model comparison via LOO-CV, calibration evalaution via PIT histograms and probability integral transform diagnostics.
## Status
Work in progress. Not yet complete paper.
