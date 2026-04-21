# vrp-volatility-model
## Bayesian Volatility Modelling and the Variance Risk Premium
This project develops a Bayesian framework for modelling equity volatility with emphasis on predictive distribution calibration rather than point forecast accuracy. 

Standard volatility models provide conditional variance estimates but offer limited characterisation of uncertainty. For risk management, regulatory capital and policy applications, the full predictive distribution of future realised variance (including tail behaviour) is the decision-relevant object.

The project evaluates whether stochastic volatility models with heavy tails, leverage effects, regime structure and hierarchical extensions produce better-calibrated predictive densities than standard benchmarks. It also examines whether the gap between model-implied and option-implied volatility can serve as a model-based proxy for the variance risk premium.

## Research questions
1. Do Bayesian stochastic volatility models improve predictive density calibration relative to standard volatility models?
2. What features (heavy tails, asymmetry, regime structure, hierarchical priors) contribute most to calibration?
3. Can the discrepancy between model-implied posterior predictive distributions and option-implied volatility be interpreted as a proxy for the variance risk premium?

## Data
- Implied volatility: VIX, OVX (CBOE)
- Equity returns: SPY (via quantmod)
- Realised variance: constructed from WRDS TAQ intraday data (in progress)
- Macro covariates: FRED (in development)

Large datasets (e.g., TAQ) are not stored in this repository.

## Methodology
Models are compared in an ablation sequence to isolate the contribution of individual features:
- Single-regime stochastic volatility (baseline)
- SV with Student-t errors (heavy tails)
- SV with leverage (asymmetry)
- SV with regime switching
- Hierarchical regime-switching SV (primary contribution)

Bayesian estimation uses Hamiltonian Monte Carlo via Stan.
For reference against standard approaches, additional benchmarks include GARCH-family models and HAR-RV.

## Evaluation
Model performance is evaluated using distributional metrics:
- Log predictive scores (LOO-CV)
- Probability integral transform (PIT) diagnostics
- Continuous ranked probability score (CRPS)
- Tail behaviour and coverage properties

The focus is on calibration and distributional accuracy, not point forecast performance.

## Structure
```text
data/
  raw/        # reproducible data inputs
  processed/  # lightweight intermediates (no model fits)

R/
  01_collect_data.R    # data pipeline
  03_model_baselines.R # GARCH and HAR-RV benchmarks
  04_model_bayesian.R  # SV ablation sequence and hierarchical model
  05_evaluate.R        # calibration evaluation

outputs/
  figures/
  tables/
```
Model fit objects (.rds) are excluded from version control due to size. Results are reproducible from scripts.

## Scope and limitations
This project is scoped to single-asset analysis of S&P 500 equity volatility. 

It does not address: 
- multi-asset hierarchical structure
- multi-horizon forecasting
- structural identification of the variance risk premium.
These are identified as directions for subsequent work.

## Reproducibility
1. Run `R/01_collect_data.R`
2. Run `R/03_model_baselines.R`
3. Run `R/04_model_bayesian.R`
4. Run `R/05_evaluate.R`

## Status
Work in progress. Benchmark models implemented. Bayesian hierarchical regime-switching model under development.
