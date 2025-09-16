# Portfolio Optimization Project

This project explores how to construct optimized portfolios using historical data from five global mutual funds. The goal is to use risk-return analysis and optimization techniques to evaluate portfolio combinations — with and without short-selling.

## 🔍 Project Overview

Using R, we:
- Download 5 years of monthly closing prices (June 2019 – June 2024) from Yahoo Finance
- Compute returns, variances, and covariances
- Construct minimum variance and tangency portfolios
- Visualize the efficient frontier and optimal asset allocations
- Evaluate portfolios with and without short-selling constraints

## 💾 Assets Analyzed

- `VFINX` – S&P 500 Index (US)
- `VEURX` – European Stock Index
- `VEIEX` – Emerging Markets Fund
- `VBISX` – Short-Term Bond Index
- `VPACX` – Pacific Stock Index

## 🧰 Files

- `portfolio_optimization.R`: Main R script for data pull, analysis, optimization, and visualization
- `portfolio_noshorts.r`: Functions for portfolio construction without short-selling
- `project_summary.docx`: Group report with findings and explanations

## 📈 Tools & Packages

- Language: **R**
- Packages used: `quantmod`, `PerformanceAnalytics`, `quadprog`, `ggplot2`, `tseries`

## 👥 Contributors

- **Christian Ortiz** (GitHub maintainer)
- **Joshua Singh**
- **Gianluca David**
