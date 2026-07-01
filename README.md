# Earthquake Data Explorer

An interactive R Shiny application for exploring the built-in `quakes` 
dataset (Fiji earthquake data) through descriptive statistics, 
correlation analysis, distribution exploration, and regression modeling.

🔗 **[Live Demo](https://phantomjelli.shinyapps.io/EarthquakeDataExplorer/)**

## Overview

This app was built as a proof-of-concept to demonstrate how exploratory 
data analysis, statistical inference, and regression modeling can be 
combined into a single interactive tool using Shiny. It's designed for 
exploring the dataset's structure and relationships, not for real-world 
seismic forecasting.

## Features

- **Exploratory Data Analysis** — descriptive statistics, a correlation 
  heatmap, and interactive scatterplots/histograms with optional 
  regression lines and density curves
- **Distribution Explorer** — maps values to percentiles and percentiles 
  to values interactively, with single or range-based quantile selection
- **Statistical Tests** — one-sample t-tests with configurable alternative 
  hypotheses, plus correlation testing (Pearson, Spearman, Kendall) with 
  method-specific visualizations
- **Regression** — builds multiple linear regression models with 
  user-selected predictors and interaction terms, showing observed-vs-
  predicted diagnostics and full model summaries

## Tech Stack

R · Shiny · ggplot2 · shinythemes

## Run Locally

```r
# Install dependencies
install.packages(c("shiny", "ggplot2", "modeldata", "shinythemes"))

# Run the app
shiny::runApp("app.R")
```

## Data

Uses the `quakes` dataset (Fiji earthquake locations, 1000+ seismic 
events) built into R's `modeldata` package.
