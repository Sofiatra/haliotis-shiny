# Haliotis Shiny — Interactive Biological Data Analysis

Interactive R/Shiny application for exploring, transforming, visualizing and statistically analysing morphometric data from abalone (*Haliotis*).

## Overview

This project demonstrates an end-to-end interactive data-analysis workflow in R, from dataset import and quality inspection to exploratory statistics, visualization and report generation.

The application is designed as a modular Shiny project rather than a single monolithic script.

## Main features

- Import CSV/TSV datasets with configurable delimiters
- Interactive data preview, structure and summary
- Automatic inspection of variable types, unique values and missing values
- Derived age categories from the number of rings
- Column selection and row filtering
- Exploratory visualizations with ggplot2 and Plotly
- Correlation analysis and correlation heatmaps
- PCA in 2D and 3D
- ANOVA and Pearson correlation tests
- Interactive filtered data tables and CSV export
- Multilingual import interface (French, English and Italian)
- R Markdown report generation

## Application workflow

```text
Import data
    ↓
Data overview & quality checks
    ↓
Transformation & filtering
    ↓
Exploratory visualization
    ↓
Statistical analysis
    ↓
Interactive results
    ↓
Report generation
```

## Project structure

```text
haliotis-shiny/
├── app.R
├── modules/
│   ├── import_module.R
│   ├── data_overview.R
│   ├── data_transformation.R
│   ├── filtering_module.R
│   ├── plotting_module.R
│   ├── results_display.R
│   └── report_module.R
├── data/
├── www/
└── report_template.Rmd
```

## Technologies

- R
- Shiny
- tidyverse
- ggplot2
- Plotly
- DT
- corrplot
- ggvenn
- R Markdown / knitr
- bslib / Shiny themes

## Biological data context

The application works with morphometric and weight measurements such as length, diameter, height, whole weight, shucked weight, viscera weight, shell weight and number of rings. An age category is derived from the ring count for exploratory analyses.

The repository is intended as a portfolio demonstration of interactive biological data analysis and does not contain confidential or proprietary data.

## Reproducibility

The application includes a small synthetic default dataset so that the main interface can be explored without providing an external dataset. Users can also import their own compatible CSV/TSV data through the interface.

## Portfolio skills demonstrated

- R programming and data manipulation
- Interactive data analysis with Shiny
- Statistical exploratory analysis
- Data visualization
- PCA and correlation analysis
- Modular application design
- Reproducible reporting
- Biological data interpretation

## Future improvements

Potential extensions include formalizing the data-validation layer, adding automated tests, documenting the expected input schema, improving internationalization across all modules and adding a reproducible package environment with `renv`.
