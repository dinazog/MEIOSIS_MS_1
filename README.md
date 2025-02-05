# MEIOSIS_MS_1
First manuscript of the project MEIOSIS 
**Museum spEcImens shed light in biOdiverSIty Shrinkage**


# Butterfly Wing Length Analysis

This repository contains R scripts and data for analyzing long-term temporal trends in butterfly wing lengths, using Generalized Additive Models (GAMs).

## 📊 **Project Overview**

This project investigates long-term trends in butterfly body size in response to climate change. Using computer vision techniques on historical data from two major museum collections (593 species across 10 biomes over a century), we measured forewing length as a proxy for body size. Generalized additive models revealed a significant decline in butterfly body size over time, with more pronounced reductions in warmer, drier biomes. These findings support the temperature-size rule and highlight potential ecological impacts of climate change on butterfly populations.

## 📁 **Repository Structure**

- `scripts/`: R scripts for data preprocessing, model fitting, and visualization
- `data/`: Raw and processed datasets
- `output/`: Model outputs and visualizations

## 🗂️ **Data Sources**

- **PeriodsAverages.csv**: Average temperature periods
- **newdfALL.csv**: Specimen data with environmental covariates
- **newdfALL_tmax_Periods.csv**: Merged dataset for analysis

## 🚀 **Getting Started**

### **1. Prerequisites**

Ensure you have R installed. Install the required packages:

```R
install.packages(c("mgcv", "MASS", "stringr", "gamm4", "tidyr", 
                   "ggplot2", "ggthemes", "viridis", "cowplot", 
                   "kableExtra", "knitr", "tibble", "dplyr", 
                   "gratia", "latex2exp", "itsadug", "ggdist", 
                   "missRanger"))
```

### **2. Running the Scripts**

- Load all necessary libraries and datasets.
- Perform data cleaning and preprocessing.
- Fit GAMs (Model 1 and Model 2) to analyze trends.
- Generate plots for visual interpretation.

Run the analysis with:

```R
source("scripts/wing_length_analysis.R")
```

## 📊 **Statistical Models**

### **Model 1: Temporal Trends in Wing Length**

- **Response Variable:** Corrected wing length (`length_cor`)
- **Predictors:** Year, BIOME, GenusSpecies (random effect)
- **Model Type:** Generalized Additive Model (GAM) with REML

### **Model 2: BIOME-Specific Effects**

- Explores wing length trends across different BIOMEs.
- Includes nested random effects to account for species-specific variation.

## 📈 **Key Visualizations**

- Partial effects of year on wing length
- BIOME-specific trends over time
- Post-hoc comparisons between BIOMEs

## 🧪 **Model Diagnostics**

Model assumptions are checked using:

- Residual diagnostics (`gam.check()`)
- Simulated appraisals (`appraise()`)
- Smoothness checks (`k.check()`)

## 💡 **Contributing**

Contributions are welcome! Please fork the repository, make changes, and submit a pull request.

## 📜 **License**

This project is licensed under the MIT License.
