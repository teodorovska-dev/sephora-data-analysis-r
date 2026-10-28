# Sephora Product Data Analysis & Visualization

Exploratory data analysis of **Sephora product prices and ratings** using **R** and **ggplot2**. The project investigates price distributions, differences between beauty product categories, Sephora exclusivity, and the relationship between price and product ratings.

## Dataset

**Source:** [Sephora Products and Skincare Reviews — Kaggle](https://www.kaggle.com/datasets/nadyinky/sephora-products-and-skincare-reviews/data)

The analysis uses `product_info.csv`, which contains **8,494 products and 27 original variables**. Twelve variables were selected for data preparation and analysis, including product prices, ratings, review counts, categories, and Sephora exclusivity status.

The dataset is **not included** in this repository; download it from Kaggle to reproduce the analysis.

## Analysis

- **Data preparation:** type validation, missing-value assessment, and selection of relevant variables.
- **Descriptive statistics:** central tendency and variability of prices and ratings, including category-level comparisons.
- **Visualizations:** histograms, density plots, boxplots, and violin plots implemented with **Base R** and **ggplot2**.
- **Relationship analysis:** price–rating scatter plot and **Spearman's rank correlation**.

For category comparisons, categories with at least **100 products** were retained. Rating comparisons use products with at least **10 reviews**; these are practical analysis thresholds, not formal reliability guarantees.

## Key Findings

- Product prices are right-skewed: the **median is $35**, while the **mean is $51.66**.
- **Fragrance** has the highest category median price (**$80**); **Mini Size** has the lowest (**$18**).
- Within **Skincare**, Sephora-exclusive products have a lower median price (**$34 vs. $49**) but a slightly higher median rating (**4.38 vs. 4.29**) than non-exclusive products.
- The price–rating relationship is **weakly positive** (**Spearman's ρ = 0.163**), so price alone is not a strong indicator of product ratings.

These findings describe associations in the dataset and do not establish causation.

## How to Run

1. Install **R** and **RStudio**.
2. Download the dataset from the Kaggle link above and place `product_info.csv` at `data/raw/product_info.csv` relative to the project directory.
3. Install the plotting package in R: `install.packages("ggplot2")`.
4. Open the project directory in RStudio and run `Lab2.R` (or execute `source("Lab2.R")` from that directory).

The script saves the prepared data to `data/processed/`, statistical summaries to `results/`, and generated figures to `results/plots/`.

## Repository Contents

- `Lab2.R` — complete R script for data preparation, analysis, and visualization.
- `README.md` — project overview and instructions.

**Tools:** R · RStudio · ggplot2 · Base R
