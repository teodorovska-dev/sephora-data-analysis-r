# Sephora Product Data Analysis & Visualization

Exploratory data analysis of **Sephora product prices and ratings** using **R**, **Base R**, and **ggplot2**. The project explores price distributions, differences across beauty product categories, Sephora exclusivity, and the relationship between price and product ratings.

## Dataset

**Source:** [Sephora Products and Skincare Reviews — Kaggle](https://www.kaggle.com/datasets/nadyinky/sephora-products-and-skincare-reviews/data)

The analysis uses `product_info.csv`, containing **8,494 products and 27 original variables**. Twelve relevant variables were retained, including prices, ratings, review counts, product categories, and Sephora exclusivity status. Each row represents one product.

The dataset is **not included** in this repository. Download it from Kaggle to reproduce the analysis.

## Analysis

- **Data preparation:** variable selection, data-type and validity checks, and missing-value assessment.
- **Descriptive statistics:** central tendency and variability of prices and ratings, including comparisons across product categories.
- **Price distributions:** histograms using equal-width bins on a `log10(price)` scale. The **overall price histograms display prices up to $500** for readability; all valid prices, including those above $500, are retained in statistical calculations.
- **Density comparisons:** kernel density estimates of skincare prices by Sephora exclusivity, supplemented by **two-panel histogram and density visualizations**. The groups use common bin boundaries, smoothing bandwidth, and axis scales; their distributions are normalized separately.
- **Category comparisons:** boxplots of prices and violin plots of ratings, implemented in **Base R** and **ggplot2**.
- **Relationship analysis:** a price–rating scatter plot, **Spearman's rank correlation**, and an additional skincare rating comparison by exclusivity.

Category comparisons focus on groups with at least **100 products**. Rating comparisons include products with at least **10 reviews**. These are practical analysis thresholds, not formal reliability guarantees.

## Key Findings

- Prices are right-skewed: the **median is $35**, compared with a **mean of $51.66**.
- **Fragrance** has the highest category median price (**$80**) and price IQR (**$89**); **Mini Size** has the lowest (**$18**) and smallest IQR (**$9**).
- Within **Skincare**, Sephora-exclusive products have a lower median price (**$34 vs. $49**) but a slightly higher median rating (**4.38 vs. 4.29**) than non-exclusive products.
- Price and rating have a **weak positive association** (**Spearman's ρ = 0.163**); price alone is not a strong indicator of rating.

These are descriptive findings and do **not** establish causation.

## How to Run

1. Install **R** and **RStudio**.
2. Download the dataset from Kaggle and place `product_info.csv` in `data/raw/` within the project directory.
3. Install ggplot2 in R using `install.packages("ggplot2")`.
4. Open the project directory in RStudio and run `Lab2.R` (or execute `source("Lab2.R")` from the project directory).

The script saves prepared data to `data/processed/`, analysis outputs to `results/`, and generated plots to `results/plots/`.

## Repository Contents

- `Lab2.R` — R script for the full analysis and visualizations.
- `README.md` — project overview, findings, and reproduction instructions.

**Tools:** R · RStudio · Base R · ggplot2
