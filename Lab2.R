# LABORATORY WORK No. 2 — DATA ANALYSIS AND VISUALIZATION
# Dataset: https://www.kaggle.com/datasets/nadyinky/sephora-products-and-skincare-reviews/data


# STAGE 1 — DATA PREPARATION

# Initialize workspace and load source data

rm(list = ls())
graphics.off()
options(stringsAsFactors = FALSE, scipen = 999)

raw_path <- file.path("data", "raw", "product_info.csv")
processed_dir <- file.path("data", "processed")
results_dir <- "results"

if (!file.exists(raw_path)) {
  stop("File not found: data/raw/product_info.csv. Check your RStudio project and file location.")
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

sephora_raw <- read.csv(
  raw_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA"),
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

cat("Original dataset:", nrow(sephora_raw), "rows and",
    ncol(sephora_raw), "columns\n\n")

str(sephora_raw)

print(head(
  sephora_raw[, seq_len(min(6L, ncol(sephora_raw))), drop = FALSE],
  5
))


# Select variables and standardize data types

selected_columns <- c(
  "product_id", "product_name", "brand_name",
  "primary_category", "secondary_category",
  "price_usd", "rating", "reviews", "loves_count", "child_count",
  "sephora_exclusive", "online_only"
)

missing_columns <- setdiff(selected_columns, names(sephora_raw))

if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

sephora <- sephora_raw[, selected_columns, drop = FALSE]

text_columns <- c(
  "product_id", "product_name", "brand_name",
  "primary_category", "secondary_category"
)

for (column in text_columns) {
  values <- trimws(as.character(sephora[[column]]))
  values[!is.na(values) & values == ""] <- NA_character_
  sephora[[column]] <- values
}

numeric_columns <- c(
  "price_usd", "rating", "reviews", "loves_count", "child_count"
)

valid_numeric_types <- vapply(
  sephora[numeric_columns], is.numeric, logical(1)
)

if (!all(valid_numeric_types)) {
  stop("Check numeric column types: ",
       paste(names(valid_numeric_types)[!valid_numeric_types], collapse = ", "))
}

flag_columns <- c("sephora_exclusive", "online_only")

for (column in flag_columns) {
  values <- sephora[[column]]
  
  if (!all(na.omit(values) %in% c(0, 1))) {
    stop("Unexpected values in binary column: ", column)
  }
  
  sephora[[column]] <- factor(
    values, levels = c(0, 1), labels = c("No", "Yes")
  )
}

sephora$primary_category <- factor(sephora$primary_category)


# Validate data integrity without removing observations

missing_ids <- sum(is.na(sephora$product_id))
duplicate_ids <- sum(
  duplicated(sephora$product_id[!is.na(sephora$product_id)])
)

cat("\nMissing product IDs:", missing_ids, "\n")
cat("Duplicate product IDs:", duplicate_ids, "\n")

if (missing_ids > 0 || duplicate_ids > 0) {
  stop("Product identifiers require inspection before analysis.")
}

invalid_price <- !is.na(sephora$price_usd) &
  (!is.finite(sephora$price_usd) | sephora$price_usd <= 0)

invalid_rating <- !is.na(sephora$rating) &
  (!is.finite(sephora$rating) | sephora$rating < 1 | sephora$rating > 5)

invalid_counts <- vapply(
  c("reviews", "loves_count", "child_count"),
  function(column) {
    x <- sephora[[column]]
    sum(!is.na(x) & (!is.finite(x) | x < 0 | x != floor(x)))
  },
  integer(1)
)

cat("Invalid prices:", sum(invalid_price), "\n")
cat("Invalid ratings:", sum(invalid_rating), "\n")
print(invalid_counts)

if (any(invalid_price) || any(invalid_rating) || any(invalid_counts > 0)) {
  stop("Invalid numeric values detected. Inspect the original data before proceeding.")
}


# Inspect missing values and numerical summaries

missing_report <- data.frame(
  variable = names(sephora),
  missing_count = colSums(is.na(sephora)),
  missing_percent = round(100 * colMeans(is.na(sephora)), 2),
  row.names = NULL
)

missing_report <- missing_report[
  order(-missing_report$missing_count),
]

cat("\nMissing values by variable:\n")
print(missing_report, row.names = FALSE)

cat("\nNumerical summary:\n")
print(summary(sephora[numeric_columns]))

cat("\nProduct categories:\n")
print(table(sephora$primary_category, useNA = "ifany"))


# Save prepared data and missing-value report

write.csv(
  sephora,
  file.path(processed_dir, "sephora_selected.csv"),
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  missing_report,
  file.path(results_dir, "missing_values.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nPrepared dataset:", nrow(sephora), "rows and",
    ncol(sephora), "selected columns\n")

cat("Saved: data/processed/sephora_selected.csv\n")
cat("Saved: results/missing_values.csv\n")

View(sephora)


# STAGE 2 — DESCRIPTIVE STATISTICAL ANALYSIS

# Overall descriptive statistics for price and rating

analysis_columns <- c("price_usd", "rating")

describe_numeric <- function(x, variable_name) {
  valid <- x[!is.na(x)]
  n <- length(valid)
  
  if (n == 0L) {
    stop("No valid observations for: ", variable_name)
  }
  
  q <- quantile(valid, probs = c(0.25, 0.75))
  
  data.frame(
    variable = variable_name,
    n_valid = n,
    n_missing = sum(is.na(x)),
    min = min(valid),
    q1 = unname(q[1]),
    median = median(valid),
    mean = mean(valid),
    q3 = unname(q[2]),
    max = max(valid),
    sd = if (n >= 2L) sd(valid) else NA_real_
  )
}

overall_stats <- do.call(rbind, lapply(analysis_columns, function(column) {
  describe_numeric(sephora[[column]], column)
}))

rownames(overall_stats) <- NULL

stat_columns <- c("min", "q1", "median", "mean", "q3", "max", "sd")

overall_stats[stat_columns] <- lapply(
  overall_stats[stat_columns], round, digits = 3
)

cat("\nOVERALL DESCRIPTIVE STATISTICS\n")
print(overall_stats, row.names = FALSE)


# Price and rating comparison across major product categories

category_counts <- sort(table(sephora$primary_category), decreasing = TRUE)

# Include categories with at least 100 products.
main_categories <- names(category_counts)[category_counts >= 100]

# Exclude products with fewer than 10 reviews from rating comparisons.
rated_10plus <- sephora[
  !is.na(sephora$rating) &
    !is.na(sephora$reviews) &
    sephora$reviews >= 10,
  , drop = FALSE
]

category_stats <- do.call(rbind, lapply(main_categories, function(category_name) {
  
  group <- sephora[
    !is.na(sephora$primary_category) &
      sephora$primary_category == category_name,
    , drop = FALSE
  ]
  
  prices <- group$price_usd[!is.na(group$price_usd)]
  
  ratings <- rated_10plus$rating[
    !is.na(rated_10plus$primary_category) &
      rated_10plus$primary_category == category_name
  ]
  
  data.frame(
    category = category_name,
    n_products = nrow(group),
    n_rated_10plus = length(ratings),
    median_price_usd = round(median(prices), 2),
    price_iqr_usd = round(IQR(prices), 2),
    median_rating = if (length(ratings) > 0L) {
      round(median(ratings), 3)
    } else NA_real_
  )
}))

rownames(category_stats) <- NULL

cat("\nPRICE AND RATING STATISTICS BY CATEGORY\n")
print(category_stats, row.names = FALSE)

cat("\nProducts with at least 10 reviews (all categories):",
    nrow(rated_10plus), "\n")


# Save descriptive statistics

write.csv(
  overall_stats,
  file.path(results_dir, "descriptive_statistics.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  category_stats,
  file.path(results_dir, "category_statistics.csv"),
  row.names = FALSE,
  na = ""
)


# STAGE 3 — HISTOGRAM ANALYSIS

# Prepare price data and visualization settings

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Run install.packages('ggplot2').")
}

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

prices_all <- sephora$price_usd
prices_all <- prices_all[!is.na(prices_all) & is.finite(prices_all)]

if (length(prices_all) < 2L || any(prices_all <= 0)) {
  stop("At least two positive, finite prices are required.")
}

n_all <- length(prices_all)
mean_all <- mean(prices_all)
median_all <- median(prices_all)
log_prices <- log10(prices_all)

# Equal-width bins on the log10 scale accommodate the wide price range.
bin_width_log <- 0.07

log_breaks <- seq(
  floor(min(log_prices) / bin_width_log) * bin_width_log,
  ceiling(max(log_prices) / bin_width_log) * bin_width_log + bin_width_log,
  by = bin_width_log
)

price_ticks <- c(3, 5, 10, 20, 35, 50, 100, 200, 500, 1000, 1900)

price_tick_labels <- paste0(
  "$", format(price_ticks, big.mark = ",", trim = TRUE)
)

# Consistent visualization style
background_color <- "#FCFAFF"
bar_color <- "#9A6AFF"
text_color <- "#25233A"
mean_color <- "#E65083"
grid_color <- "#E8E3F0"

cat("Products analyzed:", n_all, "\n")
cat(sprintf("Price range: $%.2f to $%.2f\n",
            min(prices_all), max(prices_all)))
cat(sprintf("Mean: $%.2f | Median: $%.2f\n",
            mean_all, median_all))


# Histogram using ggplot2

hist_data <- data.frame(log_price = log_prices)

median_label <- sprintf("Median  $%.2f", median_all)
mean_label <- sprintf("Mean  $%.2f", mean_all)

reference_lines <- data.frame(
  statistic = factor(
    c(median_label, mean_label),
    levels = c(median_label, mean_label)
  ),
  log_price = log10(c(median_all, mean_all))
)

line_colors <- setNames(
  c(text_color, mean_color),
  c(median_label, mean_label)
)

line_types <- setNames(
  c("solid", "dashed"),
  c(median_label, mean_label)
)

histogram_plot <- ggplot2::ggplot(
  hist_data,
  ggplot2::aes(x = log_price)
) +
  
  ggplot2::geom_histogram(
    breaks = log_breaks,
    closed = "left",
    fill = bar_color,
    color = background_color,
    linewidth = 0.25
  ) +
  
  ggplot2::geom_vline(
    data = reference_lines,
    mapping = ggplot2::aes(
      xintercept = log_price,
      color = statistic,
      linetype = statistic
    ),
    inherit.aes = FALSE,
    linewidth = 1.05
  ) +
  
  ggplot2::scale_color_manual(
    values = line_colors,
    name = NULL
  ) +
  
  ggplot2::scale_linetype_manual(
    values = line_types,
    name = NULL
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = log10(price_ticks),
    labels = price_tick_labels,
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  
  ggplot2::scale_y_continuous(
    labels = function(x) format(
      x, big.mark = ",", trim = TRUE
    ),
    expand = ggplot2::expansion(mult = c(0, 0.10))
  ) +
  
  ggplot2::labs(
    title = "Distribution of Sephora Product Prices",
    subtitle = sprintf(
      "Price distribution across %s Sephora products",
      format(n_all, big.mark = ",")
    ),
    x = "Product price (USD, logarithmic scale)",
    y = "Number of products",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Equal-width intervals in log10(price); all valid prices included."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = background_color, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = background_color, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 25,
      face = "bold",
      color = text_color,
      margin = ggplot2::margin(b = 8)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 13,
      color = "#66616B",
      margin = ggplot2::margin(b = 18)
    ),
    axis.title = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = text_color
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 16)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 14)
    ),
    axis.text = ggplot2::element_text(
      size = 10,
      color = text_color
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color = grid_color,
      linewidth = 0.35
    ),
    legend.position = "top",
    legend.justification = "right",
    legend.text = ggplot2::element_text(
      size = 11,
      color = text_color
    ),
    legend.key = ggplot2::element_rect(
      fill = background_color,
      color = NA
    ),
    plot.caption = ggplot2::element_text(
      size = 9,
      color = "#746F77",
      hjust = 0,
      margin = ggplot2::margin(t = 16)
    ),
    plot.margin = ggplot2::margin(25, 30, 22, 28)
  )

print(histogram_plot)

ggplot2::ggsave(
  filename = file.path(plots_dir, "hist_ggplot_prices.png"),
  plot = histogram_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300,
  bg = background_color
)


# Histogram using Base R with the same data and bin boundaries

base_histogram <- hist(
  log_prices,
  breaks = log_breaks,
  right = FALSE,
  include.lowest = TRUE,
  plot = FALSE
)

grDevices::png(
  filename = file.path(plots_dir, "hist_base_prices.png"),
  width = 12,
  height = 7,
  units = "in",
  res = 300,
  bg = background_color
)

par(
  mar = c(6.5, 5.5, 4, 2),
  bg = background_color,
  col.axis = text_color,
  col.lab = text_color,
  col.main = text_color
)

plot(
  base_histogram,
  freq = TRUE,
  col = bar_color,
  border = background_color,
  main = "Distribution of Sephora Product Prices",
  xlab = "Product price (USD, logarithmic scale)",
  ylab = "Number of products",
  xaxt = "n",
  ylim = c(0, max(base_histogram$counts) * 1.20),
  cex.main = 1.35,
  cex.lab = 1,
  cex.axis = 0.9
)

axis(
  side = 1,
  at = log10(price_ticks),
  labels = price_tick_labels,
  cex.axis = 0.85
)

abline(
  v = log10(median_all),
  col = text_color,
  lwd = 2
)

abline(
  v = log10(mean_all),
  col = mean_color,
  lty = 2,
  lwd = 2
)

legend(
  "topright",
  legend = c(median_label, mean_label),
  col = c(text_color, mean_color),
  lty = c(1, 2),
  lwd = 2,
  bty = "n",
  cex = 0.9
)

mtext(
  "Source: Sephora Products and Skincare Reviews (Kaggle). Equal-width intervals in log10(price).",
  side = 1,
  line = 4.5,
  adj = 0,
  cex = 0.75,
  col = "#746F77"
)

grDevices::dev.off()

cat("\nSaved histograms:\n")
cat("results/plots/hist_ggplot_prices.png\n")
cat("results/plots/hist_base_prices.png\n")


# STAGE 4 — DENSITY PLOT ANALYSIS

# Prepare skincare prices by Sephora exclusivity

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Run install.packages('ggplot2').")
}

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

valid_prices <- !is.na(sephora$price_usd) &
  is.finite(sephora$price_usd) & sephora$price_usd > 0

skincare_density <- sephora[
  !is.na(sephora$primary_category) &
    as.character(sephora$primary_category) == "Skincare" &
    !is.na(sephora$sephora_exclusive) & valid_prices,
  c("price_usd", "sephora_exclusive"), drop = FALSE
]

skincare_density$sephora_exclusive <- factor(
  as.character(skincare_density$sephora_exclusive),
  levels = c("No", "Yes")
)

skincare_density$log_price <- log10(skincare_density$price_usd)

regular_prices <- skincare_density$price_usd[
  skincare_density$sephora_exclusive == "No"
]

exclusive_prices <- skincare_density$price_usd[
  skincare_density$sephora_exclusive == "Yes"
]

if (length(unique(regular_prices)) < 2L ||
    length(unique(exclusive_prices)) < 2L) {
  stop("Both exclusivity groups must contain at least two different prices.")
}

# Use a shared bandwidth on log10(price) for comparable smoothing.
skincare_bw <- stats::bw.nrd0(skincare_density$log_price)

if (!is.finite(skincare_bw) || skincare_bw <= 0) {
  stop("Invalid smoothing bandwidth for skincare comparison.")
}

regular_median <- median(regular_prices)
exclusive_median <- median(exclusive_prices)

cat(sprintf("Skincare, non-exclusive: n = %d; median = $%.2f\n",
            length(regular_prices), regular_median))

cat(sprintf("Skincare, Sephora-exclusive: n = %d; median = $%.2f\n",
            length(exclusive_prices), exclusive_median))


# Consistent Lavender & Raspberry palette

density_bg <- "#FCFAFF"
density_text <- "#25233A"
density_grid <- "#E8E3F0"
density_lavender <- "#9A6AFF"
density_raspberry <- "#E65083"
density_subtitle <- "#69627A"
density_caption <- "#797286"

density_colors <- c(
  "No" = density_lavender,
  "Yes" = density_raspberry
)

skincare_ticks <- c(
  3, 5, 10, 20, 35, 50, 100, 200, 500, 1000, 1900
)

skincare_tick_labels <- paste0(
  "$", format(skincare_ticks, big.mark = ",", trim = TRUE)
)

density_subtitle_text <- sprintf(
  "Non-exclusive: n = %s, median = $%.0f  |  Sephora-exclusive: n = %s, median = $%.0f",
  format(length(regular_prices), big.mark = ","),
  regular_median,
  format(length(exclusive_prices), big.mark = ","),
  exclusive_median
)


# Density plot using ggplot2

plot_skincare_density <- ggplot2::ggplot(
  skincare_density,
  ggplot2::aes(
    x = log_price,
    color = sephora_exclusive,
    fill = sephora_exclusive
  )
) +
  
  ggplot2::geom_density(
    bw = skincare_bw,
    alpha = 0.20,
    linewidth = 1.15
  ) +
  
  ggplot2::scale_color_manual(
    values = density_colors,
    breaks = c("No", "Yes"),
    labels = c("Non-exclusive", "Sephora-exclusive")
  ) +
  
  ggplot2::scale_fill_manual(
    values = density_colors,
    breaks = c("No", "Yes"),
    labels = c("Non-exclusive", "Sephora-exclusive")
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = log10(skincare_ticks),
    labels = skincare_tick_labels,
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.10))
  ) +
  
  ggplot2::labs(
    title = "Distribution of Skincare Prices by Exclusivity",
    subtitle = density_subtitle_text,
    x = "Product price (USD, log10 scale)",
    y = "Density per log10(price) unit",
    color = "Sephora status",
    fill = "Sephora status",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Each curve is independently normalized; comparisons are descriptive."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = density_bg, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = density_bg, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 22,
      face = "bold",
      color = density_text,
      margin = ggplot2::margin(b = 8)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 11.5,
      color = density_subtitle,
      margin = ggplot2::margin(b = 16)
    ),
    axis.title = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = density_text
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 14)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 14)
    ),
    axis.text = ggplot2::element_text(
      size = 10,
      color = density_text
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color = density_grid,
      linewidth = 0.4
    ),
    legend.position = "top",
    legend.justification = "right",
    legend.title = ggplot2::element_text(
      size = 10,
      color = density_text
    ),
    legend.text = ggplot2::element_text(
      size = 10,
      color = density_text
    ),
    legend.key = ggplot2::element_rect(
      fill = density_bg,
      color = NA
    ),
    plot.caption = ggplot2::element_text(
      size = 9,
      color = density_caption,
      hjust = 0,
      margin = ggplot2::margin(t = 16)
    ),
    plot.margin = ggplot2::margin(25, 30, 22, 28)
  )

print(plot_skincare_density)

ggplot2::ggsave(
  filename = file.path(
    plots_dir, "density_04_ggplot_skincare_comparison.png"
  ),
  plot = plot_skincare_density,
  width = 11,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = density_bg
)


# Density plot using Base R

regular_density <- stats::density(
  log10(regular_prices),
  bw = skincare_bw
)

exclusive_density <- stats::density(
  log10(exclusive_prices),
  bw = skincare_bw
)

density_ylim <- c(
  0,
  max(regular_density$y, exclusive_density$y) * 1.15
)

density_xlim <- range(
  regular_density$x,
  exclusive_density$x
)

grDevices::png(
  filename = file.path(
    plots_dir, "density_04_base_skincare_comparison.png"
  ),
  width = 11,
  height = 6.5,
  units = "in",
  res = 300,
  bg = density_bg
)

par(
  mar = c(6.5, 5.5, 5.5, 2),
  bg = density_bg,
  col.axis = density_text,
  col.lab = density_text,
  col.main = density_text,
  family = "sans"
)

plot(
  regular_density,
  type = "n",
  xlim = density_xlim,
  ylim = density_ylim,
  main = "Distribution of Skincare Prices by Exclusivity",
  xlab = "Product price (USD, log10 scale)",
  ylab = "Density per log10(price) unit",
  xaxt = "n",
  axes = FALSE,
  cex.main = 1.4,
  cex.lab = 1
)

mtext(
  density_subtitle_text,
  side = 3,
  line = 0.5,
  cex = 0.85,
  col = density_subtitle
)

abline(
  h = pretty(density_ylim),
  col = density_grid,
  lwd = 0.8
)

polygon(
  c(regular_density$x, rev(regular_density$x)),
  c(regular_density$y, rep(0, length(regular_density$y))),
  col = grDevices::adjustcolor(density_lavender, alpha.f = 0.20),
  border = NA
)

polygon(
  c(exclusive_density$x, rev(exclusive_density$x)),
  c(exclusive_density$y, rep(0, length(exclusive_density$y))),
  col = grDevices::adjustcolor(density_raspberry, alpha.f = 0.20),
  border = NA
)

lines(
  regular_density,
  col = density_lavender,
  lwd = 2.5
)

lines(
  exclusive_density,
  col = density_raspberry,
  lwd = 2.5
)

axis(
  side = 1,
  at = log10(skincare_ticks),
  labels = skincare_tick_labels,
  cex.axis = 0.85,
  col.axis = density_text
)

axis(
  side = 2,
  las = 1,
  cex.axis = 0.9,
  col.axis = density_text
)

box(bty = "l", col = density_text)

legend(
  "topright",
  legend = c("Non-exclusive", "Sephora-exclusive"),
  col = c(density_lavender, density_raspberry),
  lty = 1,
  lwd = 2.5,
  bty = "n",
  cex = 0.95
)

mtext(
  "Source: Sephora Products and Skincare Reviews (Kaggle). Density normalized within each group.",
  side = 1,
  line = 4.5,
  adj = 0,
  cex = 0.72,
  col = density_caption
)

grDevices::dev.off()

cat("\nSaved density plots:\n")
cat("results/plots/density_04_ggplot_skincare_comparison.png\n")
cat("results/plots/density_04_base_skincare_comparison.png\n")


# STAGE 5 — BOXPLOT ANALYSIS

# Prepare price data for category comparisons

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Run install.packages('ggplot2').")
}

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!exists("main_categories") || length(main_categories) == 0L) {
  stop("Run Stage 2 first: main_categories was not found.")
}

price_box_data <- sephora[
  !is.na(sephora$primary_category) &
    as.character(sephora$primary_category) %in% main_categories &
    !is.na(sephora$price_usd) &
    is.finite(sephora$price_usd) &
    sephora$price_usd > 0,
  c("primary_category", "price_usd"),
  drop = FALSE
]

if (nrow(price_box_data) < 2L) {
  stop("Not enough valid observations for the boxplot comparison.")
}

# Order categories by median price.
category_medians <- vapply(main_categories, function(category_name) {
  median(price_box_data$price_usd[
    as.character(price_box_data$primary_category) == category_name
  ])
}, numeric(1))

category_order <- names(sort(category_medians))

price_box_data$primary_category <- factor(
  as.character(price_box_data$primary_category),
  levels = category_order
)

category_labels <- setNames(
  paste0(
    category_order,
    "   |   Median $",
    sprintf("%.0f", category_medians[category_order])
  ),
  category_order
)


# Consistent Lavender & Raspberry palette

box_bg <- "#FCFAFF"
box_text <- "#25233A"
box_grid <- "#E8E3F0"
box_border <- "#25233A"
box_outliers <- "#E65083"
box_subtitle <- "#69627A"
box_caption <- "#797286"

box_colors <- setNames(
  grDevices::colorRampPalette(
    c("#C5ACFF", "#9A6AFF", "#7843DF")
  )(length(category_order)),
  category_order
)

cat("Products included:", nrow(price_box_data), "\n")
cat("Median product price by category (USD):\n")
print(round(sort(category_medians, decreasing = TRUE), 2))

box_subtitle_text <- sprintf(
  "Price distributions across %d categories  |  %s products",
  length(category_order),
  format(nrow(price_box_data), big.mark = ",", trim = TRUE)
)


# Boxplot using ggplot2

price_box_plot <- ggplot2::ggplot(
  price_box_data,
  ggplot2::aes(
    x = primary_category,
    y = price_usd,
    fill = primary_category
  )
) +
  
  ggplot2::geom_boxplot(
    width = 0.55,
    color = box_border,
    linewidth = 0.70,
    outlier.color = box_outliers,
    outlier.alpha = 0.32,
    outlier.size = 1.0
  ) +
  
  ggplot2::stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 2.7,
    stroke = 0.85,
    color = box_border,
    fill = "white"
  ) +
  
  ggplot2::scale_fill_manual(
    values = box_colors,
    guide = "none"
  ) +
  
  ggplot2::scale_x_discrete(
    labels = category_labels
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = seq(0, 200, by = 25),
    labels = function(x) paste0("$", x),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  
  # Zoom the display without changing boxplot statistics.
  ggplot2::coord_flip(ylim = c(0, 200)) +
  
  ggplot2::labs(
    title = "Distribution of Sephora Product Prices by Category",
    subtitle = box_subtitle_text,
    x = NULL,
    y = "Product price (USD)",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Boxes show Q1–Q3, median and whiskers (1.5 × IQR rule).",
      "Display limited to $200; all prices retained in calculations."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = box_bg, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = box_bg, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 23,
      face = "bold",
      color = box_text,
      margin = ggplot2::margin(b = 9)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      color = box_subtitle,
      margin = ggplot2::margin(b = 20)
    ),
    axis.text.y = ggplot2::element_text(
      size = 11.5,
      color = box_text,
      face = "bold",
      margin = ggplot2::margin(r = 9)
    ),
    axis.text.x = ggplot2::element_text(
      size = 10.5,
      color = box_text
    ),
    axis.title.x = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = box_text,
      margin = ggplot2::margin(t = 17)
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = box_grid,
      linewidth = 0.45
    ),
    plot.caption = ggplot2::element_text(
      size = 8.9,
      color = box_caption,
      hjust = 0,
      margin = ggplot2::margin(t = 20)
    ),
    plot.margin = ggplot2::margin(25, 35, 22, 28)
  )

print(price_box_plot)

ggplot2::ggsave(
  filename = file.path(plots_dir, "box_ggplot_category_prices.png"),
  plot = price_box_plot,
  width = 12.5,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = box_bg
)


# Boxplot using Base R

grDevices::png(
  filename = file.path(plots_dir, "box_base_category_prices.png"),
  width = 12.5,
  height = 7.5,
  units = "in",
  res = 300,
  bg = box_bg
)

par(
  mar = c(6.5, 13.5, 5.5, 2),
  bg = box_bg,
  col.axis = box_text,
  col.lab = box_text,
  col.main = box_text,
  family = "sans"
)

boxplot(
  price_usd ~ primary_category,
  data = price_box_data,
  horizontal = TRUE,
  col = unname(box_colors[category_order]),
  border = box_border,
  boxwex = 0.55,
  medcol = box_border,
  medlwd = 2,
  outcol = grDevices::adjustcolor(
    box_outliers, alpha.f = 0.32
  ),
  outpch = 16,
  outcex = 0.55,
  outline = TRUE,
  names = unname(category_labels),
  ylim = c(0, 200),
  axes = FALSE,
  main = "Distribution of Sephora Product Prices by Category",
  xlab = "Product price (USD)",
  ylab = "",
  cex.main = 1.35,
  cex.lab = 1
)

axis(
  side = 1,
  at = seq(0, 200, by = 25),
  labels = paste0("$", seq(0, 200, by = 25)),
  cex.axis = 0.9,
  col.axis = box_text
)

axis(
  side = 2,
  at = seq_along(category_order),
  labels = unname(category_labels),
  las = 1,
  tick = FALSE,
  cex.axis = 0.9,
  col.axis = box_text
)

points(
  x = category_medians[category_order],
  y = seq_along(category_order),
  pch = 21,
  bg = "white",
  col = box_border,
  cex = 1.1,
  lwd = 1.2
)

mtext(
  box_subtitle_text,
  side = 3,
  line = 0.5,
  cex = 0.9,
  col = box_subtitle
)

mtext(
  paste(
    "Source: Sephora Products and Skincare Reviews (Kaggle).",
    "1.5 × IQR whiskers; display limited to $200."
  ),
  side = 1,
  line = 4.5,
  adj = 0,
  cex = 0.75,
  col = box_caption
)

grDevices::dev.off()

cat("\nSaved boxplots:\n")
cat("results/plots/box_ggplot_category_prices.png\n")
cat("results/plots/box_base_category_prices.png\n")


# STAGE 6 — VIOLIN PLOT ANALYSIS

# Prepare product ratings for category comparisons

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Run install.packages('ggplot2').")
}

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (!exists("main_categories") || length(main_categories) == 0L) {
  stop("Run Stage 2 first: main_categories was not found.")
}

# Include products with at least 10 reviews for rating comparisons.
violin_rating_data <- sephora[
  !is.na(sephora$primary_category) &
    as.character(sephora$primary_category) %in% main_categories &
    !is.na(sephora$rating) & is.finite(sephora$rating) &
    sephora$rating >= 1 & sephora$rating <= 5 &
    !is.na(sephora$reviews) & sephora$reviews >= 10,
  c("primary_category", "rating"),
  drop = FALSE
]

if (nrow(violin_rating_data) < 2L) {
  stop("Not enough valid ratings for category comparisons.")
}

# Order categories by median rating.
violin_medians <- tapply(
  violin_rating_data$rating,
  as.character(violin_rating_data$primary_category),
  median
)

violin_order <- names(sort(violin_medians))

violin_rating_data$primary_category <- factor(
  as.character(violin_rating_data$primary_category),
  levels = violin_order
)

violin_counts <- table(violin_rating_data$primary_category)

if (any(violin_counts < 2L)) {
  stop("Each displayed category requires at least two rated products.")
}

# Use a shared bandwidth for comparable density smoothing.
violin_bw <- stats::bw.nrd0(violin_rating_data$rating)

if (!is.finite(violin_bw) || violin_bw <= 0) {
  stop("Cannot determine a valid density bandwidth for ratings.")
}

category_labels <- setNames(
  paste0(
    violin_order,
    "   |   Median ",
    sprintf("%.2f", violin_medians[violin_order])
  ),
  violin_order
)

cat("Products analyzed:", nrow(violin_rating_data), "\n")
cat("Products and median rating by category:\n")

print(data.frame(
  category = violin_order,
  n_products = as.integer(violin_counts[violin_order]),
  median_rating = round(
    as.numeric(violin_medians[violin_order]), 3
  ),
  row.names = NULL
), row.names = FALSE)


# Consistent Lavender & Raspberry palette

violin_bg <- "#FCFAFF"
violin_text <- "#25233A"
violin_grid <- "#E8E3F0"
violin_border <- "#25233A"
violin_subtitle <- "#69627A"
violin_caption <- "#797286"

violin_colors <- setNames(
  grDevices::colorRampPalette(
    c("#C5ACFF", "#9A6AFF", "#7843DF")
  )(length(violin_order)),
  violin_order
)

violin_subtitle_text <- sprintf(
  "Rating distributions across %d categories  |  %s products with 10+ reviews",
  length(violin_order),
  format(nrow(violin_rating_data), big.mark = ",", trim = TRUE)
)


# Violin plot using ggplot2

violin_rating_plot <- ggplot2::ggplot(
  violin_rating_data,
  ggplot2::aes(
    x = primary_category,
    y = rating,
    fill = primary_category
  )
) +
  
  ggplot2::geom_violin(
    bw = violin_bw,
    trim = TRUE,
    scale = "width",
    width = 0.74,
    color = violin_border,
    linewidth = 0.6,
    alpha = 0.93
  ) +
  
  ggplot2::geom_boxplot(
    width = 0.13,
    fill = "white",
    color = violin_border,
    linewidth = 0.58,
    outlier.shape = NA
  ) +
  
  ggplot2::stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 2.55,
    stroke = 0.9,
    fill = "white",
    color = violin_border
  ) +
  
  ggplot2::scale_fill_manual(
    values = violin_colors,
    guide = "none"
  ) +
  
  ggplot2::scale_x_discrete(
    labels = function(x) unname(category_labels[x])
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = seq(1, 5, by = 0.5),
    labels = function(x) sprintf("%.1f", x),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  
  ggplot2::coord_flip(ylim = c(1, 5)) +
  
  ggplot2::labs(
    title = "Distribution of Sephora Product Ratings by Category",
    subtitle = violin_subtitle_text,
    x = NULL,
    y = "Product rating (1–5)",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Labels show median ratings; internal boxes show Q1–Q3 and median.",
      "Violin widths are normalized by category, not by sample size."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = violin_bg, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = violin_bg, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 23,
      face = "bold",
      color = violin_text,
      margin = ggplot2::margin(b = 9)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      color = violin_subtitle,
      margin = ggplot2::margin(b = 20)
    ),
    axis.text.y = ggplot2::element_text(
      size = 11.5,
      face = "bold",
      color = violin_text,
      margin = ggplot2::margin(r = 10)
    ),
    axis.text.x = ggplot2::element_text(
      size = 10.5,
      color = violin_text
    ),
    axis.title.x = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = violin_text,
      margin = ggplot2::margin(t = 17)
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = violin_grid,
      linewidth = 0.45
    ),
    plot.caption = ggplot2::element_text(
      size = 8.8,
      color = violin_caption,
      hjust = 0,
      margin = ggplot2::margin(t = 20)
    ),
    plot.margin = ggplot2::margin(25, 35, 22, 28)
  )

print(violin_rating_plot)

ggplot2::ggsave(
  filename = file.path(
    plots_dir, "violin_ggplot_category_ratings.png"
  ),
  plot = violin_rating_plot,
  width = 12.5,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = violin_bg
)


# Violin plot using Base R

grDevices::png(
  filename = file.path(
    plots_dir, "violin_base_category_ratings.png"
  ),
  width = 12.5,
  height = 7.5,
  units = "in",
  res = 300,
  bg = violin_bg
)

par(
  mar = c(6.5, 13.5, 5.5, 2),
  bg = violin_bg,
  col.axis = violin_text,
  col.lab = violin_text,
  col.main = violin_text,
  family = "sans"
)

plot(
  NA_real_, NA_real_,
  xlim = c(1, 5),
  ylim = c(0.5, length(violin_order) + 0.5),
  type = "n",
  axes = FALSE,
  main = "Distribution of Sephora Product Ratings by Category",
  xlab = "Product rating (1–5)",
  ylab = "",
  cex.main = 1.35,
  cex.lab = 1
)

abline(
  v = seq(1, 5, by = 0.5),
  col = violin_grid,
  lwd = 0.8
)

# Construct equal-width violin shapes from kernel density estimates.
for (i in seq_along(violin_order)) {
  
  category_name <- violin_order[i]
  
  ratings <- violin_rating_data$rating[
    violin_rating_data$primary_category == category_name
  ]
  
  density_est <- stats::density(
    ratings,
    bw = violin_bw,
    from = min(ratings),
    to = max(ratings),
    n = 512
  )
  
  half_width <- 0.37 * density_est$y / max(density_est$y)
  
  polygon(
    x = c(density_est$x, rev(density_est$x)),
    y = c(i + half_width, rev(i - half_width)),
    col = grDevices::adjustcolor(
      violin_colors[[category_name]], alpha.f = 0.93
    ),
    border = violin_border,
    lwd = 0.8
  )
  
  boxplot(
    ratings,
    horizontal = TRUE,
    add = TRUE,
    at = i,
    boxwex = 0.13,
    col = "white",
    border = violin_border,
    medcol = violin_border,
    medlwd = 1.5,
    outline = FALSE,
    axes = FALSE
  )
  
  points(
    x = median(ratings),
    y = i,
    pch = 21,
    bg = "white",
    col = violin_border,
    cex = 1.1,
    lwd = 1.2
  )
}

axis(
  side = 1,
  at = seq(1, 5, by = 0.5),
  labels = sprintf("%.1f", seq(1, 5, by = 0.5)),
  cex.axis = 0.9,
  col.axis = violin_text
)

axis(
  side = 2,
  at = seq_along(violin_order),
  labels = unname(category_labels[violin_order]),
  las = 1,
  tick = FALSE,
  cex.axis = 0.9,
  col.axis = violin_text
)

box(
  bty = "l",
  col = violin_text
)

mtext(
  violin_subtitle_text,
  side = 3,
  line = 0.5,
  cex = 0.9,
  col = violin_subtitle
)

mtext(
  paste(
    "Source: Sephora Products and Skincare Reviews (Kaggle).",
    "Equal-width violins; products with 10+ reviews."
  ),
  side = 1,
  line = 4.5,
  adj = 0,
  cex = 0.75,
  col = violin_caption
)

grDevices::dev.off()

cat("\nSaved violin plots:\n")
cat("results/plots/violin_ggplot_category_ratings.png\n")
cat("results/plots/violin_base_category_ratings.png\n")


# STAGE 7 — RELATIONSHIP ANALYSIS

# Prepare the sample for price-rating analysis

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required. Run install.packages('ggplot2').")
}

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

required_columns <- c("price_usd", "rating", "reviews")
missing_columns <- setdiff(required_columns, names(sephora))

if (length(missing_columns) > 0L) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}

if (!all(vapply(sephora[required_columns], is.numeric, logical(1)))) {
  stop("Price, rating and review count must be numeric.")
}

# Include products with valid values and at least 10 reviews.
price_rating_data <- sephora[
  is.finite(sephora$price_usd) & sephora$price_usd > 0 &
    is.finite(sephora$rating) &
    sephora$rating >= 1 & sephora$rating <= 5 &
    is.finite(sephora$reviews) & sephora$reviews >= 10,
  c("price_usd", "rating"),
  drop = FALSE
]

if (nrow(price_rating_data) < 3L ||
    length(unique(price_rating_data$price_usd)) < 2L ||
    length(unique(price_rating_data$rating)) < 2L) {
  stop("Not enough varied observations for correlation analysis.")
}


# Spearman rank correlation

# Spearman measures monotonic association without assuming linearity.
spearman_rho <- unname(stats::cor(
  price_rating_data$price_usd,
  price_rating_data$rating,
  method = "spearman"
))

n_relationship <- nrow(price_rating_data)
price_rating_data$log_price <- log10(price_rating_data$price_usd)

cat("Products with at least 10 reviews:", n_relationship, "\n")
cat(sprintf(
  "Spearman correlation (price vs rating): %.4f\n",
  spearman_rho
))
cat("Correlation describes association, not causation.\n")


# Consistent Lavender & Raspberry palette

relation_bg <- "#FCFAFF"
relation_text <- "#25233A"
relation_grid <- "#E8E3F0"
relation_points <- "#7843DF"
relation_trend <- "#E65083"
relation_subtitle <- "#69627A"
relation_caption <- "#797286"

price_axis_ticks <- c(
  3, 5, 10, 20, 35, 50, 100, 200, 500, 1000, 1900
)


# Scatter plot using ggplot2

# Jitter affects display only; correlation and trend use original ratings.
price_rating_plot <- ggplot2::ggplot(
  price_rating_data,
  ggplot2::aes(x = log_price, y = rating)
) +
  
  ggplot2::geom_point(
    color = relation_points,
    alpha = 0.22,
    size = 1.05,
    position = ggplot2::position_jitter(
      width = 0,
      height = 0.035,
      seed = 42
    )
  ) +
  
  ggplot2::geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = relation_trend,
    fill = relation_trend,
    alpha = 0.16,
    linewidth = 1.15
  ) +
  
  ggplot2::scale_x_continuous(
    breaks = log10(price_axis_ticks),
    labels = paste0(
      "$",
      format(price_axis_ticks, big.mark = ",", trim = TRUE)
    ),
    expand = ggplot2::expansion(mult = c(0.02, 0.03))
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = seq(1, 5, by = 0.5),
    labels = function(x) sprintf("%.1f", x),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  
  ggplot2::coord_cartesian(ylim = c(1, 5)) +
  
  ggplot2::labs(
    title = "Relationship Between Product Price and Rating",
    subtitle = sprintf(
      "%s Sephora products with 10+ reviews  |  Spearman rho = %+.3f",
      format(n_relationship, big.mark = ",", trim = TRUE),
      spearman_rho
    ),
    x = "Product price (USD, logarithmic scale)",
    y = "Product rating (1–5)",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Points are jittered for visibility; calculations use original values.",
      "Linear trend fitted on log10(price); association does not imply causation."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = relation_bg, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = relation_bg, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 23,
      face = "bold",
      color = relation_text,
      margin = ggplot2::margin(b = 9)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      color = relation_subtitle,
      margin = ggplot2::margin(b = 20)
    ),
    axis.title = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = relation_text
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 17)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 16)
    ),
    axis.text = ggplot2::element_text(
      size = 10.5,
      color = relation_text
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      color = relation_grid,
      linewidth = 0.45
    ),
    plot.caption = ggplot2::element_text(
      size = 8.7,
      color = relation_caption,
      hjust = 0,
      margin = ggplot2::margin(t = 18)
    ),
    plot.margin = ggplot2::margin(26, 36, 24, 30)
  )

print(price_rating_plot)

ggplot2::ggsave(
  filename = file.path(
    plots_dir, "scatter_ggplot_price_rating.png"
  ),
  plot = price_rating_plot,
  width = 12.5,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = relation_bg
)


# Additional comparison — skincare ratings by exclusivity

skincare_rating_data <- sephora[
  !is.na(sephora$primary_category) &
    as.character(sephora$primary_category) == "Skincare" &
    !is.na(sephora$sephora_exclusive) &
    is.finite(sephora$rating) &
    sephora$rating >= 1 & sephora$rating <= 5 &
    is.finite(sephora$reviews) & sephora$reviews >= 10,
  c("rating", "sephora_exclusive"),
  drop = FALSE
]

skincare_rating_data$exclusivity <- factor(
  as.character(skincare_rating_data$sephora_exclusive),
  levels = c("No", "Yes"),
  labels = c("Non-exclusive", "Sephora-exclusive")
)

if (anyNA(skincare_rating_data$exclusivity)) {
  stop("Unexpected Sephora exclusivity values.")
}

rating_groups <- split(
  skincare_rating_data$rating,
  skincare_rating_data$exclusivity,
  drop = TRUE
)

if (length(rating_groups) != 2L ||
    any(lengths(rating_groups) < 2L) ||
    any(vapply(
      rating_groups,
      function(x) length(unique(x)) < 2L,
      logical(1)
    ))) {
  stop("Both skincare groups need at least two different valid ratings.")
}

rating_n <- lengths(rating_groups)
rating_median <- vapply(rating_groups, median, numeric(1))

cat("\nSKINCARE RATINGS BY EXCLUSIVITY\n")

for (group_name in names(rating_groups)) {
  cat(sprintf(
    "%s: n = %d, median = %.3f\n",
    group_name,
    rating_n[[group_name]],
    rating_median[[group_name]]
  ))
}

# Use the same bandwidth for both density estimates.
common_rating_bw <- stats::bw.nrd0(skincare_rating_data$rating)

if (!is.finite(common_rating_bw) || common_rating_bw <= 0) {
  stop("Unable to compute a valid common density bandwidth.")
}

exclusivity_colors <- c(
  "Non-exclusive" = "#9A6AFF",
  "Sephora-exclusive" = "#E65083"
)

exclusivity_labels <- setNames(
  paste0(
    names(rating_groups),
    "   |   n = ",
    format(rating_n, big.mark = ","),
    "   |   Median ",
    sprintf("%.2f", rating_median)
  ),
  names(rating_groups)
)


# Additional violin plot using ggplot2

exclusivity_rating_plot <- ggplot2::ggplot(
  skincare_rating_data,
  ggplot2::aes(
    x = exclusivity,
    y = rating,
    fill = exclusivity
  )
) +
  
  ggplot2::geom_violin(
    trim = TRUE,
    scale = "width",
    bw = common_rating_bw,
    alpha = 0.88,
    color = relation_text,
    linewidth = 0.65,
    width = 0.75
  ) +
  
  ggplot2::geom_boxplot(
    width = 0.11,
    fill = "white",
    color = relation_text,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  
  ggplot2::stat_summary(
    fun = median,
    geom = "point",
    shape = 21,
    size = 2.6,
    fill = "white",
    color = relation_text,
    stroke = 0.9
  ) +
  
  ggplot2::scale_fill_manual(
    values = exclusivity_colors,
    guide = "none"
  ) +
  
  ggplot2::scale_x_discrete(
    labels = exclusivity_labels
  ) +
  
  ggplot2::scale_y_continuous(
    breaks = seq(1, 5, by = 0.5),
    labels = function(x) sprintf("%.1f", x),
    expand = ggplot2::expansion(mult = c(0.02, 0.03))
  ) +
  
  ggplot2::coord_flip(ylim = c(1, 5)) +
  
  ggplot2::labs(
    title = "Distribution of Skincare Ratings by Exclusivity",
    subtitle = sprintf(
      "Rating distributions for %s skincare products with 10+ reviews",
      format(nrow(skincare_rating_data), big.mark = ",", trim = TRUE)
    ),
    x = NULL,
    y = "Product rating (1–5)",
    caption = paste(
      "Source: Sephora Products and Skincare Reviews (Kaggle).",
      "Internal boxes show Q1–Q3; white points mark medians.",
      "Violin widths are normalized by group; comparison is descriptive."
    )
  ) +
  
  ggplot2::theme_minimal(
    base_size = 13,
    base_family = "sans"
  ) +
  
  ggplot2::theme(
    plot.background = ggplot2::element_rect(
      fill = relation_bg, color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = relation_bg, color = NA
    ),
    plot.title = ggplot2::element_text(
      size = 23,
      face = "bold",
      color = relation_text,
      margin = ggplot2::margin(b = 9)
    ),
    plot.subtitle = ggplot2::element_text(
      size = 12.5,
      color = relation_subtitle,
      margin = ggplot2::margin(b = 20)
    ),
    axis.title.x = ggplot2::element_text(
      size = 10,
      face = "bold",
      color = relation_text,
      margin = ggplot2::margin(t = 17)
    ),
    axis.text.x = ggplot2::element_text(
      size = 10.5,
      color = relation_text
    ),
    axis.text.y = ggplot2::element_text(
      size = 11.5,
      face = "bold",
      color = relation_text,
      margin = ggplot2::margin(r = 10)
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = relation_grid,
      linewidth = 0.45
    ),
    plot.caption = ggplot2::element_text(
      size = 8.8,
      color = relation_caption,
      hjust = 0,
      margin = ggplot2::margin(t = 20)
    ),
    plot.margin = ggplot2::margin(25, 35, 22, 28)
  )

print(exclusivity_rating_plot)

ggplot2::ggsave(
  filename = file.path(
    plots_dir, "violin_ggplot_skincare_exclusivity.png"
  ),
  plot = exclusivity_rating_plot,
  width = 12.5,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = relation_bg
)

cat("\nSaved relationship analysis plots:\n")
cat("results/plots/scatter_ggplot_price_rating.png\n")
cat("results/plots/violin_ggplot_skincare_exclusivity.png\n")

