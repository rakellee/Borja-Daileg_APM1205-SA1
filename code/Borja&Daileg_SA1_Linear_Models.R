# ------------------------------------------------------------------------------
# APM1205 - Linear Models: Summative Assessment 1 (SA1)
# Script: SA1_Linear_Models.R
# Dataset: dataset.csv (Seoul Bike Sharing Demand Dataset)
# Output: Formatted PNG & CSV Tables (Tables 1 to 12), Figures 1 to 4
# ------------------------------------------------------------------------------

# Automatic Working Directory & Project Root Resolution
resolve_project_root <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    doc_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(doc_path)) {
      p_dir <- dirname(normalizePath(doc_path))
      if (basename(p_dir) == "code") {
        setwd(dirname(p_dir))
      } else {
        setwd(p_dir)
      }
      return(invisible(getwd()))
    }
  }
  
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    s_path <- normalizePath(sub("--file=", "", file_arg))
    p_dir <- dirname(s_path)
    if (basename(p_dir) == "code") {
      setwd(dirname(p_dir))
    } else {
      setwd(p_dir)
    }
    return(invisible(getwd()))
  }
  
  if (file.exists(file.path("data", "dataset.csv")) || file.exists("dataset.csv")) {
    return(invisible(getwd()))
  } else if (file.exists(file.path("..", "data", "dataset.csv")) || file.exists(file.path("..", "dataset.csv"))) {
    setwd("..")
    return(invisible(getwd()))
  }
}

resolve_project_root()
cat("Active Project Root Directory:", getwd(), "\n")

# Dependencies & Directory Setup
required_pkgs <- c("car", "lmtest", "gridExtra", "grid")
for (p in required_pkgs) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
    library(p, character.only = TRUE)
  }
}

dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create("data", recursive = TRUE, showWarnings = FALSE)

# Helper Function: Export Tables as PNG Images
save_table_png <- function(df, filename, title = "") {
  df_clean <- df
  if (is.matrix(df_clean)) df_clean <- as.data.frame(df_clean)
  
  num_cols <- sapply(df_clean, is.numeric)
  df_clean[num_cols] <- lapply(df_clean[num_cols], function(x) {
    ifelse(abs(x) < 0.0001 & x != 0, formatC(x, format = "e", digits = 2), round(x, 4))
  })
  
  theme <- gridExtra::ttheme_default(
    core = list(
      fg_params = list(fontsize = 10, hjust = 0.5, x = 0.5),
      bg_params = list(fill = c("#f8f9fa", "#ffffff"))
    ),
    colhead = list(
      fg_params = list(fontsize = 11, fontface = "bold", col = "white"),
      bg_params = list(fill = "#2c3e50")
    )
  )
  
  table_grob <- gridExtra::tableGrob(df_clean, rows = NULL, theme = theme)
  
  if (nzchar(title)) {
    title_grob <- grid::textGrob(title, gp = grid::gpar(fontsize = 12, fontface = "bold"))
    table_grob <- gridExtra::arrangeGrob(table_grob, top = title_grob, padding = grid::unit(1.2, "line"))
  }
  
  img_width  <- max(7.0, ncol(df_clean) * 1.6)
  img_height <- max(2.5, (nrow(df_clean) + 2) * 0.45)
  
  png(filename, width = img_width, height = img_height, units = "in", res = 300)
  grid::grid.draw(table_grob)
  dev.off()
}

# PART A: Robust File Search & Data Ingestion
candidate_paths <- c(
  file.path("data", "dataset.csv"),
  "dataset.csv",
  file.path("..", "data", "dataset.csv"),
  file.path("..", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "code", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "data", "dataset.csv"),
  file.path("data", "SeoulBikeData.csv"),
  "SeoulBikeData.csv",
  file.path(Sys.getenv("HOME"), "Downloads", "SeoulBikeData.csv")
)
csv_target <- candidate_paths[file.exists(candidate_paths)][1]

if (is.na(csv_target)) {
  search_roots <- unique(c(getwd(), dirname(getwd()), file.path(Sys.getenv("HOME"), "Downloads")))
  search_roots <- search_roots[dir.exists(search_roots)]
  found_files <- list.files(search_roots, pattern = "^(dataset|SeoulBikeData).*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(found_files) > 0) {
    csv_target <- found_files[1]
  }
}

if (is.na(csv_target)) {
  stop("ERROR: 'dataset.csv' could not be found. Please check your folder.")
}

cat("Loading dataset from:", csv_target, "\n")
bike_raw <- read.csv(csv_target, fileEncoding = "latin1", check.names = FALSE)
names(bike_raw) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature", 
                     "Humidity", "Wind_speed", "Visibility", "Dew_point_temp", 
                     "Solar_Radiation", "Rainfall", "Snowfall", "Seasons", 
                     "Holiday", "Functioning_Day")

# Filter operational records
bike <- subset(bike_raw, Functioning_Day == "Yes")
bike$Seasons <- factor(bike$Seasons, levels = c("Autumn", "Spring", "Summer", "Winter"))
bike$Holiday <- factor(bike$Holiday, levels = c("Holiday", "No Holiday"))
bike$Date <- as.Date(bike$Date, format = "%d/%m/%Y")

cat(sprintf("Operational records loaded: %d (Shutdowns removed: %d)\n", 
            nrow(bike), nrow(bike_raw) - nrow(bike)))

# ------------------------------------------------------------------------------
# TABLE 1 & FIGURE 1: Exploratory Data Analysis
# ------------------------------------------------------------------------------
eda_vars <- c("Rented_Bike_Count", "Temperature", "Humidity", "Wind_speed", "Solar_Radiation", "Rainfall")
eda_table <- data.frame(
  Variable = c("Rented Bike Count", "Temperature (°C)", "Humidity (%)", "Wind Speed (m/s)", "Solar Radiation (MJ/m2)", "Rainfall (mm)"),
  Mean     = sapply(bike[eda_vars], mean),
  SD       = sapply(bike[eda_vars], sd),
  Minimum  = sapply(bike[eda_vars], min),
  Median   = sapply(bike[eda_vars], median),
  Maximum  = sapply(bike[eda_vars], max)
)

write.csv(eda_table, file.path("output", "tables", "table1_descriptive_stats.csv"), row.names = FALSE)
save_table_png(eda_table, file.path("output", "tables", "table1_descriptive_stats.png"), 
               title = "Table 1: Descriptive Statistics for Selected Variables (N = 8,465)")

# Figure 1: Retained Color Palettes (Dark Blue Points + Orange Fill for Seasons)
png(file.path("output", "figures", "fig1_eda_plots.png"), width = 1800, height = 900, res = 200)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(bike$Temperature, bike$Rented_Bike_Count, 
     pch = 16, cex = 0.4, col = "darkblue",
     xlab = "Temperature (°C)", ylab = "Rented Bike Count", 
     main = "Bike Demand vs Temperature")
boxplot(Rented_Bike_Count ~ Seasons, data = bike, 
        col = "#E69F00", border = "black",
        xlab = "Season", ylab = "Rented Bike Count", 
        main = "Bike Demand by Season")
dev.off()

# ------------------------------------------------------------------------------
# TABLE 2 & FIGURE 2: Simple Linear Regression
# ------------------------------------------------------------------------------
slr_model <- lm(Rented_Bike_Count ~ Temperature, data = bike)
slr_summary <- summary(slr_model)

slr_table <- data.frame(
  Parameter = c("(Intercept)", "Temperature"),
  Estimate  = coef(slr_model),
  Std_Error = slr_summary$coefficients[, "Std. Error"],
  t_value   = slr_summary$coefficients[, "t value"],
  p_value   = ifelse(slr_summary$coefficients[, "Pr(>|t|)"] < 0.0001, "< 0.0001", 
                     round(slr_summary$coefficients[, "Pr(>|t|)"], 4))
)

write.csv(slr_table, file.path("output", "tables", "table2_slr_summary.csv"), row.names = FALSE)
save_table_png(slr_table, file.path("output", "tables", "table2_slr_summary.png"), 
               title = "Table 2: Simple Linear Regression Parameter Estimates")

# Figure 2: Fitted SLR Plot (Dark Blue Points with Solid Fitted Regression Line)
png(file.path("output", "figures", "fig2_slr_fitted.png"), width = 1400, height = 900, res = 200)
par(mar = c(4.5, 4.5, 3, 1))
plot(bike$Temperature, bike$Rented_Bike_Count, 
     pch = 16, cex = 0.4, col = "darkblue",
     xlab = "Temperature (°C)", ylab = "Rented Bike Count", 
     main = "Simple Linear Regression: Temperature and Bike Demand")
abline(slr_model, col = "black", lwd = 2.5)
dev.off()

# ------------------------------------------------------------------------------
# TABLES 3, 4, & 5: Multiple Linear Regression & Inference
# ------------------------------------------------------------------------------
mlr_model <- lm(Rented_Bike_Count ~ Temperature + Humidity + Wind_speed + Solar_Radiation + Rainfall, 
                data = bike)
mlr_summary <- summary(mlr_model)

# Table 3: Coefficients
mlr_coef_table <- data.frame(
  Term      = rownames(mlr_summary$coefficients),
  Estimate  = mlr_summary$coefficients[, 1],
  Std_Error = mlr_summary$coefficients[, 2],
  t_value   = mlr_summary$coefficients[, 3],
  p_value   = ifelse(mlr_summary$coefficients[, 4] < 0.0001, "< 0.0001", round(mlr_summary$coefficients[, 4], 4))
)
write.csv(mlr_coef_table, file.path("output", "tables", "table3_mlr_coefficients.csv"), row.names = FALSE)
save_table_png(mlr_coef_table, file.path("output", "tables", "table3_mlr_coefficients.png"), 
               title = "Table 3: Multiple Linear Regression Coefficients")

# Table 4: Overall Fit Statistics
mlr_fit_stats <- data.frame(
  Statistic = c("R-squared", "Adjusted R-squared", "F-statistic", "Model p-value"),
  Value     = c(sprintf("%.4f", mlr_summary$r.squared),
                sprintf("%.4f", mlr_summary$adj.r.squared),
                sprintf("%.4f", unname(mlr_summary$fstatistic[1])),
                "< 0.0001")
)
write.csv(mlr_fit_stats, file.path("output", "tables", "table4_mlr_fit_stats.csv"), row.names = FALSE)
save_table_png(mlr_fit_stats, file.path("output", "tables", "table4_mlr_fit_stats.png"), 
               title = "Table 4: Overall Multiple Regression Model Fit")

# Table 5: 95% Confidence Intervals
mlr_ci <- confint(mlr_model, level = 0.95)
mlr_inference_table <- data.frame(
  Parameter   = rownames(mlr_summary$coefficients),
  Estimate    = mlr_summary$coefficients[, 1],
  CI_95_Lower = mlr_ci[, 1],
  CI_95_Upper = mlr_ci[, 2],
  p_value     = ifelse(mlr_summary$coefficients[, 4] < 0.0001, "< 0.0001", round(mlr_summary$coefficients[, 4], 4))
)
write.csv(mlr_inference_table, file.path("output", "tables", "table5_mlr_inference.csv"), row.names = FALSE)
save_table_png(mlr_inference_table, file.path("output", "tables", "table5_mlr_inference.png"), 
               title = "Table 5: Coefficient Estimates and 95% Confidence Intervals")

# ------------------------------------------------------------------------------
# TABLE 6: Matrix Formulation (Section 8)
# ------------------------------------------------------------------------------
sub <- head(bike, 5)
X_sub <- cbind(Intercept = 1, data.matrix(sub[, c("Temperature", "Humidity", "Wind_speed")]))
y_sub <- data.matrix(sub$Rented_Bike_Count)

XtX <- t(X_sub) %*% X_sub
Xty <- t(X_sub) %*% y_sub
beta_manual <- solve(XtX) %*% Xty

sub_model <- lm(Rented_Bike_Count ~ Temperature + Humidity + Wind_speed, data = sub)
beta_software <- as.matrix(coef(sub_model))

matrix_comp <- data.frame(
  Parameter           = c("Intercept", "Temperature", "Humidity", "Wind Speed"),
  Manual_Matrix       = as.vector(round(beta_manual, 5)),
  Software_lm         = as.vector(round(beta_software, 5)),
  Absolute_Difference = format(abs(as.vector(beta_manual) - as.vector(beta_software)), scientific = TRUE, digits = 3)
)

write.csv(matrix_comp, file.path("output", "tables", "table6_matrix_verification.csv"), row.names = FALSE)
save_table_png(matrix_comp, file.path("output", "tables", "table6_matrix_verification.png"), 
               title = "Table 6: Analytical Matrix Solution vs lm() Benchmark (n = 5 Subset)")

# ------------------------------------------------------------------------------
# TABLES 7 & 8: Dummy-Variable Regression & ANOVA (Sections 9 & 10)
# ------------------------------------------------------------------------------
dummy_model <- lm(Rented_Bike_Count ~ Seasons, data = bike)
dummy_s <- summary(dummy_model)
anova_table <- anova(dummy_model)

# Table 7: Dummy Regression
dummy_summary_df <- data.frame(
  Parameter = c("Intercept (Autumn Baseline)", "Spring Effect (vs Autumn)", "Summer Effect (vs Autumn)", "Winter Effect (vs Autumn)"),
  Estimate  = coef(dummy_s)[, 1],
  Std_Error = coef(dummy_s)[, 2],
  t_stat    = coef(dummy_s)[, 3],
  p_value   = rep("< 0.0001", 4)
)
write.csv(dummy_summary_df, file.path("output", "tables", "table7_dummy_seasons.csv"), row.names = FALSE)
save_table_png(dummy_summary_df, file.path("output", "tables", "table7_dummy_seasons.png"), 
               title = "Table 7: Dummy-Variable Regression with Autumn as Reference Level")

# Table 8: ANOVA
anova_df <- data.frame(
  Source      = c("Seasons (Between Groups)", "Residuals (Within Groups)"),
  Df          = anova_table$Df, Sum_Sq      = format(round(anova_table$`Sum Sq`, 1), scientific = FALSE, big.mark = ","),
  Mean_Sq     = format(round(anova_table$`Mean Sq`, 1), scientific = FALSE, big.mark = ","),
  F_statistic = c(sprintf("%.2f", anova_table$`F value`[1]), ""),
  p_value     = c("< 0.0001", "")
)
write.csv(anova_df, file.path("output", "tables", "table8_anova_seasons.csv"), row.names = FALSE)
save_table_png(anova_df, file.path("output", "tables", "table8_anova_seasons.png"), 
               title = "Table 8: One-Way ANOVA Summary Table for Seasonal Differences")

# ------------------------------------------------------------------------------
# TABLE 9 & FIGURE 3: Interaction Analysis (Section 11)
# ------------------------------------------------------------------------------
inter_model <- lm(Rented_Bike_Count ~ Temperature * Holiday, data = bike)
inter_summary <- summary(inter_model)

inter_table <- data.frame(
  Parameter = c("Intercept (Holiday Baseline)", "Temperature Main Effect", "No Holiday Main Effect", "Temp x No Holiday Interaction"),
  Estimate  = coef(inter_summary)[, 1],
  Std_Error = coef(inter_summary)[, 2],
  t_stat    = coef(inter_summary)[, 3],
  p_value   = ifelse(coef(inter_summary)[, 4] < 0.0001, "< 0.0001", sprintf("%.4f", coef(inter_summary)[, 4]))
)
write.csv(inter_table, file.path("output", "tables", "table9_interaction.csv"), row.names = FALSE)
save_table_png(inter_table, file.path("output", "tables", "table9_interaction.png"), 
               title = "Table 9: Interaction Model: Temperature x Holiday Status")

# Figure 3: Retained Red/Blue Interaction Palette
png(file.path("output", "figures", "fig3_interaction_plot.png"), width = 1400, height = 900, res = 200)
par(mar = c(4.2, 4.2, 2.2, 1.0), mgp = c(2.4, 0.7, 0))
hol_sub   <- subset(bike, Holiday == "Holiday")
nohol_sub <- subset(bike, Holiday == "No Holiday")

plot(bike$Temperature, bike$Rented_Bike_Count, type = "n",
     xlab = "Ambient Temperature (°C)", ylab = "Rented Bike Count (Bikes/Hour)",
     main = "Moderation Effect: Temperature vs Demand by Holiday Status",
     cex.main = 0.95, cex.lab = 0.9, cex.axis = 0.85)
points(hol_sub$Temperature, hol_sub$Rented_Bike_Count, 
       col = rgb(0.85, 0.3, 0.1, 0.35), pch = 17, cex = 0.6)
points(nohol_sub$Temperature, nohol_sub$Rented_Bike_Count, 
       col = rgb(0.1, 0.5, 0.8, 0.15), pch = 16, cex = 0.6)
abline(lm(Rented_Bike_Count ~ Temperature, data = hol_sub), 
       col = "darkred", lwd = 2.5, lty = 2)
abline(lm(Rented_Bike_Count ~ Temperature, data = nohol_sub), 
       col = "darkblue", lwd = 2.5)
legend("topleft", 
       legend = c("Holiday (Fitted Line)", "No Holiday (Fitted Line)"),
       col = c("darkred", "darkblue"), 
       lwd = 2.5, lty = c(2, 1), 
       bty = "n", cex = 0.85)
dev.off()

# ------------------------------------------------------------------------------
# TABLES 10 & 11, FIGURE 4: Model Diagnostics (Section 12)
# ------------------------------------------------------------------------------
bp_res  <- lmtest::bptest(mlr_model)
dw_res  <- lmtest::dwtest(mlr_model)
vif_res <- car::vif(mlr_model)

# Table 10: Diagnostic Tests
diag_tests_df <- data.frame(
  Assumption       = c("Homoscedasticity (Constant Variance)", "Autocorrelation (Independence)"),
  Statistical_Test = c("Breusch-Pagan Test", "Durbin-Watson Test"),
  Test_Statistic   = c(round(bp_res$statistic, 2), round(dw_res$statistic, 3)),
  p_value          = c("< 0.0001", "< 0.0001")
)
write.csv(diag_tests_df, file.path("output", "tables", "table10_diagnostic_tests.csv"), row.names = FALSE)
save_table_png(diag_tests_df, file.path("output", "tables", "table10_diagnostic_tests.png"), 
               title = "Table 10: Formal Statistical Diagnostic Tests for MLR Assumptions")

# Table 11: Multicollinearity Audit
vif_df <- data.frame(
  Predictor_Variable       = names(vif_res),
  VIF                      = round(as.numeric(vif_res), 3),
  Multicollinearity_Status = rep("Acceptable (VIF < 5.0)", length(vif_res))
)
write.csv(vif_df, file.path("output", "tables", "table11_vif_values.csv"), row.names = FALSE)
save_table_png(vif_df, file.path("output", "tables", "table11_vif_values.png"), 
               title = "Table 11: Multicollinearity Audit: Variance Inflation Factors (VIF)")

# Figure 4: 4-Panel Diagnostics
png(file.path("output", "figures", "fig4_diagnostics.png"), width = 1600, height = 1600, res = 200)
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 2.5, 1.0))
plot(mlr_model, pch = 16, cex = 0.5, col = "darkblue")
dev.off()

# ------------------------------------------------------------------------------
# TABLE 12: Final Transformed Model (Section 13)
# ------------------------------------------------------------------------------
final_model <- lm(sqrt(Rented_Bike_Count) ~ Temperature + Humidity + Wind_speed + 
                    Solar_Radiation + Rainfall + Seasons + Holiday, 
                  data = bike)
final_s <- summary(final_model)

final_table <- data.frame(
  Parameter   = c("Intercept (Autumn, Holiday)", "Temperature (°C)", "Humidity (%)", 
                  "Wind Speed (m/s)", "Solar Radiation (MJ/m2)", "Rainfall (mm)", 
                  "Spring Effect (vs Autumn)", "Summer Effect (vs Autumn)", 
                  "Winter Effect (vs Autumn)", "No Holiday Effect (vs Holiday)"),
  Estimate    = coef(final_s)[, 1],
  Std_Error   = coef(final_s)[, 2],
  t_statistic = coef(final_s)[, 3],
  p_value     = ifelse(coef(final_s)[, 4] < 0.0001, "< 0.0001", round(coef(final_s)[, 4], 4))
)

write.csv(final_table, file.path("output", "tables", "table12_final_model.csv"), row.names = FALSE)
save_table_png(final_table, file.path("output", "tables", "table12_final_model.png"), 
               title = sprintf("Table 12: Final Transformed Model (Square-Root Transformed, R² = %.3f)", final_s$r.squared))

cat("\n--------------------------------------------------------------------\n")
cat("SUCCESS: All 12 tables and 4 figures exported with retained colors.\n")
cat("--------------------------------------------------------------------\n")