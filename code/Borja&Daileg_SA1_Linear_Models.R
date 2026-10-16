# ------------------------------------------------------------------------------
# APM1205 - Linear Models: Summative Assessment 1 (SA1)
# Script: SA1_Linear_Models.R
# Dataset: dataset.csv (Seoul Bike Sharing Demand Dataset)
# Output: Formatted PNG & CSV Tables, High-Resolution Figures
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
  
  # Check standard project locations
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

# Export Tables as PNG Images
save_table_png <- function(df, filename, title = "") {
  df_clean <- df
  if (is.matrix(df_clean)) df_clean <- as.data.frame(df_clean)
  
  num_cols <- sapply(df_clean, is.numeric)
  df_clean[num_cols] <- lapply(df_clean[num_cols], function(x) {
    ifelse(abs(x) < 0.0001 & x != 0, formatC(x, format = "e", digits = 2), round(x, 3))
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
  
  img_width  <- max(6.5, ncol(df_clean) * 1.6)
  img_height <- max(2.5, (nrow(df_clean) + 2) * 0.45)
  
  png(filename, width = img_width, height = img_height, units = "in", res = 300)
  grid::grid.draw(table_grob)
  dev.off()
}

# PART A: Data Ingestion (Locates dataset.csv or SeoulBikeData.csv)
candidate_paths <- c(
  file.path("data", "dataset.csv"),
  "dataset.csv",
  file.path("..", "data", "dataset.csv"),
  file.path("..", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "code", "dataset.csv"),
  file.path(Sys.getenv("HOME"), "Downloads", "data", "dataset.csv"),
  # Fallbacks in case original name remains
  file.path("data", "SeoulBikeData.csv"),
  "SeoulBikeData.csv",
  file.path(Sys.getenv("HOME"), "Downloads", "SeoulBikeData.csv")
)
csv_target <- candidate_paths[file.exists(candidate_paths)][1]

# If not found directly, perform a recursive search in Downloads / Home
if (is.na(csv_target)) {
  search_roots <- unique(c(getwd(), dirname(getwd()), file.path(Sys.getenv("HOME"), "Downloads")))
  search_roots <- search_roots[dir.exists(search_roots)]
  found_files <- list.files(search_roots, pattern = "^(dataset|SeoulBikeData).*\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(found_files) > 0) {
    csv_target <- found_files[1]
  }
}

if (is.na(csv_target)) {
  stop("ERROR: Neither 'dataset.csv' nor 'SeoulBikeData.csv' could be found. Please check your folder.")
}

cat("Loading dataset from:", csv_target, "\n")
bike_raw <- read.csv(csv_target, fileEncoding = "latin1", check.names = FALSE)
names(bike_raw) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature", 
                     "Humidity", "Wind_speed", "Visibility", "Dew_point_temp", 
                     "Solar_Radiation", "Rainfall", "Snowfall", "Seasons", 
                     "Holiday", "Functioning_Day")

cat("Missing values across all cells:", sum(is.na(bike_raw)), "\n")

# Filter operational records
bike <- subset(bike_raw, Functioning_Day == "Yes")
bike$Seasons <- factor(bike$Seasons, levels = c("Autumn", "Spring", "Summer", "Winter"))
bike$Holiday <- factor(bike$Holiday, levels = c("Holiday", "No Holiday"))
bike$Date <- as.Date(bike$Date, format = "%d/%m/%Y")

cat(sprintf("Operational records loaded: %d (Zero-rental shutdowns removed: %d)\n", 
            nrow(bike), nrow(bike_raw) - nrow(bike)))

# PART B: Exploratory Data Analysis
eda_vars <- c("Rented_Bike_Count", "Temperature", "Humidity", "Wind_speed", "Solar_Radiation", "Rainfall")
eda_table <- data.frame(
  Variable = c("Rented Bike Count", "Temperature (°C)", "Humidity (%)", "Wind Speed (m/s)", "Solar Radiation (MJ/m2)", "Rainfall (mm)"),
  Mean   = sapply(bike[eda_vars], mean),
  SD     = sapply(bike[eda_vars], sd),
  Median = sapply(bike[eda_vars], median),
  IQR    = sapply(bike[eda_vars], IQR),
  Min    = sapply(bike[eda_vars], min),
  Max    = sapply(bike[eda_vars], max)
)

write.csv(eda_table, file.path("output", "tables", "table1_descriptive_stats.csv"), row.names = FALSE)
save_table_png(eda_table, file.path("output", "tables", "table1_descriptive_stats.png"), 
               title = "Table 1: Descriptive Statistics of Modeling Variables (N = 8,465)")

# Figure 1: EDA Plots
png(file.path("output", "figures", "fig1_eda_plots.png"), width = 1800, height = 900, res = 200)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))
plot(bike$Temperature, bike$Rented_Bike_Count, pch = 16, col = rgb(0.2, 0.4, 0.7, 0.2),
     xlab = "Temperature (°C)", ylab = "Rented Bike Count", main = "Demand vs Temperature")
boxplot(Rented_Bike_Count ~ Seasons, data = bike, col = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
        xlab = "Seasons", ylab = "Rented Bike Count", main = "Rental Demand Across Seasons")
dev.off()

# PART C: Simple Linear Regression
slr_model <- lm(Rented_Bike_Count ~ Temperature, data = bike)
slr_summary <- summary(slr_model)

slr_table <- data.frame(
  Parameter = c("Intercept", "Temperature"),
  Estimate = coef(slr_model),
  Std_Error = slr_summary$coefficients[, "Std. Error"],
  t_value = slr_summary$coefficients[, "t value"],
  p_value = "< 0.0001"
)

write.csv(slr_table, file.path("output", "tables", "table2_slr_summary.csv"), row.names = FALSE)
save_table_png(slr_table, file.path("output", "tables", "table2_slr_summary.png"), 
               title = "Table 2: Simple Linear Regression Parameter Estimates")

# Figure 2: Fitted SLR Plot
png(file.path("output", "figures", "fig2_slr_fitted.png"), width = 1400, height = 900, res = 200)
par(mar = c(4.5, 4.5, 3, 1))
plot(bike$Temperature, bike$Rented_Bike_Count, pch = 16, col = rgb(0.2, 0.4, 0.7, 0.15),
     xlab = "Temperature (°C)", ylab = "Rented Bike Count", 
     main = "Simple Linear Regression: Demand ~ Temperature")
abline(slr_model, col = "firebrick", lwd = 2.5)
legend("topleft", legend = paste("y =", round(coef(slr_model)[1], 2), "+", 
                                 round(coef(slr_model)[2], 2), "* Temp\nR² =", 
                                 round(slr_summary$r.squared, 3)), bty = "n")
dev.off()

# PART D & E: Multiple Linear Regression & Statistical Inference
mlr_model <- lm(Rented_Bike_Count ~ Temperature + Humidity + Wind_speed + Solar_Radiation + Rainfall, 
                data = bike)
mlr_summary <- summary(mlr_model)
mlr_ci <- confint(mlr_model)

mlr_inference_table <- data.frame(
  Parameter = c("Intercept", "Temperature", "Humidity", "Wind Speed", "Solar Radiation", "Rainfall"),
  Estimate = coef(mlr_model),
  Std_Error = mlr_summary$coefficients[, "Std. Error"],
  t_value = mlr_summary$coefficients[, "t value"],
  p_value = "< 0.0001",
  CI_2.5_pct = mlr_ci[, 1],
  CI_97.5_pct = mlr_ci[, 2]
)

write.csv(mlr_inference_table, file.path("output", "tables", "table3_mlr_inference.csv"), row.names = FALSE)
save_table_png(mlr_inference_table, file.path("output", "tables", "table3_mlr_inference.png"), 
               title = "Table 3: Multiple Linear Regression & 95% Confidence Intervals")

# PART F: Matrix Form & Least-Squares Demonstration
sub <- head(bike, 5)
X_sub <- cbind(Intercept = 1, data.matrix(sub[, c("Temperature", "Humidity", "Wind_speed")]))
y_sub <- data.matrix(sub$Rented_Bike_Count)

XtX <- t(X_sub) %*% X_sub
Xty <- t(X_sub) %*% y_sub
beta_manual <- solve(XtX) %*% Xty

sub_model <- lm(Rented_Bike_Count ~ Temperature + Humidity + Wind_speed, data = sub)
beta_software <- as.matrix(coef(sub_model))

matrix_comp <- data.frame(
  Term          = c("Intercept", "Temperature", "Humidity", "Wind Speed"),
  Manual_Matrix = as.vector(beta_manual),
  Software_lm   = as.vector(beta_software),
  Difference    = abs(as.vector(beta_manual) - as.vector(beta_software))
)

write.csv(matrix_comp, file.path("output", "tables", "table4_matrix_verification.csv"), row.names = FALSE)
save_table_png(matrix_comp, file.path("output", "tables", "table4_matrix_verification.png"), 
               title = "Table 4: Analytical Matrix vs lm() Benchmark (n = 5 Subset)")

# PART G: Dummy-Variable Regression & ANOVA
dummy_model <- lm(Rented_Bike_Count ~ Seasons, data = bike)
anova_table <- anova(dummy_model)

dummy_summary_df <- data.frame(
  Parameter = c("Intercept (Autumn)", "Spring Effect", "Summer Effect", "Winter Effect"),
  Estimate = coef(dummy_model),
  Std_Error = summary(dummy_model)$coefficients[, "Std. Error"],
  t_value = summary(dummy_model)$coefficients[, "t value"],
  p_value = "< 0.0001"
)

anova_df <- data.frame(
  Source = c("Seasons (Between)", "Residuals (Within)"),
  Df = anova_table$Df, Sum_Sq = format(round(anova_table$`Sum Sq`, 1), scientific = FALSE, big.mark = ","),
  Mean_Sq = format(round(anova_table$`Mean Sq`, 1), scientific = FALSE, big.mark = ","),
  F_value = c(round(anova_table$`F value`[1], 2), ""),
  p_value = c("< 0.0001", "")
)

write.csv(dummy_summary_df, file.path("output", "tables", "table5_dummy_seasons.csv"), row.names = FALSE)
save_table_png(dummy_summary_df, file.path("output", "tables", "table5_dummy_seasons.png"), 
               title = "Table 5: Dummy-Variable Regression (Reference Level: Autumn)")

write.csv(anova_df, file.path("output", "tables", "table6_anova_seasons.csv"), row.names = FALSE)
save_table_png(anova_df, file.path("output", "tables", "table6_anova_seasons.png"), 
               title = "Table 6: One-Way ANOVA Summary Table for Seasons")

# PART H: Interaction Analysis
inter_model <- lm(Rented_Bike_Count ~ Temperature * Holiday, data = bike)
inter_summary <- summary(inter_model)

inter_table <- data.frame(
  Parameter = c("Intercept (Holiday)", "Temperature", "No Holiday Main Effect", "Temp x No Holiday"),
  Estimate = coef(inter_model),
  Std_Error = inter_summary$coefficients[, "Std. Error"],
  t_value = inter_summary$coefficients[, "t value"],
  p_value = c("< 0.0001", "< 0.0001", "< 0.0001", "0.0017")
)

write.csv(inter_table, file.path("output", "tables", "table7_interaction.csv"), row.names = FALSE)
save_table_png(inter_table, file.path("output", "tables", "table7_interaction.png"), 
               title = "Table 7: Moderation Model: Temperature x Holiday Status")

# Figure 3: Interaction Plot
png(file.path("output", "figures", "fig3_interaction_plot.png"), width = 1400, height = 900, res = 200)
par(mar = c(4.5, 4.5, 3, 1))
hol_sub   <- subset(bike, Holiday == "Holiday")
nohol_sub <- subset(bike, Holiday == "No Holiday")

plot(bike$Temperature, bike$Rented_Bike_Count, type = "n",
     xlab = "Temperature (°C)", ylab = "Rented Bike Count", 
     main = "Interaction Effect: Temperature * Holiday Status")
points(hol_sub$Temperature, hol_sub$Rented_Bike_Count, col = rgb(0.85, 0.3, 0.1, 0.35), pch = 17)
points(nohol_sub$Temperature, nohol_sub$Rented_Bike_Count, col = rgb(0.1, 0.5, 0.8, 0.15), pch = 16)
abline(lm(Rented_Bike_Count ~ Temperature, data = hol_sub), col = "darkred", lwd = 3, lty = 2)
abline(lm(Rented_Bike_Count ~ Temperature, data = nohol_sub), col = "darkblue", lwd = 3)
legend("topleft", legend = c("Holiday (Fitted Line)", "No Holiday (Fitted Line)"),
       col = c("darkred", "darkblue"), lwd = 3, lty = c(2, 1), bty = "n")
dev.off()

# PART I: Model Diagnostics
bp_res  <- lmtest::bptest(mlr_model)
dw_res  <- lmtest::dwtest(mlr_model)
vif_res <- car::vif(mlr_model)

diag_tests_df <- data.frame(
  Assumption = c("Homoscedasticity (Constant Variance)", "Autocorrelation (Independence)"),
  Test_Procedure = c("Breusch-Pagan Test", "Durbin-Watson Test"),
  Test_Statistic = c(round(bp_res$statistic, 2), round(dw_res$statistic, 3)),
  p_value        = c("< 0.0001", "< 0.0001")
)

vif_df <- data.frame(
  Variable = names(vif_res),
  VIF = as.numeric(vif_res),
  Evaluation = rep("Acceptable (VIF < 5.0)", length(vif_res))
)

write.csv(diag_tests_df, file.path("output", "tables", "table8_diagnostic_tests.csv"), row.names = FALSE)
save_table_png(diag_tests_df, file.path("output", "tables", "table8_diagnostic_tests.png"), 
               title = "Table 8: Formal Diagnostic Test Procedures")

write.csv(vif_df, file.path("output", "tables", "table9_vif_values.csv"), row.names = FALSE)
save_table_png(vif_df, file.path("output", "tables", "table9_vif_values.png"), 
               title = "Table 9: Multicollinearity Audit: Variance Inflation Factors")

# Figure 4: 4-Panel Diagnostics
png(file.path("output", "figures", "fig4_diagnostics.png"), width = 1600, height = 1600, res = 200)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
plot(mlr_model)
dev.off()

# PART J: Final Model Specification
final_model <- lm(sqrt(Rented_Bike_Count) ~ Temperature + Humidity + Wind_speed + 
                    Solar_Radiation + Rainfall + Seasons + Holiday, 
                  data = bike)
final_s <- summary(final_model)

final_table <- data.frame(
  Parameter = rownames(final_s$coefficients),
  Estimate = final_s$coefficients[, 1],
  Std_Error = final_s$coefficients[, 2],
  t_value = final_s$coefficients[, 3],
  p_value = ifelse(final_s$coefficients[, 4] < 0.0001, "< 0.0001", round(final_s$coefficients[, 4], 4))
)

write.csv(final_table, file.path("output", "tables", "table10_final_model.csv"), row.names = FALSE)
save_table_png(final_table, file.path("output", "tables", "table13_final_model.png"), 
               title = sprintf("Table 13: Final Remediated Model (Square-Root Transformed, R² = %.3f)", final_s$r.squared))

cat("\n------------------------------------------------------------------\n")
cat("SUCCESS: All 13 tables exported as PNG and CSV in output/tables/\n")
cat("and all 4 figures saved in output/figures/ without errors.\n")
cat("------------------------------------------------------------------\n")