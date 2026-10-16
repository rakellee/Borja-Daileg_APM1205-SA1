# APM1205 – Summative Assessment 1
## Comprehensive Linear Modeling Analysis

### Students
- **Borja, Rachelle Erika D.**
- **Daileg, Kristine Arllaine S.**

### Course
**APM1205 – Linear Models**

**Section: 1**

### Assessment
**Summative Assessment 1 (SA1) – Comprehensive Linear Modeling Analysis**

---

## 1. Project Overview

This project presents a comprehensive linear modeling analysis of hourly bicycle rental demand using the **Seoul Bike Sharing Demand Dataset**.

The analysis investigates how environmental conditions and calendar-related factors are associated with hourly bike rental demand. The study applies simple and multiple linear regression, statistical inference, matrix-based least-squares estimation, dummy-variable regression, ANOVA, interaction analysis, and regression diagnostics.

The analysis also evaluates the assumptions of ordinary least squares (OLS) and applies a square-root transformation to the response variable as a remedial measure for heteroscedasticity and right-skewness.

---

## 2. Dataset

### Dataset
**Seoul Bike Sharing Demand Dataset**

### Source
**UCI Machine Learning Repository**

The dataset contains hourly information on bicycle rental demand together with weather, seasonal, holiday, and operational information.

The original dataset contains **8,760 hourly observations** covering December 1, 2017 to November 30, 2018.

The response variable used in this analysis is:

- `Rented_Bike_Count`

The principal explanatory variables include:

- `Temperature`
- `Humidity`
- `Wind_speed`
- `Solar_Radiation`
- `Rainfall`
- `Seasons`
- `Holiday`

The analysis excludes observations where `Functioning_Day == "No"`, because those observations represent hours when the bike-sharing system was not operational. This produces an analysis sample of **8,465 functioning hours**.

The dataset is stored in the repository as:

```text
data/dataset.csv
```

Dataset source:

https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand

---

## 3. Research Questions

The analysis addresses four main research questions:

1. **Temperature and Bike Demand:**  
   Does ambient temperature significantly predict hourly bike rental demand in Seoul?

2. **Weather and Bike Demand:**  
   How do temperature, humidity, wind speed, solar radiation, and rainfall jointly explain variation in hourly bike rental demand while controlling for the other weather variables?

3. **Seasonal Differences:**  
   Does the expected mean hourly bicycle demand differ across Autumn, Spring, Summer, and Winter?

4. **Temperature and Holiday Status:**  
   Does the relationship between temperature and bike rental demand differ between holidays and standard operating days?

These research questions are investigated using linear regression, ANOVA, dummy-variable regression, and interaction modeling.

---

## 4. Analysis Summary

The analysis follows the major requirements of the APM1205 Summative Assessment 1.

### Exploratory Data Analysis
Descriptive statistics and graphical displays are used to examine bike demand and its relationships with temperature, humidity, rainfall, and season.

### Simple Linear Regression
A simple linear regression model is fitted using temperature as the predictor of hourly bike rental demand.

The fitted model indicates a positive relationship between temperature and bike demand, with temperature alone explaining approximately **31.7%** of the observed variation in hourly rental demand.

### Multiple Linear Regression
A multiple linear regression model is fitted using:

- Temperature
- Humidity
- Wind speed
- Solar radiation
- Rainfall

The model evaluates the simultaneous contribution of these atmospheric variables to bike rental demand.

### Statistical Inference
Hypothesis tests, regression coefficients, p-values, and 95% confidence intervals are used to evaluate the statistical significance of the predictors.

### Matrix Form and Least Squares
The multiple regression model is expressed in matrix form as:

```text
y = Xβ + ε
```

The least-squares estimator is demonstrated using:

```text
β̂ = (XᵀX)⁻¹Xᵀy
```

A small five-observation subset is used to demonstrate the matrix calculation and compare the manual result with the estimates produced by R's `lm()` function.

### Dummy Variables and ANOVA
The categorical variable `Seasons` is represented using dummy variables with **Autumn as the reference category**. A one-way ANOVA is also performed to test whether mean bike demand differs across seasons.

### Interaction Analysis
An interaction between temperature and holiday status is fitted to determine whether the effect of temperature on bike demand changes between holidays and non-holidays.

### Model Diagnostics
The analysis evaluates:

- Linearity
- Constant variance
- Normality of residuals
- Independence
- Multicollinearity
- Potential influential observations

The diagnostic analysis identifies heteroscedasticity and positive autocorrelation in the untransformed count model.

### Final Model
A square-root transformation of `Rented_Bike_Count` is applied to address the observed right-skewness and heteroscedasticity.

The final model includes:

- Temperature
- Humidity
- Wind speed
- Solar radiation
- Rainfall
- Season
- Holiday status

The final transformed model achieves an **R² of approximately 0.540**.

---

## 5. Software and Packages

The analysis was conducted using **R/RStudio and R Markdown**.

### Required R Packages

The following packages are used in the analysis:

- **car** – variance inflation factor (VIF) analysis
- **lmtest** – Breusch-Pagan and Durbin-Watson tests
- **knitr** – tables and report formatting

The analysis also uses functions from base R, including `lm()`, `anova()`, `confint()`, `plot()`, `solve()`, and other standard statistical and data-processing functions.

For rendering the R Markdown document, an R Markdown environment with the required PDF/HTML rendering dependencies is also needed.

---

## 6. Repository Structure

Borja-Daileg_APM1205-SA1/
.
├── README.md                                                     # Executive project summary, setup, and key results
├── report/
│   ├── Borja&Daileg_SA1_Linear_Models_Report.pdf                 # Compiled academic final report PDF
│
├── code/
│   ├── Borja&Daileg_SA1_Linear_Models.R                          # R script (computes & exports all assets)
│   └── Borja&Daileg_SA1_Linear_Models_Report.Rmd                 # Full reproducible R Markdown report source
│   └── Borja&Daileg_SA1_Linear_Models_Report.html                # Full HTML report source
│
├── data/
│   └── dataset.csv                                               # Filtered operational dataset (SeoulBikeData.csv)
│
└── output/
    ├── figures/                                                  # Generated high-resolution diagnostic & regression plots
    │   ├── Figure1_eda_plots.png                                 # Exploratory scatter & seasonal boxplot
    │   ├── Figure2_slr_fitted.png                                # Simple linear regression fit line
    │   ├── Figure3_interaction_plot.png                          # Temperature × Holiday moderation plot
    │   └── Figure4_diagnostics.png                               # 4-panel OLS residual diagnostic plots
    │
    └── tables/                                                   # Exported CSV data & PNG render tables (1 to 12)
        ├── table1_descriptive_stats.csv / .png
        ├── table2_slr_summary.csv / .png
        ├── table3_mlr_coefficients.csv / .png
        ├── table4_mlr_fit_stats.csv / .png
        ├── table5_mlr_inference.csv / .png
        ├── table6_matrix_verification.csv / .png
        ├── table7_dummy_seasons.csv / .png
        ├── table8_anova_seasons.csv / .png
        ├── table9_interaction.csv / .png
        ├── table10_diagnostic_tests.csv / .png
        ├── table11_vif_values.csv / .png
        └── table12_final_model.csv / .png

---

## 7. Reproducing the Analysis

To reproduce the analysis:

### Step 1: Obtain the Repository

Clone or download this repository and open the project directory in RStudio.

### Step 2: Check the Dataset

Make sure the dataset is located at:

```text
data/dataset.csv
```

The R Markdown file searches for the dataset in the project directories and reads `dataset.csv`.

### Step 3: Install Required Packages

In R, install the required packages if they are not already installed:

```r
install.packages(c("car", "lmtest", "knitr"))
```

The R Markdown file also checks for these packages and installs them automatically when necessary.

### Step 4: Open the R Markdown File

Open:

```text
code/Daileg&Borja_LM_SA1.Rmd
```

in RStudio.

### Step 5: Run or Knit the Analysis

The R Markdown document performs the following:

1. Loads the required packages.
2. Reads `data/dataset.csv`.
3. Checks and prepares the dataset.
4. Removes non-functioning-day observations.
5. Performs exploratory data analysis.
6. Fits the simple linear regression model.
7. Fits the multiple linear regression model.
8. Performs statistical inference.
9. Demonstrates the matrix least-squares solution.
10. Performs dummy-variable regression.
11. Conducts ANOVA.
12. Fits and interprets the temperature-by-holiday interaction.
13. Performs model diagnostics.
14. Applies the square-root transformation.
15. Fits the final transformed regression model.
16. Produces the final report.

### Step 6: Generate the Final Outputs

The R Markdown document can be rendered into both:

```text
LM_SA1.html
LM_SA1.pdf
```

The HTML and PDF versions contain the same complete statistical analysis and are provided as the final report outputs.

---

## 8. Final Findings

The analysis found that environmental and calendar conditions are important predictors of hourly bicycle rental demand.

Temperature has a strong positive association with bike demand, while humidity and rainfall have negative associations in the multiple regression model. Seasonal differences are substantial, with winter showing considerably lower demand than the other seasons.

The interaction analysis also indicates that the relationship between temperature and demand differs according to holiday status.

Diagnostic analysis identified violations of constant variance and independence in the untransformed model. A square-root transformation was therefore used in the final model to improve the behavior of the residuals and stabilize the variance.

The final transformed model explains approximately **54.0% of the variation in transformed hourly bike rental demand**.

---

## 9. References

Dua, D., & Graff, C. (2019). *UCI Machine Learning Repository: Seoul Bike Sharing Demand Dataset*. University of California, Irvine, School of Information and Computer Science.

Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression* (3rd ed.). Sage.

Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W. (2005). *Applied Linear Statistical Models* (5th ed.). McGraw-Hill Irwin.

---

## 10. Reproducibility Statement

All analyses, tables, figures, statistical tests, and model results presented in the final report are generated from the R Markdown source and the dataset included in the repository.

The repository is organized so that another user can obtain the dataset, install the required R packages, run the R Markdown analysis, and reproduce the major results reported in the HTML and PDF outputs.
