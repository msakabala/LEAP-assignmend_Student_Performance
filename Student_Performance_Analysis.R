# Run this file in a folder containing student-mat.csv.
# Main models exclude G1 and G2 as required.

################################################################################
# FINAL PROJECT SCENARIO 2: UNDERSTANDING STUDENT PERFORMANCE
# Starter R script
#
# This script is designed to run from top to bottom.
# It gives you working code for the required analysis, together with many
# comments explaining what each section does.
#
# IMPORTANT
# 1. The code produces results, but your task is to interpret them.
# 2. Do not copy raw R output into your report without explanation.
# 3. Do not treat an association as evidence of causation.
# 4. Do not use G1 or G2 as predictors in the main regression models.
# 5. You may adapt the suggested predictor sets when you can justify your choices.
################################################################################

# ---------------------------------------------------------------------------- #
# 0. SETUP
# ---------------------------------------------------------------------------- #

# The project uses a small number of common packages.
# If one is missing, R will install it automatically.
needed_packages <- c("ggplot2", "dplyr", "broom")

missing_packages <- needed_packages[!(needed_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(ggplot2)
library(dplyr)
library(broom)

# Make printed numbers a little easier to read.
options(digits = 4)


# ============================================================================ #
# QUESTION 1. UNDERSTAND AND PREPARE THE DATA
# ============================================================================ #

# The project uses the Mathematics-course file from the UCI Student Performance
# dataset. The official dataset page is:
# https://archive.ics.uci.edu/dataset/320/student+performance
#
# The code below downloads the UCI ZIP file if student-mat.csv is not already
# present in your working folder. If your institution or network blocks direct
# downloads from R, download the dataset manually from the UCI page and place
# student-mat.csv in your working folder.

local_file <- "student-mat.csv"

if (!file.exists(local_file)) {
  zip_url <- "https://archive.ics.uci.edu/static/public/320/student+performance.zip"
  zip_file <- tempfile(fileext = ".zip")
  extract_dir <- tempfile("student_data_")
  dir.create(extract_dir)

  download.file(zip_url, zip_file, mode = "wb")

  # Find the Mathematics CSV inside the archive without assuming a particular
  # folder structure.
  zip_contents <- unzip(zip_file, list = TRUE)$Name
  math_name <- zip_contents[grepl("student-mat\\.csv$", zip_contents)]

  if (length(math_name) != 1) {
    stop("Could not identify student-mat.csv inside the downloaded ZIP file.")
  }

  unzip(zip_file, files = math_name, exdir = extract_dir)
  extracted_file <- file.path(extract_dir, math_name)
  file.copy(extracted_file, local_file, overwrite = TRUE)
}

# The UCI file uses semicolons as separators, so read.csv2() is convenient.
students <- read.csv2(local_file, stringsAsFactors = FALSE)

# Look at the first few rows.
head(students)

# Number of rows and columns.
dim(students)

# Variable names.
names(students)

# Structure and data types.
str(students)

# Check missing values and duplicated rows.
colSums(is.na(students))
sum(is.na(students))
sum(duplicated(students))

# Basic summary of every variable.
summary(students)

# Several variables are categorical even though some other variables are coded
# with numbers. Convert the clearly categorical variables to factors.
factor_variables <- c(
  "school", "sex", "address", "famsize", "Pstatus",
  "Mjob", "Fjob", "reason", "guardian", "schoolsup", "famsup",
  "paid", "activities", "nursery", "higher", "internet", "romantic"
)

students[factor_variables] <- lapply(students[factor_variables], factor)

# Create a binary pass/fail outcome from the final grade G3.
# In this project, G3 >= 10 is treated as Pass.
students <- students %>%
  mutate(
    pass = factor(
      ifelse(G3 >= 10, "Pass", "Fail"),
      levels = c("Fail", "Pass")
    )
  )

# Check the new variable.
table(students$pass)

# G1 and G2 are earlier-period grades. They are strongly related to G3.
# We keep them in the dataset because they are part of the original data, but
# we will NOT use them as predictors in the main models. The aim is to examine
# broader student characteristics rather than simply predict the final grade
# from earlier grades.

# FOR YOUR REPORT
# - What does one row represent?
# - What are the main outcomes?
# - Which variables are numerical, categorical or ordered in meaning?
# - Are there missing values, duplicated rows or obvious data-quality problems?
# - Why should information about students be handled and interpreted carefully?


# ============================================================================ #
# QUESTION 2. EXPLORE THE DATA
# ============================================================================ #

# Helper function for common descriptive statistics.
describe_numeric <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

# Descriptive statistics for the final grade.
describe_numeric(students$G3)

# Descriptive statistics for a few potentially useful predictors.
describe_numeric(students$age)
describe_numeric(students$absences)
describe_numeric(students$studytime)

# Frequency table for pass/fail.
table(students$pass)
prop.table(table(students$pass)) * 100

# Frequency tables for two categorical variables.
table(students$sex)
table(students$higher)

# Distribution of the final Mathematics grade.
ggplot(students, aes(x = G3)) +
  geom_histogram(binwidth = 1, boundary = 0, colour = "white") +
  labs(
    title = "Distribution of final Mathematics grade",
    x = "Final grade (G3)",
    y = "Count"
  ) +
  theme_minimal()

# Final grade by weekly study-time category.
# studytime is coded 1 to 4 in the original data.
ggplot(students, aes(x = factor(studytime), y = G3)) +
  geom_boxplot() +
  labs(
    title = "Final grade by weekly study-time category",
    x = "Study-time category",
    y = "Final grade (G3)"
  ) +
  theme_minimal()

# Final grade by intention to continue to higher education.
ggplot(students, aes(x = higher, y = G3)) +
  geom_boxplot() +
  labs(
    title = "Final grade by intention to continue to higher education",
    x = "Wants higher education",
    y = "Final grade (G3)"
  ) +
  theme_minimal()

# Pass/fail percentages by sex.
pass_by_sex <- students %>%
  count(sex, pass) %>%
  group_by(sex) %>%
  mutate(percent = 100 * n / sum(n))

print(pass_by_sex)

ggplot(pass_by_sex, aes(x = sex, y = percent, fill = pass)) +
  geom_col(position = "stack") +
  labs(
    title = "Pass/fail percentages by sex",
    x = "Sex",
    y = "Percentage",
    fill = "Outcome"
  ) +
  theme_minimal()

# FOR YOUR REPORT
# - What does the distribution of G3 look like?
# - What proportion of students pass and fail?
# - Are there clear differences between groups?
# - Are there unusual observations or patterns that matter for later analysis?


# ============================================================================ #
# QUESTION 3. INVESTIGATE RELATIONSHIPS
# ============================================================================ #

# ---- Correlation for selected numerical or ordered variables ----------------

# Some variables below are genuinely numerical, while others are ordered
# categories stored as numbers. Pearson correlations involving ordered category
# codes should therefore be interpreted cautiously rather than treated as exact
# continuous measurements.
cor_vars <- students %>%
  select(
    age, Medu, Fedu, traveltime, studytime, failures,
    famrel, freetime, goout, Dalc, Walc, health, absences, G3
  )

cor_matrix <- cor(cor_vars, use = "complete.obs", method = "pearson")
round(cor_matrix, 2)

# Correlations with the final grade.
g3_correlations <- sort(cor_matrix[, "G3"], decreasing = TRUE)
print(g3_correlations)

# A simple heatmap.
heatmap(
  cor_matrix,
  symm = TRUE,
  margins = c(7, 7),
  main = "Correlation heatmap"
)

# ---- Contingency tables and conditional percentages -------------------------

# Example 1: pass/fail by intention to continue to higher education.
tab_higher <- table(students$pass, students$higher)
print(tab_higher)

# Column percentages answer: within each 'higher' category, what percentage
# passed and what percentage failed?
round(prop.table(tab_higher, margin = 2) * 100, 1)

# Example 2: pass/fail by school support.
tab_support <- table(students$pass, students$schoolsup)
print(tab_support)
round(prop.table(tab_support, margin = 2) * 100, 1)

# ---- Plots for relationships -------------------------------------------------

# Absences and final grade.
ggplot(students, aes(x = absences, y = G3)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Final grade versus absences",
    x = "Number of absences",
    y = "Final grade (G3)"
  ) +
  theme_minimal()

# Previous class failures and final grade.
# failures is a small-count variable, so a boxplot is easier to read than a
# scatterplot with many overlapping points.
ggplot(students, aes(x = factor(failures), y = G3)) +
  geom_boxplot() +
  labs(
    title = "Final grade by number of previous class failures",
    x = "Previous class failures",
    y = "Final grade (G3)"
  ) +
  theme_minimal()

# FOR YOUR REPORT
# - Which student characteristics appear related to final grade?
# - Which categorical characteristics show differences in pass/fail rates?
# - Which variables look most useful for modelling, and why?
# - Where should you be cautious because a numerical code actually represents
#   an ordered category?


# ============================================================================ #
# QUESTION 4. BUILD AND INTERPRET STATISTICAL MODELS
# ============================================================================ #

# ---- Multiple linear regression ---------------------------------------------

# This is a ready-to-run starting model. It deliberately excludes G1 and G2.
# It includes both numerical and categorical predictors, so R will create dummy
# variables automatically for factors such as sex and higher.
linear_model <- lm(
  G3 ~ studytime + failures + absences + sex + higher,
  data = students
)

summary(linear_model)

# Coefficients with confidence intervals.
tidy(linear_model, conf.int = TRUE)

# Model-level information, including R-squared and adjusted R-squared.
glance(linear_model)

# FOR YOUR REPORT
# - Interpret at least two important coefficients.
# - For a categorical predictor, identify the reference category before
#   interpreting the coefficient.
# - Use 'holding the other predictors constant' correctly.
# - What does R-squared mean for this model?


# ---- Logistic regression for pass/fail --------------------------------------

# The outcome 'pass' has levels Fail and Pass. Because Fail is the first level,
# glm() models the probability of the second level, Pass.
logistic_model <- glm(
  pass ~ studytime + failures + absences + sex + higher,
  data = students,
  family = binomial
)

summary(logistic_model)

# Logistic coefficients are on the log-odds scale.
tidy(logistic_model, conf.int = TRUE)

# Exponentiating the coefficients converts them to odds ratios, which are often
# easier to interpret.
logistic_odds_ratios <- tidy(
  logistic_model,
  conf.int = TRUE,
  exponentiate = TRUE
)

print(logistic_odds_ratios)

# Predicted probability of passing for the first few students.
students$predicted_pass_probability <- predict(
  logistic_model,
  type = "response"
)

head(
  students %>%
    select(G3, pass, predicted_pass_probability)
)

# FOR YOUR REPORT
# - Interpret at least two logistic-regression coefficients or odds ratios.
# - Remember: an odds ratio above 1 means higher odds of Pass, while an odds
#   ratio below 1 means lower odds of Pass, holding the other predictors constant.
# - Do not describe an odds ratio as a direct percentage change in probability.
# - What does the logistic model add that the linear model does not?


# ============================================================================ #
# QUESTION 5. EVALUATE THE ANALYSIS
# ============================================================================ #

# ---- Linear-model residuals --------------------------------------------------

diagnostic_data <- augment(linear_model)

ggplot(diagnostic_data, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.65) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals versus fitted values",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

# Look for obvious structure. A strong curve or a clear change in residual
# spread would be a warning sign for the linear model.

# ---- Statistical uncertainty: linear model ----------------------------------

linear_coef_table <- tidy(linear_model, conf.int = TRUE)
print(linear_coef_table)

# ---- Statistical uncertainty: logistic model -------------------------------

logistic_coef_table <- tidy(logistic_model, conf.int = TRUE)
print(logistic_coef_table)

print(logistic_odds_ratios)

# ---- Simple check for relationships among numerical predictors --------------

# This is not a complete multicollinearity diagnostic. It is a simple way to
# notice whether some of the numerical predictors in the model contain strongly
# overlapping information.
model_numeric_cor <- cor(
  students %>% select(studytime, failures, absences),
  use = "complete.obs"
)
round(model_numeric_cor, 2)

# FOR YOUR REPORT
# - Does the residual plot show any clear warning signs?
# - For at least two linear-model coefficients, discuss the estimate and its
#   confidence interval, t-value or p-value.
# - For at least two logistic-model coefficients, discuss the uncertainty using
#   confidence intervals or p-values.
# - Which results are statistically convincing?
# - Which results seem practically important?
# - Are there reasons to be cautious about the model specifications or the
#   overlap between predictors?


# ============================================================================ #
# QUESTION 6. DRAW APPROPRIATE CONCLUSIONS
# ============================================================================ #

# No new code is required for this question.
# Use the results you have already produced and think critically about what they
# do and do not support.

# FOR YOUR REPORT
# Consider the following points:
# - Are these experimental or observational data?
# - Can the regression coefficients be interpreted causally?
# - Could important confounding variables be missing?
# - Some information was collected through questionnaires. How might this affect
#   the quality or interpretation of the data?
# - Would the findings necessarily generalise to students in another school,
#   country or educational system?
# - Which conclusions are supported by the evidence?
# - Which claims would go beyond the evidence?


# ============================================================================ #
# SEPARATE DELIVERABLE: DECISION-MAKER PRESENTATION
# ============================================================================ #

# The presentation is NOT another analysis section and does not require new R
# code. It should communicate the most important results from the report to a
# school or education decision-maker who is not a statistician.
#
# Suggested 4 to 6 slide structure:
# 1. The question and the data
# 2. The most important patterns in the data
# 3. The main linear and/or logistic regression findings
# 4. What the results mean in practical terms
# 5. The main uncertainty or limitation
# 6. What should and should not be concluded
#
# Use only a small number of well-chosen figures or statistics. Avoid raw R
# output and unnecessary technical notation.

################################################################################
# END OF STARTER SCRIPT
################################################################################


# EXTRA: explicit validation of expected ranges
stopifnot(all(students$G3 >= 0 & students$G3 <= 20))
stopifnot(all(students$studytime %in% 1:4))

# EXTRA: useful cross-tabulation by previous failures
print(round(prop.table(table(students$failures, students$pass), 1) * 100, 1))
