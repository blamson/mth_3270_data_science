# Libraries ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidymodels)

# Read in / bind data -----------------------------------------------------------------------------
# Note: File paths are for my personal machine and will need to be modified for other devices ----
data_2017 <- 
    dplyr::bind_rows(
        readr::read_csv("../project_1/data/HEsegDataviz_CollegeData_4-year_v5.csv"), 
        readr::read_csv("../project_1/data/HEsegDataviz_CollegeData_2-year_v5.csv")
    ) %>%
    dplyr::filter(year == 2017) %>%
    dplyr::select(-year)


# ---------------------[Multiple Linear Regression]------------------------------------------------
# For this I want to look at the relationship between a schools representation of a specific
# demographic (hispanic) based on the differences in representation of other groups

# Create a recipe using our desired formula. ----
# Other pre-processing steps can go here, but it doesn't seem necessary this time.
# None of the variables have NAs and are already scaled, so we're good to go.
linear_recipe <- 
    recipe(dif_hispa ~ dif_black + dif_white + dif_asian, data = data_2017) %>%
    step_select(dif_hispa, dif_black, dif_white, dif_asian)

# Set up linear regression model ----
linear_model <-
    linear_reg(mode = "regression", engine = "lm")

# Create workflow ----
linear_worflow <-
    workflow() %>%
    add_model(linear_model) %>%
    add_recipe(linear_recipe) %>%
    fit(data_2017)

# See detailed info on coefficients and save to a kable ----
linear_worflow %>%
    broom::tidy() %>%
    mutate(p.value = p.value %>% as.character()) %>%
    kableExtra::kbl(caption = "Linear Coefficients", digits = 3) %>%
    kableExtra::kable_classic(full_width = F) %>%
    kableExtra::save_kable(file = "linear_table.png", zoom = 3)

# Our coefficients show that as the representation of white, black, and asian
# groups grows, the representation of hispanic communities drops. The intercept
# Shows that even when all three of the other groups are represented properly
# the hispanic community is still underrepresented.

# Bootstrap the training data 5 times ----
# Set seed for consistency
set.seed(2)
bstraps <-
    bootstraps(
        data_2017,
        times = 5
    )

# Fit model to resamples ----
linear_res <-
    linear_worflow %>%
    fit_resamples(
        resamples = bstraps,
        # Extract root mean squared error and r^2
        metrics = metric_set(rmse, rsq)
    )

# Overall metrics for the samples ----
linear_metrics <-
    linear_res %>%
    collect_metrics()

linear_metrics %>%
    select(-.config) %>%
    kableExtra::kbl(caption = "Linear Model Metrics", digits = 3) %>%
    kableExtra::kable_classic(full_width = F) %>%
    kableExtra::save_kable(file = "linear_metrics_table.png", zoom = 3)

# These metrics show us that this model is quite poor. The r^2 is very low indicating a poor 
# fit to the data and the rmse is quite high considering the data we're working with.

# --------------------[Logistic Regression]--------------------------------------------------------
log_recipe <- 
    recipe(public ~ dif_black + dif_white, data = data_2017) %>%
    step_select(public, dif_black, dif_white) %>%
    # Convert classification to factor ----
    step_mutate(public = as.factor(public))

# Set up linear regression model ----
log_model <-
    logistic_reg(mode = "classification") %>%
    set_engine("glm")

# Create workflow and fit the data ----
log_workflow <-
    workflow() %>%
    add_model(log_model) %>%
    add_recipe(log_recipe) %>%
    fit(data_2017)

# Collect coefficient details and save to a kable ---- 
log_workflow %>%
    broom::tidy() %>%
    mutate(p.value = p.value %>% as.character()) %>%
    kableExtra::kbl(caption = "Logistic Coefficients", digits = 3) %>%
    kableExtra::kable_classic(full_width = F) %>%
    kableExtra::save_kable(file = "logistic_table.png", zoom = 3)

# --------------------[Random Forest]--------------------------------------------------------
# First we set up our recipe as always.
# Here we want to omit NAs as only half the rows have this value
# We'll setup the four-year category as a function of the difference
# variables for black, white and Hispanic.
rf_recipe <- 
    recipe(fourcat ~ dif_black + dif_white + dif_hispa, data = data_2017) %>%
    step_select(fourcat, dif_black, dif_white, dif_hispa) %>%
    step_naomit(fourcat, skip = TRUE) %>%
    step_mutate(fourcat = fourcat %>% as.factor())

# Setup a random forest model with hyper-parameters to tune later
rf_model <-
    rand_forest(
        # mtry specifies how many predictors will be selected at each split
        # I use 3 to simply use them all. Better practice would be to utilize
        # a grid system to determine the best value of mtry. 
        mtry = 3,
        trees = 1000
    ) %>%
    set_engine("ranger") %>%
    set_mode("classification")

# Setup our workflow ----
rf_workflow <-
    workflow() %>%
    add_model(rf_model) %>%
    add_recipe(rf_recipe) %>%
    fit(data_2017)

# Set seet, bootstrap the data ----
set.seed(3)
bstraps <-
    bootstraps(
        data_2017,
        times = 5
    )

# Fit model to bootstrapped data ----
rf_bstrap_results <-
    rf_workflow %>%
    fit_resamples(
        resamples = bstraps,
        # Extract f1 score, area under roc curve and accuracy
        metrics = metric_set(f_meas, roc_auc, accuracy),
        control = control_resamples(save_pred = TRUE)
    )

# Showcase random forest metrics ----
rf_bstrap_results %>%
    collect_metrics() %>%
    select(-.config) %>%
    kableExtra::kbl(caption = "Random Forest Model Metrics", digits = 3) %>%
    kableExtra::kable_classic(full_width = F) %>%
    kableExtra::save_kable(file = "rf_metrics_table.png", zoom = 3)

# Show sample of predictions ----
rf_bstrap_results %>%
    collect_predictions() %>%
    select(fourcat, .pred_class) %>%
    rename(
        prediction = .pred_class,
        real_class = fourcat
    ) %>%
    head() %>%
    kableExtra::kbl(caption = "Random Forest Example Predictions") %>%
    kableExtra::kable_classic(full_width = F) %>%
    kableExtra::save_kable(file = "rf_predictions_table.png", zoom = 3)

# Show confusion matrix for predictions ----
rf_bstrap_results %>%
    collect_predictions() %>%
    conf_mat(truth = fourcat, estimate = .pred_class)
