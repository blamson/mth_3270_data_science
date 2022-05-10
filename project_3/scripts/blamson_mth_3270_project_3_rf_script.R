# Libraries ---------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidymodels)

# Read in / join data -----------------------------------------------------------------------------
# Note: File paths are for my personal machine and will need to be modified for other devices ----
data_2017 <- 
    readr::read_csv("project_1/data/HEsegDataviz_CollegeData_4-year_v5.csv") %>%
    # Filter out only 2017 data ---
    dplyr::filter(year == 2017) %>%
    # Join the supplemental data to the 4-year data by the institution name ---
    dplyr::left_join(
        readr::read_csv("project_3/Supp2017Data.csv"), 
        by = c("inst_name" = "institution name")
    ) %>%
    # Remove duplicate ID, year and graduation rate variables ---
    dplyr::select(-c(unitid.y, year.y, year.x, `DRVGR2017_RV.Graduation rate, total cohort...9`)) %>%
    # Rename variables for usability ---
    dplyr::rename(
        id = unitid.x,
        enrollment = `DRVEF2017_RV.Undergraduate enrollment`,
        student_faculty_ratio = `EF2017D_RV.Student-to-faculty ratio`,
        grad_rate = `DRVGR2017_RV.Graduation rate, total cohort...6`,
        tuition_and_fees = `DRVIC2017.Tuition and fees, 2016-17`,
        headcount = `DRVEF122017_RV.12-month unduplicated headcount, undergraduate: 2016-17`
    )

# Set initial seed for reproducibility ----
set.seed(500)

# Split data 75/25 ----
split <- initial_split(data_2017, prop = .75)
train <- training(split)
test <- testing(split)

# ---------------------------------[ RANDOM FOREST ]-----------------------------------------------

# For this recipe I'll do some pretty simple pre-processing. Random Forests dont need much.
# According to the ranger docs, dummy variables aren't even required.
#   - ranger docs: https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html
# What pre-processing that IS necessary is NA handling. For this I will opt for 
# data imputation using the mean of the missing value. I want to avoid removing rows if I can. 
# Another thing of note is that I use median imputation for enrollment, that is due
# To the gigantic difference between the median and mean.

# See enrollment summary statistics
data_2017$enrollment %>% summary()

rf_recipe <-
    recipe(fourcat ~ student_faculty_ratio + grad_rate + enrollment, data = train) %>%
    step_impute_mean(student_faculty_ratio, grad_rate) %>%
    step_impute_median(enrollment)

# Model tuning ----
# First we need to know what our model hyper-parameters even are. 
show_model_info("rand_forest")

# Parameters:
# - mtry: Number of variables to use. 
# - trees: Number of decision trees to create
# - min_n: Minimum node size 

# To tune these parameters I use a method called grid search. Essentially, this creates
# different sets of parameter values from a range I provide. I'll later be able to fit all of these
# models and figure out which is best based on whatever metric I decide.
# The number of combinations uses the formula: levels^(number of parameters to tune)
# As such care is necessary to ensure minimum processing time on weaker machines

# Specify what model parameters to tune ----
tune_spec <-
    rand_forest(
        mtry = tune(),
        trees = tune(),
        min_n = tune()
    ) %>%
    set_engine("ranger") %>%
    set_mode("classification")

# set up a grid for re sampling ----
tree_grid <- 
    grid_regular(
        mtry() %>% range_set(c(1,3)),
        trees() %>% range_set(c(50,300)),
        min_n() %>% range_set(c(2,10)),
        levels = 3
    )

# Create workflow using our various tuned models and recipe ----
tuned_wf <-
    workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(rf_recipe)

# Generate our re-sampled data using cross-validation ----
folds <- vfold_cv(train, v = 10, strata = fourcat)

# Fit the various models to the re-sampled data ----
# tic() and toc() used to gauge how long this process takes
tictoc::tic()
tree_results <-
    tuned_wf %>%
    tune_grid(
        resamples = folds,
        grid = tree_grid,
        metrics = metric_set(f_meas, roc_auc, accuracy),
        control = control_resamples(save_pred = TRUE)
    )
tictoc::toc()

# This is our dataset of metrics for each combination of values.
tree_results %>% 
    collect_metrics()

# Create condensed table for writeup ----
tree_results %>%
    collect_metrics() %>%
    dplyr::filter(.metric == "accuracy") %>%
    head(n = 10) %>%
    kableExtra::kbl(caption = "Grid Search Models", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)

# We can pull different models from this based on what metric we want to optimize! 
# For the sake of the project I will do this 3 times, one for each metric. We'll then look
# at the accuracy for each of them and compare.

# ------------------- Select model based on f_measure ------------------
best_tree <- 
    tree_results %>%
    select_best("f_meas")

# Now we finalize, or update, the workflow 
final_wf <-
    tuned_wf %>%
    finalize_workflow(best_tree)

# This fits the model to the full training set and evaluates the 
# finalized model on the testing data
final_fit <-
    final_wf %>%
    last_fit(split)

# Show collected metrics for the final fit
final_fit %>%
    collect_metrics() %>%
    kableExtra::kbl(caption = "Final Metrics (f_measure choice)", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)

# Parameters of this tree
best_tree %>%
    kableExtra::kbl(caption = "Best f_measure parameters", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)

# ------------------- Select model based on f_accuracy ------------------
best_tree <- 
    tree_results %>%
    select_best("accuracy")

# Now we finalize, or update, the workflow 
final_wf <-
    tuned_wf %>%
    finalize_workflow(best_tree)

# This fits the model to the full training set and evaluates the 
# finalized model on the testing data
final_fit <-
    final_wf %>%
    last_fit(split)

# Show collected metrics for the final fit
final_fit %>%
    collect_metrics() %>%
    kableExtra::kbl(caption = "Final Metrics (accuracy choice)", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)

# Parameters of this tree
best_tree %>%
    kableExtra::kbl(caption = "Best accuracy parameters", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)


# ------------------- Select model based on roc_auc ------------------
best_tree <- 
    tree_results %>%
    select_best("roc_auc")

# Now we finalize, or update, the workflow 
final_wf <-
    tuned_wf %>%
    finalize_workflow(best_tree)

# This fits the model to the full training set and evaluates the 
# finalized model on the testing data
final_fit <-
    final_wf %>%
    last_fit(split)

# Show collected metrics for the final fit
final_fit %>%
    collect_metrics() %>%
    kableExtra::kbl(caption = "Final Metrics (roc_auc) choice)", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)

# Parameters of this tree
best_tree %>%
    kableExtra::kbl(caption = "Best roc_auc parameters", digits = 3) %>%
    kableExtra::kable_classic_2(full_width = F)
    