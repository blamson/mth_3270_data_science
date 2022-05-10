# Check counts for each type of category
data_2017 %>%
    dplyr::group_by(fourcat) %>%
    dplyr::summarise(count = n())

# NOTE: THE WORK DONE IN THIS SCRIPT MIMICS THAT DONE IN RANDOM FOREST
# FOR MORE DETAILED EXPLANATIONS ON MY PROCESS REFER TO THAT SCRIPT

# ---------------------------------[ NEURAL NETWORK ]-----------------------------------------------

# For this recipe I'll do slightly more pre-processing. We'll do data imputation again and also normalize
# our numeric predictors to make sure they're all on the same scale. 

nn_recipe <-
    recipe(fourcat ~ student_faculty_ratio + grad_rate + enrollment, data = train) %>%
    step_impute_mean(student_faculty_ratio, grad_rate) %>%
    step_impute_median(enrollment) %>%
    step_normalize(all_numeric_predictors())

nn_modeling_process <- function(recipe, k = 10){
    
    # Model tuning k = 10 by default ----
    nn_model <-
        mlp(
            hidden_units = k
        ) %>%
        set_engine("nnet") %>%
        set_mode("classification")
    
    nn_wflow <-
        workflow() %>%
        add_model(nn_model) %>%
        add_recipe(nn_recipe)
    
    # Bootstrap the training data 5 times ----
    bstraps <-
        bootstraps(
            train,
            times = 5,
            strata = fourcat
        )
    
    resample_results <-
        nn_wflow %>%
        fit_resamples(
            resamples = bstraps,
            # List off metrics I'm interested in capturing
            metrics = metric_set(accuracy),
            # Save predictions for confusion matrix
            control = control_resamples(save_pred = TRUE)
        )
    
    # Accuracy of the model on training set ----
    training_table <-
        resample_results %>%
        collect_metrics() %>% 
        dplyr::select(-c(n, std_err)) %>%
        dplyr::rename(.estimate = mean) %>%
        add_column(set = "training")
    
    final_fit <-
        nn_wflow %>%
        last_fit(split)
    
    # Show collected metrics for the final fit, combine with training metrics
    final_fit %>%
        collect_metrics() %>%
        add_column(set = "testing") %>%
        dplyr::bind_rows(training_table) %>%
        dplyr::select(-c(.estimator, .config)) %>%
        kableExtra::kbl(caption = glue::glue("Final Metrics, k = {k})"), digits = 3) %>%
        kableExtra::kable_classic_2(full_width = F)
}

nn_modeling_process(nn_recipe, k = 10)
nn_modeling_process(nn_recipe, k = 15)
nn_modeling_process(nn_recipe, k = 5)
