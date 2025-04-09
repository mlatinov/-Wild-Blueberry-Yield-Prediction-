
#### Libraries ####
library(tidymodels)
library(tidyverse)
library(baguette)
library(doFuture)
library(future)
library(patchwork)
library(finetune)
tidymodels_prefer()

# Env Spec
# Plan for using 3 cores
options(tidymodels.dark = TRUE)
plan(multisession, workers = 3)

## Load the data 
train <- read_csv("train.csv")

# Split the training data into train and validation 
split <- initial_split(data = train,prop = 0.8,strata = yield)

# Create model_training set and validation set
model_training <- training(split)
validation_set <- testing(split)

#### Feature engineering ####

## Create a recipe with 4 PCA groups for better interpretability
recipe_pca_4 <- recipe(yield ~ .,data = model_training) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Jonson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Normalize the features
  step_normalize(all_predictors()) %>%
  
  # PCA for fruit related features
  step_pca(starts_with("fruit") | contains("seeds"),prefix = "pca_fruit",num_comp = tune("fruit_pca"),id = "fruit_pca") %>%
  
  # PCA for Temperature-related variables
  step_pca(starts_with("AverageOf") | contains("TRange"),prefix = "pca_temp",num_comp = tune("temp_pca"),id = "temp_pca") %>%
  
  # PCA for Bee related featues
  step_pca(columns = c("bumbles", "andrena", "osmia", "honeybee"),prefix = "pca_bee",num_comp = tune("bee_pca"),id = "bee_pca") %>%
  
  # PCA for Rain Related Featues 
  step_pca(c("RainingDays", "AverageRainingDays"),prefix = "pca_rain_",num_comp = tune("rain_pca"),id = "rain_pca") 
  
# Extract the params and update the number of components
params_4_pca <- parameters(recipe_pca_4) %>%
  update(
    fruit_pca = num_comp(range = c(1,3)),
    temp_pca  = num_comp(range = c(1,4)),
    bee_pca = num_comp(range = c(1,2)),
    rain_pca = num_comp(range = c(1,2))
  )

# Create a grid
grid_4_pca <- grid_space_filling(params_4_pca,size  = 10)

## Create a recipe with one PCA for performance
recipe_pca_1 <- recipe(yield ~ .,data = model_training) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Johnson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Normalize the features
  step_normalize(all_predictors()) %>%
  
  # PCA 
  step_pca(all_predictors(),num_comp = tune("all_pca"),id = "all_pca")
  
# Extract params and update the number of components
params_1_pca <- parameters(recipe_pca_1) %>%
  update(
    all_pca = num_comp(range = c(1,10))
  )

# Create a grid 
grid_pca_1 <- grid_space_filling(params_1_pca,size = 10)

## Create a recipe with one PLS for performance
recipe_pls_1 <- recipe(yield ~ .,data = model_training) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Johnson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Normalize the features
  step_normalize(all_predictors()) %>%
  
  # PLS 
  step_pls(all_predictors(),outcome ="yield",num_comp = tune("all_pls"),id = "all_pls")

## Create a recipe with 4 groups pls
recipe_pls_4 <- recipe(yield ~ .,data = model_training) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Jonson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Normalize all features
  step_normalize(all_predictors()) %>%
  
  # PLS for fruit related features
  step_pls(starts_with("fruit") | contains("seeds"),prefix = "pls_fruit",num_comp = tune("fruit_pls"),id = "fruit_pls",outcome = "yield") %>%
  
  # PLS for Temperature-related variables
  step_pls(starts_with("AverageOf") | contains("TRange"),prefix = "pls_temp",num_comp = tune("temp_pls"),id = "temp_pls",outcome = "yield") %>%
  
  # PLS for Bee related features
  step_pls(columns = c("bumbles", "andrena", "osmia", "honeybee"),prefix = "pls_bee",num_comp = tune("bee_pls"),id = "bee_pls",outcome = "yield") %>%
  
  # PLS for Rain Related features
  step_pls(c("RainingDays", "AverageRainingDays"),prefix = "pls_rain_",num_comp = tune("rain_pls"),id = "rain_pls",outcome = "yield") 

#### Model Spec ####

# Null Model
null_model <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

## Tree based models

# Random Forest
rf_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

# XGB
xgb_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

## Mars Models 

# Mars Models Mars
mars_model <- mars() %>%
  set_engine("earth") %>%
  set_mode("regression")

# Bagged Mars Model
bagged_mars_model <- bag_mars() %>%
  set_engine("earth") %>%
  set_mode("regression")

## Neural Networks Models

# Multilayer perception 
mlp <- mlp() %>%
  set_engine("brulee") %>%
  set_mode("regression")

# Bagged Neural Networks 
bagged_nn <- bag_mlp() %>%
  set_engine("nnet") %>%
  set_mode("regression")

## Tune the recipes

# Set Metric
metric <- metric_set(mae)

# Tune recipe func
tune_pca <- function(recipe){
  
  # Create a workflow set for tuning the recipes
  workflow_recipe_set <- workflow_set(
    preproc = list(recipe = recipe),
      models = list(
        mlp = mlp,
        mars_model = mars_model,
        random_forest = rf_model
        ))
  
  # Workflow map Tune_race_anova
  tuned_results <- workflow_recipe_set %>%
    workflow_map(
    fn = "tune_race_anova",
    resamples = vfold_cv(data = train,v = 5,strata = yield),
    grid = 20,
    control = control_race(save_pred = TRUE, parallel_over = "everything", verbose = TRUE),
    metrics = metric
  )
  
  # Extract the results
  results_all_details <- tuned_results %>%
   collect_metrics()
  
  # Best results
  best_result <- tuned_results %>%
    extract_workflow_set_result("recipe_mlp") %>%
    select_best(metric = "mae")
  
  # Return 
  return(list(
    best = best_result,
    result = results_all_details,
    tuned_results = tuned_results)
  )
}

# Take the best result 

# PCA recipes
pca_4_best <- tune_pca(recipe = recipe_pca_4)
pca_1_best <- tune_pca(recipe = recipe_pca_1)

# PLS recipe
pls_1_best <- tune_pca(recipe = recipe_pls_1)
pls_4_best <- tune_pca(recipe = recipe_pls_4)

# Finalize the recipes

# PCA
pca_4_final <- finalize_recipe(recipe_pca_4,parameters = pca_4_best$best)
pca_1_final <- finalize_recipe(recipe_pca_1,parameters = pca_1_best$best)

# PLS
pls_4_final <- finalize_recipe(recipe_pls_4,parameters = pls_4_best$best)
pls_1_final <- finalize_recipe(recipe_pls_1,parameters = pls_1_best$best)

#### Model Selection ####

## Update the Model to Include Tuning Parameters
# Workflow set Light Tune
workflow_light_tune <- workflow_set(
  preproc = list(
    pca_4 = pca_4_final,
    pca_1 = pca_1_final,
    pls_1 = pls_1_final,
    pls_4 = pls_4_final
  ),
  models = list(
    mlp = mlp,
    bagged_nn = bagged_nn %>% update(hidden_units = tune() ,epochs = tune()),
    mars_model = mars_model %>% update(num_terms = tune(),prod_degree = tune()),
    bagged_mars_model = bagged_mars_model %>% update(num_terms = tune(),prod_degree = tune()),
    random_forest = rf_model %>% update(mtry = tune() ,trees = 300 ,min_n = tune()),
    xgb = xgb_model %>% update(mtry = tune() ,trees = tune() ,tree_depth = tune(),learn_rate = tune() ,loss_reduction =tune() ,sample_size = tune()),
    null_model = null_model),
  cross = TRUE)

# Execute the Light Tune set
tuned_models <- workflow_light_tune %>%
  workflow_map(
    fn = "tune_race_anova",
    resamples = vfold_cv(data = validation_set, v = 5, strata = yield),
    grid = 15,
    control = control_race(save_pred = TRUE, verbose_elim = TRUE, parallel_over = "everything"),
    metrics = metric
  )

# Get all the workflow IDs
wf_ids <- tuned_models$wflow_id

# For each workflow, extract the best parameters
best_params_all <- map(wf_ids, ~ tuned_models %>%
                         extract_workflow_set_result(.x) %>%
                         select_best(metric = "mae"))

# List of workflows 
all_workflows <- tuned_models$result

# finalize workflows
final_workflows <- pmap(
  list(
    wf = all_workflows,
    params = best_params_all
  ),
  .f = finalize_workflow
)

# Workflow set Selection
model_select_workflow_set <- workflow_set(
  preproc = list(
    pca_4 = pca_4_final,
    pca_1 = pca_1_final,
    pls_1 = pls_1_final,
    pls_4 = pls_4_final
    ),
  models = list(
    mlp = mlp,
    bagged_nn = bagged_nn,
    mars_model = mars_model,
    bagged_mars_model = bagged_mars_model,
    random_forest = rf_model,
    xgb = xgb_model,
    null_model = null_model),
  cross = TRUE)

## Workflow map 
# Execute workflow selection
model_select_workflow_map <- model_select_workflow_set %>%
  workflow_map(
    fn = "fit_resamples",
    verbose = TRUE,
    resamples = vfold_cv(data = validation_set,v = 10,strata = yield),
    control = control_grid(save_pred = TRUE,parallel_over = "everything",verbose = TRUE),
    metrics = metric)

# Collect the Results
model_select_results <- collect_metrics(model_select_workflow_map)
model_select_resamples <-collect_predictions(model_select_workflow_map)

# Model Selection Function
model_selection_func <- function(df_collect_metrics, model_of_interest, model_resamples) {
  
  # Col plot all models best mean results
  p1 <- df_collect_metrics %>%
    ggplot(aes(
      x = mean,
      y = fct_reorder(factor(wflow_id), .fun = sum, .x = mean, .desc = TRUE),
      fill = model)) +
    geom_col() +
    scale_fill_viridis_d(option = "C") +
    geom_text(aes(label = round(mean, 2), hjust = -0.1),size = 3) +
    theme_minimal() +
    labs(
      title = "Models MAE",
      fill = "Models",
      x = "Mean MAE",
      y = ""
    )
  
  # Heatmap Model ~ Recipe
  p2 <- df_collect_metrics %>%
    filter(model != "null_model") %>%
    mutate(recipe_type = substr(wflow_id, start = 1, stop = 5)) %>%
    ggplot(aes(x = model, y = recipe_type, fill = mean)) +
    geom_tile() +
    scale_fill_viridis_c(option = "A", begin = 0.4, end = 0.9, direction = -1) +
    geom_text(aes(label = round(mean, 2))) +
    theme_minimal() +
    labs(
      title = "Recipe ~ Models mean MAE",
      x = "Models",
      y = "Recipes",
      fill = "Mean MAE"
    )
  
  # Selected Model VB plot
  p3 <- model_resamples %>%
    filter(model == model_of_interest) %>%
    mutate(residuals = yield - .pred) %>%
    ggplot(aes(x = residuals, y = wflow_id, fill = wflow_id)) +
    geom_boxplot() +
    geom_violin(alpha = 0.3) +
    scale_fill_viridis_d(option = "A") +
    theme_minimal() +
    labs(
      title =paste0("Residuals ",model_of_interest,""),
      x = "Residuals",
      fill = "Recipe",
      y = "")
  
  # Combine the plots
  final_plot <- (p1 + p2) / p3 & 
    plot_annotation(title = "Model Selection")
  
  # Return final combined plot
  return(final_plot)
}

# Pick Models to tune
model_selection_func(
  df_collect_metrics = model_select_results,
  model_of_interest = "mlp",
  model_resamples = model_select_resamples
)

# Back to default (1 core)
plan(sequential)  

