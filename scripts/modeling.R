
#### Libraries ####
library(tidymodels)
library(tidyverse)
library(baguette)
library(doFuture)
library(future)
tidymodels_prefer()

# Env Spec
# Plan for using 3 cores
options(tidymodels.dark = TRUE)
plan(multisession, workers = 3)

## Load the data 
train <- read_csv("train.csv")
train <- train %>%
  sample_n(size = 5000)

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
grid_4_pca <- grid_space_filling(params_4_pca,size  = 20)

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

# Create a grid
grid_pls_1 <- parameters(recipe_pls_1) %>%
  update(
    all_pls = num_comp(range = c(1,10)))%>%
  grid_space_filling(size = 10)

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

# Create a Grid
grid_pls_4 <- parameters(recipe_pls_4) %>%
  update(
    fruit_pls = num_comp(range = c(1,3)),
    temp_pls = num_comp(range = c(1,4)),
    bee_pls = num_comp(range = c(1,2)),
    rain_pls = num_comp(range = c(1,2))
  ) %>%
  grid_space_filling(size = 20)
  
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

# Multilayer perceptron 
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

tune_pca <- function(recipe,grid){
  
  # Create a workflow set for tuning the recipes
  workflow_recipe_set <- workflow_set(
    preproc = list(recipe = recipe),
      models = list(
        mlp = mlp,
        bagged_nn = bagged_nn,
        mars_model = mars_model,
        bagged_mars_model = bagged_mars_model,
        random_forest = rf_model,
        xgb = xgb_model,
        null_model = null_model))
  
  # Workflow map 
  tuned_results <- workflow_recipe_set %>%
    workflow_map(
    fn = "tune_grid",
    resamples = vfold_cv(data = train,v = 5,strata = yield),
    grid = grid,
    control = control_grid(save_pred = TRUE,parallel_over = "everything",verbose = TRUE),
    metrics = metric
  )
  
  # Extract the results
  results_all_details <- tuned_results %>%
   collect_metrics()
  
  # Best results
  best_result <- tuned_results %>%
    extract_workflow_set_result("recipe_mlp") %>%
    select_best(metric = "mse")
  
  # Return 
  return(list(
    best = best_result,
    result = results_all_details,
    tuned_results = tuned_results)
  )
}

# Take the best result Model Id = recipe + model

# PCA recipes
pca_4_best <- tune_pca(recipe = recipe_pca_4,grid = grid_4_pca)
pca_1_best <- tune_pca(recipe = recipe_pca_1,grid = grid_pca_1)

# PLS recipe
pls_1_best <- tune_pca(recipe = recipe_pls_1,grid = grid_pls_1)
pls_4_best <- tune_pca(recipe = recipe_pls_4,grid = grid_pls_4)

# Finalize the recipes

# PCA
pca_4_final <- finalize_recipe(recipe_pca_4,parameters = pca_4_best$best)
pca_1_final <- finalize_recipe(recipe_pca_1,parameters = pca_1_best$best)

# PLS
pls_4_final <- finalize_recipe(recipe_pls_4,parameters = pls_4_best$best)
pls_1_final <- finalize_recipe(recipe_pls_1,parameters = pls_1_best$best)

#### Model Selection ####

# Workflow set
model_select_workflow_set <- workflow_set(
  preproc = list(
    pca_4 = pca_4_final,
    pca_1 = pca_1_final,
    pls_1 = pls_1_final,
    pls_4 = pls_4_final),
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
# Execute workflow map
model_select_workflow_map <- model_select_workflow_set %>%
  workflow_map(
    fn = "fit_resamples",
    verbose = TRUE,
    resamples = vfold_cv(data = validation_set,v = 5,strata = yield),
    control = control_grid(save_pred = TRUE,parallel_over = "everything",verbose = TRUE),
    metrics = metric)

# Collect the Results
model_select_results <- collect_metrics(model_select_workflow_map)

## Evaluate Candidates

# Back to default (1 core)
plan(sequential)  

