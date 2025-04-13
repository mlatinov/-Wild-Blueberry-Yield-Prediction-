
#### Libraries ####
library(tidymodels)
library(tidyverse)
library(baguette)
library(doFuture)
library(future)
library(patchwork)
library(finetune)
library(scimo)
tidymodels_prefer()

# Env Spec
# Plan for using 3 cores
options(tidymodels.dark = TRUE)
plan(multisession, workers = 3)

## Load the data 
train <- read_csv("train.csv")

#### Feature engineering ####

## Create a recipe with 4 PCA groups for better interpretability
recipe_pca_4 <- recipe(yield ~ .,data = train) %>%
  
  # Isolate ID
  update_role(id, new_role = "ID") %>%  
  
  # Create new Features
  step_mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    temp_stability = (MaxOfUpperTRange - MinOfUpperTRange) + (MaxOfLowerTRange - MinOfLowerTRange),
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
    ) %>%
  
  # Remove old features 
  step_rm(removals = c(
    "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
    "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
    "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Jonson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Center and Scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # PCA for fruit related Features
  step_pca(columns = c("mass_per_seed","seed_efficiency","clonesize_temp_diff"),prefix = "pca_fruit",num_comp = tune("fruit_pca"),id = "fruit_pca") %>%
  
  # PCA for Temperature-related Feaures
  step_pca(columns = c("average_temp_range","temp_stability"),prefix = "pca_temp",num_comp = tune("temp_pca"),id = "temp_pca") %>%
  
  # PCA for Bee related Features
  step_pca(columns = c("total_polinators","wild_dom_ratio"),prefix = "pca_bee",num_comp = tune("bee_pca"),id = "bee_pca") %>%
  
  # PCA for Rain Related Features
  step_pca(columns = c("rain_anomaly", "rain_per_pollinator"),prefix = "pca_rain_",num_comp = tune("rain_pca"),id = "rain_pca") 

## Grid PCA 4
grid_pca_4 <- parameters(recipe_pca_4) %>%
  update(
    fruit_pca = num_comp(range = c(1,4)),
    temp_pca = num_comp(range = c(1,4)),
    bee_pca = num_comp(range = c(1,4)),
    rain_pca = num_comp(range = c(1,4))
  ) %>%
  grid_space_filling(size = 20)

## Create a recipe with one PCA for performance
recipe_pca_1 <- recipe(yield ~ .,data = train) %>%
  
  # Isolate ID
  update_role(id, new_role = "ID") %>% 
  
  # Create new Features
  step_mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    temp_stability = (MaxOfUpperTRange - MinOfUpperTRange) + (MaxOfLowerTRange - MinOfLowerTRange),
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
  ) %>%
  
  # Remove old features 
  step_rm(removals =  c(
    "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
    "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
    "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Johnson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Center and Scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # PCA 
  step_pca(all_predictors(),num_comp = tune("all_pca"),id = "all_pca")

## Grid PCA 1
grid_pca_1 <- parameters(recipe_pca_1) %>%
  update(
    all_pca = num_comp(range = c(1,10)))%>%
  grid_space_filling(size = 10)

## Create a recipe with one PLS for performance
recipe_pls_1 <- recipe(yield ~ .,data = train) %>%
  
  # Isolate ID
  update_role(id, new_role = "ID") %>% 
  
  # Create new Features
  step_mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    temp_stability = (MaxOfUpperTRange - MinOfUpperTRange) + (MaxOfLowerTRange - MinOfLowerTRange),
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
  ) %>%
  
  # Remove old features 
  step_rm(removals =  c(
    "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
    "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
    "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Johnson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Center and Scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # PLS 
  step_pls(all_predictors(),outcome ="yield",num_comp = tune("all_pls"),id = "all_pls")

## Grid PLS 1
grid_pls_1 <- parameters(recipe_pls_1) %>%
  update(
    all_pls = num_comp(range = c(1,10)))%>%
  grid_space_filling(size = 10)


## Create a recipe with 4 groups pls
recipe_pls_4 <- recipe(yield ~ .,data = train) %>%
  
  # Isolate ID
  update_role(id, new_role = "ID") %>% 

  # Create new Features
  step_mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    temp_stability = (MaxOfUpperTRange - MinOfUpperTRange) + (MaxOfLowerTRange - MinOfLowerTRange),
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
  ) %>%
  
  # Remove old features 
  step_rm(removals =  c(
    "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
    "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
    "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")) %>%

  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Jonson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Center and Scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # PCA for fruit related Features
  step_pls(columns = c("mass_per_seed","seed_efficiency","clonesize_temp_diff"),prefix = "pls_fruit",num_comp = tune("fruit_pls"),id = "fruit_pca",outcome ="yield") %>%
  
  # PCA for Temperature-related Feaures
  step_pls(columns = c(" average_temp_range","temp_stability"),prefix = "pls_temp",num_comp = tune("temp_pls"),id = "temp_pls",outcome ="yield") %>%
  
  # PCA for Bee related Features
  step_pls(columns = c("total_polinators","wild_dom_ratio"),prefix = "pls_bee",num_comp = tune("bee_pls"),id = "bee_pls",outcome ="yield") %>%
  
  # PCA for Rain Related Features
  step_pls(columns = c("rain_anomaly", "rain_per_pollinator"),prefix = "pls_rain_",num_comp = tune("rain_pls"),id = "rain_pls",outcome ="yield")

## Grid PLS 4 
grid_pls_4 <- parameters(recipe_pls_4) %>%
  update(
    fruit_pls = num_comp(range = c(1,4)),
    temp_pls = num_comp(range = c(1,4)),
    bee_pls = num_comp(range = c(1,4)),
    rain_pls = num_comp(range = c(1,4))
  ) %>%
  grid_space_filling(size = 20)


## Create a Hierarchical clustering recipe based on hclust
recipe_hclust_2 <- recipe(yield ~ .,data = train) %>%
  
  # Isolate ID
  update_role(id, new_role = "ID") %>% 
  
  # Create new Features
  step_mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    temp_stability = (MaxOfUpperTRange - MinOfUpperTRange) + (MaxOfLowerTRange - MinOfLowerTRange),
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
  ) %>%
  
  # Remove old features 
  step_rm(removals =  c(
    "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
    "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
    "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Jonson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Center and Scale
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  
  # Apply Hclust
  step_aggregate_hclust(all_numeric_predictors(),n_clusters = 2,linkage_method = "ward.D2",dist_metric = "manhattan",fun_agg = mean)

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
metric <- metric_set(yardstick::mae)

# Tune recipe func
tune_pca <- function(recipe,grid,recipe_id){
  
  # Create a workflow set for tuning the recipes
  workflow_recipe_set <- workflow_set(
    preproc = list(recipe = recipe),
      models = list(
        mlp = mlp,
        bagged_mars_model = bagged_mars_model,
        xgb_model = xgb_model
        ))
  
  # Workflow map 
  tuned_results <- workflow_recipe_set %>%
    workflow_map(
    fn = "tune_grid",
    resamples = vfold_cv(data = train,v = 10,strata = yield),
    grid = grid,
    control = control_grid(verbose = TRUE,save_workflow = TRUE,save_pred = TRUE),
    metrics = metric
  )
  
  # Extract the results
  results_all_details <- tuned_results %>%
   collect_metrics()
  
  # Best results
  best_result <- tuned_results %>%
    extract_workflow_set_result(recipe_id) %>%
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
pca_4_best <- tune_pca(recipe = recipe_pca_4,grid = grid_pca_4,recipe_id = "recipe_mlp")
pca_1_best <- tune_pca(recipe = recipe_pca_1,grid = grid_pca_1,recipe_id = "recipe_mlp")

# PLS recipe
pls_1_best <- tune_pca(recipe = recipe_pls_1,grid = grid_pls_1,recipe_id = "recipe_mlp")
pls_4_best <- tune_pca(recipe = recipe_pls_4,grid = grid_pls_4,recipe_id = "recipe_mlp")

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
    
    ## Neural Networks Models
    mlp = mlp %>% update(hidden_units = tune() ,epochs = tune()),
    bagged_nn = bagged_nn %>% update(hidden_units = tune() ,epochs = tune()),
    
    ## Mars Models 
    mars_model = mars_model %>% update(num_terms = tune(),prod_degree = tune()),
    bagged_mars_model = bagged_mars_model %>% update(num_terms = tune(),prod_degree = tune()),
    
    ## Tree based models
    random_forest = rf_model %>% update(mtry = tune() ,trees = 300 ,min_n = tune()),
    xgb = xgb_model %>% update(mtry = tune() ,trees = tune() ,tree_depth = tune(),learn_rate = tune() ,loss_reduction =tune() ,sample_size = tune()),
    
    ## Control Model
    null_model = null_model),
  cross = TRUE)

# Execute the Light Tune set
tuned_models <- workflow_light_tune %>%
  workflow_map(
    fn = "tune_race_anova",
    resamples = vfold_cv(data = train, v = 10, strata = yield),
    grid = 15,
    control = control_race(save_pred = TRUE, verbose_elim = TRUE, parallel_over = "everything"),
    metrics = metric
  )

# Get all the workflow IDs
wf_ids <- tuned_models$wflow_id

# Extract the best parameters
best_params_func <- function(workflow_map,model_id,metric){
  
  best <- workflow_map %>%
    extract_workflow_set_result(model_id) %>%
    select_best(metric = "mae")
}

# Save the best params from tune_race_anova
best_mlp <- best_params_func(workflow_map = tuned_models,model_id ="pca_4_bagged_nn")

best_bagged_nn <- best_params_func(workflow_map = tuned_models,model_id ="pca_4_bagged_nn")

best_mars <- best_params_func(workflow_map = tuned_models,model_id ="pca_4_mars_model")

best_bagged_mars_model <- best_params_func(workflow_map = tuned_models,model_id = "pca_4_bagged_mars_model")

best_random_forest <- best_params_func(workflow_map = tuned_models,model_id = "pca_4_random_forest")

best_xgb <- best_params_func(workflow_map = tuned_models,model_id = "pca_4_xgb")

# Workflow set Selection
model_select_workflow_set <- workflow_set(
  preproc = list(
    hculst_2 = recipe_hclust_2,
    pca_4 = pca_4_final,
    pca_1 = pca_1_final,
    pls_1 = pls_1_final,
    pls_4 = pls_4_final
    ),
  models = list(
    
    ## Neural Networks Models
    mlp = mlp %>% update(hidden_units = best_mlp$hidden_units ,epochs = best_mlp$epochs),
    bagged_nn = bagged_nn %>% update(hidden_units = best_bagged_nn$hidden_units ,epochs = best_bagged_nn$epochs),
    
    ## Mars Models 
    mars_model = mars_model %>% update(num_terms = best_mars$num_terms,prod_degree = best_mars$prod_degree),
    bagged_mars_model = bagged_mars_model %>% update(num_terms = best_bagged_mars_model$num_terms,prod_degree = best_bagged_mars_model$prod_degree),
    
    ## Tree based models
    random_forest = rf_model %>% update(mtry = best_random_forest$mtry ,trees = 300 ,min_n = best_random_forest$min_n),
    xgb = xgb_model %>% update(mtry = best_xgb$mtry,trees = best_xgb$trees,tree_depth = best_xgb$tree_depth,learn_rate = best_xgb$learn_rate,
                               loss_reduction = best_xgb$loss_reduction ,sample_size = best_xgb$sample_size),
    ## Control
    null_model = null_model),
  cross = TRUE)

## Workflow map 
# Execute workflow selection
model_select_workflow_map <- model_select_workflow_set %>%
  workflow_map(
    fn = "fit_resamples",
    verbose = TRUE,
    resamples = vfold_cv(data = train,v = 10,strata = yield),
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
    scale_fill_viridis_d(option = "A") +
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
      title = "Recipe ~ Models",
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

# Bag Mars
model_selection_func(
  df_collect_metrics = model_select_results,
  model_of_interest = "bag_mars",
  model_resamples = model_select_resamples
)

# XGB
model_selection_func(
  df_collect_metrics = model_select_results,
  model_of_interest = "boost_tree",
  model_resamples = model_select_resamples
)

# Mlp
model_selection_func(
  df_collect_metrics = model_select_results,
  model_of_interest = "mlp",
  model_resamples = model_select_resamples
)


#### Selected Models Tuning ####

# Parameters for tuning
resamples <- vfold_cv(data = train,v = 10,strata = yield)
metrics <- metric_set(mae)
control <- control_grid(verbose = TRUE,save_workflow = TRUE,save_pred = TRUE,allow_par = TRUE)

# SA Control
control_sim_anneal = control_sim_anneal(
  verbose = TRUE,
  no_improve = 20,            # More patience before restart
  restart = 10,               # Allow restarts to escape local minimal
  radius = c(0.15,0.30),      # Encourage larger jumps (default is 0.1)
  cooling = 0.05,             # Faster temperature decay = more exploration early on
)

# BO Control
control_bo <- control_bayes(
  verbose = TRUE,
  verbose_iter = TRUE,
  no_improve = 30,
  save_workflow = TRUE,save_pred = TRUE,
  seed = 123,
  uncertain = 20)

## Bag Mars Model
# Model Spec
bag_mars_tune <- bag_mars(
  num_terms = tune(),
  prod_degree = tune()) %>%
  set_engine("earth",penalty = tune()) %>%
  set_mode("regression")

# Create a LHC grid
bag_mars_grid <- grid_space_filling(
    num_terms(range = c(2, 100)),
    prod_degree(range = c(1, 4)),
    penalty(range = c(-10, 0)),
    size = 60)

# Create a workflow
bag_mars_tune_workflow <- workflow()%>%
  add_model(bag_mars_tune) %>%
  add_recipe(pca_4_final)

# Tune the model
bag_mars_initial <- bag_mars_tune_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = bag_mars_grid,
    metrics = metrics,
    control = control
  )

# Extract the best results
bag_mars_initial_best <- bag_mars_initial %>% select_best()
show_best(bag_mars_initial)

# Update the workflow
bag_mars_tune_workflow <- bag_mars_tune_workflow %>% finalize_workflow(bag_mars_initial_best)

### Optim SA ###

## SA 
sa_optim <- bag_mars_tune_workflow %>%
  tune_sim_anneal(
    resamples = resamples,
    metrics = metrics,
    initial = bag_mars_initial,
    iter = 100,
    control = control_sim_anneal
  )
  
plan(sequential) 

## MLP
# Model Spec
mlp_tuned <- 
  mlp(
    hidden_units = tune(),
    penalty = tune(),
    learn_rate = tune(),
    epochs = tune(),
    activation = "relu"
  ) %>%
  set_engine("brulee",momentum = tune()
    ) %>%
  set_mode("regression")

# Create LHC Grid
mlp_grid <- grid_space_filling(
  hidden_units(range = c(16,256)),
  penalty(range = c(-10,0)),
  learn_rate(range = c(-10,-1)),
  epochs(range = c(10,250)),
  momentum(range = c(0,1)),
  size = 50)

# Create  Workflow
mlp_worklow_tune <- workflow() %>%
  add_model(mlp_tuned)%>%
  add_recipe(pca_4_final)

# Tune MLP
tune_mlp <- mlp_worklow_tune %>%
  tune_grid(
    resamples = resamples,
    grid = mlp_grid,
    metrics = metrics,
    control = control
  )
# Take the best result
mlp_best <- tune_mlp %>% select_best()
show_best(tune_mlp)

## XGB 
xgb_tune <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  sample_size = tune(),
  loss_reduction = tune())%>%
  set_engine(
    "xgboost",
    objective = "reg:absoluteerror",
    eval_metric = "mae") %>%
  set_mode("regression")

# XGB LHC grid
xgb_grid <- grid_space_filling(
  parameters(
  mtry(range = c(3,15)),
  trees(range = c(1000,3000)),
  tree_depth(range = c(1,20)),
  min_n(range = c(2,40)),
  learn_rate(range = c(-4,-1)),
  sample_prop(range = c(0.2,1)),
  loss_reduction(range = c(-10,1.5))),
  size = 50
)

# Create a workflow 
xgb_tune_workflow <- workflow()%>%
  add_model(xgb_tune)%>%
  add_recipe(pca_4_final)

# Tune the model
xgb_initial <- xgb_tune_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = xgb_grid,
    metrics = metrics,
    control = control)

xgb_best <- xgb_initial %>% select_best()
show_best(xgb_initial)

#### Model Explanations ####

### Instance Level ###

## Break-down plots BP
## SHAP 
## LIME
## Ceteris-paribus " What if analysis "
## Local Diagnostics

### Data set Level ###

## Variables importance measures VIM
## Partial Dependence Profiles PDP
## Residuals Diagnostics Plots RDP

#### Stacking ? ####







