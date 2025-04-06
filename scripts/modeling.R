
#### Libraries ####
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

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
  step_pca(starts_with("fruit") | contains("seeds"),prefix = "pca_fruit",num_comp = tune()) %>%
  
  # PCA for Temperature-related variables
  step_pca(starts_with("AverageOf") | contains("TRange"),prefix = "pca_temp",num_comp = tune()) %>%
  
  # PCA for Bee related featues
  step_pca(columns = c("bumbles", "andrena", "osmia", "honeybee"),prefix = "pca_bee",num_comp = tune()) %>%
  
  # PCA for Rain Related Featues 
  step_pca(c("RainingDays", "AverageRainingDays"),prefix = "pca_rain_",num_comp = tune()) %>%
  
  # Remove original PCA inputs
  step_rm(fruitset, fruitmass, seeds,
          RainingDays, AverageRainingDays,
          MinOfUpperTRange, MaxOfLowerTRange, MaxOfUpperTRange,
          MinOfLowerTRange, AverageOfUpperTRange, AverageOfLowerTRange,
          bumbles, andrena, osmia, honeybee)

## Create a recipe with one PCA for performance
recipe_pca_1 <- recipe(yield ~ .,data = model_training) %>%
  
  # Remove near zero var features
  step_nzv(all_predictors()) %>%
  
  # Apply Yeo-Johnson transformation
  step_YeoJohnson(all_predictors()) %>%
  
  # Normalize the features
  step_normalize(all_predictors()) %>%
  
  # PCA for fruit related features
  step_pca(all_predictors(),num_comp = tune()) %>%
  
  # Remove original PCA inputs
  step_rm(fruitset, fruitmass, seeds,
          RainingDays, AverageRainingDays,
          MinOfUpperTRange, MaxOfLowerTRange, MaxOfUpperTRange,
          MinOfLowerTRange, AverageOfUpperTRange, AverageOfLowerTRange,
          bumbles, andrena, osmia, honeybee)















