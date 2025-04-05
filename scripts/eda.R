
#### Libraries ####
library(tidyverse)
library(nortest)
library(corrplot)
library(patchwork)
library(viridis)
library(randomForest)
library(DALEX)
library(iml)
library(future)

# Increase the memory limit
options(future.globals.maxSize = 3 * 1024^3)  # 3 GB

# Load the data
train_set <- read_csv("Data/train.csv")
eda_train <- train_set %>%
  select(-id)

# Viz the distribution
eda_train %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>%
  ggplot(aes(x =Value ,y = Feature,fill = Feature))+
  geom_boxplot()+
  geom_violin(fill = "grey90",alpha = 0.2)+
  scale_fill_viridis_d(option = "A")+
  facet_wrap(~Feature,scale = "free")+
  theme_classic()

# Correclation plot
eda_train %>%
  cor()%>%
  corrplot(method = "color",addCoef.col = "black",order = "hclust",number.cex = 0.7)

#### Hypothesis Testing ####

## Determine whether there is a statistically significant relationship between the predictor variables and yield

# Hypothesis Function
association_function <- function(df,col1,col2){
  
  # Data preparation 
  plot_data <- df %>%
    select({{col1}},{{col2}})%>%
    pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value")
  
  ## Visual Check for normal distribution
  # Density Plot
  p1 <- ggplot(data = plot_data,aes(x = Value,fill = Feature))+
    geom_density()+
    scale_fill_viridis(option = "B",discrete = TRUE,begin = 0.4,end = 0.7,alpha = 0.5)+
    facet_wrap(~Feature,scales = "free")+
    theme_minimal()+
    labs(
      title = "Density Plot",
      x = "",
      y = "")+
      theme(
        title = element_text(face = "italic",size = 12),
        strip.text = element_text(size = 8)
        )
  # QQ Plot
  p2 <- ggplot(data = plot_data,aes(sample = Value,color = Feature))+
    stat_qq()+
    stat_qq_line()+
    scale_color_viridis(option = "B",begin = 0.4,end = 0.7,discrete = TRUE,alpha = 0.5)+
    facet_wrap(~Feature,scales = "free")+
    theme_minimal()+
    labs(
      title = "QQPlot",
      x = "",
      y = ""
      )+
    theme(
      title = element_text(face = "italic",size = 12),
      strip.text = element_text(size = 8),
      legend.position = "none"
      )
  
  ## Statistical test to check for Normal distribution
  
  # Anderson-Darling normality test
  anderson_col1 <- ad.test(x = df[[col1]])
  anderson_col2 <- ad.test(x = df[[col2]])
  
  ## IF Anderson-Darling normality test >= 0.05 Then Use Pearson cor test and linear regression 
  if (anderson_col1$p.value >= 0.05 && anderson_col2$p.value >= 0.05){
    
    ## Correlation Test for Normal Distribution
    # Pearson Cor
    pearson_cor <- cor.test(x = df[[col1]],y =df[[col2]],method = "pearson")
    
    # Linear model
    linear_model <- summary(lm(formula = df[[col2]] ~ df[[col1]],data = df))
    
    # Extract params
    pearson_cor <- pearson_cor$estimate
    linear_model_rs <- linear_model$r.squared
    linear_model_p <- linear_model$coefficients[2, 4]
    
    ## Plots 
    # Viz the relationship
    normal_plot <-ggplot(data = df,aes(x = df[[col1]],y = df[[col2]]))+
      geom_point(colour = "grey80",alpha = 0.2)+
      geom_smooth(method = "lm",colour = "red4")+
      theme_minimal()+
      labs(
        title = paste0("Scatter plot of ", col1, " and ", col2),
        x = col1,
        y = col2,
        subtitle = paste0(
          "Pearson Correlation ~ ",pearson_cor, 
          " R Squared: ",linear_model_rs, 
          " P Value: ", linear_model_p
        ))+
          theme(
            title = element_text(face = "italic",size = 12)
          )
    
    # Combine the plots
    final_plot <- (p1+p2)/normal_plot&
      plot_annotation(
        title = "Relantionships",
        theme = theme_minimal())
    # Return 
    return(final_plot)
  }
  ## IF Anderson-Darling normality test < 0.05 Then Use Spearman cor test and Kendall's Tau Test
  else {
    ## Correlation Test for NON Normal Distribution
    # Spearman Correlation
    spearman <- cor.test(x = df[[col1]],df[[col2]],method = "spearman",exact = FALSE)
    
    # Extract params
    spearman_p_value <- spearman$p.value
    spearman_estimate <- round(spearman$estimate,digits = 2)
    
    ## Plots
    # Viz the relationship
    non_normal_plot <- ggplot(data = df,aes(x = df[[col1]],y = df[[col2]]))+
      geom_point(colour = "grey80",alpha = 0.2)+
      geom_smooth(method = "glm",colour = "red4")+
      theme_minimal()+
      labs(
        title = paste0("Scatter plot of ", col1, " and ", col2),
        x = col1,
        y = col2,
        subtitle = paste0(
          "Spearman Correlation :",spearman_estimate
        ))+
      theme(
        title = element_text(face = "italic",size = 12)
      )
    
    # Combine the plots
    final_plot <- (p1+ p2)/non_normal_plot & 
      plot_annotation(
        title = "Relationships",
        theme = theme_minimal()
      )
    
    # Return 
    return(final_plot)
  }
}

## Test between Yield ~ Positive predictions
association_function(df = eda_train,col1 = "seeds",col2 = "yield")
association_function(df = eda_train,col1 = "fruitset",col2 = "yield")
association_function(df = eda_train,col1 = "fruitmass",col2 = "yield")

## Test between Yield ~ Negative Predictors
association_function(df = eda_train,col1 = "AverageRainingDays",col2 = "yield")
association_function(df = eda_train,col1 = "clonesize",col2 = "yield")
association_function(df = eda_train,col1 = "honeybee",col2 = "yield")

#### Initial Feature engineering ####
model_data <- train_set %>% select(-id)

## Train a Baseline model RF
rf_model <- randomForest(
  yield ~ .,
  data = model_data,
  importance = TRUE)

## Evaluate the features with DALEX

# Create explainer
rf_explainer <- DALEX::explain(
  model = rf_model,
  data = model_data[,-17],
  y = model_data$yield,
  label = "Random Forest")

# Compute mean permutation-based value of the RMSE with 50 permutations
vip_50 <- model_parts(
  explainer = rf_explainer,loss_function = loss_root_mean_square,B = 50,type = "difference")

# Plot the importance 
plot(vip_50) + 
  ggtitle("Mean variable-importance over 50 permutations", "") 

## Check for interactions with iml

# Create a predictor
predictor <- Predictor$new(rf_model,data = model_data[,-17],y = model_data$yield)

# Calculate interactions 
interaction_obj <- Interaction$new(predictor)

# Plot interactions 
interaction_obj$plot()

## Viz the interactions

# Interaction function 
viz_interactions <- function(df,model,col1,col2){
  
  
}




