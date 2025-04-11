
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
library(factoextra)
library(hopkins)
library(clValid)
library(ggbeeswarm)

# Increase the memory limit
options(future.globals.maxSize = 3 * 1024^3)  # 3 GB

# Load the data
train_set <- read_csv("train.csv")
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
  cor() %>%
  corrplot(method = "color", 
           addCoef.col = "black", 
           order = "hclust", 
           number.cex = 0.7, 
           tl.srt = 45) 

## Create New Features 
eda_train <- eda_train %>%
  mutate(
    
    # Add Pollinator-related features
    total_polinators = honeybee + bumbles + andrena + osmia,
    wild_dom_ratio = (bumbles + andrena + osmia) / (honeybee + 1),
    
    # Temperature Features
    upper_temp_range = MaxOfUpperTRange - MinOfUpperTRange,
    lower_temp_range = MaxOfLowerTRange - MinOfLowerTRange,
    temp_stability = upper_temp_range + lower_temp_range,
    average_temp_range = (AverageOfUpperTRange + AverageOfLowerTRange) / 2,
    
    # Rain Patterns
    rain_anomaly =  RainingDays - AverageRainingDays,
    rain_per_pollinator = RainingDays / (total_polinators + 1),
    
    # Crop Features
    mass_per_seed = fruitmass / (seeds + 1),
    seed_efficiency = seeds / (fruitmass + 1),
    clonesize_temp_diff = clonesize / (AverageOfUpperTRange - AverageOfLowerTRange + 1)
  )

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

## Test between Yield ~ Positive predictors
association_function(df = eda_train,col1 = "seeds",col2 = "yield")
association_function(df = eda_train,col1 = "fruitset",col2 = "yield")
association_function(df = eda_train,col1 = "fruitmass",col2 = "yield")

## Test between Yield ~ Negative Predictors
association_function(df = eda_train,col1 = "AverageRainingDays",col2 = "yield")
association_function(df = eda_train,col1 = "clonesize",col2 = "yield")
association_function(df = eda_train,col1 = "honeybee",col2 = "yield")

## Test Between Yield ~ New Features Polinators
association_function(df = eda_train,col1 = "total_polinators",col2 = "yield")
association_function(df = eda_train,col1 = "wild_dom_ratio",col2 = "yield") # 0.35

## Test Between Yield ~ New Temperature Features
association_function(df = eda_train,col1 = "upper_temp_range",col2 = "yield")
association_function(df = eda_train,col1 = "lower_temp_range",col2 = "yield")
association_function(df = eda_train,col1 = "temp_stability",col2 = "yield")
association_function(df = eda_train,col1 = "average_temp_range",col2 = "yield")

## Test Between Yield ~ New Rain Patterns Features
association_function(df = eda_train,col1 = "rain_anomaly",col2 = "yield") # -0.5
association_function(df = eda_train,col1 = "rain_per_pollinator",col2 = "yield") # -0.48

## Test Between Yield ~ New Crop Features
association_function(df = eda_train,col1 = "mass_per_seed",col2 = "yield") # - 0.70
association_function(df = eda_train,col1 = "seed_efficiency",col2 = "yield") # 0.87
association_function(df = eda_train,col1 = "clonesize_temp_diff",col2 = "yield")

#### Initial Feature engineering ####

## Train a Baseline model RF

# List of Features  to exclude
exclude_vars <- c(
  "MaxOfUpperTRange","MaxOfLowerTRange","MinOfLowerTRange","upper_temp_range","MinOfUpperTRange",
  "lower_temp_range","honeybee","bumbles","andrena","osmia","RainingDays","AverageRainingDays",
  "fruitmass","AverageOfLowerTRange","AverageOfUpperTRange","clonesize")

# Exclude the unwanted variables from the dataset
cleaned_data <- eda_train[, !(names(eda_train) %in% exclude_vars)]

# Cor plot between final features
cleaned_data %>%
  cor() %>%
  corrplot(method = "color", 
           addCoef.col = "black", 
           order = "hclust", 
           number.cex = 0.7, 
           tl.srt = 45) 

# Create a formula that excludes the features
formula_str <- paste("yield ~ . -", paste(exclude_vars, collapse = " - "))

# Convert the string back to a formula
formula <- as.formula(formula_str)

# RF model 
rf_model <- randomForest(
  formula,
  data =  eda_train,
  importance = TRUE)

## Evaluate the features with DALEX

# Create explainer
rf_explainer <- DALEX::explain(
  model = rf_model,
  data = eda_train[,-17],
  y = eda_train$yield,
  label = "Random Forest")

# Compute mean permutation-based value of the RMSE over 50 permutations
vip_50 <- model_parts(
  explainer = rf_explainer,loss_function = loss_root_mean_square,B = 50,type = "difference")

# Plot the importance 
plot(vip_50) + 
  ggtitle("Mean variable-importance over 50 permutations", "") 

## Check for interactions with iml

# Create a predictor
predictor <- Predictor$new(rf_model,data = eda_train[,-17],y = eda_train$yield)

# Calculate interactions 
interaction_obj <- Interaction$new(predictor)

# Plot interactions 
interaction_obj$plot()

## PDP for uncorrelated features
pdp_plot <- model_profile(explainer = rf_explainer,N = 100,variables= c("osmia","andrena","honeybee"),type = "partial")
plot(pdp_plot,geom = "profiles")

## ALE for correlated features
ale_plot <- model_profile(explainer = rf_explainer,N = 100,variables = c("seeds","fruitset","fruitmass","RainingDays","clonesize","MaxOfLowerTRange"),type = "accumulated")
plot(ale_plot,geom = "profiles")

#### Cluster Analysis ####

##  Assessing clustering tendency

# Scale the data 
cluster_data <- scale(cleaned_data[,-4])

# Visual inspection 
fviz_pca_ind(
  X = prcomp(cluster_data),
  title = "PCA Clusters",geom = "point",ggtheme = theme_minimal())

## Compute K-means
kmeans <- kmeans(x = cluster_data,centers = 2)

# Viz the clusters
fviz_cluster(
  list(data = cluster_data,cluster = kmeans$cluster),
  geom = "point",ellipse.type = "norm",stand = FALSE,
  show.clust.cent = TRUE,ggtheme = theme_minimal(),pallete = "jco"
)

# Hopkins statistic ~ 1
hopkins::hopkins(X = cluster_data,k = 4,m = nrow(cluster_data)/2)

# VAT
subset_data <- cluster_data[sample(nrow(cluster_data),1000),] 

# Plot the 1000 sample from the train cluster data
fviz_dist(dist.obj = dist(subset_data),order = TRUE,show_labels = FALSE)+
  labs(title = "VAT")

## Choosing the Best Clustering Algorithm
internal <- clValid(
  obj = subset_data,nClust = 2:5,
  clMethods = c("kmeans","pam","hierarchical"),
  validation = "internal",
  metric = "manhattan",
  verbose = TRUE)

stability <- clValid(
  obj = subset_data,nClust = 2:5,
  clMethods = c("kmeans","pam","hierarchical"),
  validation = "stability",
  metric = "manhattan",
  verbose = TRUE)

## Hierarchical clustering

# Compute the dissimilarity matrix
h_clust_data <- dist(cluster_data,method = "manhattan")

# Create the hierarchical tree
hclust <- hclust(h_clust_data,method = "ward.D2")

# Compute the cophrnrtic dist
clust_cop <- cophenetic(hclust)

# Cor between cop and the original distance
cor(h_clust_data,clust_cop) #  0.7757071

# Viz the tree
fviz_dend(x = hclust,k = 2,palette = "jco",show_labels = FALSE,horiz = TRUE,color_labels_by_k = TRUE,rect = TRUE)

## Explore Clusters

# Cut the tree to form 3 clusters
clusters_hclust <- cutree(hclust, k = 2)

# Add cluster labels to the dataset
eda_train$cluster <- as.factor(clusters_hclust)

## Function to compaere clusters
cluster_exploration_func <- function(df,col1){
  
  ## Hypothesis testing to compere means between clusters
  
  # Kruskal
  kruskal_test <- kruskal.test(df[[col1]] ~ df$cluster)
   
  # Anova
   anova_test <- aov(df[[col1]] ~ factor(df$cluster), data = df)
   
   # Extract p-value
   p_value <- summary(anova_test)[[1]]$`Pr(>F)`[1]
                                           
   # Boxplot for visual comparison
   p1 <- ggplot(data = df, aes(x = factor(cluster), y = .data[[col1]], fill = factor(cluster))) +
     geom_boxplot() +
     scale_fill_viridis_d(option = "C",begin = 0.5,end = 0.9)+
     labs(
       title = paste("Boxplot of", col1, "by Cluster"),
       x = "Cluster",
       y = col1,
       fill = "Clusters",
       subtitle = paste0(
       "Kruskal Test ~ ",round(kruskal_test$p.value,10),"
        Anova_test ~ ",round(p_value,4),""))+
     theme_minimal()+
     theme(
       title = element_text(size = 13)
     )
   
   # Density plot
   p2 <- ggplot(data = df,aes( x = .data[[col1]],fill = cluster))+
     geom_density(alpha = 0.5)+
     theme_minimal()+
     scale_fill_viridis_d(option = "C",begin = 0.5,end = 0.9)+
     labs(
       title = "Density Plot"
     )
   
   # Bee Swarm Plot
   p3 <- ggplot(data = df, aes(x = factor(cluster), y = .data[[col1]], color = factor(cluster))) +
     geom_beeswarm() +
     scale_color_viridis_d(option = "C", begin = 0.5, end = 0.9) +
     theme_minimal() +
     labs(
       title = "Bee Swarm Plot",
       color = "Cluster",
       x = "Cluster",
       y = col1
     )
   
   # Combine the plots
   final_plot <- (p1+p2)/p3 & plot_annotation(title = "Cluster Comparison")
   
   # Return
   return(
     final_plot
   )
}

# Test the Clusters
cluster_exploration_func(df = eda_train,col1 = "yield")
cluster_exploration_func(df = eda_train,col1 = "total_polinators")
cluster_exploration_func(df = eda_train,col1 = "wild_dom_ratio")
cluster_exploration_func(df = eda_train,col1 = "rain_anomaly")
cluster_exploration_func(df = eda_train,col1 = "temp_stability")
cluster_exploration_func(df = eda_train,col1 = "seeds")


