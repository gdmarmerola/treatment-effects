# importing libraries
library(grf)
library(dplyr)
library(ggplot2)

# setting the working directory
setwd('C:/Users/Guilherme/Documents/Projetos & Estudos/Side Projects/causal_inference')

# reading our data
df <- read.csv('treat_effect_data.csv')

# extracting relevant variables
X <- df[,1:20]
W <- df[,22]
Y <- df[,23]
cluster <- df[,21]

# training the causal forest
tau.forest <- causal_forest(X, Y, W)

# Estimate treatment effects for the training data using out-of-bag prediction.
tau.hat.oob <- predict(tau.forest)

# creating a dataframe for evaluation
eval_df <- data.frame(cluster=cluster, effect=tau.hat.oob$predictions, y=Y, treated=W)

# showing a histogram of effects
eval_df %>% ggplot(aes(x=effect)) + geom_histogram(bins=60)

# real_treatment effects
real_effects <- eval_df %>% 
                group_by(cluster, treated) %>% 
                summarise(y_mean=mean(y)) %>% 
                group_by(cluster) %>% 
                summarise(real_effect=y_mean[2] - y_mean[1])

# showing the estimated treatment effect by cluster
estimated_effects <- eval_df %>%
                     group_by(cluster) %>%
                     summarise(estimated_effect=mean(effect))

# joining
merge(real_effects, estimated_effects)

# saving dataframe
write.csv(eval_df, 'estimated_effects_grf.csv')
