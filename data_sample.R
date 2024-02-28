
library(rethinking)
library(dplyr)

setwd('/Users/pauladigun/Documents/ES219')

df <- read_csv("ES295.csv")

pairs(df)

library(dagitty)

# Define the DAG structure based on the relationships between variables
dag <- dagitty("dag {
  Population -> Mortality
  HF -> Mortality
  HD -> Mortality
}")

# Plot the DAG
plot(dag)


df_std <- df %>%
  mutate(across(c(Mortality, Population, HF,HD), scale))

# Fit the Bayesian hierarchical model for Mortality and Population
m_1 <- alist(
  Mortality ~ dnorm(mu, sigma),
  mu <- a + Ba * Population,
  a ~ dnorm(0, 0.2),
  Ba ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
) %>%
  quap(data = df_std)

# Summary of the model
summary(m_1)

# Fit the Bayesian hierarchical model for Mortality and HF
m_2 <- alist(
  Mortality ~ dnorm(mu, sigma),
  mu <- a + Bh * HF,
  a ~ dnorm(0, 0.2),
  Bh ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
) %>%
  quap(data = df_std)

# Summary of the model
summary(m_2)


# Fit the Bayesian hierarchical model for Mortality and HD
m_3 <- alist(
  Mortality ~ dnorm(mu, sigma),
  mu <- a + Bh * HD,
  a ~ dnorm(0, 0.2),
  Bh ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
) %>%
  quap(data = df_std)

# Summary of the model
summary(m_3)


# Fit the Bayesian hierarchical model for Mortality using all three predictors
m_4 <- alist(
  Mortality ~ dnorm(mu, sigma),
  mu <- a + Ba * Population + Bh * HF + Bd * HD,
  a ~ dnorm(0, 0.2),
  Ba ~ dnorm(0, 0.5),
  Bh ~ dnorm(0, 0.5),
  Bd ~ dnorm(0, 0.5),
  sigma ~ dexp(1)
) %>%
  quap(data = df_std)

# Summary of the model
summary(m_4)



library(tidyverse)
library(broom)

# Function to extract coefficients from model objects
tidy_coef <- function(model) {
  coef_df <- tidy(model)
  coef_df$model <- as.character(substitute(model))
  coef_df
}

library(dagitty)
library(ggplot2)



library(ggdag)
library(daggity)
str(m_1)
# List of models
model_list <- list(m_1, m_2, m_3, m_4)

# Extract coefficients from each model
coef_data <- model_list %>%
  map(tidy_coef) %>%
  bind_rows()

# Plot coefficients
coef_data %>%
  mutate(coef_mod = str_c("Model ", model, ": ", term),
         coef_mod = fct_inorder(coef_mod)) %>%
  ggplot(aes(x = estimate, y = coef_mod)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, color = "grey40") +
  labs(x = "Estimate", y = NULL, title = "Coefficients of Bayesian Models") +
  theme_minimal()
