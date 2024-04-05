library(tidyverse)


pollution <- read.csv('a.csv', header=TRUE)

View(pollution)

initial_model <- pollution %>% lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, .)

summary(initial_model)

significant_model <- pollution %>% lm(Y ~ X2 + X3, .)
summary(significant_model)

df <- pollution

cor_matrix <- cor(df)

# Create pairs plot with correlation coefficients
pairs(df, lower.panel = function(x, y, ...) {
  points(x, y, ...)
  text(0.5, 0.5, sprintf("%0.2f", cor_matrix[rownames(cor_matrix) == names(df)[y], colnames(cor_matrix) == names(df)[x]]), cex = 1.2)
})



df <- read.csv('data.csv')
df <- df %>% mutate(NE = (Region == 'NE') * 1)

df <- df %>% mutate(X5NE = X5 * NE,
                    X6NE = X6 * NE)


no_interaction_model <- df %>% lm(Y ~ X2 + X3 + NE, .)

par(mfrow= c(2,2))
plot(no_interaction_model)


interaction_A <- df %>% lm(Y ~ X2 + X3 + X5NE, .)
plot(interaction_A)




interaction_B <- df %>% lm(Y ~ X2 + X3 + NE + X6NE, .)
plot(interaction_B)





data <- read.csv('../p3/data.csv') 
head(data)
data <- data %>% mutate(
  'Female' = as.integer(Gender == 'Female'), 
  'NormalLight' = as.integer(Light == 'Normal Light'),
  'LogLH' = log(LH)
)


head(data)

data <- data %>% mutate(
  'Female:NormalLight' = Female * NormalLight,
  'Female:Dosage1250' = Female*Dosage1250
)







# Convert 'variable' to factor
data$Dosage <- factor(data$Dosage)

# Create dummy variables using model.matrix
dummy_variables <- model.matrix(~ Dosage - 1, data)

# Convert the result to a data frame
dummy_variables_df <- as.data.frame(dummy_variables)

# Combine with original data
data <- cbind(data, dummy_variables_df)

# View the result
head(data)


model <- data %>% lm(LH ~ Female + NormalLight + Dosage10 + Dosage50 + Dosage250 + Dosage1250 + Female:NormalLight + `Female:Dosage1250`, .)


logmodel <- data %>% lm(LogLH ~ Female + NormalLight + Dosage10 + Dosage50 + Dosage250 + Dosage1250 + Female:NormalLight + `Female:Dosage1250`, .)




