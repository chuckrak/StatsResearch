library(tidyverse)
library(arm)
library(caret)
library(plotROC)
library(pROC)


df <- read.csv('../p4/data.csv')

df %>%head()


naive_model <- df %>% glm(NodalInvolvement ~ Age + AcidLevel + XRayResult + TumourSize + TumourGrade, ., family=binomial)

df <- df %>% mutate(
  'TumourSize:TumourGrade' = TumourSize * TumourGrade,
  'type' = ifelse(NodalInvolvement == 1, "Yes", "No")
)

interaction_model <- df %>% glm(NodalInvolvement ~ AcidLevel + XRayResult + TumourSize, ., family=binomial)


summary(interaction_model)


binnedplot(fitted(interaction_model), 
           residuals(interaction_model, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

confusionMatrix(as.factor(ifelse(fitted(interaction_model) > .5, "Yes", "No")), as.factor(df$type), positive="Yes")


invisible(plot(roc(factor(ifelse(df$type == "Yes", 1, 0)), fitted(interaction_model)), print.thres = c(.1, .5), col = "red", print.auc = T))


chisq.test(df$TumourGrade, df$TumourSize)

chisq.test(df$TumourGrade, df$XRayResult)

chisq.test(df$XRayResult, df$TumourSize)
