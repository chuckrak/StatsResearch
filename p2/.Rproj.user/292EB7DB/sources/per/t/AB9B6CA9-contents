library(tidyverse)

car_df <- read.csv('p1f.csv',header=TRUE, sep=',')



nuke_df <- read.table('p1a.txt',header=TRUE)



simple_model <- nuke_df %>% lm(Cost ~ T1, .)
model <- nuke_df %>% lm(Cost ~ T1 + T2 + S + PR + NE + CT + N, .)
model_revised <- nuke_df %>% lm(Cost ~ T1 + T2 + S + PR + NE, .)

t1_model <- simple_model
t2_model <- nuke_df %>% lm(Cost ~ T2, .)
summary(t2_model)
summary(t1_model)

## T2 looks far less relevant than T1, leave out?

plot(nuke_df$T1, nuke_df$Cost)
abline(t1_model)
plot(nuke_df$T2, nuke_df$Cost)
abline(t2_model)
plot(nuke_df$S, nuke_df$Cost)


s_model <- nuke_df %>% lm(Cost ~ S, .)
summary(s_model) # good! Rsquared ~ 0.22
plot(nuke_df$S, nuke_df$Cost)
abline(s_model)


pr_model <- nuke_df %>% lm(Cost ~ PR, .)
plot(nuke_df$PR, nuke_df$Cost)
summary(pr_model) # no
abline(pr_model)

ne_model <- nuke_df %>% lm(Cost ~ NE, .)
plot(nuke_df$NE, nuke_df$Cost)
summary(ne_model)
# not bad!!
abline(ne_model)


ct_model <- nuke_df %>% lm(Cost ~ CT, .)
plot(nuke_df$CT, nuke_df$Cost)
summary(ct_model)
# not good
abline(ct_model)

n_model <- nuke_df %>% lm(Cost ~ N, .)
plot(nuke_df$N, nuke_df$Cost)
summary(n_model)
# not good
abline(n_model)





#model <- nuke_df %>% lm(Cost ~ T1 + S + NE + CT + N, .)
nuke_df$T <- nuke_df$T2 - nuke_df$T1
View(nuke_df)
model <- nuke_df %>% lm(Cost ~ T1 + T2 + S + PR + NE, .)
summary(model)

plot(nuke_df$N, nuke_df$Cost)


normalized_nuke_df <- as.data.frame(scale(nuke_df))
normalized_nuke_df$Cost <- nuke_df$Cost

norm_model <- normalized_nuke_df %>% lm(Cost ~ T1 + T2 + S + PR + NE + CT + N, .)
summary(norm_model)






abline(simple_model, col="red")
abline(model, col="blue")

plot(nuke_df$T1, nuke_df$T2)
abline(lm(T2~T1, nuke_df), col="green")

correlation_mat <- cor(nuke_df)


melted_correlation <- melt(correlation_mat)

# Plot heatmap
ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

