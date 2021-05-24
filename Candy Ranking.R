# Load all the packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(corrplot)
library(fivethirtyeight)

# Load the candy_rankings dataset from the fivethirtyeight package
data(candy_rankings)

# Take a glimpse() at the dataset
glimpse(candy_rankings)

# Gather the categorical variables to make them easier to plot
candy_rankings_long <- gather(candy_rankings, key = "feature", value = "value", chocolate: pluribus )
# Make a bar plot showing the distribution of each variable
ggplot(candy_rankings_long, aes(value)) + geom_bar() + facet_wrap(~feature)

# Make a lollipop chart of pricepercent
ggplot(candy_rankings, aes(reorder(competitorname, pricepercent), pricepercent)) +
  geom_segment(aes(xend = reorder(competitorname, pricepercent), yend = 0)) +
  geom_point() +
  coord_flip()

ggplot(candy_rankings, aes(winpercent, fill = "blue89")) + geom_histogram(binwidth = 13)

ggplot(candy_rankings, aes(reorder(competitorname, winpercent), winpercent)) +
  geom_segment(aes(xend = reorder(competitorname, winpercent), yend = 0)) +
  geom_point() +
  coord_flip()

# Plot the correlation matrix
candy_rankings %>% 
  select(-competitorname) %>% 
  cor() %>% 
  corrplot()

# Fit a linear model of winpercent explained by all variables 
# except competitorname
win_mod <- lm(winpercent ~ . -competitorname, data = candy_rankings)

# Take a look at the summary
summary(win_mod)

# Plot the residuals vs the fitted values
ggplot(augment(win_mod), aes(.fitted, .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0)

# Fit a glm() of chocolate
choc_mod <- glm(chocolate ~ . - competitorname, family = 'binomial', data = candy_rankings)

# Print the summary
summary(choc_mod)

# Make a data frame of predictions
preds <- augment(choc_mod, type.predict = "response") %>% 
  mutate(prediction = .fitted > 0.5)

# Create the confusion matrix
conf_mat <- preds  %>%
  select(chocolate, prediction) %>%
  table()

# Calculate the accuracy
accuracy <- sum(diag(conf_mat))/sum(conf_mat)
accuracy