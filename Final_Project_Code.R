# Necessary library loading
library(ResourceSelection) # For goodness-of-fit test
library(MASS)              # For statistical models
library(ggplot2)           # For data visualization
library(dplyr)             # For data manipulation

# Data preparation
data <- read.csv("Data.csv", header = TRUE) # Read dataset
data <- subset(data, select = -c(Territory)) # Drop 'Territory' column

# EDA (Exploratory Data Analysis)
# Histogram for snacirema count distribution
ggplot(data, aes(x = Snacirema)) + 
  geom_histogram(binwidth = 1, fill = 'blue', color = 'black') +
  labs(title = "Distribution of Snacirema Counts", x = "Snacirema Count", y = "Frequency")

# Boxplot to observe yearly snacirema count trends
ggplot(data, aes(x = as.factor(Year), y = Snacirema)) +
  geom_boxplot() +
  labs(title = "Snacirema Counts Over Years", x = "Year", y = "Snacirema Count")

# Jitter plot for snacirema counts vs tree survival
ggplot(data, aes(x = Snacirema, y = Alive)) +
  geom_jitter(aes(color = Alive), width = 0.2, height = 0.2, alpha = 0.5) +
  labs(title = "Snacirema Counts vs. Tree Survival Status", x = "Snacirema Count", y = "Alive (1 or 0)")

# Bar plot for alive trees proportion by snacirema presence
data %>%
  group_by(Snacirema) %>%
  summarise(ProportionAlive = mean(Alive)) %>%
  ggplot(aes(x = as.factor(Snacirema), y = ProportionAlive)) +
  geom_bar(stat = "identity", fill = 'green', color = 'black') +
  labs(title = "Proportion of Alive Trees by Snacirema Presence", x = "Snacirema Presence", y = "Proportion Alive")

# Correlation plot for numerical variables
library(GGally) # For extended visualizations
ggpairs(data[,sapply(data, is.numeric)]) # Pairwise plots for numeric variables


## Results

# Poisson Regression Model for snacirema counts
Full_Model_Po <- glm(Snacirema ~ (.)^2, family = poisson, data = data)
Final_Model_Po <- stepAIC(Full_Model_Po, direction = "both", trace = FALSE)
# Check for overdispersion
overdispersion_Po <- sum(residuals(Final_Model_Po, type="pearson")^2) / df.residual(Final_Model_Po)
print(paste("Overdispersion:", overdispersion_Po))

# Negative Binomial Regression Model if overdispersion is present
Full_Model_Nb <- glm.nb(Snacirema ~ (.)^2, data = data)
Final_Model_Nb <- stepAIC(Full_Model_Nb, direction = "both", trace = FALSE)
# Model summary and confidence intervals
summary(Final_Model_Nb)
confint(Final_Model_Nb)
# Model comparisons
AIC(Full_Model_Nb)
AIC(Final_Model_Nb)
# Goodness-of-fit test
hoslem.test(Final_Model_Nb$y, fitted(Final_Model_Nb))
# Diagnostic plots
par(mfrow=c(2,2))
plot(Final_Model_Nb)

# Logistic Regression Model for tree survival
data$Year <- as.factor(data$Year) # Convert Year to a factor
Full_Model_Lo <- glm(Alive ~ (.)^2, family = binomial(link="logit"), data = data)
Final_Model_Lo <- stepAIC(Full_Model_Lo, direction = "both", trace = FALSE)
# Model summary and confidence intervals
summary(Final_Model_Lo)
confint(Final_Model_Lo)
# Model comparisons
AIC(Final_Model_Lo)
AIC(Final_Model_Lo)
# Goodness-of-fit test
hoslem.test(Final_Model_Lo$y, fitted(Final_Model_Lo))
# Diagnostic plots
par(mfrow=c(2,2))
plot(Final_Model_Lo)
# Check for overdispersion
overdispersion_Lo <- sum(residuals(Final_Model_Lo, type="pearson")^2) / df.residual(Final_Model_Lo)
print(paste("Overdispersion:", overdispersion_Lo))