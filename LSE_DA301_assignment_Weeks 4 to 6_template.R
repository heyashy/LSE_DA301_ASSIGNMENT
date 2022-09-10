## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 4. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
# Install all packages required for this project so that R doesn't need to 
# restart the kernel
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("skimr")
install.packages("DataExplorer")

library(ggplot2)
library(gridExtra)
library(tidyverse)

# Import the data set.
getwd()

data <- read.csv(file.choose(), header = T)
#data <- read.csv('turtle_sales.csv', header = T)

# Print the data frame.
data
View(data)
str(data)
typeof(data)
class(data)
dim(data)
# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns.
full_data <- data
drop <- c("Ranking", "Year", "Genre", "Publisher")
trimmed_data <- data[,!(names(data) %in% drop)]

# View the data frame.
View(data)

# View the descriptive statistics.
summary(data)


count(data %>%
    group_by(Platform))

count(data %>%
    group_by(Genre))

################################################################################

# 2. Review plots to determine insights into the data set.

# Create a new column of pc vs console
data <- data %>% mutate(pc_vs_console = ifelse(Platform == "PC", "PC", "Console"))
view(data)
## 2a) Scatterplots
# Create scatterplots.
options(repr.plot.width = 15, repr.plot.height = 8)

p <- ggplot(data, aes(y = Genre, x = Global_Sales)) 
g1  <- p  + geom_jitter() 
          + labs(title = "Global Sales by Product", 
                 x = "Global Sales", y = "Product")

p <- ggplot(data, aes(y = pc_vs_console, x = Global_Sales))
g2 <- p + geom_jitter(width = 1, height = 0.2) 
        + labs(title = "Global Sales by Platform", 
               x = "Global Sales", y = "Platform")

g1grob <- ggplotGrob(g1)
g2grob <- ggplotGrob(g2)

output <- grid.arrange(g1grob, g2grob, ncol = 2, nrow = 1)

## 2b) Histograms
# Create histograms.
# Change histogram plot line colors by groups
# By pc_vs_console
g1 <- ggplot(data, aes(x = EU_Sales, color = pc_vs_console)) +
        geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
        labs(title = "EU Sales by Platform", x = "EU Sales", y = "Frequency")
g2 <- ggplot(data, aes(x = Global_Sales, color = pc_vs_console)) +
    geom_histogram(fill = "white", alpha = 0.5, position = "identity") +
    labs(title = "Global Sales by Platform", x = "EU Sales", y = "Frequency")

g1grob <- ggplotGrob(g1)
g2grob <- ggplotGrob(g2)

output <- grid.arrange(g1grob, g2grob, ncol = 1, nrow = 2)

# load the library
library(forcats)

# Reorder following the value of another column:
g1 <- data %>%
        mutate(name = fct_reorder(Genre, Global_Sales)) %>%
        ggplot(aes(x = Genre, y = Global_Sales)) +
        geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
        coord_flip() +
        xlab("") +
        theme_bw()

g2 <-  data %>%
        mutate(name = fct_reorder(Genre, EU_Sales)) %>%
        ggplot(aes(x = Genre, y = EU_Sales)) +
        geom_bar(stat = "identity", fill = "#f68060", alpha = .6, width = .4) +
        coord_flip() +
        xlab("") +
        theme_bw()

g1grob <- ggplotGrob(g1)
g2grob <- ggplotGrob(g2)

output <- grid.arrange(g1grob, g2grob, ncol = 1, nrow = 2)



## 2c) Boxplots
# Create boxplots.
g1 <- data %>%
        mutate(class = fct_reorder(Genre, Global_Sales, .fun = "median")) %>%
        ggplot(aes(y = reorder(Genre, Global_Sales), x = Global_Sales, fill = class)) +
        geom_boxplot() +
        ylab("class") +
        theme(legend.position = "none") +
        ylab("")

g2 <- data %>%
        mutate(class = fct_reorder(Genre, EU_Sales, .fun = "median")) %>%
        ggplot(aes(y = reorder(Genre, EU_Sales), x = EU_Sales, fill = class)) +
        geom_boxplot() +
        ylab("class") +
        theme(legend.position = "none") +
        ylab("")

g1grob <- ggplotGrob(g1)
g2grob <- ggplotGrob(g2)

output <- grid.arrange(g1grob, g2grob, ncol = 1, nrow = 2)

###############################################################################

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
global_sales_by_product <- data %>%
                                group_by(Product, Genre) %>%
                                summarise(total_sales  = sum(Global_Sales))


# View the data frame.
global_sales_by_product
View(global_sales_by_product)
str(global_sales_by_product)
typeof(global_sales_by_product)
class(global_sales_by_product)
dim(global_sales_by_product)
colnames(global_sales_by_product)



## 3b) Determine which plot is the best to compare game sales.
# Create scatterplots.
p <- ggplot(global_sales_by_product, aes(y = Genre, x = total_sales, 
                                         colour = Product))
p + geom_jitter() + labs(title = "Global Sales by Product", 
                         x = "Global Sales", y = "Product")

p <- ggplot(global_sales_by_product, aes(y = Product, 
                                         x = total_sales, colour = Genre))
p + geom_jitter() + labs(title = "Global Sales by Product", 
                         x = "Global Sales", y = "Product")


# Create histograms.
ggplot(global_sales_by_product, aes(x = total_sales)) +
    geom_histogram(fill = "red", alpha = 1, position = "identity") +
    labs(title = "Total Sales by Platform", x = "Total Sales", y = "Frequency")

# Create boxplots.
global_sales_by_product %>%
    mutate(class = fct_reorder(Genre, total_sales, .fun = "median")) %>%
    ggplot(aes(y = reorder(Genre, total_sales), x = total_sales, fill = class)) +
    geom_boxplot() +
    ylab("class") +
    theme(legend.position = "none") +
    ylab("")

###############################################################################

# 4. Observations and insights

# Our sales data contains 352 games, 10 platforms and 12 genres. The release date from games
# ranges from 1980 to 2016. Global mean sales are 5.335 million, while the EU mean sales are 1.644 million.
# Globally Sports, shooter and action games are the most popular.In Europe Shooters are the most popular,
# followed by Platform and Action.The least popular games are Strategy and Adventure.
# There are some outliers in sports sales with very high sales numbers that have very high sales numebrs
# The median Racing game sells better than any other game genre.

###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 3. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 4. Include your insights and observations.

################################################################################



# The whole tidyverse library.
library(tidyverse)
# Import and read CSV file.
library(readr)
# Data wrangling.
library(dplyr)
# Data wrangling.
library(tidyr)
# Create statistical summaries.
library(skimr)
# Create a report as an HTML file.
library(DataExplorer)

# 1. Load and explore the data
data <- read.csv(file.choose(), header = T)
# View data frame created in Week 4.
view(data)
as_tibble(data)
# Check output: Determine the min, max, and mean values.
data %>%
    select(NA_Sales, EU_Sales, Global_Sales) %>%
    summarise(min = min(NA_Sales), max = max(NA_Sales), 
              median = median(NA_Sales))
data %>%
    select(NA_Sales, EU_Sales, Global_Sales) %>%
    summarise(min = min(EU_Sales), max = max(EU_Sales), 
              median = median(EU_Sales))
data %>%
    select(NA_Sales, EU_Sales, Global_Sales) %>%
    summarise(min = min(Global_Sales), max = max(Global_Sales), 
              median = median(Global_Sales))

# View the descriptive statistics.
# Method 1
view(summary(data))
# Method 2
summary(data)

###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.
par(mfrow = c(1, 1))
qqnorm(data$NA_Sales)
qqline(data$NA_Sales)

qqnorm(data$EU_Sales)
qqline(data$EU_Sales)

qqnorm(data$Global_Sales)
qqline(data$Global_Sales)

par(mfrow = c(3, 1)) ## one row, two columns
qqplot(qnorm(ppoints(50)), data$NA_Sales, main = "NA_Sales Q-Q Plot")
qqline(data$NA_Sales)
qqplot(qnorm(ppoints(50)), data$EU_Sales, main= "EU_Sales Q-Q Plot")
qqline(data$EU_Sales)
qqplot(qnorm(ppoints(50)), data$Global_Sales, main = "EU_Sales Q-Q Plot")
qqline(data$Global_Sales)
## 2b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(data$NA_Sales)
# The Data is not normally distributed with a p value of 2.2e-16

shapiro.test(data$EU_Sales)
# The Data is not normally distributed with a p value of 2.2e-16

shapiro.test(data$Global_Sales)
# The Data is not normally distributed with a p value of 2.2e-16

## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(data$NA_Sales)
kurtosis(data$NA_Sales)
# Leptokurtic distribution with a positive skewness.

skewness(data$EU_Sales)
kurtosis(data$EU_Sales)
# Leptokurtic distribution with a positive skewness.

skewness(data$Global_Sales)
kurtosis(data$Global_Sales)
# Leptokurtic distribution with a positive skewness.


## 2d) Determine correlation
# Determine correlation.
round(cor(data$NA_Sales, data$Global_Sales), digits=2)
# High positive correcation between NA_Sales and Global_Sales of 0.93
round(cor(data$EU_Sales, data$Global_Sales), digits=2)
# High positive correcation between EU_Sales and Global_Sales of 0.88
round(cor(data$NA_Sales, data$EU_Sales), digits=2)
# High positive correcation between NA_Sales and EU_Sales of 0.71

###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.

ggplot(
    data = data,
    mapping = aes(x = Global_Sales, y = Publisher, color = Genre)
) +
    geom_point(position = "jitter", alpha = 0.5, size = 1.5) +
    labs(title = "Global Sales by Publisher", x = "Global Sales", y = "Publisher")


ggplot(
    data = data,
    mapping = aes(x = Global_Sales, y = EU_Sales)
) +
    geom_point(color = 'red', alpha = 0.5, size = 1.5) +
    labs(title = "Global Sales by Publisher", x = "Global Sales", y = "EU Sales")


data %>%
    mutate(name = fct_reorder(Publisher, Global_Sales)) %>%
    ggplot(aes(x = Publisher, y = Global_Sales)) +
    geom_bar(stat = "identity", fill = "#f68060", alpha = .9, width = .4) +
    coord_flip() +
    xlab("") +
    theme_bw()

data %>%
    mutate(name = fct_reorder(Genre, EU_Sales)) %>%
    ggplot(aes(x = Genre, y = EU_Sales)) +
    geom_bar(stat = "identity", fill = "#f68060", alpha = .9, width = .4) +
    coord_flip() +
    xlab("") +
    theme_bw()

data %>%
    mutate(name = fct_reorder(Platform, Global_Sales)) %>%
    ggplot(aes(x = Platform, y = Global_Sales)) +
    geom_bar(stat = "identity", fill = "#f68060", alpha = .9, width = .4) +
    coord_flip() +
    xlab("") +
    theme_bw()


###############################################################################

# 4. Observations and insights
# Your observations and insights here...

# We can see that the data in all our sales figures is not normally distributed. With 
# our Shapiro-Wilk showing p values of 2.2e-16 on all our sales data. We can also see
# that the data is leptokurtic with a very high positive skewness. This means shows that
# our data is not normally distributed and is very skewed. There is a game from Nintendo
# that has outsold all other games in our dataset. The sales of this game were 67.85 million
# Nintendo aslo has the highest sales for any publisher in our list. Wii is the most popular
# platform followed by Xbox 360. 
#
# When we look at the correlation between the sales data we can see that there is a positive
# correlation between all the sales data. This means that as one sales figure increases the
# other sales figures will also increase. The highest correclation is between NA_Sales and
# Global_Sales with a correlation of 0.93. 


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################
library(tidyverse)

# 1. Load and explor the data
# View data frame created in Week 5.
data <- read.csv(file.choose(), header = T)


# Determine a summary of the data frame.
summary(data)

plot(data$NA_Sales, data$EU_Sales)
plot(data$NA_Sales, data$Global_Sales)
plot(data$EU_Sales, data$Global_Sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
attach(data)

# Pearson's correlation:
cor(EU_Sales, Global_Sales)
# High correlation between EU_Sales and Global_Sales of 87.75%
cor(EU_Sales, NA_Sales)
# Medium correlation between EU_Sales and Global_Sales of 70.55%
cor(Global_Sales, NA_Sales)
# Very High correlation between EU_Sales and Global_Sales of 93.49%
mod <- lm(NA_Sales ~ Global_Sales)

mod
summary(mod)
attributes(mod)

mod$coef
## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(NA_Sales, Global_Sales, main="Regression for NA_Sales and Global_Sales")

abline(mod, col=2, lwd=3)

confint(mod)

# anova table
anova(mod)
SSE1 = sum(mod$residuals^2)
SSE1


# Regression Diagnositcs
plot(mod)
par(mfrow=c(2,2))
plot(mod)
mtext("Regression Diagnostics NA_Sales vs Global_Sales", side = 3, 
      line = -2, outer = TRUE)


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

# Assumptions - we are using individual line items rather than sum to have
# more data points for our model. 

View(data)
num_data <- data %>%
  select(c(2, 7, 8, 9))

View(num_data)
attach(num_data)
cor(num_data)

#               Product   NA_Sales   EU_Sales Global_Sales
#Product       1.0000000 -0.4047865 -0.3894246   -0.4409046
#NA_Sales     -0.4047865  1.0000000  0.7055236    0.9349455
#EU_Sales     -0.3894246  0.7055236  1.0000000    0.8775575
#Global_Sales -0.4409046  0.9349455  0.8775575    1.0000000
# NA_Sales and Global_Sales is what we should look at.

# Multiple linear regression model.
mmod <- lm(Global_Sales ~ NA_Sales + EU_Sales)
summary(mmod)

mmod$coef

par(mfrow=c(2,2))
plot(mmod)
mtext("Multiple Regression", side = 3, 
      line = -2, outer = TRUE)


# Remove product from our model as it has a negative correlation

View(data)

df <- data %>% 
      group_by(Product) %>% 
      summarise(NA_Sales_sum = sum(NA_Sales),
                EU_Sales_sum = sum(EU_Sales),
                Global_Sales_sum = sum(Global_Sales))
sales_data <- df %>%
  select(c(2, 3, 4))
View(sales_data)


attach(sales_data)
model <- lm(Global_Sales ~ NA_Sales + EU_Sales)
summary(model)

model$coef

par(mfrow=c(2,2))
plot(model)
mtext("Multiple Regression", side = 3, 
      line = -2, outer = TRUE)


# Model is not accurate after sales of 35 million and errors are accurate 
# between -1 to 1 quantile

###############################################################################


# 4. Predictions based on given values
# Compare with observed values for a number of records.

Product  <- c(107, 107,107,107,107)
NA_Sales <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales <- c(23.80, 1.56, 0.65, 0.97, .052)
#Product <-  c(107)
#NA_Sales <- c(34.02)
#EU_Sales <- c(23.80)
predict_data <- data.frame(Product, NA_Sales, EU_Sales)
prediction <- predict(model, newdata = predict_data)
View(prediction)
prediction

#         1         2         3         4         5 
# 71.468572  6.856083  4.248367  4.134744 25.803524 
###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Looking at our Pearson's correlation we can see that the highest correlation 
# in our data is between North America Sales and Global Sales with a index of 
# 93.49% followed by EU and Global Sales of 87.75% with the least being EU and 
# North American Sales of 70.55%.

# We started with a Linear Regression model of North American and Global Sales. 
# Although there is a very high positive correlation we can see some errors in 
# our regression diagnostics. Firstly our Residual vs Fitted  shows a megaphone 
# shape which would indicate variances increase as the values go up. This would 
# mean larger values are associated with large errors or residuals. In addition 
# our Normal Q-Q plot shows minimal errors between the -1 and 1 quantiles. T
# he errors increase considerably when we are not within this zone. 

# Using multi linear regression we can see that product number has a negative 
# correlation and our final model was made without product as a correlation 
# vector. The regular vs fitted values changes dramatically for very high value 
# showing that linearity is not met as we look at high sales figures.

###############################################################################
###############################################################################




