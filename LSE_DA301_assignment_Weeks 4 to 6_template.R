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
install.packages("tidyverse")
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
install.packages("ggplot2")
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
options(repr.plot.width = 15, repr.plot.height = 8)

p <- ggplot(data, aes(y = Genre, x = Global_Sales)) 
g1  <- p + geom_jitter() + labs(title = "Global Sales by Product", x = "Global Sales", y = "Product")

p <- ggplot(data, aes(y = pc_vs_console, x = Global_Sales))
g2 <- p + geom_jitter(width = 1, height = 0.2) + labs(title = "Global Sales by Platform", x = "Global Sales", y = "Platform")

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
p <- ggplot(global_sales_by_product, aes(y = Genre, x = total_sales, colour = Product))
p + geom_jitter() + labs(title = "Global Sales by Product", x = "Global Sales", y = "Product")

p <- ggplot(global_sales_by_product, aes(y = Product, x = total_sales, colour = Genre))
p + geom_jitter() + labs(title = "Global Sales by Product", x = "Global Sales", y = "Product")


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

## Your observations and insights here ......
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

# 1. Load and explore the data

# View data frame created in Week 4.


# Check output: Determine the min, max, and mean values.


# View the descriptive statistics.


###############################################################################

# 2. Determine the normality of the data set.

## 2a) Create Q-Q Plots
# Create Q-Q Plots.



## 2b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.



## 2c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.



## 2d) Determine correlation
# Determine correlation.


###############################################################################

# 3. Plot the data
# Create plots to gain insights into data.


###############################################################################

# 4. Observations and insights
# Your observations and insights here...


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

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.



###############################################################################

# 5. Observations and insights
# Your observations and insights here...


###############################################################################
###############################################################################



