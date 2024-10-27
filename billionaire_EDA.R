# Loading Libraries needed for project 
library(tidyverse) 
library(lubridate)
library(skimr)
library(outliers)
library(psych)


# Data Preparation 
demographic_data <- read.csv("Demographics_Info.csv")
economic_indicator_data <- read.csv("Economic_Indicators.csv")

# Data Processing 

# Split a column into two
demographic_data <- separate(demographic_data,name,
                             into = c("lastName", "firstName"), sep = ";" )


# Standardized final worth column making it 
# in full billions instead of thousands
demographic_data$finalWorth <- (demographic_data$finalWorth * 1000000)

demographic_data$firstName <- trimws(demographic_data$firstName)

# Checking and Correcting Errors in Name
# Checking 
 demographic_data %>%
  filter(grepl("^Fran", firstName))
# Correct the wrong Name 
demographic_data <- demographic_data %>%
  mutate(firstName = ifelse(firstName == "FranÃ§ois", "Francois", firstName))
# Verify 
filter(demographic_data, firstName == "FranÃ§ois")
filter(demographic_data, firstName == "Francois")

# Extract new related variables from birthDate 
# first covert date variable to date-datatype from character datatype
demographic_data$birthDate <- as.Date(demographic_data$birthDate)

current_Year = 2024

demographic_data <- demographic_data %>%
  mutate(birthYear = year(birthDate)) %>%
  mutate(birthMonth = format(as.Date(birthDate), "%B")) %>%
  mutate(birthDay = day(birthDate)) %>%
  mutate(age = current_Year - birthYear)


# Checking for Outilers with more focus on age and birthYear variables
# Method 1 using the outilers function directly
numeric_var <- demographic_data %>% select(age, birthYear)
outliers::outlier(numeric_var)

# method 2 using skewness and kurtosis to understand distribution deeper
skew(numeric_var)
kurtosi(numeric_var)

# Method 3 with Boxplot and histogram
numeric_var_long <- numeric_var %>%
  pivot_longer(cols = c(age,birthYear), names_to = "measures", values_to = "value")

ggplot(data = numeric_var_long, aes(y=value, fill=measures)) +
  geom_boxplot() +
  labs(title = "Boxplot to show distrubtion and outilers",
       x= "measures",
       y="values") +
  facet_wrap(~ measures, scales = "free") +
  theme_minimal() 

ggplot(data = numeric_var_long, aes(x=value, fill=measures)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black") +
  geom_density(alpha = 0.2, linewidth=0.7) +
  labs(title = "Histogram to show distrubtion and outilers",
       x= "measures",
       y="values") +
  facet_wrap(~ measures, scales = "free") +
  theme_minimal() 
  
#Correct the outilers 
demographic_data <- demographic_data %>% 
  mutate(age = ifelse(age == -5, 95, age)) %>%
  mutate(birthYear = ifelse(birthYear == 2029, 1929, birthYear)) %>%
  mutate(age = ifelse(age == -4, 96, age)) %>%
  mutate(birthYear = ifelse(birthYear == 2028, 1928, birthYear))
# Verify Results (do same for the other correction) 
filter(demographic_data, age == -5 & birthYear == 2029)
filter(demographic_data, age == 95 & birthYear == 1929)


# Merging the both demographics and economic_indicator data
billionaire_data <- merge(demographic_data, economic_indicator_data, 
                          by = "country", all.x = TRUE)
# Select only unique values due to duplicate from Join operations 
billionaire_data <- billionaire_data %>% distinct()

# Extract New Variables that will be used for Exploring the Data for insights 
billionaire_data <- billionaire_data %>% 
  mutate(young_selfmade = ifelse(selfMade == "True" & age <= 50, "Yes", "No")) %>%
  mutate(taxrate_classfication = ifelse(total_tax_rate_country >= 43, "High", "Low"))


# Remove unwanted variable from dataset 
billionaire_data <- billionaire_data %>%
  select(-category)

# verify datatype for different variables 
str(billionaire_data)

# Correcting the datatype for gdp_country as it is stored 
# as character instead of numeric 
# removing the special characters making it stored as character_datatype
billionaire_data$gdp_country <- gsub("[$,]", "", billionaire_data$gdp_country)
# changing the datatype
billionaire_data$gdp_country <- as.numeric(billionaire_data$gdp_country)
# verify 
str(billionaire_data$gdp_country)





# Analysis 

# understanding my dataset using basic summary descriptive statistics
skim_without_charts(billionaire_data)
summary(billionaire_data)


# Billionaire Level Insight 
# 1. Demographics 
# What is the average age of billionaires across industries?
billionaire_data %>%
  group_by(industries) %>%
  summarise(average_age = round(mean(age), 0)) %>%
  ggplot(aes(x = reorder(industries, average_age), y = average_age)) +
  geom_point(aes(color = industries), size = 3) +
  labs(title = "Average Age of Billionaires by Industry",
       x = "Industries",
       y = "Average Age") +
  coord_flip() +  # Flip for horizontal labels
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Distrbtion of Billionaries based on Gender
ggplot(data = billionaire_data, aes(x=gender, fill = gender)) +
  geom_bar(width = 0.4) +
  labs(title = "No of Billionaires by Gender",
       x= "Gender",
       y= "Count") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal() 
  

# Top 10 Countries with Most Billionaires
billionaire_data %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  top_n(10, Count) %>%
  ggplot(aes(x=country,y=Count, fill = country)) +
  geom_col(position = "dodge", width = 0.3) +
  labs(title = "Top 10 Countries with Most Billionaires",
       x= "Countries",
       y= "Count") +
  theme_minimal() 

#  BirthMonth Distrubtion of Billionaires 
ggplot( data = billionaire_data, aes(x=birthMonth, fill = birthMonth)) +
  geom_bar(width = 0.4) +
  labs(title = "No of Billionaires by birthmonth",
       x= "Month",
       y= "Count") +
  theme_minimal() 
  
# Are Billionaires More of selfmade or their wealth is inherent
ggplot( data = billionaire_data, aes(x=selfMade, fill = selfMade)) +
  geom_bar(width = 0.4) +
  labs(title = "selfmade vs Inherent wealth",
       x= "selfmade_status",
       y= "Count") +
  theme_minimal()

# o	How does age correlate with the wealth of billionaires? 
# Are older billionaires generally wealthier
numeric_var_cor <- billionaire_data %>% select(age, finalWorth)
cor(numeric_var_cor)
ggplot(data = numeric_var_cor, aes(x=finalWorth, y=age)) +
  geom_point(alpha=0.5) + 
  geom_smooth(linewidth=0.5) +
  labs(title = "Realtionship between age and wealth of billionaires", 
       x="wealth",
       y="age") +
  theme_minimal() 

# How does self-made status correlate with age or 
# industry, and does it impact wealth accumulation?
billionaire_data <- billionaire_data %>%
  mutate(ecoded_selfmade_status = as.numeric(factor(selfMade))) %>%
  mutate(ecoded_industry = as.numeric(factor(industries)))

numeric_var_cor2 <- billionaire_data %>% 
  select(ecoded_selfmade_status, ecoded_industry, age,finalWorth)

cor(numeric_var_cor2)


# 2. Wealth and Industry
# Which industries dominate among billionaires
billionaire_data %>%
  group_by(industries) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(industries, Count), y = Count)) +
  geom_point(aes(color = industries), size = 3) +
  labs(title = "No of  Billionaires by Industry",
       x = "Industries",
       y = "Count") +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none")


# How does the total wealth of billionaires differ across industries and regions?
# Industries
billionaire_data %>%
  group_by(industries) %>%
  summarise(Total_Wealth = sum(finalWorth)) %>%
  ggplot(aes(x = reorder(industries, Total_Wealth), y = Total_Wealth)) +
  geom_point(aes(color = industries), size = 3) +
  labs(title = "Wealth of  Billionaires by Industry",
       x = "Industries",
       y = "Wealth") +
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none")
# Countries 
billionaire_data %>%
  group_by(country) %>%
  summarise(Total_Wealth = sum(finalWorth)) %>%
  arrange(desc(Total_Wealth))

# Are they any emerging industries creating new 
# billionaires and impact on wealth distrubtion
billionaire_data %>%
  group_by(industries) %>%
  filter(young_selfmade == "Yes") %>%
  summarise(Count = n()) %>%
  ggplot(aes(x=industries,y=Count, fill = industries)) +
  geom_col(position = "dodge", width = 0.3) +
  labs(title = "Emerning Industries creating more billionaires that are young & selfmade",
         x= "Industries",
         y= "Count") +
  theme_minimal() 
# Impact on Wealth Distrubtion 
billionaire_data %>%
  group_by(industries) %>%
  filter(young_selfmade == "Yes") %>%
  summarise(total_wealth = sum(finalWorth)) %>%
  ggplot(aes(x=industries,y=total_wealth, fill = industries)) +
  geom_col(position = "dodge", width = 0.3) +
  labs(title = " Wealth for Emerning Industries creating more billionaires that are young & selfmade",
       x= "Industries",
       y= "Wealth") +
  theme_minimal() 

# 3. Wealth and Gender 
# How does gender representation vary among 
# billionaires across different industries and countries
# Industries 
billionaire_data %>%
  group_by(gender, industries) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = gender, values_from = Count, values_fill = 0) 
# countries 
billionaire_data %>%
  group_by(gender, country) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = gender, values_from = Count, values_fill = 0) 

# What is the average wealth by gender across industries
billionaire_data %>%
  group_by(gender, industries) %>%
  summarise(average_age = round(mean(finalWorth), 0), .groups = 'drop') %>%
  pivot_wider(names_from = gender, values_from = average_age, values_fill = 0) 



# Country-wise Economic Indicators Insights and Analysis
# 1.Geography
# How do economic factors differ between countries with more billionaires and those with fewer
# Top 10 countries
billionaire_data %>%
  filter(country %in% c("United States", "China", "United Kingdom", 
                  "Germany", "India", "Switzerland", "Russia",
                  "France", "Australia", "Italy")) %>%
  summarise(average_lifeexpectany = mean(life_expectancy_country),
            average_cpicountry = mean(cpi_country),
            average_gdpcountry = mean(gdp_country),
            average_taxreveue = mean(tax_revenue_country_country),
            average_taxrate = mean(total_tax_rate_country),
            average_population = mean(population_country))
# Last 10 countries
billionaire_data %>%
  filter(country %in% c('Chile', 'Colombia', 'Egypt', 
                        'Malaysia', 'Netherlands', 'New Zealand', 'Norway',
                        'Poland', 'Turkey', 'Ukraine')) %>%
  summarise(average_lifeexpectany = mean(life_expectancy_country),
            average_cpicountry = mean(cpi_country),
            average_gdpcountry = mean(gdp_country),
            average_taxreveue = mean(tax_revenue_country_country),
            average_taxrate = mean(total_tax_rate_country),
            average_population = mean(population_country))

# 2. Economic Impact
# Is there a pattern between countries with higher tax rates and the number of billionaires
billionaire_data %>% 
  group_by(taxrate_classfication) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=taxrate_classfication,y=count, fill = taxrate_classfication)) +
  geom_col(position = "dodge", width = 0.3) +
  labs(title = "no of billionaires based on taxrate classification",
       x= "taxrate_classfication",
       y= "count") +
  theme_minimal() 

# Do billionaires from high-tax countries have 
# significantly different wealth than those from low-tax countries?
billionaire_data %>% 
  group_by(taxrate_classfication) %>%
  summarise(total_wealth = sum(finalWorth)) %>%
  ggplot(aes(x=taxrate_classfication,y=total_wealth, fill = taxrate_classfication)) +
  geom_col(position = "dodge", width = 0.3) +
  labs(title = "wealth of  billionaires based on taxrate classification",
       x= "taxrate_classfication",
       y= "wealth") +
  theme_minimal() 

