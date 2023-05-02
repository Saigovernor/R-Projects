---
title: '*Staying Caffeinated: Customer Retention Strategies for a Coffee Store*'
output:
  html_document: default
  pdf_document: default
css: styles.css
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

![](cof.jpg)

## **Introduction**

Coffee House is a multinational chain of coffeehouses and roastery reserves headquartered in Seattle, Washington. It is the world's largest coffeehouse chain. We have a large customer base and have consistently experienced high retention rates in the past.

Over the past year, Coffee House noticed a decline in the number of repeat customers and a corresponding decline in revenue. After reviewing customer feedback and conducting surveys, they have determined that the decline in retention rates is likely due to a combination of factors, including a lack of variety in our product offerings, poor customer service, and increased competition from other retailers. They have decided to conduct a data analysis project to better understand the factors contributing to the decline in retention rates and identify potential solutions to improve retention and increase revenue.

## **Project Objectives** 

An end-to-end analysis should be conducted to achieve the following objectives:

1. Cleaning and preparing the given dataset for analysis.

2. Explore data to identify trends and patterns.

3. Model data using statistics to identify factors contributing to retention rates.

4. Developing solutions to the given problem statement in the light of your data insights using visualizations.

## **Methodology**

The entirety of this project was done using R. Different variables were compared against the response variable (continue_buying) to identify possible causes of the problem. Chi square test was adopted as the relevant statistical test due to the nature of the dataset, since most variables have been treated as categorical variables. 

### **Data Pre-processing**

Load the relevant packages which would be utilized during the course of this analysis.

```{r cars}
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(janitor))
library(ggplot2)
library(forcats)

```
Read the data from excel file into R workspace
```{r opts}
coffee_house <-  readxl::read_xlsx("Data/coffee-house-satisfactory-survey.xlsx")
```
The structure of the dataset has to be inspected to have an idea of the data we're working with.   

```{r, out.extra = '\\n'}
head(coffee_house)
glimpse(coffee_house)
```
A lot of these columns have very long names and it would be a better approach to shorten the lengths of the titles of each column to present for better viewing.  
<br>
```{r colnames, out.extra = '\\n'}
colnames(coffee_house)[1:5] <- c("time", "gender", "age_range", 
                                 "employment_status", "annual_income")
colnames(coffee_house)[6:15] <- c("visit", "order_preference", "spent_time", 
                                  "branch_distance", "membership", "frequent_purchase",
                                  "avg_spend", "coffee_house_rating", 
                                  "price_range_rating", "promo_importance")
colnames(coffee_house)[16:21] <- c("ambiance_rating", "wifi_rating", "overall_service_rating",
                                   "future_business_likelihood", "hear_promo", 
                                   "continue_buying")
```
The column names have been shortened now using this approach, and the dataset can be inspected to ensure that the changes have been effected.
<br>
```{r echo=FALSE, out.extra = '\\n'}
glimpse(coffee_house)
```
Noticeably, most variables are either in **_Character_** format or **_Double_**, which will not be particularly useful for my analysis, so it becomes imperative to convert the class of these variables to **_Factor_**
<br>
```{r factors}
factor_cols <-coffee_house[, 2:19]
coffee_house_1 <- lapply(factor_cols, as.factor)
coffee_house_1$frequent_purchase = as.character(coffee_house_1$frequent_purchase)
coffee_house_1 <- data.frame(coffee_house_1)
coffee_house[, 2:19] <- coffee_house_1
coffee_house$continue_buying = as.factor(coffee_house$continue_buying)
```
<br>
The factors of some variables need to be reordered to make the data appear cleaner on plots. The `Forcats` function is used for varying levels of modifications to factors.
```{r}
# change the levels of age_range factor
unique(coffee_house$age_range)
coffee_house$age_range <- fct_recode(
  coffee_house$age_range, "<20" = "Below 20", 
  "20-29" = "From 20 to 29", "30 -39" = "From 30 to 39", ">40" = "40 and above"
)
levels(coffee_house$age_range)

#change the levels of annual_income factor
unique(coffee_house$annual_income)
coffee_house$annual_income <- fct_recode(
  coffee_house$annual_income, "<25,000" = "Less than RM25,000", ">150,000" = "More than RM150,000", "100,000 - 150,000" = 
    "RM100,000 - RM150,000", "25,000-50,000" = "RM25,000 - RM50,000", "50,000 - 100,000" = "RM50,000 - RM100,000"
)
levels(coffee_house$annual_income)

#change the levels of branch_distance
unique(coffee_house$branch_distance)
coffee_house$branch_distance <- fct_recode(coffee_house$branch_distance,
  "< 1km" = "within 1km", "> 3km" = "more than 3km"
)
levels(coffee_house$branch_distance)

# change the levels of avg_spend
unique(coffee_house$avg_spend)
coffee_house$avg_spend <- fct_recode(coffee_house$avg_spend,
   "<20" = "Less than RM20", "20 - 40" = "Around RM20 - RM40",  ">40" = "More than RM40",  "0" = "Zero"
)
levels(coffee_house$avg_spend)

#change levels of spent_time
unique(coffee_house$spent_time)
coffee_house$spent_time <- fct_recode(coffee_house$spent_time, 
"30 mins - 1 hour" = "Between 30 minutes to 1 hour", "< 30mins" = "Below 30 minutes", "> 3hours" = "More than 3 hours", 
"1 - 2 hours" = "Between 1 hour to 2 hours", " 2 - 3 hours" = "Between 2 hours to 3 hours"
)
levels(coffee_house$spent_time)
```
<br>
The time column has to be converted to the right format.
```{r time}
# convert the time column to date format
coffee_house$time = as.Date(coffee_house$time)
```
<br>
Finally, the data has to be inspected for missing rows and duplicate values.
<br>
```{r missing and duplicate}
# check for rows with missing values
coffee_house[!complete.cases(coffee_house), ]
# no missing data

#check for duplicate data
sum(duplicated(coffee_house))
# no duplicate data, analysis can now begin 
```
<br>
After all these have been modifications done, the dataset has to be inspected for correctness or any lagging issues yet to be corrected 
```{r}
head(coffee_house, 15)
```
Looks like the dataset has shaped up nicely and it is **finally** ready for inspection and analysis!

### **Exploratory Data Analysis**
In this section, the objective is clear- to find out what variables are directly related to customer retention i.e the customer_buying column.  
The first analysis to be done is to test if the gender of customers is a factor to be considered when analysisng retention. 
```{r}
gender_eff <- coffee_house %>% 
  select(gender, continue_buying) %>% 
  group_by(gender) %>%
  table()
```
```{r}
# plot bar graph of gender based on customer retention
ggplot(coffee_house, aes(fill =continue_buying, x = gender)) + geom_bar(position = "dodge") +
  labs(title = "Customer retention by gender")
```
<br>
From the plot, we can infer that;
1. there are more female customers 
2. a slightly higher number of female customers do not want to purchase anything from the company again 
However, do these figures have any statistical significance? A chi square test can be used to test this hypothesis.
```{r}
cont_table <-with(coffee_house, table(gender, continue_buying))
alpha = 0.05 # significance level of 5% is arbitrarily chosen
chisq.test(cont_table)
```
A p-value of 1 means we have to fail to reject the hypothesis, thereby establishing that there is no relationship whatsoever between gender and customer retention. 
<br>
<br>
The coffee store has a membership scheme which this dataset doesn't indicate whether there are perks attached to that or not. But, you'd like to assume that surely there are some incentives for customers to become members or purchase membership tags. So, this could possibly have an effect on whether they choose to return or not. In this case, we're particularly interested in the factors behind customers not returning so it is important to consider the effect of membership on this.  
* One pertinent question is, what percentage of customers are return buyers?
```{r}
mem_return<- coffee_house %>%
  select(membership, continue_buying) %>% 
  group_by(membership) %>% 
  count(continue_buying) %>% 
  mutate(percentage = n/sum(n)*100)

# visualize this data for better inferences
 ggplot(mem_return, aes(x=membership, y = percentage, fill = continue_buying)) +  geom_col(position = "stack") +
    theme_classic() + labs(title = "Percentage of members that are return buyers") +
   scale_fill_manual(values = c("#8197c9", "#5e2a4b"))
```
<br> 
Visualizing this data makes it immediately obvious that, compared to members, a larger percentage of non-members opt not to return to Coffee house. A chi square test is needed to verify that these variables are indeed correlated. 
```{r}
cont_table_2 <- with(coffee_house, table(membership, continue_buying))
  alpha = 0.05
  chisq.test(cont_table_2)
```
The p-value here is indeed less than `alpha` and therefore the null hypothesis has to be rejected. 

>  NOTE: In a chi squared test, the null hypothesis, H0, states that no relationship exists between the categorical variable. 

This confirms the association between membership and customer retention, an important point to look out for. 
<br>
<br>
Usually, when you want to check out any new store, you have a look around for the reviews from other people who have visited that store. For this dataset, reviews of users have been captured in the **_"Overall_service_rating"_** variable.   
This service rating is an important metric for stores and it is therefore imperative that any associations between this variable and customer retention.  
To test for this, I have initially converted the variable from a factor to a numeric variable, and then calculated for the average rating. 
```{r}
 coffee_house$overall_service_rating = as.numeric(coffee_house$overall_service_rating)
  class(coffee_house$overall_service_rating)
```
```{r}
# calculate for average service rating 
 avg_service <- coffee_house %>% 
    summarise(avg_service = mean(overall_service_rating)) %>% 
    pull(avg_service) 
print(avg_service)
```
This relationship is now reprsented using a bar plot
```{r}
    ggplot(coffee_house, aes(x = overall_service_rating))  + geom_bar() + theme_classic() +
  labs(x = "Overall Service Rating")

```
<br>
The bar plot corroborates what we already know from the average service rating value of 3.74 gotten in the previous calculation. Majority of the ratings fall between 3 and 4,and the task for the company is clear, figure out how to completely eliminate those lower ratings.  
<br>
From each of these ratings, let's find out what percentage of customers that give each of these would like to come back 
```{r}
rating_ret <- coffee_house %>% 
      group_by(overall_service_rating) %>% 
      count(continue_buying) %>% 
      mutate(pct = (n/sum(n))*100)
    ggplot(rating_ret, aes(overall_service_rating, pct, fill = continue_buying)) + geom_col(position = "dodge") + labs(y = "Percentage", x= "Overall service rating") + scale_fill_manual(values = c("#bd8fa7", "#4d574498")) + 
      theme_classic()
```
<br>
```{r}
# they generally seem satisfied but is this statistically relevant to customer retention rate?
    cont_table_3 <- with(coffee_house, table(overall_service_rating, continue_buying))
    alpha = 0.05
   suppressWarnings(chisq.test(cont_table_3))
   # our p value is less than alpha, which means there's an association
```
We have indeed established that overall service rating plays a major part in customer retention. Logically, a myriad of factors are considered by a customer before giving the overall service rating, so it is important for the company to know what some of these factors could be and try to improve upon them in order to increase customer retention and conseqeuntly, profits.  
I've picked out two factors to analyze:

* wifi rating
* price range rating
```{r}
# convert from factor to numeric
coffee_house$wifi_rating = as.numeric(coffee_house$wifi_rating)
   avg_wifi_rating <- coffee_house %>% 
     summarise(avg_wifi_rating = mean(wifi_rating)) %>% 
     pull(avg_wifi_rating)
   # is this mean less than the group mean
   avg_wifi_rating < avg_service

# convert from factor to numeric
coffee_house$price_range_rating = as.numeric(coffee_house$price_range_rating)
  avg_price_rating <- coffee_house %>% 
    summarise(avg_price_rating = mean(price_range_rating))
  # is it also less than the group mean?
  avg_price_rating<avg_service
```

Analysis shouldn't be done in isolation, so it is important to corroborate this with the fisher Exact test. This is used instead of the chi sqaure test because it is more appropriate for smaller sample sizes. 
```{r Fisher test}
cont_table_4 <- with(coffee_house, table(overall_service_rating, wifi_rating))
cont_table_5 <- with(coffee_house, table(overall_service_rating, price_range_rating))

suppressWarnings(fisher.test(cont_table_4, simulate.p.value = T))  # to decrease computational time of fisher test
suppressWarnings(fisher.test(cont_table_5, simulate.p.value = T)) 

```
The p-value once again is less than the alpha value of 0.05, which means we have to reject the null hypothesis.  
Both of these variables are important factors when it comes to overall service rating, so seeking to improve them would ultimately have an effect on customer retention.
<br>
<br>
Finally, I want to explore what observations can be drawn from the order preferences column. This will be done by getting the average rating by each preference type.
```{r Order preferences}
 # all values are to be converted to lower case because there's a "Never" and another "never" value
 coffee_house_2 <- coffee_house %>% # create a new df 
   mutate(order_pref = tolower(order_preference))
coffee_house_2$overall_service_rating = as.numeric(coffee_house_2$overall_service_rating)
 
# to get the average ratings
  pref_rating <-  coffee_house_2 %>% 
   group_by(order_pref) %>% 
   filter(order_pref != "na") %>% 
   summarise(rating = mean(overall_service_rating)) %>% 
    arrange(-rating)
  pref_rating
  
  # visualize this relationship
  pref_rating_1 <-  pref_rating %>% 
    filter(order_pref %in% c("take away", "dine in", "drive-thru"))
  pref_rating_1 %>% 
    ggplot(aes(x = order_pref, y = rating, fill = rating)) + geom_col() + ylim(0, 5) + labs(x = "order preferences") + theme_classic()
  
```
<br>
From the visual, customers using the drive-in method gave the least ratings on average. 
<br>
<br>

##  **Key Insights** 

1. Membership programs have a significant impact on customer retention and revenue for the coffee store.
2. Providing good quality and reliable wifi is important for customers, and improving this helps to increase customer satisfaction and attract new customers.
3. Price is an important factor for customers, and overpriced products led to lower ratings and decreased customer satisfaction.
4. Customer service is critical, especially for the drive-through medium, and improvements have to be made to increase customer satisfaction and loyalty 

## **Recommendations**
From my analysis of the coffee store dataset and the insights drawn, I will make the following recommendations:

1. Encourage more customers to become members by offering incentives such as discounts, free coffee, or a membership reward system like loyalty points.
2. Improve the quality and speed of the wifi to increase customer satisfaction and attract more customers who need a place to work or study.
3. Re-evaluate the pricing of certain products to ensure that they are competitive and in line with customers' expectations.
4. Provide additional training and support to staff working in the drive-through to improve their communication skills and speed of service.

