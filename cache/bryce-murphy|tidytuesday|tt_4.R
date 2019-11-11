library(readr)
library(RCurl)
library(dplyr)
library(tidyr)
library(ggplot2)

#READ IN DATA FROM THE GITHUB SOURCE
path <- getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week4_australian_salary.csv")

wages <- 
  read_csv(path) %>%
  rename(income = average_taxable_income) # make variable easier to type

# Find top 10 highest paying female professions
top_female <-
  wages %>% 
  filter(gender == "Female") %>%
  arrange(desc(income)) %>%
  head(n = 10)

top_female_jobs <- unique(top_female$occupation)  # Creates vector of names to use for filtering


# Filter males so that they match the highest earning females - join the 2 dfs 
male_counterparts <- 
  wages %>%
  filter(gender == "Male", occupation %in% top_female_jobs) %>%
  full_join(top_female)

# Barplot of top 10 female occupations
ggplot(male_counterparts, aes(x = reorder(occupation, income), y = income, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_text(aes(label = individuals, y = max(income)), position = position_dodge(1)) +
  labs(x = "Occupation",
       y = "Average Taxable Income (USD)",
       title = "Gender Comparison of Taxable Income\n by Top 10 Female Occupations") +
  scale_fill_discrete(name = "Gender")
  
  

