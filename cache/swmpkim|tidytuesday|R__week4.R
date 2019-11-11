# week 4
# 4-23-2018 kac

library(tidyverse)
library(skimr)

dat <- read.csv("data/week4_australian_salary.csv")
skim(dat)

# pull out the top 10 occupations for women
# this will be used to filter all data for these jobs
dat_top_women <- dat %>%
    filter(gender == "Female",
           gender_rank <= 10) %>%
    select(occupation)

# pull out those occupations for both genders;
# spread and put in order by women's salaries;
# gather again for easier plotting;
# relevel the gender factor to make it show up the way I want
dat_top_both <- dat %>%
    filter(occupation %in% dat_top_women$occupation) %>%
    select(occupation, gender, average_taxable_income) %>%
    spread(key = gender, value = average_taxable_income) %>%
    mutate(occupation = fct_reorder(occupation, Female, .desc = FALSE)) %>%
    gather(key = gender, value = average_taxable_income, -occupation) %>%
    mutate(gender = fct_relevel(gender, "Female", "Male"))

# plot it
ggplot(dat_top_both) +
    geom_col(aes(x = occupation, y = average_taxable_income/1000, 
                 fill = gender), 
             col = "gray50", position = position_dodge(width = -0.3), 
             width = 0.9) +
    ggtitle("Gender differences in income in Australia", 
            subtitle = "10 top-paying professions for women") +
    xlab("Occupation") +
    ylab("Mean salary \n(thousands of dollars)") +
    scale_fill_manual(values = c("cadetblue3", "gray80")) +
    theme_minimal() +
    coord_flip()



# for overlapping bars:
# spread and put in order by women's salaries;
dat_top_overlap <- dat %>%
    filter(occupation %in% dat_top_women$occupation) %>%
    select(occupation, gender, average_taxable_income) %>%
    spread(key = gender, value = average_taxable_income) %>%
    mutate(occupation = fct_reorder(occupation, Female, .desc = FALSE))
    

# plot it
ggplot(dat_top_overlap) +
    geom_col(aes(x = occupation, y = Male/1000, fill = "Male"),
             col = "gray50", width = 0.8) +
    geom_col(aes(x = occupation, y = Female/1000, fill = "Female"), 
             col = "gray50", width = 0.5) +
    ggtitle("Gender differences in income in Australia", 
            subtitle = "10 top-paying professions for women") +
    xlab("Occupation") +
    ylab("Mean salary \n(thousands of dollars)") +
    scale_fill_manual(name = "gender",
                      values = c("Female" = "cadetblue3", "Male" = "gray80")) +
    theme_minimal() +
    coord_flip()
