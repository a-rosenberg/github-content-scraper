library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)

Housing_Burden <- read_excel("Datasets/CHAS HousingCostBurdenByCounty-AllYears.xlsx")

names(Housing_Burden)

#Recode Burden variables

#Grade 1 = Burden <= 30%
Housing_Burden$Grade1 <- Housing_Burden$`# Households housing cost burden is less than or equal to 30%`

#Grade 2 = Burden >30% & <=50%
Housing_Burden$Grade2 <- Housing_Burden$`# Households housing cost burden is greater than 30% but less than or equal to 50%`

#Grade 3 = Burden > 50%
Housing_Burden$Grade3 <- Housing_Burden$`# Households housing cost burden is greater than 50%`

#Format date
x = ymd(Housing_Burden$`Start Year`)

Housing_Burden$Start_Year <- year(x)

#Create region lists
New_England <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
Middle_Atlantic <- c("Delaware", "Maryland", "New Jersey", "New York", "Pennsylvania")
South <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi", 
           "Missouri", "North Carolina", "South Carolina", "Tennessee", "Virginia", 
           "West Virginia")
Midwest <- c(	"Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Nebraska", 
              "North Dakota", "Ohio", "South Dakota", "Wisconsin")
Southwest <- c("Arizona", "New Mexico", "Oklahoma", "Texas")
West <- c("Alaska", "California", "Colorado", "Hawaii", "Idaho", "Montana", "Nevada", 
          "Oregon", "Utah", "Washington", "Wyoming")

#Filter out start years that are not 2008, add variable for region, and select only relevant variables
Housing_Burden.2008 <- Housing_Burden %>%
  filter(Start_Year == 2008) %>%
  mutate(Region = ifelse(`State Name` %in% New_England, "New England", 
                         ifelse(`State Name` %in% Middle_Atlantic, "Middle Atlantic",
                                ifelse(`State Name` %in% South, "South", 
                                       ifelse(`State Name` %in% Midwest, "Midwest", 
                                              ifelse(`State Name` %in% Midwest, "Midwest", 
                                                     ifelse(`State Name` %in% Southwest, "Southwest",
                                                            ifelse(`State Name` %in% West, "West", "Other"))))))))%>%
  mutate(Grade1.perc = (Grade1/`Total Households`)*100, 
         Grade2.perc = (Grade2/`Total Households`)*100,
         Grade3.perc = (Grade3/`Total Households`)*100) %>%
  filter(Region != "Other") %>%
  gather(key = Grade, value = Percentage, 
         Grade1.perc, Grade2.perc, Grade3.perc) %>%
  select(`State Name`, Region, `County Name`, Start_Year, Tenure, `Total Households`, Grade) %>%

#Convert Grade to factor type variable
Housing_Burden.2008$Grade <- as.factor(Housing_Burden.2008$Grade)

#Change labels for Grade
levels(Housing_Burden.2008$Grade) <- c("Less than or equal to 30% of Income", 
                                       ">30% but <= 50% of Income", 
                                       "Greater than 50% of Income")

#Boxplot with Percentage by Region, faceted by Housing burden
ggplot(data = Housing_Burden.2008, aes(x = Region, y = Percentage, col = Tenure))+ 
  geom_boxplot()+
  facet_grid(~Grade)+
  ggtitle("Housing Cost Burden on Residents by Region (2008 to 2012)")+
  ylab("Percentage of Residents")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom", plot.title = element_text(hjust = 0.5))
