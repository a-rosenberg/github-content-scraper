## ----setup, include = TRUE-----------------------------------------------
library(tidyverse)
library(rjson)
colors <- rjson::fromJSON(file = "colors.json")


## ----function, echo=TRUE, include=TRUE-----------------------------------
plotting_function <- 
  function(
    data,
    yearOfInterest,
    countryName,
    title = paste("Share of death by cause, ", countryName, ", ", yearOfInterest, sep = ""),
    subtitle = "",
    caption = "Source: IHME, Global Burden of Disease"
  ){
    if (countryName == "World") {
      data <- data %>% 
        filter(year == yearOfInterest) %>% 
        select(-c(country, country_code, year)) %>% 
        summarize_all(funs("sum"))
    } else {
      data <- data %>%
        filter(year == yearOfInterest, country == countryName) %>%
        select(-c(country, country_code, year))
    }
    data %>% 
      gather(key = "disease", value = "deaths") %>% 
      mutate(deaths = deaths / sum(deaths)) %>% 
      ggplot(aes(x = reorder(disease, deaths), y = deaths, fill = disease))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = paste(round(100 * deaths, 2), "%")), hjust = -0.1)+
      scale_y_continuous(labels = scales::percent, limits = c(0, 0.35))+
      scale_fill_manual(values = colors)+
      guides(fill = FALSE)+
      coord_flip()+
      xlab("")+
      ylab("")+
      theme_classic()+
      labs(title = title, subtitle = subtitle, caption = caption)+
      theme(
        panel.grid.major.x = element_line(linetype = "dotted", color = "#5043484A")
      )
  }


## ----data_import---------------------------------------------------------
data <- readxl::read_xlsx("global_mortality.xlsx") %>% 
  rename_all(funs(stringr::str_remove_all(., "[(%)]"))) %>% 
  na.omit() 


## ----plots---------------------------------------------------------------
plotting_function(data = data, yearOfInterest = 2016, countryName = "World")

plotting_function(data = data, yearOfInterest = 2016, countryName = "United States")

plotting_function(data = data, yearOfInterest = 2016, countryName = "Chile")


