## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(collapsibleTree)
load(system.file("extdata/Geography.rda", package = "collapsibleTree"))


## ----summary-------------------------------------------------------------
summary(Geography)


## ----plot----------------------------------------------------------------
collapsibleTree(
  Geography,
  hierarchy = c("continent", "type", "country"),
  width = 800
)


## ----plotcolored---------------------------------------------------------
collapsibleTree(
  Geography,
  hierarchy = c("continent", "type", "country"),
  width = 800,
  fill = c(
    # The root
    "white",
    # Unique continents
    rep("firebrick", length(unique(Geography$continent))),
    # Unique types per continent
    rep("steelblue", length(unique(paste(Geography$continent, Geography$type)))),
    # Unique countries
    rep("green", length(unique(Geography$country)))
  )
)


## ----plotgradient, warning=FALSE-----------------------------------------
library(dplyr, warn.conflicts = FALSE)

# Continents are a simple gradient
continentColors <- RColorBrewer::brewer.pal(length(unique(Geography$continent)), "Reds")
# Types will be a gradient that resets between continents
typeColors <- Geography %>%
  arrange(continent, type) %>% 
  group_by(continent) %>%
  distinct(type) %>%
  mutate(colors = colorspace::sequential_hcl(length(type))[seq_along(type)])
# Countries will also be a gradient that resets between continents, but not types
countryColors <- Geography %>%
  arrange(continent, type) %>% 
  group_by(continent) %>%
  distinct(country) %>%
  mutate(colors = colorspace::rainbow_hcl(length(country))[seq_along(country)])

Geography %>%
  arrange(continent, type, country) %>%
  collapsibleTree(
    hierarchy = c("continent", "type", "country"),
    root = "Geography",
    width = 800,
    fill = c("white", continentColors, typeColors$colors, countryColors$colors)
  )


## ----plotsummary, warning=FALSE------------------------------------------
Geography %>%
  group_by(continent, type) %>%
  summarise(`Number of Countries` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("continent", "type"),
    root = "Geography",
    width = 800,
    attribute = "Number of Countries"
  )


## ----NAs-----------------------------------------------------------------
collapsibleTree(
  Geography,
  hierarchy = c("continent", "sub_region"),
  width = 800
)

