## ---- message = FALSE, warning = FALSE-----------------------------------
# Setup -------------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw())


## ------------------------------------------------------------------------
# Get Data ----------------------------------------------------------------

# download data directly from github and store them as Rdata locally.

dat_path <- "data/2-04-prison.Rdata"
dat_url <- paste0("https://raw.githubusercontent.com/",
                  "rfordatascience/tidytuesday/master/data/",
                  "/2019/2019-01-22/prison_population.csv")

if(!file.exists(dat_path)) {
  
  dat <- read_csv(dat_url)
  save(dat, file = dat_path)
  
} else {
  load(dat_path)
}


## ------------------------------------------------------------------------
# how many NAs in the variable prison_population?
dat$prison_population %>% is.na() %>% sum()


## ---- fig.height=6-------------------------------------------------------
# Try sum(na.rm = TRUE) --------------------------------------------------------

# summarize the data by state and year
dat_state <- 
  dat %>% 
  filter(pop_category == "Total",
         year > 1982, 
         year != 2016) %>% 
  group_by(year, state) %>% 
  summarise(prison_population = prison_population %>% sum(na.rm = TRUE),
            population = population %>% sum(na.rm = TRUE)) 

# plot them as an heatmap
dat_state %>% 
  ggplot(aes(x = year,
             y = state,
             fill = prison_population)) +
  geom_raster() +
  scale_fill_viridis_c(trans = "log10", breaks = 10^(1:5)) +
  # Do not add padding around x limits
  scale_x_continuous(expand = expand_scale(0))


## ------------------------------------------------------------------------
dat_clean <- 
  dat %>% 
  filter(pop_category == "Total",
         # to include more counties, I have reduced the time span
         year >= 1990,
         year != 2016) %>% 
  group_by(state, county_name) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()


## ------------------------------------------------------------------------
dat_clean %>% 
  # first summarize data by state and year
  group_by(year, state) %>% 
  summarise(prison_population = sum(prison_population),
            population = sum(population)) %>% 
  # then plot the heatmap
  ggplot(aes(x = year,
             y = state,
             fill = prison_population)) +
  geom_raster() +
  scale_fill_viridis_c(
    # trans = "log10", breaks = 10^(1:5)
    ) +
  # Do not add padding around x limits
  scale_x_continuous(expand = expand_scale(0))


## ------------------------------------------------------------------------
# Which State is missing in the clean dataset?
setdiff(dat$state, dat_clean$state)


## ------------------------------------------------------------------------
# Try with more details ---------------------------------------------------
by_cat <- 
  dat %>% 
  filter(
    pop_category != "Other",
    year >= 1990,
    year != 2016) %>% 
  group_by(state, county_name, pop_category) %>% 
  mutate(has_na = anyNA(prison_population)) %>% 
  filter(!has_na) %>% 
  ungroup()


## ----fig.height=7--------------------------------------------------------
by_cat %>% 
  mutate(ratio = prison_population/population) %>% 
  ggplot(aes(x = year,
             y = ratio,
             group = year)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_grid(pop_category ~ .,
             scales = "free",
             # Wrap the text in the strip labels
             labeller = label_wrap_gen(width = 10))


## ---- fig.width=8--------------------------------------------------------
by_cat %>% 
  mutate(ratio = prison_population/population) %>% 
  ggplot(aes(x = year,
             y = state,
             fill = prison_population)) +
  geom_raster() +
  facet_grid(. ~ pop_category) +
  scale_fill_viridis_c(trans = "log10", breaks = 10^c(1:5)) +
  # because of facetting, the x axis is very tight
  theme(axis.text.x = element_text(angle = 90, vjust = .5))


## ------------------------------------------------------------------------
by_cat_sum <- 
  by_cat %>% 
  filter(pop_category != "Other") %>% 
  group_by(pop_category, year) %>% 
  summarise(population = sum(population),
            prison_population = sum(prison_population)) %>% 
  ungroup()


## ------------------------------------------------------------------------
# prepare a table for hyopergeometric test:
# get category total next to each other category

by_cat_tot <- 
  by_cat_sum %>% 
  filter(pop_category == "Total") %>% 
  rename_all(funs(paste0(., "_total")))

by_cat_hyp <- 
  by_cat_sum %>% 
  left_join(by_cat_tot, by = c("year" = "year_total"))

# apply phyper() using pmap

# Define phyper() wrapper that contains "..."
# So that it can be used in pmap with extra variables
# Test enrichment
# inspired from
# https://github.com/GuangchuangYu/DOSE/blob/master/R/enricher_internal.R

phyper2 <- function(q, m, n, k, ...) {
  phyper(q, m, n, k, log.p = TRUE, lower.tail = FALSE)
  }

by_cat_hyp <- 
  by_cat_hyp %>% 
  # rename arguments for dhyper
  transmute(year = year,
            pop_category = pop_category,
            q = prison_population, # white balls drawn
            # x = prison_population, # white balls drawn
            m = population, # white balls in the urn
            n = population_total - population, # black balls in the urn
            k = prison_population_total) # balls drawn from the urn


## ------------------------------------------------------------------------
# apply dhyper() to every row
by_cat_hyp <- 
  by_cat_hyp %>% 
  mutate(log_p = pmap(., .f = phyper2) %>% purrr::flatten_dbl())


## ------------------------------------------------------------------------
p <- 
  by_cat_hyp %>% 
  # I could have filtered out this earlier,
  # but it served as practical control
  filter(pop_category != "Total") %>% 
  # filter categories not overepresented
  filter(log_p < -100) %>% 
  ggplot(aes(x = year,
             y = -log_p)) +
  geom_bar(stat = "identity",
           fill = "orange",
           colour = "black") +
  facet_grid(pop_category ~ .) 

p %>% print()


## ------------------------------------------------------------------------
p2 <- 
  p +
  labs(title = "Categories that are Overrepresented in US Prisons",
       subtitle = str_wrap("A quick exploratory analysis of 
                           the VERA dataset, using a hypergeometric
                           test to estimate which category is more 
                           represented than expected"), width = 27,
       y = "-log(p-value)",
       x = "Year",
       caption = "Source: www.vera.org | Plot by @othomn") +
  theme(text = element_text(family = "Arial Narrow",
                            colour = "grey40",
                            size = 11),
        axis.title = element_text(size = 14),
        strip.text = element_text(colour = "grey20",
                                  size = 14),
        plot.title = element_text(colour = "grey20",
                                  face = "bold",
                                  size = 18),
        plot.subtitle = element_text(face = "bold",
                                     size = 12),
        aspect.ratio = .2,   
        plot.margin = margin(t = 10, r = 10, b = 0, l = 3,
                             unit = "mm"))

p2 %>% print()


## ---- echo = FALSE, eval = FALSE-----------------------------------------
## # save as png
## png(filename = "plots/2-04-prison.png",
##     height = 1400, width = 2300,
##     res = 300)
## p2 %>% print()
## dev.off()

