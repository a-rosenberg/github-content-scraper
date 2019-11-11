library(tidyverse)
library(magrittr)
library(cranlogs)
library(lubridate)
library(gridExtra)


# Set time span of one year to now ----------------------------------------

to <- today()

from <- to %m-% months(12)

# Get data ----------------------------------------------------------------

dat_path <- "data/cran-downloads.Rdata"

if(!file.exists(dat_path)) {
  dat <- 
    installed.packages() %>%
    as_tibble() %$%
    Package %>%
    # cran_downloads(when = "last-month")
    cran_downloads(from = from,
                   to = to)
  
  save(dat, file = dat_path)
} else {
  load(dat_path)
}

# dat %>% View()

# Tidy --------------------------------------------------------------------

# packages with 0 counts?
no_counts <- 
  dat %>%
  group_by(package) %>%
  summarise(counts = sum(count)) %>%
  filter(counts == 0) %$%
  package

dat <- 
  dat %>%
  filter(! package %in% no_counts) %>%
  as_tibble()

# Plot --------------------------------------------------------------------

dat_scaled <- 
  dat %>% 
  group_by(package) %>%
  filter(count > 0) %>%
  mutate(scaled_count = count %>%
           log() %>%
           scales::rescale(from = range(.),
                           to = c(0,1)))

p <- dat_scaled %>% 
  ggplot(aes(x = date,
             y = package,
             fill = scaled_count)) +
  geom_tile()

p + 
  scale_fill_viridis_c()

png("cards/pack_heat.png",
    height = 2000,
    width = 700,
    res = 300)
p + 
  scale_fill_viridis_c(guide = FALSE) +
  theme_void() 
dev.off()


# VIRIDIS plot ------------------------------------------------------------------

set.seed(1)

pack_sub <- 
  dat_scaled$package %>% 
  unique() %>%
  sample(100)

plot_heat <- function(dat_subset) {
  p_heat <- 
    dat_subset %>% 
    ggplot(aes(x = date,
               y = package,
               fill = scaled_count)) +
    geom_raster() +
    # scale_fill_viridis_c(guide = FALSE) +
    theme_void() +
    theme(aspect.ratio = 1/1.6,
          plot.background = element_rect(colour = "grey80", size = .1),
          plot.margin = margin(t = 10, r = 8,
                               b = 10, l = 8, unit = "mm"))

  return(p_heat)  
}


p_heat <- 
  dat_scaled %>%
  filter(package %in% pack_sub) %>% 
  plot_heat()

p_vir <- 
  p_heat + 
  scale_fill_viridis_c(guide = FALSE)
  
pdf("cards/pack-heat-viridis.pdf",
    width = 11.7, 
    height = 8.27,
    paper = "a4r")
grid.arrange(grobs = list(p_vir, p_vir, p_vir, p_vir),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()

# INFERNO plot ------------------------------------------------------------



set.seed(25)
pack_sub_2 <- 
  dat_scaled$package %>% 
  unique() %>%
  sample(100)

p_heat_2 <- 
  dat_scaled %>%
  filter(package %in% pack_sub_2) %>% 
  plot_heat()

p_inf <- 
  p_heat_2 +
  scale_fill_viridis_c(option = "B", 
                       guide = FALSE) 
  

png("cards/pack-heat-subset-inferno.png",
    height = 1000,
    width = 1200,
    res = 300)
p_inf
dev.off()

pdf("cards/pack-heat-inferno.pdf",
    width = 11.7, 
    height = 8.27,
    paper = "a4r")
grid.arrange(grobs = list(p_inf, p_inf, p_inf, p_inf),
             layout_matrix = matrix(1:4, ncol = 2))
dev.off()

png(filename = "cards/pack-heat-single.png",
    width = 11.7/2, 
    height = 8.27/2,
    units = "in",
    res = 300)
p_vir
dev.off()

png(filename = "cards/pack-heat-single-inf.png",
    width = 11.7/2, 
    height = 8.27/2,
    units = "in",
    res = 300)
p_inf
dev.off()
