## ------------------------------------------------------------------------
heights


## ----warnings = FALSE----------------------------------------------------
ggplot(heights, aes(height, income)) +
  geom_point()


## ------------------------------------------------------------------------
n <- nrow(heights)
heights <- heights %>% filter(income < 150000)
nrow(heights) / n


## ------------------------------------------------------------------------
heights <- heights %>% filter(between(height, 59, 78))
nrow(heights) / n

ggplot(heights, aes(height, income, group = height)) +
  geom_boxplot()


## ------------------------------------------------------------------------
income ~ height


## ------------------------------------------------------------------------
h <- lm(income ~ height, data = heights)
h 


## ------------------------------------------------------------------------
coef(h)


## ------------------------------------------------------------------------
ggplot(heights, aes(height, income)) +
  geom_boxplot(aes(group = height)) +
  geom_smooth(method = lm, se = FALSE)


## ------------------------------------------------------------------------
ggplot(heights, aes(height, colour = sex)) + 
  geom_freqpoly(binwidth = 1)
ggplot(heights, aes(income, colour = sex)) + 
  geom_freqpoly(binwidth = 5000)


## ------------------------------------------------------------------------
h2 <- lm(income ~ height * sex, data = heights)
grid <- heights %>% 
  expand(height, sex) %>% 
  add_predictions(h2, "income")

ggplot(heights, aes(height, income)) + 
  geom_point() + 
  geom_line(data = grid) +
  facet_wrap(~sex)


## ------------------------------------------------------------------------
h3 <- lm(income ~ height + sex, data = heights)
grid <- heights %>% 
  expand(height, sex) %>% 
  gather_predictions(h2, h3)

ggplot(grid, aes(height, pred, colour = sex)) + 
  geom_line() +
  facet_wrap(~model)


## ------------------------------------------------------------------------
ggplot(heights, aes(education)) + geom_bar()
heights_ed <- heights %>% filter(education >= 12)
nrow(heights) / n


## ------------------------------------------------------------------------
he1 <- lm(income ~ height + education, data = heights_ed)
he2 <- lm(income ~ height * education, data = heights_ed)


## ------------------------------------------------------------------------
grid <- heights_ed %>% 
  expand(height, education) %>% 
  gather_predictions(he1, he2)

ggplot(grid, aes(height, education, fill = pred)) + 
  geom_raster() +
  facet_wrap(~model)


## ------------------------------------------------------------------------
ggplot(grid, aes(height, pred, group = education)) + 
  geom_line() +
  facet_wrap(~model)
ggplot(grid, aes(education, pred, group = height)) + 
  geom_line() +
  facet_wrap(~model)


## ------------------------------------------------------------------------
heights_ed %>% 
  expand(
    height = seq_range(height, 10), 
    education = mean(education, na.rm = TRUE)
  ) %>% 
  add_predictions(he1, "income") %>% 
  ggplot(aes(height, income)) + 
    geom_line()

heights_ed %>% 
  expand(
    height = mean(height, na.rm = TRUE), 
    education = seq_range(education, 10)
  ) %>% 
  add_predictions(he1, "income") %>% 
  ggplot(aes(education, income)) + 
    geom_line()


## ------------------------------------------------------------------------
s <- lm(income ~ sex, data = heights)
tidy(s)


## ------------------------------------------------------------------------
heights$sex <- factor(heights$sex, levels = c("male", "female"))


## ------------------------------------------------------------------------
hes <- lm(income ~ height + education + sex, data = heights)
tidy(hes)


## ------------------------------------------------------------------------
heights %>% 
  group_by(sex)  %>% 
  do(glance(lm(income ~ height, data = .)))


## ------------------------------------------------------------------------
hes2 <- lm(income ~ height + education * sex, data = heights)
tidy(hes2)


## ------------------------------------------------------------------------
ggplot(heights_ed, aes(education, income)) + 
  geom_boxplot(aes(group = education)) +
  geom_smooth(se = FALSE)


## ------------------------------------------------------------------------
mod_e1 <- lm(income ~ education, data = heights_ed)
mod_e2 <- lm(income ~ education + I(education ^ 2) + I(education ^ 3), data = heights_ed)

heights_ed %>% 
  expand(education) %>% 
  gather_predictions(mod_e1, mod_e2) %>% 
  ggplot(aes(education, pred, colour = model)) +
    geom_point() + 
    geom_line()


## ------------------------------------------------------------------------
mod_e1 <- lm(income ~ education, data = heights_ed)
mod_e2 <- lm(income ~ poly(education, 2), data = heights_ed)
mod_e3 <- lm(income ~ poly(education, 3), data = heights_ed)

heights_ed %>% 
  expand(education) %>% 
  gather_predictions(mod_e1, mod_e2, mod_e3) %>% 
  ggplot(aes(education, pred, colour = model)) +
    geom_point() + 
    geom_line()


## ------------------------------------------------------------------------
tibble(education = seq(5, 25)) %>% 
  gather_predictions(mod_e1, mod_e2, mod_e3) %>% 
  ggplot(aes(education, pred, colour = model)) +
    geom_line()


## ------------------------------------------------------------------------
library(splines)
mod_e1 <- lm(income ~ education, data = heights_ed)
mod_e2 <- lm(income ~ ns(education, 2), data = heights_ed)
mod_e3 <- lm(income ~ ns(education, 3), data = heights_ed)

tibble(education = seq(5, 25)) %>% 
  gather_predictions(mod_e1, mod_e2, mod_e3) %>% 
  ggplot(aes(education, pred, colour = model)) +
    geom_line()


## ---- dev = "png"--------------------------------------------------------
library(mgcv)
gam(income ~ s(education), data = heights)

ggplot(data = heights, mapping = aes(x = education, y = income)) +
  geom_point() +
  geom_smooth(method = gam, formula = y ~ s(x))

