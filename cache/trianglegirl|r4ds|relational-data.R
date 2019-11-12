## ----setup, message = FALSE----------------------------------------------
library(tidyverse)
library(nycflights13)


## ------------------------------------------------------------------------
airlines


## ------------------------------------------------------------------------
airports


## ------------------------------------------------------------------------
planes


## ------------------------------------------------------------------------
weather


## ---- echo = FALSE-------------------------------------------------------
knitr::include_graphics("diagrams/relational-nycflights.png")


## ------------------------------------------------------------------------
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)


## ------------------------------------------------------------------------
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)


## ------------------------------------------------------------------------
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2


## ------------------------------------------------------------------------
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")


## ------------------------------------------------------------------------
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-setup.png")

## ------------------------------------------------------------------------
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     3, "x3"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2",
     4, "y3"
)


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-setup2.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-inner.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-inner.png")


## ------------------------------------------------------------------------
x %>% 
  inner_join(y, by = "key")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-outer.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-venn.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-one-to-many.png")


## ------------------------------------------------------------------------
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     2, "x3",
     1, "x4"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2"
)
left_join(x, y, by = "key")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-many-to-many.png")


## ------------------------------------------------------------------------
x <- tribble(
  ~key, ~val_x,
     1, "x1",
     2, "x2",
     2, "x3",
     3, "x4"
)
y <- tribble(
  ~key, ~val_y,
     1, "y1",
     2, "y2",
     2, "y3",
     3, "y4"
)
left_join(x, y, by = "key")


## ------------------------------------------------------------------------
flights2 %>% 
  left_join(weather)


## ------------------------------------------------------------------------
flights2 %>% 
  left_join(planes, by = "tailnum")


## ------------------------------------------------------------------------
flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))


## ---- eval = FALSE-------------------------------------------------------
## airports %>%
##   semi_join(flights, c("faa" = "dest")) %>%
##   ggplot(aes(lon, lat)) +
##     borders("state") +
##     geom_point() +
##     coord_quickmap()


## ---- eval = FALSE, include = FALSE--------------------------------------
## worst <- filter(flights, !is.na(dep_time), month == 6, day == 13)
## worst %>%
##   group_by(dest) %>%
##   summarise(delay = mean(arr_delay), n = n()) %>%
##   filter(n > 5) %>%
##   inner_join(airports, by = c("dest" = "faa")) %>%
##   ggplot(aes(lon, lat)) +
##     borders("state") +
##     geom_point(aes(size = n, colour = delay)) +
##     coord_quickmap()


## ------------------------------------------------------------------------
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest


## ------------------------------------------------------------------------
flights %>% 
  filter(dest %in% top_dest$dest)


## ------------------------------------------------------------------------
flights %>% 
  semi_join(top_dest)


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-semi.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-semi-many.png")


## ---- echo = FALSE, out.width = NULL-------------------------------------
knitr::include_graphics("diagrams/join-anti.png")


## ------------------------------------------------------------------------
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)


## ------------------------------------------------------------------------
airports %>% count(alt, lon) %>% filter(n > 1)


## ------------------------------------------------------------------------
df1 <- tribble(
  ~x, ~y,
   1,  1,
   2,  1
)
df2 <- tribble(
  ~x, ~y,
   1,  1,
   1,  2
)


## ------------------------------------------------------------------------
intersect(df1, df2)

# Note that we get 3 rows, not 4
union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

