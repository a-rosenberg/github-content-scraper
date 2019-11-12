## ------------------------------------------------------------------------
view(film)
str(film)


## ------------------------------------------------------------------------
film$release_date <- mdy(film$release_date)
film$mpaa_rating <- as.factor(film$mpaa_rating)
film$genre <- as.factor(film$genre)
levels(film$genre)


## ------------------------------------------------------------------------
film%>% 
  is.na() %>% 
  colSums()


## ------------------------------------------------------------------------
film <- film %>% 
  filter(production_budget, worldwide_gross) %>%
  drop_na(distributor, mpaa_rating) %>% 
  mutate(
    global_ratio = (worldwide_gross / production_budget),
    global_ratio = round(global_ratio, 2),
    domestic_ratio = (domestic_gross / production_budget),
    domestic_ratio = round(domestic_ratio, 2),
    text = paste("Worldwide Gross:", worldwide_gross)
  )


## ------------------------------------------------------------------------
low_profit_movie <-film %>% 
  filter(worldwide_gross <= 50000000) %>% 
  group_by(movie) %>% 
  ungroup() %>% 
  arrange(desc(movie)) 


## ------------------------------------------------------------------------
medium_profit_movie <-film %>% 
filter(worldwide_gross >= 50000000, worldwide_gross <= 100000000) %>% 
group_by(movie) %>% 
ungroup() %>% 
arrange(desc(movie)) 


## ------------------------------------------------------------------------
high_profit_movie <-film %>% 
filter(worldwide_gross >= 100000000) %>% 
group_by(movie) %>% 
ungroup() %>% 
arrange(desc(movie))


## ------------------------------------------------------------------------
action_tidy <- film %>% 
filter(genre == "Action") %>% 
group_by(genre) %>% 
ungroup() %>% 
  mutate(
    Jumlah_Film = n(),
    genre = reorder(genre, Jumlah_Film),
    text = paste("Banyaknya Film:")
  )


## ------------------------------------------------------------------------
drama_tidy <- film %>% 
filter(genre == "Drama") %>% 
group_by(genre) %>% 
ungroup() %>% 
  mutate(
    Jumlah_Film = n(),
    genre = reorder(genre, Jumlah_Film),
    text = paste("Banyaknya Film:")
  )


## ------------------------------------------------------------------------
horror_tidy <- film %>% 
filter(genre == "Horror") %>% 
group_by(genre) %>% 
ungroup() %>% 
  mutate(
    Jumlah_Film = n(),
    genre = reorder(genre, Jumlah_Film),
    text = paste("Banyaknya Film:")
  )


## ------------------------------------------------------------------------
comedy_tidy <- film %>% 
filter(genre == "Comedy") %>% 
group_by(genre) %>% 
ungroup() %>% 
  mutate(
    Jumlah_Film = n(),
    genre = reorder(genre, Jumlah_Film),
    text = paste("Banyaknya Film:")
  )


## ------------------------------------------------------------------------
adventure_tidy <- film %>% 
filter(genre == "Adventure") %>% 
group_by(genre) %>% 
ungroup() %>% 
  mutate(
    Jumlah_Film = n(),
    genre = reorder(genre, Jumlah_Film),
    text = paste("Banyaknya Film:")
  )


## ------------------------------------------------------------------------
rating_tidy <- film %>%
select(genre, global_ratio, mpaa_rating) %>% 
group_by(mpaa_rating, genre) %>% 
summarise(global_ratio = mean(global_ratio),
          global_ratio = round(global_ratio)) %>% 
ungroup() %>% 
  mutate(
    mpaa_rating = reorder(mpaa_rating, global_ratio),
    text = paste("Global Ratio:", global_ratio)
  )


## ------------------------------------------------------------------------
str(rating_tidy)


## ------------------------------------------------------------------------
rating<- ggplot(data = rating_tidy, aes(x = genre, y= global_ratio, text = text))+
geom_col(aes(fill = rating_tidy$mpaa_rating), position = "dodge")+
labs( x = NULL,
       y= "Global Ratio")+
ggtitle("Global Ratio based on Genre")+
geom_hline(yintercept = 2.0, color = "darkred")+
coord_flip()+
theme_minimal()
ggplotly(rating, tooltip = "text")


## ------------------------------------------------------------------------
ggplot(film, aes(x= worldwide_gross,
                 y= genre,
                 fill = genre))+
  geom_density_ridges(alpha = 0.4)+
  theme_minimal()


## ------------------------------------------------------------------------
film %>%
      filter(mpaa_rating == "PG") %>% 
      select(genre, mpaa_rating, worldwide_gross) %>% 
      group_by(mpaa_rating, genre) %>% 
      ungroup()

