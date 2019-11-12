## ------------------------------------------------------------------------
library(tidyverse)


## ---- warning=FALSE, message=FALSE---------------------------------------
#without the pipe
data <- read_csv("/Users/sctew/Desktop/week19_airline_safety.csv") #import the .csv file and save it in an object called data. Of course your filepath will be different from this one.

data <- rename(data, seat_km = avail_seat_km_per_week) # rename the variable avail_seat_km_per_week in seat_km

data <- mutate(data, seat_km = seat_km/1000000000) #for readability: transform the count in billion seats per kilometer

#with saving each result again as data we are overwriting the previous dataset


## ---- warning=FALSE, message=FALSE---------------------------------------
#with the pipe

data <- 
  read_csv("/Users/sctew/Desktop/week19_airline_safety.csv") %>% #we take this as the input in our pipeline
  rename(seat_km = avail_seat_km_per_week) %>% #in this step of the pipeline we shorten the variable name
  mutate(seat_km = seat_km/1000000000) #we take the result from the previous step and apply for readability: transform the count in billion seats per kilometer

#the result of piping the original data through all the command is stored in data and we are not bothered with the interim results


## ----eval=FALSE, error=FALSE, message=FALSE, warning=FALSE, include=TRUE----
## data %>% #this is our good old data wrangling with the pipe. We are putting ingredients in and then pufff BREAD!
##   filter(...) %>%
##   summarize(...) %>% #now we take that bread and pipe it again (we could also save it as an object and then pipe that object)
## 
## ggplot(aes(x, y))  #we pipe it into ggplot, which is our sandwich maker. ggplot also wants to know what kind of sandwich we want. Specifying the aesthetics (basically what goes on the axis) is like saying we want a Tomato-Hoummus Sandwich.
## 
## + # Now ggplot knows the flavour of the sanwich, but we still need some instructions on how to make that sandwich. We add instruction with a + sign
## 
##   geom_bar() #a basic instruction is called a geom, which basically decides how the plot (sandwich) will look like in the end


## ------------------------------------------------------------------------
data %>% 
  filter(type_of_event == "fatal_accidents") %>% 
ggplot (aes(year_range, n_events)) +
  geom_dotplot(binaxis= "y", stackdir = "center")



## ------------------------------------------------------------------------
data %>% 
  filter(type_of_event == "incidents") %>% 
ggplot (aes(seat_km, n_events)) +
  geom_point() +
  geom_smooth()


## ------------------------------------------------------------------------
data %>% 
  group_by(airline) %>% 
  summarize(total_events = sum(n_events)) %>% 
  print() %>% 
  ggplot(aes(airline, total_events)) +
  geom_bar(stat = "identity") +
  coord_flip()

# OR you maybe did it in two seperate steps and saved the summary dataset as a new object

# summary <-
 #  data %>% 
 #  group_by(airline) %>% 
 # summarize(total_events = sum(n_events)) %>% 
 # print() OR View() OR neither if you just click on summary in your environment to see what the dataframe looks like

# summary %>% 
  # ggplot(aes(airline, total_events)) +
  #  geom_bar(stat = "identity") +
  #  coord_flip()



## ------------------------------------------------------------------------
data %>% 
  ggplot(aes(year_range, n_events, fill = year_range)) +
  geom_violin() +
  facet_wrap(~type_of_event, scales = "free_y")

