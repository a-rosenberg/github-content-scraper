r4ds_members <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")

column_names <- c('year', 'month', 'dates', 'conf_name', 'conf_link', 'city', 'country', 'fancyname')
r_events <- readr::read_csv("https://jumpingrivers.github.io/meetingsR/events.csv", 
                            skip = 1,
                            col_names = column_names)
# thanks to https://jumpingrivers.github.io/meetingsR/the-data.html

readr::write_csv(r4ds_members, 'slack_data.csv')
readr::write_csv(r_events, 'events_data.csv')
