## import pandas as pd


## from siuba import _, nest, unnest, group_by


## gap_country = nest(gapminder, -_.country)


## # unnest is its inverse (except for some sorting!)


## # specifying columns to nest directly


## # equivalent to


## from siuba import _, unnest, mutate


## split_sent = sent >> mutate(data = _.sentence.str.split(" "))


## split_sent >> unnest()

