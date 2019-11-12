## import pandas as pd


## from siuba import _, select


## mtcars >> select(_.mpg, _.cyl)


## # two other ways to keep the same columns


## # simple select with exclusion


## # select with rename


## mtcars >> select(_["mpg": "hp"])


## print(mtcars.columns[0:4])


## mtcars >> select(-_["mpg": "hp"])


## # prints columns that contain the letter d


## mtcars >> select(-_.contains('d'))


## str_methods = dir(mtcars.columns.str)

