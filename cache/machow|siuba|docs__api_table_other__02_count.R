## import pandas as pd


## from siuba import _, group_by, summarize, count


## # longer approach


## mtcars >> count(_.cyl, _.gear, sort = True)


## mtcars >> count(_.cyl, many_gears = _.gear > 3)


## from siuba import add_count

