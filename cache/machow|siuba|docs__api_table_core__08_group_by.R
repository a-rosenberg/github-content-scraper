## import pandas as pd


## from siuba import _, group_by, ungroup, filter, mutate, summarize


## g_cyl = small_cars >> group_by(_.cyl)


## # keep rows where hp is greater than mean hp within cyl group


## g_cyl >> mutate(avg_hp = _.hp.mean())


## g_cyl >> summarize(avg_hp = _.hp.mean())


## small_cars >> group_by(_.cyl, _.gear)


## small_cars >> group_by(high_hp = _.hp > 300)


## small_cars >> group_by(_.cyl) >> ungroup()

