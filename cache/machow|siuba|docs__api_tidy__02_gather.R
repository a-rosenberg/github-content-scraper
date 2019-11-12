## import pandas as pd


## from siuba import _, nest, unnest, group_by, gather


## costs = pd.DataFrame({


## costs >> gather('measure', 'value', _.price_x, _.price_y, _.price_z)


## costs >> gather('measure', 'value', _["price_x":"price_z"])


## costs >> gather('measure', 'value', -_.id)

