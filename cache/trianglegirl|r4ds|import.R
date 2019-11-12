## ----setup, message = FALSE----------------------------------------------
library(tidyverse)


## ---- message = TRUE-----------------------------------------------------
heights <- read_csv("data/heights.csv")


## ------------------------------------------------------------------------
read_csv("a,b,c
1,2,3
4,5,6")


## ------------------------------------------------------------------------
read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")


## ------------------------------------------------------------------------
read_csv("1,2,3\n4,5,6", col_names = FALSE)


## ------------------------------------------------------------------------
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))


## ------------------------------------------------------------------------
read_csv("a,b,c\n1,2,.", na = ".")


## ---- eval = FALSE-------------------------------------------------------
## "x,y\n1,'a,b'"


## ---- eval = FALSE-------------------------------------------------------
## read_csv("a,b\n1,2,3\n4,5,6")
## read_csv("a,b,c\n1,2\n1,2,3,4")
## read_csv("a,b\n\"1")
## read_csv("a,b\n1,2\na,b")
## read_csv("a;b\n1;3")


## ------------------------------------------------------------------------
str(parse_logical(c("TRUE", "FALSE", "NA")))
str(parse_integer(c("1", "2", "3")))
str(parse_date(c("2010-01-01", "1979-10-14")))


## ------------------------------------------------------------------------
parse_integer(c("1", "231", ".", "456"), na = ".")


## ------------------------------------------------------------------------
x <- parse_integer(c("123", "345", "abc", "123.45"))


## ------------------------------------------------------------------------
x


## ------------------------------------------------------------------------
problems(x)


## ------------------------------------------------------------------------
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))


## ------------------------------------------------------------------------
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")


## ------------------------------------------------------------------------
# Used in America
parse_number("$123,456,789")

# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))

# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))


## ------------------------------------------------------------------------
charToRaw("Hadley")


## ------------------------------------------------------------------------
x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

x1
x2


## ------------------------------------------------------------------------
parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))


## ------------------------------------------------------------------------
guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))


## ------------------------------------------------------------------------
fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)


## ------------------------------------------------------------------------
parse_datetime("2010-10-01T2010")
# If time is omitted, it will be set to midnight
parse_datetime("20101010")


## ------------------------------------------------------------------------
parse_date("2010-10-01")


## ------------------------------------------------------------------------
library(hms)
parse_time("01:10 am")
parse_time("20:10:01")


## ------------------------------------------------------------------------
parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")


## ------------------------------------------------------------------------
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))


## ------------------------------------------------------------------------
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"


## ------------------------------------------------------------------------
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))

str(parse_guess("2010-10-10"))


## ------------------------------------------------------------------------
challenge <- read_csv(readr_example("challenge.csv"))


## ------------------------------------------------------------------------
problems(challenge)


## ---- eval = FALSE-------------------------------------------------------
## challenge <- read_csv(
##   readr_example("challenge.csv"),
##   col_types = cols(
##     x = col_integer(),
##     y = col_character()
##   )
## )


## ------------------------------------------------------------------------
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)


## ------------------------------------------------------------------------
tail(challenge)


## ------------------------------------------------------------------------
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)


## ------------------------------------------------------------------------
challenge2 <- read_csv(readr_example("challenge.csv"), guess_max = 1001)
challenge2


## ------------------------------------------------------------------------
challenge2 <- read_csv(readr_example("challenge.csv"), 
  col_types = cols(.default = col_character())
)


## ------------------------------------------------------------------------
df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df

# Note the column types
type_convert(df)


## ---- eval = FALSE-------------------------------------------------------
## write_csv(challenge, "challenge.csv")


## ---- warning = FALSE----------------------------------------------------
challenge
write_csv(challenge, "challenge-2.csv")
read_csv("challenge-2.csv")


## ------------------------------------------------------------------------
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")


## ---- eval = FALSE-------------------------------------------------------
## library(feather)
## write_feather(challenge, "challenge.feather")
## read_feather("challenge.feather")
## #> # A tibble: 2,000 x 2
## #>       x      y
## #>   <dbl> <date>
## #> 1   404   <NA>
## #> 2  4172   <NA>
## #> 3  3004   <NA>
## #> 4   787   <NA>
## #> 5    37   <NA>
## #> 6  2332   <NA>
## #> # ... with 1,994 more rows


## ---- include = FALSE----------------------------------------------------
file.remove("challenge-2.csv")
file.remove("challenge.rds")

