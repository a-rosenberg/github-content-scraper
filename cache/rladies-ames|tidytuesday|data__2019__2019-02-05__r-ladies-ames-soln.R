## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----pkgs----------------------------------------------------------------
library(tidyverse)


## ----getdata-------------------------------------------------------------
mort <- read_csv("mortgage.csv")
recess <- read_csv("recessions.csv")
hpi <- read_csv("state_hpi.csv")


## ----glimpse-------------------------------------------------------------
glimpse(mort)
glimpse(recess)
glimpse(hpi)

