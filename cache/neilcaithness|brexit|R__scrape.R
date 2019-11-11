library(magrittr)
library(rvest)
library(tidyverse)

# URL to be scraped
url <- 'https://www.theguardian.com/uk-news/ng-interactive/2019/mar/27/how-did-your-mp-vote-in-the-indicative-votes'

# Extract nodes from the relevant table
rows <- url %>%
    read_html %>%
    html_nodes('.int-table') %>%
    html_nodes(xpath = '//div[@class="int-row int-row--mp"]')

# Define a scraper function
scrape_row <- function(i) {
    name <- i %>%
        html_nodes(xpath = 'div[@class="int-cell int-cell--name"]') %>%
        html_text() %>%
        str_replace_all("\n", "")
    const <- i %>%
        html_nodes(xpath = 'div[@class="int-cell int-cell--const"]') %>%
        html_text() %>%
        str_replace_all("\n", "")
    party <- i %>%
        html_nodes(xpath = 'div[1]') %>%
        html_text() %>%
        str_replace_all("\n", "")
    colour <- i %>%
        html_nodes(xpath = 'div[1]') %>%
        html_attrs() %>%
        as.character() %>%
        str_replace("int-cell int-cell--party int-color--", "")
    vote <- i %>%
        html_nodes(xpath = 'div[@class="int-cell int-cell--vote"]/node()/*') %>%
        html_attrs() %>%
        unlist() %>%
        str_replace("gv-vote-blob gv-", "")
    vote_id <- paste0("V", seq(1, 8))

    data.frame(
        name,
        const,
        party,
        vote_id,
        vote
    ) %>% return()
}

# Apply scraper function
out_list <- rows %>%
    lapply(scrape_row)

# Row bind and spread
mp <- do.call(rbind, out_list) %>%
    spread(key = "vote_id", value = "vote")

# Make the metadata columns characters instead of factors
cols <- c("name", "const" ,"party")
mp[cols] %<>%
    lapply(as.character)

# Make the spread of vote columns ordered factors
cols <- c("V1","V2","V3","V4","V5","V6","V7","V8")
mp[cols] %<>%
    lapply(factor,
           ordered = TRUE,
           levels = c("against", "did-not-vote", "for"))

# Manual corrections and split the `const` field
mp[466, "const"] <- "Anglesey (Leave 62%)"
mp[476, "const"] <- "Weston-super-Mare (Leave 57%)"
mp[577, "const"] <- "Blackley and Broughton (Leave 50%)"
mp[580, "const"] <- "Richmond Yorks (Leave 55%)"
mp %<>% separate(col = "const",
                 into = c("const", "referendum"),
                 sep = "\\(" )
mp %<>% separate(col = "referendum",
                 into = c("referendum", "percentage"),
                 sep = " " )
mp[, "percentage"] <- str_replace(mp[, "percentage"], "%\\)", "")

# Clean-up unwanted variables
rm(url, rows, out_list, cols, scrape_row)
