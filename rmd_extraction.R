cache_dir <- './cache'

files <- paste0(cache_dir, '/', list.files(cache_dir))

rmd_flag <- tools::file_ext(tolower(files)) == "rmd"

r_code <- purrr::map_if(files, rmd_flag, purrr::possibly(knitr::purl, NA))

