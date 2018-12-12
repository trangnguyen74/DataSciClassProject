library(tidyverse)
library(rjson)


if (!dir.exists(here::here("data_download"))) { 
    dir.create(here::here("data_download"))
}
if (!file.exists(here::here("data_download", "train_simplified.zip"))) {
    paste0(c("You need to download the data file train_simplified.zip from\n",
             "https://www.kaggle.com/c/quickdraw-doodle-recognition/data",
             "first!"))
}

some.edibles <- c("apple", "asparagus",
                  "banana", "birthday cake", "blackberry", "blueberry",
                  "bread", "broccoli",
                  "cake", "carrot", "cookie",
                  "donut",
                  "grapes",
                  "hamburger", "hot dog",
                  "ice cream", 
                  "lollipop",
                  "mushroom",
                  "onion",
                  "peanut", "pear", "peas", "pineapple", "pizza", "popsicle",
                  "potato", 
                  "sandwich", "steak", "strawberry", "string bean",
                  "watermelon")


get_raw_file <- function(category) {
    
    if (!dir.exists(here::here("data"))) { dir.create(here::here("data")) }
    
    if(!file.exists(here::here("data", paste0(category, "_raw.rds")))) {
        dat <- read_csv(unz(here::here("data_download", "train_simplified.zip"),
                            paste0(category,".csv")), 
                        col_types = cols()) %>%
            select(-c(timestamp, word, countrycode))
        
        saveRDS(dat, file = here::here("data", paste0(category, "_raw.rds")))
    }
}

for (i in some.edibles) { get_raw_file(i) }



