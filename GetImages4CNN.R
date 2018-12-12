library(tidyverse)
library(rjson)



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


get_images <- function(category, recognized = TRUE) {
    
    status <- ifelse(recognized==TRUE, "True", "False")
    num    <- ifelse(recognized==TRUE,  12000, 1000)
    
    dat <- readRDS(here::here("data", "raw", paste0(category, "_raw.rds"))) %>%
        filter(recognized==status)
    
    if (nrow(dat) > num) { dat <- dat %>% sample_n(num) }
    
    dat %>%
        # select(-c(timestamp, word, countrycode)) %>%
        select(-recognized)
}

raw_to_strokes <- function(data) {
    
     data %>%
        mutate(drawing = map(drawing, fromJSON)) %>%
        unnest() %>%
        mutate(drawing = map(drawing, ~ 
                                 do.call(., what="rbind") %>%
                                 t() %>%
                                 as_tibble())) %>%
        unnest(.id = "stroke") %>%
        rename(x = "V1", y = "V2") %>%
        mutate(x = as.integer(x), y = as.integer(y))
}

center_x <- function(data) {
    data %>%
        group_by(key_id) %>%
        mutate(x = x + (0+255)/2 - ((min(x)+max(x))/2)) %>%
        ungroup()
}

center_y <- function(data) {
    data %>%
        group_by(key_id) %>%
        mutate(y = y + (0+255)/2 - ((min(y)+max(y))/2)) %>%
        ungroup()
}

resolution_256_to_32 <- function(data) {
    data %>%
        mutate(x = floor(x/8),
               y = floor(y/8))
}

strokes_to_points <- function(data) {
    data <- data %>%
        group_by(key_id) %>%
        mutate(x.lead = lag(x),
               y.lead = lag(y),
               same.stroke = stroke==lag(stroke)) %>%
        ungroup() %>%
        filter(same.stroke == TRUE) %>%
        select(-same.stroke) %>%
        mutate(xdiff = x - x.lead,
               ydiff = y - y.lead)
    
    points.1 <- data %>%
        filter(xdiff == 0) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = .$x,
                      y = seq(.$y.lead, .$y)))
    
    points.2 <- data %>%
        filter(xdiff != 0, ydiff == 0) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = seq(.$x.lead, .$x),
                      y = .$y))
    
    points.3 <- data %>%
        filter(xdiff != 0, ydiff != 0, abs(xdiff) >= abs(ydiff)) %>%
        mutate(slope = ydiff / xdiff) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      x = seq(.$x.lead, .$x),
                      y = .$y.lead + round(seq(0, .$xdiff) * .$slope)))
    
    points.4 <- data %>%
        filter(xdiff != 0, ydiff != 0, abs(xdiff) < abs(ydiff)) %>%
        mutate(slope = xdiff / ydiff) %>%
        rowwise() %>%
        do(data.frame(key_id = .$key_id,
                      stroke = .$stroke,
                      y = seq(.$y.lead, .$y),
                      x = .$x.lead + round(seq(0, .$ydiff) * .$slope))) %>%
        select(key_id, stroke, x, y)
    
    points <- bind_rows(points.1,
                        points.2,
                        points.3,
                        points.4) %>%
        group_by(key_id, stroke) %>%
        distinct() %>%
        ungroup() %>%
        select(-stroke)
    points
}

points_to_raster <- function(data) {
    data %>%
        mutate(x = factor(x, levels = seq(0, 31)),
               y = factor(y, levels = seq(0, 31)),
               value = 1) %>%
        complete(key_id, x, y) %>%
        mutate(value = ifelse(is.na(value), 0, value)) %>%
        mutate(x.y = paste0(formatC(x, width = 2, flag = 0), 
                            formatC(y, width = 2, flag = 0))) %>%
        select(-c(x, y)) %>%
        distinct() %>%
        mutate(value = as.integer(value)) %>%
        spread(x.y, value)
}


extract_xy_data <- function(data) {
    x <- data %>% 
        select(-c(label, category, key_id)) %>%
        mutate_all(as.integer) %>%
        t()
    dim(x) <- c(32, 32, 1, ncol(x))
    x <- aperm(x, c(4, 1, 2, 3))
    
    list(id       = data %>% pull(key_id), 
         category = data %>% pull(category),
         label    = data %>% pull(label),
         y        = data %>% pull(label) %>% to_categorical(),
         x        = x)
}






set.seed(1022239211)
data <- lapply(1:length(some.edibles), function(i) {
    
    cat <- some.edibles[i]
    tmp <- get_images(category = cat, recognized = TRUE) %>%
        raw_to_strokes() %>%
        center_x() %>%
        center_y() %>%
        resolution_256_to_32() %>%
        strokes_to_points() %>%
        points_to_raster() %>%
        mutate(category = cat, label = i-1)
    
    rows  <- sample(nrow(tmp), 2000)
    
    validation <- tmp[rows[   1:1000],]
    test       <- tmp[rows[1001:2000],]
    train      <- tmp[-rows,]
    train_mini <- train %>% sample_n(4000)
    train_nano <- train %>% sample_n(2000)
    
    rm(tmp, rows)
    
    unrecognized <- get_images(category = cat, recognized = FALSE) %>%
        raw_to_strokes() %>%
        center_x() %>%
        center_y() %>%
        resolution_256_to_32() %>%
        strokes_to_points() %>%
        points_to_raster() %>%
        mutate(category = cat, label = i-1)
    
    out <- list(train        = train,
                train_mini   = train_mini,
                train_nano   = train_nano,
                validation   = validation,
                test         = test,
                unrecognized = unrecognized)
    
    saveRDS(out, file = here::here("data", "uniform", 
                                   paste0(cat, "_uniform.rds")))
    
    out
})

names(data) <- some.edibles



train        <- NULL
train_mini   <- NULL
train_nano   <- NULL
validation   <- NULL
test         <- NULL
unrecognized <- NULL

for (i in some.edibles) {
    train        <- rbind(train,        data[[i]]$train       )
    train_mini   <- rbind(train_mini,   data[[i]]$train_mini  )
    train_nano   <- rbind(train_nano,   data[[i]]$train_nano  )
    validation   <- rbind(validation,   data[[i]]$validation  )
    test         <- rbind(test,         data[[i]]$test        )
    unrecognized <- rbind(unrecognized, data[[i]]$unrecognized)
}

rm(data)

train        <- train        %>% extract_xy_data()
train_mini   <- train_mini   %>% extract_xy_data()
train_nano   <- train_nano   %>% extract_xy_data()
validation   <- validation   %>% extract_xy_data()
test         <- test         %>% extract_xy_data()
unrecognized <- unrecognized %>% extract_xy_data()


saveRDS(train,        file = here::here("data", "ready", "train.rds"       ))
saveRDS(train_mini,   file = here::here("data", "ready", "train_mini.rds"  ))
saveRDS(train_nano,   file = here::here("data", "ready", "train_nano.rds"  ))
saveRDS(validation,   file = here::here("data", "ready", "validation.rds"  ))
saveRDS(test,         file = here::here("data", "ready", "test.rds"        ))
saveRDS(unrecognized, file = here::here("data", "ready", "unrecognized.rds"))


