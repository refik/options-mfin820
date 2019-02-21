library(lubridate)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(here)

source("options-data.R")

# Parsing data time
data_date <- dmy_hms(time_of_data, tz = "EST")

# Filtering active contracts and simplifying data
active <- function(df) {
    transmute(df,
        strike_price = Strike,
        last_trade = ymd_hm(Last.Trade.Date, tz = "EST"),
        last_price = Last.Price,
        volume = Volume
    ) %>% 
        # Last trade in 3 hours and volume bigger than 20
        filter(difftime(data_date, last_trade, units = "hours") < 3,
               volume > 20)
}

# Specifying maturities with 30 and 15 days by their type
calls_30d <- active(calls_30d) %>% mutate(type = "call", maturity = 30)
calls_15d <- active(calls_15d) %>% mutate(type = "call", maturity = 15)
puts_30d <- active(puts_30d) %>% mutate(type = "put", maturity = 30)
puts_15d <- active(puts_15d) %>% mutate(type = "put", maturity = 15)

# Creating one tidy dataset
options <- bind_rows(
    calls_30d, calls_15d,
    puts_30d, puts_15d
) %>% 
    select(type, strike_price, last_price, maturity) %>% 
    arrange(type, maturity, strike_price)

strike_effect_plot <- ggplot(data = filter(options, maturity == 30)) +
    geom_line(aes(x = strike_price, y = last_price, color = type), size = 2) +
    labs(x = "Strike Price", y = "Last Price", color = "Type of Option") +
    ggtitle("Effect of Strike Price on Call and Put Premiums")

# Matching same strike prices
maturity_pair <- options %>% 
    spread(maturity, last_price) %>% 
    filter(!is.na(`15`), !is.na(`30`)) %>% 
    gather(maturity, last_price, `15`, `30`) %>% 
    mutate(maturity = as.integer(maturity))

maturity_effect_plot <- ggplot(data = maturity_pair, 
                               aes(x = maturity, y = last_price, 
                                   group = paste(type, strike_price))) +
    geom_line() +
    geom_point(aes(color = type), size = 2) +
    geom_label(aes(label = strike_price), hjust = 0, nudge_x = 0.05) +
    labs(x = "Days to Maturity", y = "Last Price", color = "Type of Option") +
    ggtitle("Effect of Remaining Maturity on Call and Put Premiums")

# Testing put call parity for 1mo options
parity_data <- options %>% 
    filter(maturity == 30) %>% 
    select(type, strike_price, last_price) %>% 
    spread(type, last_price) %>% 
    filter(!is.na(call), !is.na(put)) %>% 
    mutate(risk_free = risk_free_30d,
           spot = stock_price) %>% 
    mutate(`pv(strike)` = strike_price / (1 + risk_free / 12)) %>% 
    mutate(`call + pv(strike)` = call + `pv(strike)`,
           `put + spot` = put + spot) %>% 
    mutate(parity_difference = `put + spot` - `call + pv(strike)`)
    
