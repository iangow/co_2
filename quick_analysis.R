library(readxl)
library(readr)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

download.file("https://www.co2.earth/images/data/2100-projections_climate-scoreboard_2015-1214.xlsx",
              destfile = "global.xlsx")
download.file(file.path("http://www.environment.gov.au/system/files",
                        "resources/128ae060-ac07-4874-857e-dced2ca22347",
                        "files/aust-emissions-projects-chart-data-2018.xlsx"),
              destfile = "australia.xlsx")

global <-
    read_excel("global.xlsx", sheet = "CO2e", skip = 2,
               col_types = c("numeric", "numeric", rep("skip", 7))) %>%
    rename(year = 1,
           global_co2 = 2) %>%
    mutate(year = as.integer(year),
           global_co2 = global_co2 * 1e3)

australia_base <-
    read_excel("australia.xlsx", sheet = "Figure 3", range = "A3:AP12") %>%
    rename(category = 1) %>%
    gather(key = "year", value = "co_2_aus", -category) %>%
    filter(category == "Total (incl. LULUCF)") %>%
    select(-category) %>%
    mutate(year = as.integer(year))

australia_alt <-
    read_excel("australia.xlsx", sheet = "Figure 15", range = "A3:AP8") %>%
    rename(category = 1)  %>%
    gather(key = "year", value = "co_2_aus_alt", -category) %>%
    mutate(year = as.integer(year)) %>%
    filter(category == "Baseline" & year <= 2019L |
           category == "Trajectory to minus 26% target (in 2030)" & year > 2019L) %>%
    select(-category)

current_year <- 2020
target_year <- 2050

current <-
    australia %>% 
    filter(year==current_year) %>%
    select(co_2_aus_alt) %>% 
    pull()

green <- 
    australia %>%
    filter(year >= current_year) %>%
    mutate(co_2_aus_green = if_else(year > current_year, 
                                    (target_year - year)/(target_year - current_year) * current,
                                    co_2_aus)) %>%
    select(year, co_2_aus_green)

australia <-
    australia_base %>%
    left_join(australia_alt, by = "year") %>% 
    left_join(green, by = "year")

all_data <-
    global %>%
    inner_join(australia, by = "year") %>%
    mutate(global_lnp = global_co2 + co_2_aus_alt - co_2_aus,
           global_green = global_co2 + co_2_aus_green - co_2_aus) %>%
    gather(key = "category", value = "co_2", -year)

all_data %>%
    filter(grepl("^global", category)) %>%
    ggplot(aes(x = year, y = co_2, color = category)) +
    geom_line()
