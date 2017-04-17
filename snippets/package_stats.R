library(ggplot2)
library(dplyr)
library(zoo)
library(rworldmap)

cran <- list(luminescence = readRDS("data/CRAN_Luminescence.rds"), 
             rlumshiny = readRDS("data/CRAN_RLumShiny.rds"))

world_map <- map_data("world")

gg_Timeline <- function(x) {
  lapply(x, function(y) {
    select(y, date) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("date", "freq")) %>%
      filter(freq < 300) %>%
      mutate(rollmean = rollmean(.$freq, 14, fill = c(NA, NULL, NA))) %>% 
      ggplot(aes(as.Date(date), freq)) + 
      theme_minimal() +
      labs(x = "Date", y = "Downloads") +
      geom_step(color = "grey60") +
      geom_line(aes(as.Date(date), rollmean), color = "red")
  })
}

gg_Country <- function(x) {
  lapply(x, function(y) {
    browser()
    select(cran$luminescence, country) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("country", "freq")) %>%
      .[order(.$freq, decreasing = TRUE), ] %>%
      .[1:30, ] %>% 
      ggplot(aes(country, freq)) +
      geom_col() +
      coord_flip() +
      labs(y = "CRAN Downloads", x = "Country Code")
  })
}

gg_Map <- function(x) {
  lapply(x, function(y) {
    select(y, country) %>% 
      setNames("code") %>% 
      select(code) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("code", "freq")) %>% 
      joinCountryData2Map(joinCode = "ISO2", nameJoinColumn = "code", verbose = FALSE) %>% 
      mapBubbles(nameZSize = "freq", 
                 landCol = "grey",
                 borderCol = "black",
                 legendTitle = "Downloads", 
                 legendHoriz = FALSE, 
                 colourPalette = "rainbow", 
                 oceanCol = "white")
  })
}

gg_ByVersion <- function(x) {
  lapply(x, function(y) {
    select(y, r_version) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("version", "freq")) %>% 
      ggplot(aes(version, freq, fill = version)) + 
      geom_col() + 
      coord_polar()
  })
}

gg_ByOS <- function(x) {
  lapply(x, function(y) {
    tmp <- select(y, r_os) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("os", "freq"))
    
    df <- data.frame(os = c("darwin", "cygwin", "linux", "mingw"),
                     freq = c(
                       sum(tmp$freq[grep("darwin", tmp$os)]),
                       sum(tmp$freq[grep("cygwin", tmp$os)]),
                       sum(tmp$freq[grep("linux", tmp$os)]),
                       sum(tmp$freq[grep("mingw", tmp$os)])
                     ))
    
    ggplot(df, aes(os, freq, fill = os)) +
      geom_col() +
      coord_polar()
  })
}

gg_ByArch <- function(x) {
  lapply(x, function(y) {
    select(y, r_arch) %>% 
      table() %>% 
      as.data.frame() %>% 
      setNames(c("arch", "freq")) %>% 
      ggplot(aes(arch, freq, fill = arch)) +
      geom_col() +
      coord_polar()
  })
}