library(shiny)
library(tidyverse)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(shinyjs)

# Create a connection to the database -------------------------------------
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "poisondb",
                      host = "ewtcsusrv01.postgres.database.azure.com",
                      port = 5432,
                      user = "awpd_viewer@ewtcsusrv01",
                      password = "awpd6847#"
)

## List tables in database
dbListTables(con)

wp_data <- dbGetQuery(con, 'SELECT * FROM awpd_data_for_r') %>%
  as_tibble %>%
  janitor::clean_names() %>%
  rename(poison_reason = reason_for_p) %>%
  mutate(tag = ifelse(is.na(tag),"ALL",tag)) %>%
  mutate(vernacularname = str_trim(vernacularname))

# Check data --------------------------------------------------------------
wp_data %>%
  group_by(mortality) %>%
  tally()

# Headline stats ----------------------------------------------------------

## Total mortalities
sum(wp_data$mortality)

## Total incidents
length(unique(wp_data$global_id))

## Total vulture mortalities
wp_data %>%
  filter(str_detect(vernacularname, "Vulture")) %>%
  summarise(vult_mort_total = sum(mortality))

# Add attributes ----------------------------------------------------------

## Decade - there are lots of missing obsdate so don't use this.
# wp_data <- wp_data %>%
#   mutate(year = lubridate::year(obsdate))

## Remove 'zero' dates
wp_data <- wp_data %>%
  filter(year > 1950)

## Assign taxon group
ani_group <- read_csv("species_list_designation.csv")

wp_data <- wp_data %>%
  left_join(ani_group)

wp_data %>%
  filter(is.na(taxa)) %>%
  distinct(vernacularname) %>%
  write_csv("add_spp_to_designation.csv")

# Create lists ------------------------------------------------------------
list_countries <- sort(unique(wp_data$country))
list_poisons <- sort(unique(wp_data$poison_family))
list_species <- sort(unique(wp_data$vernacularname))
list_reason <- sort(unique(wp_data$poison_reason))
list_year_start <- as.integer(min(wp_data$year, na.rm = TRUE))
list_year_end <- as.integer(max(wp_data$year, na.rm = TRUE))
list_taxa <- sort(unique(wp_data$taxa))

wp_data %>% filter(taxa == "Mammal") %>% pull(vernacularname) %>% unique() %>% sort()

# Define plot theme and colors --------------------------------------------
awpd_theme <- theme_bw()+
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15))

bar_fill <- alpha("grey", 0.8)
bar_col <- "black"

# Country figure ----------------------------------------------------------
data <- wp_data %>%
  filter(country %in% sample(list_countries, 10)) %>%
  filter(vernacularname %in% sample(list_species, 200)) %>%
  filter(poison_family %in% sample(list_poisons, 3)) %>%
  filter(poison_reason %in% sample(list_reason, 5)) %>%
  filter(year > 1980) %>%
  filter(year < 2010) %>%
  group_by(country) %>%
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

data <- wp_data %>%
  group_by(country) %>%
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))


plot_country <- function(data, y_choice, ylab){
  data %>%
    ggplot(aes(x = reorder(country, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    awpd_theme
}

plot_country(data, total_incidents,"Incidents")
plot_country(data, total_mort, "Mortalities")

# Poison type figure ------------------------------------------------------
data <- wp_data %>%
  group_by(poison_family) %>% # Taxon, decade,
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

plot_poison <- function(data, y_choice, ylab){
  data %>%
    ggplot(aes(x = reorder(poison_family, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    awpd_theme
}

plot_poison(data, total_incidents,"Incidents")
plot_poison(data, total_mort, "Mortalities")

# Poison reason figure ----------------------------------------------------
data <- wp_data %>%
  group_by(poison_reason) %>% # Taxon, decade,
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

plot_reason <- function(data, y_choice, ylab){
  data %>%
    ggplot(aes(x = reorder(poison_reason, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    theme_bw()+
    awpd_theme
}

plot_reason(data, total_incidents, "Incidents")
plot_reason(data, total_mort, "Mortalities")

# Year figure -------------------------------------------------------------
data <- wp_data %>%
  group_by(year) %>% # Taxon, decade,
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

plot_year <- function(data, y_choice, ylab){
  data %>%
    ggplot(aes(x = year,
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    # coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    awpd_theme
}

plot_year(data, total_incidents, "Incidents")
plot_year(data, total_mort, "Mortalities")

# Animal group figure -----------------------------------------------------
data <- wp_data %>%
  group_by(taxa) %>% # Taxon, decade,
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

plot_animal <- function(data, y_choice, ylab){
  data %>%
    ggplot(aes(x = reorder(taxa, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    awpd_theme
}

plot_animal(data, total_incidents, "Incidents")
plot_animal(data, total_mort, "Mortalities")


# Top 20 species figure ---------------------------------------------------

## Or top n species?

data <- wp_data %>%
  group_by(vernacularname) %>% # Taxon, decade,
  summarise(total_mort = sum(mortality),
            total_incidents = length(unique(global_id)))

data %>%
  arrange(desc(total_mort)) %>%
  ungroup() %>%
  top_n(20)

data %>%
  arrange(desc(total_mort)) %>%
  ungroup() %>%
  filter(row_number() %in% 1:20)

data %>%
  arrange(desc(total_incidents))

plot_top20 <- function(data, y_choice, ylab){
  data %>%
    arrange(desc({{y_choice}})) %>%
    filter(row_number() %in% 1:20) %>%
    ggplot(aes(x = reorder(vernacularname, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    awpd_theme
}

plot_top20(data, total_incidents, "Incidents")
plot_top20(data, total_mort, "Mortalities")
