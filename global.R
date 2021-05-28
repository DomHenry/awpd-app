library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DBI)
library(RPostgres)

# Create a connection to the database -------------------------------------
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "poisondb",
                      host = "ewtcsusrv01.postgres.database.azure.com",
                      port = 5432,
                      user = "awpd_viewer@ewtcsusrv01",
                      password = "awpd6847#"
)


# List tables -------------------------------------------------------------
dbListTables(con)

wp_data <- dbGetQuery(con, 'SELECT * FROM awpd_data_for_r') %>%
  as_tibble %>%
  janitor::clean_names()

wp_data %>%
  group_by(mortality) %>%
  tally()

## Total mortalities
sum(wp_data$mortality)

## Total incidents
length(unique(wp_data$global_id))

## Total vulture mortalities
wp_data %>%
  filter(str_detect(vernacularname, "Vulture")) %>%
  summarise(vult_mort_total = sum(mortality))


# Create lists ------------------------------------------------------------
list_countries <- sort(unique(wp_data$country))
list_poisons <- sort(unique(wp_data$poison_family))
list_species <- sort(unique(wp_data$vernacularname))
list_reason <- sort(unique(wp_data$reason_for_p))
list_year_start <- as.integer(min(wp_data$year, na.rm = TRUE))
list_year_end <- as.integer(max(wp_data$year, na.rm = TRUE))

min(wp_data$obsdate, na.rm = TRUE)


# Country figure ----------------------------------------------------------
# country_data <- wp_data %>%
#   group_by(country) %>%
#   summarise(total_mort = sum(mortality),
#             total_incidents = length(unique(global_id)))
#
# # plot_country <- function(data, y_choice){
#   data %>%
#   ggplot(aes(x = reorder(country, {{y_choice}}),
#              y = {{y_choice}}))+
#   geom_bar(stat = "identity")+
#   coord_flip()
#   }

plot_country_mort <- function(data){
  data %>%
    ggplot(aes(x = reorder(country, total_mort),
               y = total_mort))+
    geom_bar(stat = "identity")+
    coord_flip()
}

plot_country_incid <- function(data){
  data %>%
    ggplot(aes(x = reorder(country, total_incidents),
               y = total_incidents)) +
    geom_bar(stat = "identity")+
    coord_flip()
}

# data <- wp_data %>%
#   filter(country %in% sample(list_countries, 10)) %>%
#   filter(vernacularname %in% sample(list_species, 200)) %>%
#   filter(poison_family %in% sample(list_poisons, 3)) %>%
#   filter(reason_for_p %in% sample(list_reason, 5)) %>%
#   filter(year > 1980) %>%
#   filter(year < 2010) %>%
#   group_by(country) %>%
#   summarise(total_mort = sum(mortality),
#             total_incidents = length(unique(global_id)))
#
#
# plot_country(data, total_incidents)
# plot_country(data, total_mort)

