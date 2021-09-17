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
  mutate(vernacularname = str_trim(vernacularname)) %>%
  mutate(poison_family = ifelse(is.na(poison_family), "Unknown", poison_family))

# Check data --------------------------------------------------------------

## Animal groups - check NA values with LR (for now replace with "Unknown")
unique(wp_data$taxa)
table(wp_data$taxa, useNA = "always")

wp_data <- wp_data %>%
  mutate(taxa = case_when(is.na(taxa) ~ "Unknown",
                          TRUE ~ as.character(taxa)))
## Remove 'zero' dates
wp_data <- wp_data %>%
  filter(year > 1950)

wp_data %>%
  group_by(vernacularname) %>%
  tally() %>%
  write_csv("checks/vern_tally.csv")

wp_data %>%
  filter(str_detect(vernacularname, "Vulture")) %>%
  group_by(vernacularname) %>%
  tally() %>%
  write_csv("checks/vern_vult_tally.csv")

(wp_data %>%
  filter(str_detect(vernacularname, "Vulture|Griffon")) %>%
  group_by(vernacularname) %>%
  summarise(total = sum(mortality)) %>%
  write_csv("checks/vern_vult_mort_tally.csv"))

wp_data %>%
  group_by(vernacularname) %>%
  arrange() %>%
  tally() %>%
  write_csv("checks/vern_spp_list.csv")

## New tag
unique(wp_data$tag)

## Poison type - check for NAs
unique(wp_data$poison_family)
table(wp_data$poison_family, useNA = "always")

## Poison reason - check for NAs
unique(wp_data$poison_reason)
table(wp_data$poison_reason, useNA = "always")

## Common names - check for NAs
unique(wp_data$vernacularname)
table(wp_data$vernacularname, useNA = "always")


# Create lists ------------------------------------------------------------
(list_countries <- sort(unique(wp_data$country)))
(list_poisons <- sort(unique(wp_data$poison_family)))
(list_species <- sort(unique(wp_data$vernacularname)))
(list_reason <- sort(unique(wp_data$poison_reason)))
(list_year_start <- as.integer(min(wp_data$year, na.rm = TRUE)))
(list_year_end <- as.integer(max(wp_data$year, na.rm = TRUE)))
(list_taxa <- sort(unique(wp_data$taxa)))

