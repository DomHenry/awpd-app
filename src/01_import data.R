# Import steps moved to server.

# Check data --------------------------------------------------------------

## Animal groups - check NA values with LR (for now replace with "Unknown")
unique(wp_data$taxa)
table(wp_data$taxa, useNA = "always")

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
