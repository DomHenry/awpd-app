# Define plot theme and colors --------------------------------------------
awpd_theme <- theme_bw()+
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15))

bar_fill <- alpha("grey", 0.8)
bar_col <- "black"

# Function to calculate y-axis limits -------------------------------------

## Allows the geom_text on the largest values to render correctly
calc_ylim <- function(x){

  if (x > 17000){
    return(x + 2000)
  } else if (x > 14000 & x < 17000){
    return(x + 1000)
  } else if (x > 10000 & x < 14000){
    return(x + 800)
  } else if (x > 3000 & x < 10000){
    return(x + 500)
  } else if (x > 1000 & x < 3000){
    return(x + 500)
  } else if (x < 1000){
    return(x + 50)
  }
}

calc_ylim(3500)
calc_ylim(400)

# Country figure ----------------------------------------------------------
plot_country <- function(data, y_choice, ylab){

  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    ggplot(aes(x = reorder(country, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}), hjust = -0.5)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}

# Poison type figure ------------------------------------------------------
plot_poison <- function(data, y_choice, ylab){

  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    ggplot(aes(x = reorder(poison_family, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}), hjust = -0.5)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}

# Poison reason figure ----------------------------------------------------
plot_reason <- function(data, y_choice, ylab){

  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    ggplot(aes(x = reorder(poison_reason, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}), hjust = -0.5)+
    xlab("")+
    ylab(ylab)+
    coord_flip()+
    theme_bw()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}

# Year figure -------------------------------------------------------------
plot_year <- function(data, y_choice, ylab){

  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    ggplot(aes(x = year,
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}, angle = 90), hjust = -0.1)+
    # coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}

# Animal group figure -----------------------------------------------------
plot_animal <- function(data, y_choice, ylab){

  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    filter({{y_choice}} > 0) %>%
    ggplot(aes(x = reorder(taxa, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}), hjust = -0.5)+
    coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}

# Species figure ----------------------------------------------------------

plot_species <- function(data, y_choice, ylab){


  max_val <- data %>% select({{y_choice}}) %>% pull() %>% max

  data %>%
    arrange(desc({{y_choice}})) %>%
    filter(row_number() %in% 1:20) %>%
    ggplot(aes(x = reorder(vernacularname, {{y_choice}}),
               y = {{y_choice}}))+
    geom_bar(stat = "identity", fill = bar_fill, color = bar_col)+
    geom_text(aes(label = {{y_choice}}), hjust = -0.5)+
    coord_flip()+
    xlab("")+
    ylab(ylab)+
    theme_bw()+
    ylim(0, calc_ylim(max_val))+
    awpd_theme
}
