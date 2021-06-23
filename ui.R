library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DT)


# Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "African Wildlife Poisoning Database",
  titleWidth = 400
)


# Side bar ----------------------------------------------------------------
sidebar <- dashboardSidebar( width = 350,
                             tags$head(
                               tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }
                      "))
                             ),
  sidebarMenu(
      selectInput(
      inputId = "baseplot", label = "Choose base plot",
      choices = c(
        "Country" = "country",
        "Year" = "year",
        "Poison type" = "poison_family",
        "Poison reason" = "poison_reason",
        "Top 20 species" = "vernacularname",
        "Animal group" = "taxa"
      ),
      selected = "country"
    ),
    radioButtons(
      inputId = "y_choice", label = "Display",
      choices = c(
        "Mortalties" = "total_mort",
        "Incidents" = "total_incidents"
      ),
      selected = NULL, inline = TRUE
    ),
    sliderInput("year_slider",
                label = "Date range",
                min = list_year_start, max = list_year_end,
                value = c(list_year_start, list_year_end),
                sep = "", step = 1

    ),
    radioButtons(
      inputId = "gltca_choice", label = "Data tag",
      choices = c(
        "All" = "ALL",
        "GLTCA only" = "GLTFCA"
      ),
      selected = "ALL", inline = TRUE
    ),
    selectizeInput(
      inputId = "country_choice", label = "Country",
      choices = list_countries,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All countries selected"
      )
    ),

      selectizeInput(
        inputId = "species_choice", label = "Species",
        choices = list(
          `Vultures` = wp_data %>% filter(taxa == "Vulture") %>% pull(vernacularname) %>% unique() %>% sort(),
          `Birds` = wp_data %>% filter(taxa == "Bird") %>% pull(vernacularname) %>% unique() %>% sort(),
          `Mammals` = wp_data %>% filter(taxa == "Mammal") %>% pull(vernacularname) %>% unique() %>% sort(),
          `Reptiles` = wp_data %>% filter(taxa == "Reptile") %>% pull(vernacularname) %>% unique() %>% sort(),
          `Fish` = wp_data %>% filter(taxa == "Fish") %>% pull(vernacularname) %>% unique() %>% sort(),
          `Invertebrates` = wp_data %>% filter(taxa == "Invertebrate") %>% pull(vernacularname) %>% unique() %>% sort()
        ),
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "All species selected"
        )
      ),

    selectizeInput(
      inputId = "poison_choice", label = "Poison type",
      choices = list_poisons,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All types selected"
      )
    ),
    selectizeInput(
      inputId = "reason_choice", label = "Poison reason",
      choices = list_reason,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All reasons selected"
      )
    )
    )
  )


# Body --------------------------------------------------------------------

body <- dashboardBody(
  tags$style(HTML("
                  .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#666666
                  }

                  .box.box-solid.box-primary{
                  border-bottom-color:#666666;
                  border-left-color:#666666;
                  border-right-color:#666666;
                  border-top-color:#666666;
                  }

                  ")),
  fluidRow(
    column(
      width = 9,
      box(
        title = "",
        width = NULL,
        status = "primary",
        solidHeader = TRUE, color = "green",
        height = "600px",
        plotOutput("plot", height = 400)
      ),
      div(
        id = "clearcontroldiv",
        absolutePanel(
          id = "clear_control", class = "panel panel-default",
          fixed = FALSE, draggable = FALSE,
          top = 3, right = 20, left = "auto", bottom = "auto",
          width = "auto", height = "auto",
          actionButton("reset_plot", "Reset all values")
        ),
        div(
          id = "downloadplotdiv",
          absolutePanel(
            id = "clear_control", class = "panel panel-default",
            fixed = FALSE, draggable = FALSE,
            top = 3, right = 150, left = "auto", bottom = "auto",
            width = "auto", height = "auto",
            downloadButton("download_plot", "Download plot")
          )
        )
      )
    ),
    column(
      width = 3,
      valueBox(
        subtitle = "Total mortalities",
        value = sum(wp_data$mortality),
        icon = NULL,
        width = 10,
        color = "orange"
      ),
      valueBox(
        subtitle = "Total poisoning incidents",
        value = length(unique(wp_data$global_id)),
        icon = NULL,
        width = 10,
        color = "yellow"
      ),
      valueBox(
        subtitle = "Total vulture mortalities",
        value = wp_data %>%
          filter(str_detect(vernacularname, "Vulture")) %>%
          summarise(vult_mort_total = sum(mortality)),
        icon = NULL,
        width = 10,
        color = "red"
      )
    )
  )
)

# Dashboard page ----
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)

