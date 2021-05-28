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
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Tables", icon = icon("th"), tabName = "tables", badgeLabel = "beta", badgeColor = "green"),
    menuItem("Charts",
      icon = icon("bar-chart-o"),
      menuSubItem("Sub-item 1", tabName = "subitem1"),
      menuSubItem("Sub-item 2", tabName = "subitem2")
    )
  )
)

# Body --------------------------------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "dashboard",

      fluidRow(
        ## Filter boxes ----
        box(
          title = "Country ",
          width = 2, height = 250,
          status = "primary", solidHeader = TRUE,
          selectizeInput(
            inputId = "country_choice", label = NULL,
            choices = list_countries,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All countries selected"
            )
          )
        ),
        box(
          title = "Species",
          width = 3, height = 250,
          status = "primary", solidHeader = TRUE,
          selectizeInput(
            inputId = "species_choice", label = NULL,
            choices = list(
              `Vultures` = list_species[which(str_detect(list_species, "Vulture"))],
              `All animals` = list_species[which(!str_detect(list_species, "Vulture"))],
              `Mammals` = list("MN", "WI", "IA")
            ),
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All species selected"
            )
          ),
        ),

        box(
          title = "Poison",
          width = 2, height = 250,
          status = "primary", solidHeader = TRUE,
          selectizeInput(
            inputId = "poison_choice", label = "Type",
            choices = list_poisons,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All types selected"
            )
          ),
          selectizeInput(
            inputId = "reason_choice", label = "Reason",
            choices = list_reason,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "All reasons selected"
            )
          )
        ),
        box(
          title = "Date range",
          width = 2, height = 250,
          status = "primary", solidHeader = TRUE,
          sliderInput("year_slider",
            label = NULL,
            min = list_year_start, max = list_year_end,
            value = c(list_year_start, list_year_end),
            sep = "", step = 1
          )
        ),
        box(
          title = "Plot options",
          width = 2, height = 250,
          status = "primary", solidHeader = TRUE,
          radioButtons(
            inputId = "y_choice", label = "Choose y-axis",
            choices = c(
              "Mortalties" = "total_mort",
              "Incidents" = "total_incidents"
            ),
            selected = "total_mort"
          ),
          radioButtons(
            inputId = "gltca_choice", label = "Data tag",
            choices = c(
              "All" = "all",
              "GLTCA only" = "gltca"
            ),
            selected = "all"
          )
        )
        # box(
        #     title = "Download plots (PNG)", width = 2,
        #     status = "primary", solidHeader = TRUE,
        #     downloadButton("downloadPlot_country", "Country plot"),
        #     br(),
        #     br(),
        #     downloadButton("downloadPlot_maxcost", "Some other plot")
        #   )
      ),
      ## Plot output ----
      fluidRow(
        column(
          width = 9,
          box(
            title = "Output",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            height = "600px",
            plotOutput("plot", height = 250)
          ),
          div(
            id = "clearcontroldiv",
            absolutePanel(
              id = "clear_control", class = "panel panel-default",
              fixed = FALSE, draggable = FALSE,
              top = 3, right = 20, left = "auto", bottom = "auto",
              width = "auto", height = "auto",
              actionButton("reset_plot", "Reset all values")
            )
          )
        ),
        column(
          width = 2,
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
    ),

    tabItem(
      tabName = "tables",
      "content 2"
    ),
    tabItem(
      tabName = "subitem1",
      "content 3"
    ),
    tabItem(
      tabName = "subitem2",
      "content 4"
    )
  )
)


# Dashboard page ----
ui <- dashboardPage(
  header,
  sidebar,
  body
)
