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
        column(
          width = 3,
          box(
            title = "Filter by species",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            selectizeInput(
              inputId = "species_choice", label = "Group choice below:",
              choices = list(
                `Vultures` = list_species[which(str_detect(list_species, "Vulture"))],
                `All animals` = list_species[which(!str_detect(list_species, "Vulture"))],
                `Mammals` = list("MN", "WI", "IA")
              ),
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All species currently selected"
              )
            ),
            radioButtons(
              inputId = "y_choice", label = "Choose y-axis",
              choices = c(
                "Mortalties" = "m",
                "Incidents" = "i"
              ),
              selected = "m"
            )
          ),
          box(
            title = "Filter by country ",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            selectizeInput(
              inputId = "country_choice", label = "Group choice below:",
              choices = list_countries,
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All countries currently selected"
              )
            )
          ),
          box(
            title = "Select date range",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            sliderInput("year_slider",
                        label = h3("Year slider"),
                        min = list_year_start, max = list_year_end,
                        value = c(list_year_start, list_year_end),
                        sep = "", step = 1
            )
          ),
          box(
            title = "Filter by poison type ",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            selectizeInput(
              inputId = "poison_choice", label = "Group choice below:",
              choices = list_poisons,
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All countries currently selected"
              )
            )
          ),
          box(
            title = "Filter by poison reason ",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            selectizeInput(
              inputId = "reason_choice", label = "Group choice below:",
              choices = list_reason,
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All countries currently selected"
              )
            )
          ),
          box(
            title = "Download plots (PNG)", width = NULL,
            status = "primary", solidHeader = TRUE,
            downloadButton("downloadPlot_country", "Country plot"),
            br(),
            br(),
            downloadButton("downloadPlot_maxcost", "Some other plot")
          )
        ),
        ## Plot output ----
        column(
          width = 7,
          box(
            title = "Output",
            width = NULL,
            status = "primary", solidHeader = TRUE,
            height = "600px",
            plotOutput("plot", height = 250)
          )
        ),
        column(
          width = 2,
          valueBox(subtitle = "Total mortalities",
                   value = sum(wp_data$mortality),
                   icon = NULL,
                   width = 10,
                   color = "orange"),
          valueBox(subtitle = "Total poisoning incidents",
                   value = length(unique(wp_data$global_id)),
                   icon = NULL,
                   width = 10,
                   color = "yellow"),
          valueBox(subtitle = "Total vulture mortalities",
                   value = wp_data %>%
                     filter(str_detect(vernacularname, "Vulture")) %>%
                     summarise(vult_mort_total = sum(mortality)),
                   icon = NULL,
                   width = 10,
                   color = "red")

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
