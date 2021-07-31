library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DT)
library(shinyBS)

# Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "African Wildlife Poisoning Database",
  titleWidth = 400
)

# Side bar ----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 350,
  useShinyjs(),
  tags$head(
    tags$style(HTML("
                      .sidebar {height: 110vh; overflow-y: auto; overflow-x: hidden}
                      ")) # adding position: fixed works but messes up the scroll bar position
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

    bsTooltip("gltca_choice", "This button filters the data to only include records collected in the Great Limpopo Transfrontier Conservation Area (GLTCA) which includes South Africa, Mozambique, and Zimbabwe.",
              "bottom", options = list(container = "body"))
    ,

    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px;  padding: 1px 10px",
        selectizeInput(
          inputId = "country_choice", label = "Country",
          choices = list_countries,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "All countries selected"
          )
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_country", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
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
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_species", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        selectizeInput(
          inputId = "poison_choice", label = "Poison type",
          choices = list_poisons,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "All types selected"
          )
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_poison_type", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),

    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        selectizeInput(
          inputId = "reason_choice", label = "Poison reason",
          choices = list_reason,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "All reasons selected"
          )
        )
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_poison_reason", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    )
  )
)

# absolutePanel(
#   id = "test", class = "panel panel-default",
#   fixed = TRUE, draggable = FALSE,
#   top = 475, right = "auto", left = 200, bottom = "auto",
#   width = 0, height = 0,
#   actionButton("reset1", label = NULL, icon = icon("rotate-left", lib = "font-awesome"))
# )

# Body --------------------------------------------------------------------
body <- dashboardBody(
  tags$head(tags$style("body {overflow-y: scroll;}")),
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
    column(width = 7,
           style = "padding-top:10px",
           img(src = 'logo_combined.png', height = "100px")
           ),
    column(width = 5,
           splitLayout(
           valueBox(
             subtitle = HTML("Total <br> mortalities"),
             value = sum(wp_data$mortality),
             icon = icon("database", lib = "font-awesome"),
             width = NULL,
             color = "orange"
           ),
           valueBox(
             subtitle = HTML("Total poisoning <br> incidents"),
             value = length(unique(wp_data$global_id)),
             icon = icon("skull-crossbones", lib = "font-awesome"),
             width = NULL,
             color = "yellow"
           ),
           valueBox(
             subtitle = HTML("Total vulture <br> mortalities"),
             value = wp_data %>%
               filter(str_detect(vernacularname, "Vulture")) %>%
               summarise(vult_mort_total = sum(mortality)),
             icon = icon("feather", lib = "font-awesome"),
             width = NULL,
             color = "red"
           ),
           cellWidths = c(150,150,150),
           cellArgs = list(style="padding: 0px")
           )
           )
  ),
  tags$h4("This app provides summary graphs of the data held in the African Wildlife Poisoning Database."),
  tags$h4("For more information please visit ",a("https://africanwildlifepoisoning.org", href = "https://africanwildlifepoisoning.org"), style = "padding-bottom: 15px"),
  fluidRow(
    column(
      width = 12,
      box(
        title = "",
        width = NULL,
        status = "primary",
        solidHeader = TRUE, color = "green",
        height = "700px",
        br(),
        plotOutput("plot", width = "auto")
      ),

      div(
        absolutePanel(
          id = "abspan5", class = "panel panel-default",
          fixed = FALSE, draggable = FALSE,
          top = 3, right = 155, left = "auto", bottom = "auto",
          width = "auto", height = "auto",
          actionButton("reset_plot", "All inputs", icon = icon("rotate-left", lib = "font-awesome"))
        ),
        absolutePanel(
          id = "abspan6", class = "panel panel-default",
          fixed = FALSE, draggable = FALSE,
          top = 3, right = 20, left = "auto", bottom = "auto",
          width = "auto", height = "auto",
          downloadButton("download_plot", "Download plot")
        )
      )
    ),
    column(width = 1)
  )
)

# Dashboard page ----
ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "blue"
)
