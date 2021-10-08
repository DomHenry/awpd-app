library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(glue)
library(shinydashboard)
library(DT)
library(shinyBS)
library(RPostgres)
library(shinyjs)
library(DBI)

# DOCKER VERSION V2.2

## Use this function to update Personal Access Token from GitHub
# gitcreds::gitcreds_set()

source("src/02_plot & helper functions.R")

appCSS1 <- "
#loading-content1 {
  position: absolute;
  background: #FFFFFF;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}
"

appCSS2 <- "
#loading-content2 {
  position: absolute;
  background: #FFFFFF;
  opacity: 1;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}
"

# Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "African Wildlife Poisoning Database",
  titleWidth = 400
)

# Side bar ----------------------------------------------------------------
sidebar <- dashboardSidebar(
  width = 350,
  useShinyjs(),
  inlineCSS(appCSS1),

  # Loading message
  div(
    id = "loading-content1",
    h2("")
  ),
  tags$head(
    tags$style(HTML("
                    .sidebar {height: 160vh; overflow-y: auto; overflow-x: hidden;}
                  ")) # adding position: fixed works but messes up the scroll bar position
  ),
  sidebarMenu(
    selectInput(
      inputId = "baseplot", label = tags$h4("1. Start by choosing a base plot"),
      choices = c(
        "Country" = "country",
        "Year" = "year",
        "Animal group" = "taxa",
        "Species" = "vernacularname",
        "Poison type" = "poison_family",
        "Poison reason" = "poison_reason"
      ),
      selected = "country"
    ),
    tags$h4(HTML("&nbsp;"), HTML("&nbsp;"), "2. Adjust base plot using the filters below"),
    radioButtons(
      inputId = "y_choice", label = "Display",
      choices = c(
        "Mortalities" = "total_mort",
        "Incidents" = "total_incidents"
      ),
      selected = NULL, inline = TRUE
    ),
    uiOutput("dateslider"),
    radioButtons(
      inputId = "gltca_choice", label = "Data tag*",
      choices = c(
        "All" = "ALL",
        "GLTCA only" = "GLTFCA"
      ),
      selected = "ALL", inline = TRUE
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px;  padding: 1px 10px",
        uiOutput("select_country")
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_country", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        uiOutput("select_animal_group")
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_animal_group", "", icon = icon("rotate-left", lib = "font-awesome"))
      ),
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        uiOutput("select_species")
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_species", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        uiOutput("select_poison_choice")
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_poison_type", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    fluidRow(
      div(
        style = "display: inline-block;vertical-align:top; width:280px; padding: 1px 10px",
        uiOutput("select_reason_choice")
      ),
      div(
        style = "display: inline-block; vertical-align:top; width:50px; margin-top:31px",
        actionButton("reset_poison_reason", "", icon = icon("rotate-left", lib = "font-awesome"))
      )
    ),
    hr(),
    div(style = "padding: 1px 10px",
        tags$p("*This button filters the data to only include ",
               tags$br(),
               "records collected in the Great Limpopo",
               tags$br(),
               " Transfrontier Conservation Area (GLTCA) which",
               tags$br(),
               " includes South Africa, Mozambique, and Zimbabwe.",
               tags$br(),
               "")
    ),
    div(style = "padding: 1px 10px",
        "**When using species base plot note",
        tags$br(),
        "that in the case where there are more",
        tags$br(),
        "than 20 species within an animal group only",
        tags$br(),
        "the top 20 (in terms of mortality or incidents)",
        tags$br(),
        " will be plotted.",
        tags$br(),
        "",
        tags$br()
    )
  )
)

# Body --------------------------------------------------------------------
body <- dashboardBody(
  tags$head(tags$style(
    HTML(".wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}")
  )),
  tags$style(HTML("
                  .box.box-solid.box-primary>.box-header {
                  color:#fff;
                  background:#6e9f57
                  }

                  .box.box-solid.box-primary{
                  border-bottom-color:#6e9f57;
                  border-left-color:#6e9f57;
                  border-right-color:#6e9f57;
                  border-top-color:#6e9f57;
                  }

                  .box.box-solid.box-success>.box-header {
                  color:#fff;
                  background:#666666
                  }

                  .box.box-solid.box-success{
                  border-bottom-color:#666666;
                  border-left-color:#666666;
                  border-right-color:#666666;
                  border-top-color:#666666;
                  }

                  ")),

  # Loading message
  useShinyjs(),
  inlineCSS(appCSS2),
  div(
    id = "loading-content2",
    h2("Loading...")
  ),
  fluidRow(
    column(
      width = 7,
      style = "padding-top:10px",
      img(src = "logo_combined.png", height = "100px")
    ),
    column(
      width = 5,
      splitLayout(
        uiOutput("valbox1"),
        uiOutput("valbox2"),
        uiOutput("valbox3"),
        cellWidths = c(150, 150, 150),
        cellArgs = list(style = "padding: 0px")
      )
    )
  ),
  fluidRow(
    column(
      width = 4,
      box(
        title = "About",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        # height = "200px",
        collapsible = TRUE,
        collapsed = FALSE,
        tags$p(
          "This app provides summary graphs of the data held in the African Wildlife Poisoning Database. The data have been collated
                      from direct observations, and various reports in the media and published literature. Domestic animals are only included
                      where they were poisoned in a wildlife poisoning incident. For more information please visit",
          a("https://africanwildlifepoisoning.org",
            href = "https://africanwildlifepoisoning.org"
          )
        )
      )
    ),
    column(
      width = 4,
      box(
        title = "Citation",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        # height = "200px",
        collapsible = TRUE,
        collapsed = FALSE,
        "To cite the graphs and data, please use: The Endangered Wildlife Trust and the
                      Peregrine Fund. yyyy. The African Wildlife Poisoning Database. Downloaded from www.awpd.cloud on yyyy-mm-dd"
      )
    ),
    column(
      width = 4,
      box(
        title = "Disclaimer",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        # height = "200px",
        collapsible = TRUE,
        collapsed = FALSE,
        "Numbers of incidents and mortalities per country are directly related to reporting rates. In reality
                      the actual number of incidents and mortalities are likely to be higher than those presented here, particularly
                      in countries with low reporting rates."
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      box(
        title = "Base plot",
        width = NULL,
        status = "success",
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
