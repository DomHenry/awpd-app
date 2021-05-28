## SERVER ----
server <- function(input, output, session) {

  ## Default values ----
  values <- reactiveValues(
    country_choice = list_countries,
    species_choice = list_species,
    poison_choice = list_poisons,
    reason_choice = list_reason
  )

  observeEvent(input$country_choice, {
    values$country_choice <- input$country_choice
  })
  observeEvent(input$species_choice, {
    values$species_choice <- input$species_choice
  })
  observeEvent(input$poison_choice, {
    values$poison_choice <- input$poison_choice
  })
  observeEvent(input$reason_choice, {
    values$reason_choice <- input$reason_choice
  })

  observeEvent(input$reset_plot, {
    values$country_choice <- list_countries
    values$species_choice <- list_species
    values$poison_choice <- list_poisons
    values$reason_choice <- list_reason

    updateSelectizeInput(session,
      inputId = "country_choice", label = NULL, choices = list_countries,
      selected = NULL, options = list(
        placeholder = "All countries selected"
      )
    )
    updateSelectizeInput(session,
      inputId = "species_choice", label = NULL, choices = list_species,
      selected = NULL, options = list(
        placeholder = "All species selected"
      )
    )
    updateSelectizeInput(session,
      inputId = "poison_choice", label = NULL, choices = list_poisons,
      selected = NULL, options = list(
        placeholder = "All types selected"
      )
    )
    updateSelectizeInput(session,
      inputId = "reason_choice", label = NULL, choices = list_reason,
      selected = NULL, options = list(
        placeholder = "All reasons selected"
      )
    )
    updateSliderInput(session,
      inputId = "year_slider",
      label = NULL,
      min = list_year_start, max = list_year_end,
      value = c(list_year_start, list_year_end),
      step = 1
    )
    updateRadioButtons(session,
      inputId = "y_choice", label = "Choose y-axis",
      choices = c(
        "Mortalties" = "total_mort",
        "Incidents" = "total_incidents"
      ),
      selected = "total_mort"
    )

    updateRadioButtons(session,
      inputId = "gltca_choice", label = "Data tag",
      choices = c(
        "All" = "all",
        "GLTCA only" = "gltca"
      ),
      selected = "all"
    )
  })

  ## Data frame query ----
  query_data <- reactive({
    wp_data %>%
      filter(country %in% values$country_choice) %>%
      filter(vernacularname %in% values$species_choice) %>%
      filter(poison_family %in% values$poison_choice) %>%
      filter(reason_for_p %in% values$reason_choice) %>%
      filter(year %in% c(min(input$year_slider):max(input$year_slider))) %>%
      group_by(country) %>% # Change this to tidy eval variable .data[[input$group_var]]
      summarise(
        total_mort = sum(mortality),
        total_incidents = length(unique(global_id))
      )
  })

  ## Render country plot ----
  output$plot <- renderPlot(
    {
      plot_country(query_data(), .data[[input$y_choice]])
    },
    height = 500,
    width = 700
  )

  ## Download country plot ----
  output$downloadPlot_country <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), " country_plot.png")
    },
    content = function(file) {
      ggsave(file,
        plot = plot_country(query_data(), .data[[input$y_choice]])
        )
    }
  )
}
