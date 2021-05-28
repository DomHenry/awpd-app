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

  ## Dataframe query ----
  query_data <- reactive({
    wp_data %>%
      filter(country %in% values$country_choice) %>%
      filter(vernacularname %in% values$species_choice) %>%
      filter(poison_family %in% values$poison_choice) %>%
      filter(reason_for_p %in% values$reason_choice) %>%
      filter(year %in% c(min(input$year_slider):max(input$year_slider))) %>%
      group_by(country) %>%
      summarise(
        total_mort = sum(mortality),
        total_incidents = length(unique(global_id))
      )
  })

  ## Render country plot ----
  output$plot <- renderPlot(
    {
      if (input$y_choice == "m") {
        plot_country_mort(query_data())
      } else if (input$y_choice == "i") {
        plot_country_incid(query_data())
      }
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
             plot = if(input$y_choice == "m") {
               plot_country_mort(query_data())
             } else if (input$y_choice == "i") {
               plot_country_incid(query_data())
             }
      )
    }
  )
}
