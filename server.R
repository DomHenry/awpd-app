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
      inputId = "species_choice", label = NULL, choices = list(
        `Vultures` = wp_data %>% filter(taxa == "Vulture") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Birds` = wp_data %>% filter(taxa == "Bird") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Mammals` = wp_data %>% filter(taxa == "Mammal") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Reptiles` = wp_data %>% filter(taxa == "Reptile") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Fish` = wp_data %>% filter(taxa == "Fish") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Invertebrates` = wp_data %>% filter(taxa == "Invertebrate") %>% pull(vernacularname) %>% unique() %>% sort()
      ),
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
      label = "Date range",
      min = list_year_start, max = list_year_end,
      value = c(list_year_start, list_year_end),
      step = 1
    )

    updateRadioButtons(session,
      inputId = "y_choice", label = "Display",
      choices = c(
        "Mortalties" = "total_mort",
        "Incidents" = "total_incidents"
      ),
      selected = "total_mort", inline = TRUE
    )

    updateRadioButtons(session,
      inputId = "gltca_choice", label = "Data tag",
      choices = c(
        "All" = "ALL",
        "GLTCA only" = "GLTFCA"
      ),
      selected = "ALL", inline = TRUE
    )

    updateSelectInput(session,
      inputId = "baseplot", label = NULL,
      choices = c(
        "Country" = "country",
        "Year" = "year",
        "Poison type" = "poison_family",
        "Poison reason" = "poison_reason",
        "Top 20 species" = "vernacularname",
        "Animal group" = "taxa"
      ),
      selected = "country"
    )
  })

  ## Data frame query ----
  query_data <- reactive({
    wp_data %>%
      filter(if (input$gltca_choice == "GLTFCA") tag == "GLTFCA" else tag %in% c("ALL", "GLTFCA")) %>%
      filter(country %in% values$country_choice) %>%
      filter(vernacularname %in% values$species_choice) %>%
      filter(poison_family %in% values$poison_choice) %>%
      filter(poison_reason %in% values$reason_choice) %>%
      filter(year %in% c(min(input$year_slider):max(input$year_slider))) %>%
      group_by(.data[[input$baseplot]]) %>%
      summarise(
        total_mort = sum(mortality),
        total_incidents = length(unique(global_id))
      )
  })

  ylab_plot <- reactive({

    ifelse(input$y_choice == "total_mort", "Mortalities","Incidents")
  })

  ## Render country plot ----
  output$plot <- renderPlot(
    {
      if (input$baseplot == "country") {
        plot_country(query_data(), .data[[input$y_choice]],ylab_plot())
      } else if (input$baseplot == "poison_family") {
        plot_poison(query_data(), .data[[input$y_choice]],ylab_plot())
      } else if (input$baseplot == "year") {
        plot_year(query_data(), .data[[input$y_choice]],ylab_plot())
      } else if (input$baseplot == "poison_reason") {
        plot_reason(query_data(), .data[[input$y_choice]],ylab_plot())
      } else if (input$baseplot == "taxa") {
        plot_animal(query_data(), .data[[input$y_choice]],ylab_plot())
      } else if(input$baseplot == "vernacularname") {
        plot_top20(query_data(), .data[[input$y_choice]], ylab_plot())
      }
    },
    height = 500,
    width = 700
  )

  ## Download country plot ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("AWPD plot ", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file,
        plot = if (input$baseplot == "country") {
          plot_country(query_data(), .data[[input$y_choice]],ylab_plot())
        } else if (input$baseplot == "poison_family") {
          plot_poison(query_data(), .data[[input$y_choice]],ylab_plot())
        } else if (input$baseplot == "year") {
          plot_year(query_data(), .data[[input$y_choice]],ylab_plot())
        } else if (input$baseplot == "poison_reason") {
          plot_reason(query_data(), .data[[input$y_choice]],ylab_plot())
        } else if(input$baseplot == "vernacularname") {
          plot_top20(query_data(), .data[[input$y_choice]], ylab_plot())
        },
        width = 10, height = 8
      )
    }
  )
}
