## SERVER ----
server <- function(input, output, session) {

  source("src/01_import data.R", local = TRUE)

  output$dateslider <- renderUI({
    sliderInput("year_slider",
                label = "Date range",
                min = list_year_start, max = list_year_end,
                value = c(list_year_start, list_year_end),
                sep = "", step = 1
    )
  })

  output$select_country <- renderUI({
    selectizeInput(
      inputId = "country_choice", label = "Country",
      choices = list_countries,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All countries selected"
      )
    )
  })

  output$select_animal_group <- renderUI({
    selectizeInput(
      inputId = "animal_group_choice", label = "Animal group**",
      choices = list_taxa,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All types selected"
      )
    )
  })

  output$select_species <- renderUI({
    selectizeInput(
      inputId = "species_choice", label = "Species",
      choices = list(
        `Vultures` = wp_data %>% filter(taxa == "Vulture") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Bird (excl raptors)` = wp_data %>% filter(taxa == "Bird (excl raptors)") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Fish` = wp_data %>% filter(taxa == "Fish") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Amphibians` = wp_data %>% filter(taxa == "Amphibian") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Invertebrates` = wp_data %>% filter(taxa == "Invertebrate") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Mammals` = wp_data %>% filter(taxa == "Mammal") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Raptors` = wp_data %>% filter(taxa == "Raptors") %>% pull(vernacularname) %>% unique() %>% sort(),
        `Reptiles` = wp_data %>% filter(taxa == "Reptile") %>% pull(vernacularname) %>% unique() %>% sort()
      ),
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All species selected"
      )
    )
  })

  output$select_poison_choice <- renderUI({
    selectizeInput(
      inputId = "poison_choice", label = "Poison type",
      choices = list_poisons,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All types selected"
      )
    )
  })

  output$select_reason_choice <- renderUI({
    selectizeInput(
      inputId = "reason_choice", label = "Poison reason",
      choices = list_reason,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "All reasons selected"
      )
    )
  })

  output$valbox1 <- renderUI({
    valueBox(
      subtitle = HTML("Total <br> mortalities"),
      value = sum(wp_data$mortality),
      icon = icon("database", lib = "font-awesome"),
      width = NULL,
      color = "orange"
    )
  })

  output$valbox2 <- renderUI({
    valueBox(
      subtitle = HTML("Total poisoning <br> incidents"),
      value = length(unique(wp_data$global_id)),
      icon = icon("skull-crossbones", lib = "font-awesome"),
      width = NULL,
      color = "yellow"
    )
  })

  output$valbox3 <- renderUI({
    valueBox(
      subtitle = HTML("Total vulture <br> mortalities"),
      value = wp_data %>%
        filter(str_detect(vernacularname, "Vulture|Griffon")) %>%
        summarise(vult_mort_total = sum(mortality)),
      icon = icon("feather", lib = "font-awesome"),
      width = NULL,
      color = "red"
    )
  })

  ## Default values ----
  values <- reactiveValues(
    country_choice = list_countries,
    species_choice = list_species,
    poison_choice = list_poisons,
    reason_choice = list_reason,
    animal_group_choice = list_taxa,
    min_year = list_year_start,
    max_year = list_year_end
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

  observeEvent(input$animal_group_choice, {
    values$animal_group_choice <- input$animal_group_choice
  })

  observeEvent(input$year_slider, {
    values$min_year <- min(input$year_slider)
    values$max_year <- max(input$year_slider)
  })

  observeEvent(input$reset_plot, {
    values$country_choice <- list_countries
    values$species_choice <- list_species
    values$poison_choice <- list_poisons
    values$reason_choice <- list_reason
    values$animal_group_choice <- list_taxa

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
                         inputId = "animal_group_choice", label = NULL, choices = list_taxa,
                         selected = NULL, options = list(
                           placeholder = "All types selected"
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
                        "Animal group" = "taxa",
                        "Species" = "vernacularname",
                        "Poison type" = "poison_family",
                        "Poison reason" = "poison_reason"
                      ),
                      selected = "country"
    )
  })

  observeEvent(input$reset_animal_group, {
    values$animal_group_choice <- list_taxa

    updateSelectizeInput(session,
                         inputId = "animal_group_choice", label = NULL, choices = list_taxa,
                         selected = NULL, options = list(
                           placeholder = "All types selected"
                         )
    )
  })

  observeEvent(input$reset_poison_type, {
    values$poison_choice <- list_poisons

    updateSelectizeInput(session,
                         inputId = "poison_choice", label = NULL, choices = list_poisons,
                         selected = NULL, options = list(
                           placeholder = "All types selected"
                         )
    )
  })

  observeEvent(input$reset_poison_reason, {
    values$reason_choice <- list_reason

    updateSelectizeInput(session,
                         inputId = "reason_choice", label = NULL, choices = list_reason,
                         selected = NULL, options = list(
                           placeholder = "All reasons selected"
                         )
    )
  })

  observeEvent(input$reset_species, {
    values$species_choice <- list_species

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
  })

  observeEvent(input$reset_country, {
    values$country_choice <- list_countries
    updateSelectizeInput(session,
                         inputId = "country_choice", label = NULL, choices = list_countries,
                         selected = NULL, options = list(
                           placeholder = "All countries selected"
                         )
    )
  })


  ## Data frame query ----
  query_data <- reactive({
    wp_data %>%
      mutate(taxa = case_when(
        is.na(taxa) ~ "Unknown",
        TRUE ~ as.character(taxa)
      )) %>%
      filter(if (input$gltca_choice == "GLTFCA") tag == "GLTFCA" else tag %in% c("ALL", "GLTFCA", "Lowveld")) %>% # FINE
      filter(country %in% values$country_choice) %>% # FINE
      filter(vernacularname %in% values$species_choice) %>% # FINE
      filter(poison_family %in% values$poison_choice) %>% # FINE
      filter(poison_reason %in% values$reason_choice) %>% # FINE
      filter(taxa %in% values$animal_group_choice) %>% # FINE
      filter(year %in% c(values$min_year:values$max_year)) %>% # FINE
      group_by(.data[[input$baseplot]]) %>%
      summarise(
        total_mort = sum(mortality),
        total_incidents = length(unique(global_id))
      )
  })

  ylab_plot <- reactive({
    ifelse(input$y_choice == "total_mort", "Mortalities", "Incidents")
  })

  ## Render country plot ----
  output$plot <- renderPlot(
    {
      if (input$baseplot == "country") {
        plot_country(query_data(), .data[[input$y_choice]], ylab_plot())
      } else if (input$baseplot == "poison_family") {
        plot_poison(query_data(), .data[[input$y_choice]], ylab_plot())
      } else if (input$baseplot == "year") {
        plot_year(query_data(), .data[[input$y_choice]], ylab_plot())
      } else if (input$baseplot == "poison_reason") {
        plot_reason(query_data(), .data[[input$y_choice]], ylab_plot())
      } else if (input$baseplot == "taxa") {
        plot_animal(query_data(), .data[[input$y_choice]], ylab_plot())
      } else if (input$baseplot == "vernacularname") {
        plot_species(query_data(), .data[[input$y_choice]], ylab_plot())
      }
    },
    height = 580,
    width = 800
  )

  hide(id = "loading-content1", anim = TRUE, animType = "fade", time = 3)
  hide(id = "loading-content2", anim = TRUE, animType = "fade", time = 3)

  ## Download country plot ----
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("AWPD plot ", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file,
             plot = if (input$baseplot == "country") {
               plot_country(query_data(), .data[[input$y_choice]], ylab_plot())
             } else if (input$baseplot == "poison_family") {
               plot_poison(query_data(), .data[[input$y_choice]], ylab_plot())
             } else if (input$baseplot == "year") {
               plot_year(query_data(), .data[[input$y_choice]], ylab_plot())
             } else if (input$baseplot == "poison_reason") {
               plot_reason(query_data(), .data[[input$y_choice]], ylab_plot())
             } else if (input$baseplot == "vernacularname") {
               plot_species(query_data(), .data[[input$y_choice]], ylab_plot())
             } else if (input$baseplot == "taxa") {
               plot_animal(query_data(), .data[[input$y_choice]], ylab_plot())
             },
             width = 10, height = 8
      )
    }
  )
}
