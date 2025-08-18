# ==============================================================================
# Climate-Smart Plant Selection Application
# ==============================================================================

# Load Required Libraries =====================================================
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(data.table)

# Global Configuration ========================================================
print("Initializing Climate-Smart Plant Selection Application...")

# Constants
REQUIRED_FILTER_CATEGORIES <- c("Growth Habit", "Climate Status")
DEFAULT_PAGE_SIZE <- 20
MAX_MATCH_SCORE <- 5
DEFAULT_COLUMNS <- c("Sun Level", "Moisture Level")

STATE_ABBREVIATIONS <- c(
  "New York" = "NY", "Connecticut" = "CT", "Rhode Island" = "RI",
  "Massachusetts" = "MA", "New Hampshire" = "NH", "Vermont" = "VT",
  "Maine" = "ME", "Ohio" = "OH", "West Virginia" = "WV",
  "Virginia" = "VA", "Maryland" = "MD", "New Jersey" = "NJ",
  "Delaware" = "DE", "Pennsylvania" = "PA", "North Carolina" = "NC",
  "Kentucky" = "KY"
)

AVAILABLE_STATES <- names(STATE_ABBREVIATIONS)

# Data Loading and Preprocessing ==============================================
load_application_data <- function() {
  cat("Loading application data...\n")
  tryCatch({
    list(
      hardiness_zones = read.csv("data/state.hz.csv"),
      zipcodes = read.csv("data/zipcodes.csv"),
      plants = read.csv("data/ClimateSmart_Data.csv")
    )
  }, error = function(e) stop(paste("Failed to load data files:", e$message)))
}

create_filter_configuration <- function() {
  data.frame(
    column_name = c("Growth.Habit", "Sun.Level", "Moisture.Level", "Soil.Type", 
                   "Bloom.Period", "Color", "Interesting.Foliage", "Showy", 
                   "Garden.Aggressive",  "Wildlife.Services", "Pollinators", 
                   "Climate.Status"),
    display_name = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type", 
                    "Bloom Period", "Color", "Interesting Foliage", "Showy", 
                    "Garden Aggressive", "Wildlife Services", "Pollinators", 
                    "Climate Status"),
    stringsAsFactors = FALSE
  )
}

clean_plant_database <- function(raw_plant_data, filter_config) {
  cat("Cleaning plant database...\n")
  
  raw_plant_data %>%
    select(Scientific.Name, Common.Name, Hardiness.Zone.Low, Hardiness.Zone.High,
           all_of(filter_config$column_name), Propagation.Methods, 
           Propagation.Keywords, Climate.Status) %>%
    filter(!is.na(Hardiness.Zone.Low) & !is.na(Hardiness.Zone.High)) %>%
    distinct(Scientific.Name, .keep_all = TRUE) %>%
    rename(
      scientific_name = Scientific.Name,
      common_name = Common.Name,
      min_hardiness_zone = Hardiness.Zone.Low,
      max_hardiness_zone = Hardiness.Zone.High,
      propagation_methods = Propagation.Methods,
      propagation_keywords = Propagation.Keywords
    ) %>%
    mutate(match_score = 0)
}

# Unified ordering function with switch statement
order_by_preference <- function(values, preferred_order) {
  matched_values <- preferred_order[preferred_order %in% values]
  unmatched_values <- sort(values[!values %in% preferred_order])
  c(matched_values, unmatched_values)
}

apply_custom_ordering <- function(values, category) {
  if (length(values) == 0) return(values)
  
  clean_values <- unique(values[!is.na(values) & values != "" & values != "NA"])
  if (length(clean_values) == 0) return(character(0))
  
  preferred_order <- switch(category,
    "Growth Habit" = c("Annual", "Perennial Herb", "Grass", "Fern", "Vine", "Shrub", "Tree", "Other"),
    "Sun Level" = c("Full Sun", "Part Shade", "Full Shade", "Not Specified"),
    "Moisture Level" = c("Dry", "Medium", "Moist", "Wet", "Not Specified"),
    "Soil Type" = c("Sandy", "Loam", "Clay", "Well-drained", "Moist", "Other", "Not Specified"),
    "Bloom Period" = c("January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December",
                      "Indeterminate", "Non-flowering", "Rarely Flowers"),
    "Color" = c("Red", "Orange", "Yellow", "Green", "Blue", "Purple", "Violet", 
               "Pink", "Brown", "White", "Not Applicable"),
    "Wildlife Services" = c("Birds", "Mammals", "Reptiles", "Amphibians", "Insects", "None"),
    # Default ordering
    {
      special_endings <- c("Other", "None", "Not Specified", "Not Applicable", "Indeterminate")
      regular_values <- sort(clean_values[!clean_values %in% special_endings])
      special_values <- clean_values[clean_values %in% special_endings]
      special_sorted <- special_values[order(match(special_values, special_endings))]
      return(c(regular_values, special_sorted))
    }
  )
  
  order_by_preference(clean_values, preferred_order)
}

extract_categorical_values <- function(data, column_name) {
  if (!column_name %in% names(data)) return(character(0))
  
  all_values <- unlist(strsplit(paste(data[[column_name]], collapse = ","), "[,;/]"))
  cleaned_values <- all_values %>%
    str_trim() %>%
    .[. != "" & !is.na(.) & . != "NA"] %>%
    unique()
  
  category <- get_display_name_for_column(column_name)
  apply_custom_ordering(cleaned_values, category)
}

get_display_name_for_column <- function(column_name) {
  display_mapping <- c(
    "Growth.Habit" = "Growth Habit", "Sun.Level" = "Sun Level",
    "Moisture.Level" = "Moisture Level", "Soil.Type" = "Soil Type",
    "Bloom.Period" = "Bloom Period", "Color" = "Color",
    "Interesting.Foliage" = "Interesting Foliage", "Showy" = "Showy",
    "Garden.Aggressive" = "Garden Aggressive", "Wildlife.Services" = "Wildlife Services",
    "Pollinators" = "Pollinators", "Climate.Status" = "Climate Status",
    "propagation_keywords" = "Propagation Keywords"
  )
  
  ifelse(column_name %in% names(display_mapping), display_mapping[column_name], column_name)
}

create_filter_options <- function(plant_data, filter_config) {
  cat("Creating filter options...\n")
  
  filter_options <- data.frame(
    column_name = character(0),
    display_name = character(0),
    available_values = character(0),
    stringsAsFactors = FALSE
  )
  
  # Process all columns
  all_columns <- c(filter_config$column_name, "propagation_keywords")
  all_display_names <- c(filter_config$display_name, "Propagation Keywords")
  
  for (i in seq_along(all_columns)) {
    values <- extract_categorical_values(plant_data, all_columns[i])
    if (length(values) > 0) {
      filter_options <- rbind(filter_options, data.frame(
        column_name = all_columns[i],
        display_name = all_display_names[i],
        available_values = paste(values, collapse = ","),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Reorder with priorities first
  priority_order <- c("Growth.Habit", "Climate.Status")
  priority_rows <- filter_options[filter_options$column_name %in% priority_order, ]
  other_rows <- filter_options[!filter_options$column_name %in% priority_order, ]
  
  priority_rows <- priority_rows[match(priority_order, priority_rows$column_name), ]
  priority_rows <- priority_rows[!is.na(priority_rows$column_name), ]
  
  rbind(priority_rows, other_rows)
}

create_filter_tree <- function(filter_options) {
  cat("Creating filter tree structure...\n")
  
  tree_structure <- list()
  node_id_counter <- 1
  
  for (i in seq_len(nrow(filter_options))) {
    category <- filter_options$display_name[i]
    values <- str_trim(strsplit(filter_options$available_values[i], ",")[[1]])
    values <- values[values != "" & !is.na(values)]
    
    if (length(values) == 0) next
    
    category_node <- list(
      text = category,
      id = paste0("category_", node_id_counter),
      children = lapply(values, function(value) {
        node_id_counter <<- node_id_counter + 1
        list(text = value, id = paste0("item_", node_id_counter))
      })
    )
    node_id_counter <- node_id_counter + 1
    tree_structure <- append(tree_structure, list(category_node))
  }
  
  tree_structure
}

create_tree_mapping <- function(tree_structure) {
  mapping <- data.frame(
    node_id = character(0),
    category = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
  
  for (category_node in tree_structure) {
    if (!is.null(category_node$children)) {
      for (child_node in category_node$children) {
        mapping <- rbind(mapping, data.frame(
          node_id = child_node$id,
          category = category_node$text,
          value = child_node$text,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  mapping
}

# Initialize Application Data =================================================
app_data <- load_application_data()
hardiness_zones_data <- app_data$hardiness_zones
plant_data_raw <- app_data$plants

hardiness_zones_data$state_full_name <- names(STATE_ABBREVIATIONS)[
  match(hardiness_zones_data$State, STATE_ABBREVIATIONS)
]

filter_configuration <- create_filter_configuration()
cleaned_plant_data <- clean_plant_database(plant_data_raw, filter_configuration)
complete_filter_options <- create_filter_options(cleaned_plant_data, filter_configuration)
filter_tree_structure <- create_filter_tree(complete_filter_options)
tree_node_mapping <- create_tree_mapping(filter_tree_structure)

cat("Application initialization completed successfully!\n")

# User Interface Definition ===================================================
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css", 
      rel = "stylesheet"
    )
  ),
  
  useShinyjs(),
  
  div(class = "app-header",
    div(class = "header-container",
      h1("Climate-Smart Plant Selection"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),
  
  div(class = "main-container",
    div(class = "info-card",
      h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
      p(paste(
        "Select your state and desired site and plant characteristics below.",
        "The tool will generate a list of native and near-native plants likely",
        "to survive both current and future climate conditions."
      ))
    ),
    
    div(class = "main-layout",
      div(class = "sidebar",
        div(class = "filter-card",
          div(class = "filter-header",
            h5(tags$i(class = "fas fa-filter"), " Filter Options")
          ),
          div(class = "filter-body",
            div(class = "form-group",
              tags$label(`for` = "selected_state",
                tags$i(class = "fas fa-map-marker-alt"), " Choose your state"
              ),
              selectInput("selected_state", label = NULL,
                choices = c(selected = NULL, AVAILABLE_STATES), width = "100%")
            ),
            
            div(class = "form-group",
              tags$label(`for` = "hardiness_zone_range",
                tags$i(class = "fas fa-thermometer-half"), " Hardiness Zone Range"
              ),
              noUiSliderInput("hardiness_zone_range", label = NULL,
                min = 1, max = 12, value = c(1, 12), connect = TRUE, step = 1,
                tooltips = TRUE, format = wNumbFormat(decimals = 0), width = "95%")
            ),
            
            div(class = "characteristics-section",
              div(class = "characteristics-header",
                h6(class = "characteristics-title",
                  tags$i(class = "fas fa-seedling"), " Plant Characteristics"
                ),
                actionButton("clear_all_filters", "Clear All", 
                           class = "btn-clear", style = "margin-left: auto;")
              ),
              
              treeInput("plant_characteristic_tree", label = NULL,
                choices = filter_tree_structure, returnValue = "id", closeDepth = 0)
            )
          )
        )
      ),
      
      div(class = "main-panel",
        div(class = "results-card",
          div(class = "results-header",
            h5(tags$i(class = "fas fa-list"), " Species List"),
            div(class = "results-count",
              textOutput("plant_results_count", inline = TRUE)
            )
          ),
          
          div(class = "table-container",
            DT::dataTableOutput("plant_results_table")
          )
        )
      )
    )
  ),
  
  tags$footer(class = "app-footer",
    div(class = "footer-container",
      p("Climate Resilient Plants Database")
    )
  )
)

# Server Logic =================================================================
server <- function(input, output, session) {
  
  observeEvent(input$clear_all_filters, {
    tryCatch({
      updateTreeInput(session = session, inputId = "plant_characteristic_tree", 
                     selected = character(0))
      updateNoUiSliderInput(session = session, inputId = "hardiness_zone_range", 
                           value = c(1, 12))
    }, error = function(e) cat(sprintf("Error clearing filters: %s\n", e$message)))
  })
  
  # Unified helper functions
  parse_selected_criteria <- function(selected_ids) {
    criteria_by_category <- list()
    propagation_selected <- FALSE
    
    if (length(selected_ids) > 0 && nrow(tree_node_mapping) > 0) {
      for (node_id in selected_ids) {
        matching_nodes <- tree_node_mapping[tree_node_mapping$node_id == node_id, ]
        
        if (nrow(matching_nodes) > 0) {
          node_info <- matching_nodes[1, ]
          category <- as.character(node_info$category)
          value <- as.character(node_info$value)
          
          if (category == "Propagation Keywords") {
            propagation_selected <- TRUE
          } else {
            criteria_by_category[[category]] <- c(criteria_by_category[[category]], value)
          }
        }
      }
    }
    
    list(by_category = criteria_by_category, propagation_selected = propagation_selected)
  }
  
  get_column_name_for_category <- function(category) {
    column_mapping <- c("Growth Habit" = "Growth.Habit", "Climate Status" = "Climate.Status")
    if (category %in% names(column_mapping)) return(column_mapping[category])
    
    matching_config <- filter_configuration[filter_configuration$display_name == category, ]
    if (nrow(matching_config) > 0) return(matching_config$column_name[1])
    NULL
  }
  
  check_category_matches <- function(plant_data, column_name, target_values) {
    if (!column_name %in% names(plant_data)) return(rep(FALSE, nrow(plant_data)))
    
    column_values <- as.character(plant_data[[column_name]])
    column_values[is.na(column_values)] <- ""
    
    all_matches <- rep(FALSE, length(column_values))
    
    for (target_value in target_values) {
      if (is.na(target_value) || target_value == "") next
      
      escaped_target <- gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", target_value)
      exact_matches <- grepl(paste0("\\b", escaped_target, "\\b"), 
                           column_values, ignore.case = TRUE)
      
      if (!any(exact_matches, na.rm = TRUE)) {
        substring_matches <- grepl(escaped_target, column_values, ignore.case = TRUE)
        all_matches <- all_matches | substring_matches
      } else {
        all_matches <- all_matches | exact_matches
      }
    }
    
    all_matches
  }
  
  filter_by_category_values <- function(plant_data, column_name, target_values) {
    if (length(target_values) == 0 || !column_name %in% names(plant_data)) {
      return(plant_data)
    }
    
    category_matches <- rep(FALSE, nrow(plant_data))
    for (target_value in target_values) {
      matches <- check_category_matches(plant_data, column_name, target_value)
      category_matches <- category_matches | matches
    }
    
    plant_data[category_matches, ]
  }
  
  filtered_plant_list <- reactive({
    req(input$selected_state)
    
    tryCatch({
      # Get climate data
      state_abbreviation <- STATE_ABBREVIATIONS[input$selected_state]
      future_climate_data <- hardiness_zones_data[
        hardiness_zones_data$State == state_abbreviation & 
        hardiness_zones_data$Time_Period == "FutureWorst", 
      ]
      
      if (nrow(future_climate_data) == 0) return(data.frame())
      
      state_min_zone <- future_climate_data$Zone.Min[1]
      state_max_zone <- future_climate_data$Zone.Max[1]
      
      # Filter by climate suitability
      climate_suitable_plants <- cleaned_plant_data[
        cleaned_plant_data$max_hardiness_zone >= state_min_zone & 
        cleaned_plant_data$min_hardiness_zone <= state_max_zone, 
      ]
      
      if (nrow(climate_suitable_plants) == 0) return(data.frame())
      
      # Parse criteria
      selected_filter_ids <- input$plant_characteristic_tree
      if (is.null(selected_filter_ids)) selected_filter_ids <- character(0)
      selected_criteria <- parse_selected_criteria(selected_filter_ids)
      
      # Apply required filters
      filtered_plants <- climate_suitable_plants
      for (required_category in REQUIRED_FILTER_CATEGORIES) {
        if (required_category %in% names(selected_criteria$by_category)) {
          category_values <- selected_criteria$by_category[[required_category]]
          column_name <- get_column_name_for_category(required_category)
          
          if (!is.null(column_name) && column_name %in% names(filtered_plants)) {
            filtered_plants <- filter_by_category_values(filtered_plants, column_name, category_values)
          }
        }
      }
      
      if (nrow(filtered_plants) == 0) return(data.frame())
      
      # Apply hardiness zone range filter
      zone_range <- input$hardiness_zone_range
      if (!is.null(zone_range) && length(zone_range) == 2) {
        if (zone_range[1] != 1 || zone_range[2] != 12) {
          filtered_plants <- filtered_plants[
            filtered_plants$min_hardiness_zone <= zone_range[2] & 
            filtered_plants$max_hardiness_zone >= zone_range[1], 
          ]
        }
      }
      
      # Calculate match scores
      filtered_plants$match_score <- 0
      
      optional_categories <- names(selected_criteria$by_category)[
        !names(selected_criteria$by_category) %in% REQUIRED_FILTER_CATEGORIES
      ]
      
      for (category in optional_categories) {
        category_values <- selected_criteria$by_category[[category]]
        column_name <- get_column_name_for_category(category)
        
        if (!is.null(column_name) && column_name %in% names(filtered_plants)) {
          matches <- check_category_matches(filtered_plants, column_name, category_values)
          filtered_plants$match_score[matches] <- filtered_plants$match_score[matches] + 1
        }
      }
      
      # Bonus points
      if (!is.null(zone_range) && length(zone_range) == 2 && 
          (zone_range[1] != 1 || zone_range[2] != 12)) {
        filtered_plants$match_score <- filtered_plants$match_score + 1
      }
      
      if (selected_criteria$propagation_selected) {
        filtered_plants$match_score <- filtered_plants$match_score + 1
      }
      
      # Prepare results table
      results <- data.frame(
        "Match Score" = filtered_plants$match_score,
        "Scientific Name" = filtered_plants$scientific_name,
        "Common Name" = filtered_plants$common_name,
        "Min Zone" = filtered_plants$min_hardiness_zone,
        "Max Zone" = filtered_plants$max_hardiness_zone,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Add selected characteristic columns
      selected_categories <- names(selected_criteria$by_category)
      priority_categories <- c("Growth Habit", "Climate Status")
      
      for (category in c(priority_categories, 
                        selected_categories[!selected_categories %in% priority_categories],
                        DEFAULT_COLUMNS)) {
        column_name <- get_column_name_for_category(category)
        if (!is.null(column_name) && column_name %in% names(filtered_plants) && 
            !category %in% names(results)) {
          results[[category]] <- filtered_plants[[column_name]]
        }
      }
      
      # Add propagation if selected
      if (selected_criteria$propagation_selected && "propagation_methods" %in% names(filtered_plants)) {
        results[["Propagation Description"]] <- filtered_plants$propagation_methods
      }
      
      # Sort results
      results[order(-results$`Match Score`, results$`Scientific Name`), ]
      
    }, error = function(e) {
      cat(sprintf("Error in filtered_plant_list: %s\n", e$message))
      data.frame()
    })
  })
  
  output$plant_results_count <- renderText({
    results <- filtered_plant_list()
    if (is.null(results) || nrow(results) == 0) {
      return("0 plants found")
    }
    paste(nrow(results), "plants found")
  })
  
  output$plant_results_table <- DT::renderDataTable({
    tryCatch({
      results <- filtered_plant_list()
      
      if (is.null(results) || nrow(results) == 0) {
        return(datatable(
          data.frame("Message" = "No plants match your criteria. Try adjusting your filters."), 
          options = list(dom = 't', searching = FALSE),
          rownames = FALSE, colnames = ""
        ))
      }
      
      datatable(
        results,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          pageLength = DEFAULT_PAGE_SIZE,
          scrollX = TRUE,
          columnDefs = list(
            list(targets = 3, width = "80px", className = "dt-center"),
            list(targets = 4, width = "80px", className = "dt-center"),
            list(targets = "_all", width = "150px")
          ),
          buttons = list(
            list(extend = 'csv', text = 'Download CSV'),
            list(extend = 'excel', text = 'Download Excel'),
            list(extend = 'pdf', text = 'Download PDF')
          ),
          responsive = FALSE,
          autoWidth = FALSE,
          searching = TRUE,
          lengthChange = TRUE,
          info = FALSE,
          paging = FALSE,
          order = list(list(0, 'desc'), list(1, 'asc')),
          deferRender = TRUE,
          processing = TRUE
        ),
        rownames = FALSE,
        class = 'table-hover table-striped compact',
        style = 'bootstrap4'
      ) %>%
        formatStyle(
          'Match Score',
          backgroundColor = styleInterval(
            cuts = seq(1, MAX_MATCH_SCORE),
            values = c('#ffffff', '#e8f5e9', '#c8e6c9', '#a5d6a7', '#81c784', '#66bb6a')
          ),
          fontWeight = 'bold',
          textAlign = 'center'
        ) %>%
        formatStyle(
          c('Min Zone', 'Max Zone'),
          textAlign = 'center',
          fontWeight = '500'
        )
        
    }, error = function(e) {
      datatable(
        data.frame("Error" = paste("Error loading data:", e$message)), 
        options = list(dom = 't', searching = FALSE),
        rownames = FALSE, colnames = ""
      )
    })
  })
}

# Application Execution ========================================================
shinyApp(ui = ui, server = server)