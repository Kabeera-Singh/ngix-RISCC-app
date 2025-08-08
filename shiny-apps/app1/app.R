# ==============================================================================
# Climate-Smart Plant Selection Application
# ==============================================================================
# Purpose: Interactive tool for selecting climate-resilient native plants
# ==============================================================================

# Load Required Libraries =====================================================
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(stringr)
library(readxl)
library(shinyjs)
library(rrapply)
library(data.table)
library(bslib)
library(shinydashboard)

# Global Configuration ========================================================
print("Initializing Climate-Smart Plant Selection Application...")
print(paste("Working Directory:", getwd()))
print(paste("Available Files:", paste(list.files(), collapse = ", ")))

# Constants and Configuration
REQUIRED_FILTER_CATEGORIES <- c("Growth Habit", "Climate Status")
OPTIONAL_BONUS_CATEGORIES <- c("Hardiness Zones", "Propagation Keywords")
DEFAULT_PAGE_SIZE <- 20
MAX_MATCH_SCORE <- 5

# State Abbreviation Mapping
STATE_ABBREVIATIONS <- c(
  "New York" = "NY", "Connecticut" = "CT", "Rhode Island" = "RI",
  "Massachusetts" = "MA", "New Hampshire" = "NH", "Vermont" = "VT",
  "Maine" = "ME", "Ohio" = "OH", "West Virginia" = "WV",
  "Virginia" = "VA", "Maryland" = "MD", "New Jersey" = "NJ",
  "Delaware" = "DE", "Pennsylvania" = "PA", "North Carolina" = "NC",
  "Kentucky" = "KY"
)

# Available States (sorted for UI)
AVAILABLE_STATES <- names(STATE_ABBREVIATIONS)

# Data Loading and Preprocessing ==============================================

#' Load and validate required data files
#' @return list containing loaded datasets
load_application_data <- function() {
  cat("Loading application data...\n")
  
  # Load datasets with error handling
  tryCatch({
    hardiness_zones_by_state <- read.csv("data/state.hz.csv")
    zipcode_reference <- read.csv("data/zipcodes.csv")
    plant_database <- read.csv("data/ClimateSmart_Data_Cleaned.csv")
    
    cat("Data loading completed successfully\n")
    cat(sprintf("- Hardiness zones: %d records\n", nrow(hardiness_zones_by_state)))
    cat(sprintf("- Plant database: %d records\n", nrow(plant_database)))
    
    return(list(
      hardiness_zones = hardiness_zones_by_state,
      zipcodes = zipcode_reference,
      plants = plant_database
    ))
  }, error = function(e) {
    stop(paste("Failed to load data files:", e$message))
  })
}

#' Create filter configuration for plant characteristics
#' @return data.frame with column mappings and display names
create_filter_configuration <- function() {
  data.frame(
    column_name = c("Growth.Habit", "Sun.Level", "Moisture.Level", "Soil.Type", 
                   "Bloom.Period", "Color", "Interesting.Foliage", "Showy", 
                   "Garden.Aggressive", "Bird.Services", "Mammal.Services", 
                   "Insect.Services", "Reptile.Amphibian.Services", "Pollinators", 
                   "Climate.Status"),
    display_name = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type", 
                    "Bloom Period", "Color", "Interesting Foliage", "Showy", 
                    "Garden Aggressive", "Bird Services", "Mammal Services", 
                    "Insect Services", "Reptile/Amphibian Services", "Pollinators", 
                    "Climate Status"),
    is_filterable = rep(TRUE, 15),
    stringsAsFactors = FALSE
  )
}

#' Clean and standardize plant database
#' @param raw_plant_data Raw plant dataset
#' @param filter_config Filter configuration
#' @return Cleaned and standardized plant dataset
clean_plant_database <- function(raw_plant_data, filter_config) {
  cat("Cleaning plant database...\n")
  
  # Select and rename columns for consistency
  cleaned_plants <- raw_plant_data %>%
    select(Scientific.Name, Common.Name, Hardiness.Zone.Low, Hardiness.Zone.High,
           all_of(filter_config$column_name), Propagation.Methods, 
           Propagation.Keywords, Climate.Status) %>%
    # Remove records with missing hardiness zone information
    filter(!is.na(Hardiness.Zone.Low) & !is.na(Hardiness.Zone.High)) %>%
    # Remove duplicate scientific names
    distinct(Scientific.Name, .keep_all = TRUE) %>%
    # Standardize column names for internal consistency
    rename(
      scientific_name = Scientific.Name,
      common_name = Common.Name,
      min_hardiness_zone = Hardiness.Zone.Low,
      max_hardiness_zone = Hardiness.Zone.High,
      propagation_methods = Propagation.Methods,
      propagation_keywords = Propagation.Keywords
    ) %>%
    # Initialize match criteria score
    mutate(match_score = 0)
  
  cat(sprintf("Plant database cleaned: %d records retained\n", nrow(cleaned_plants)))
  return(cleaned_plants)
}

#' Extract and clean unique values from categorical columns
#' @param data Dataset to process
#' @param column_name Name of column to extract values from
#' @return Character vector of cleaned unique values
extract_categorical_values <- function(data, column_name) {
  if (!column_name %in% names(data)) return(character(0))
  
  # Extract all values, split by common delimiters, clean, and deduplicate
  raw_values <- data[[column_name]]
  all_values <- unlist(strsplit(paste(raw_values, collapse = ","), "[,;/]"))
  
  # Clean and filter values
  cleaned_values <- all_values %>%
    str_trim() %>%
    .[. != "" & !is.na(.) & . != "NA"] %>%
    unique() %>%
    sort()
  
  return(cleaned_values)
}

#' Create comprehensive filter options for all plant characteristics
#' @param plant_data Cleaned plant dataset
#' @param filter_config Filter configuration
#' @param hardiness_zones Available hardiness zones
#' @return data.frame with all filter categories and their options
create_filter_options <- function(plant_data, filter_config, hardiness_zones) {
  cat("Creating filter options...\n")
  
  # Initialize filter options data frame
  filter_options <- data.frame(
    column_name = character(0),
    display_name = character(0),
    available_values = character(0),
    stringsAsFactors = FALSE
  )
  
  # Process standard plant characteristics
  for (i in seq_len(nrow(filter_config))) {
    col_name <- filter_config$column_name[i]
    display_name <- filter_config$display_name[i]
    
    values <- extract_categorical_values(plant_data, col_name)
    if (length(values) > 0) {
      filter_options <- rbind(filter_options, data.frame(
        column_name = col_name,
        display_name = display_name,
        available_values = paste(values, collapse = ","),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Add hardiness zones as filterable option
  if (length(hardiness_zones) > 0) {
    filter_options <- rbind(filter_options, data.frame(
      column_name = "Hardiness.Zones",
      display_name = "Hardiness Zones",
      available_values = paste(sort(hardiness_zones), collapse = ","),
      stringsAsFactors = FALSE
    ))
  }
  
  # Add propagation keywords
  propagation_values <- extract_categorical_values(plant_data, "propagation_keywords")
  if (length(propagation_values) > 0) {
    filter_options <- rbind(filter_options, data.frame(
      column_name = "propagation_keywords",
      display_name = "Propagation Keywords",
      available_values = paste(propagation_values, collapse = ","),
      stringsAsFactors = FALSE
    ))
  }
  
  # Reorder: Growth Habit first, then Hardiness Zones, Climate Status, then others
  priority_order <- c("Growth.Habit", "Hardiness.Zones", "Climate.Status")
  priority_rows <- filter_options[filter_options$column_name %in% priority_order, ]
  other_rows <- filter_options[!filter_options$column_name %in% priority_order, ]
  
  # Sort priority rows by desired order
  priority_rows <- priority_rows[match(priority_order, priority_rows$column_name), ]
  priority_rows <- priority_rows[!is.na(priority_rows$column_name), ]
  
  final_options <- rbind(priority_rows, other_rows)
  
  cat(sprintf("Filter options created: %d categories\n", nrow(final_options)))
  return(final_options)
}

#' Get hardiness zones for a specific state
#' @param state_name Full state name
#' @param hardiness_data Hardiness zone dataset
#' @return Numeric vector of available hardiness zones
get_state_hardiness_zones <- function(state_name, hardiness_data) {
  if (!state_name %in% names(STATE_ABBREVIATIONS)) return(numeric(0))
  
  state_abbrev <- STATE_ABBREVIATIONS[state_name]
  current_conditions <- hardiness_data[
    hardiness_data$State == state_abbrev & 
    hardiness_data$Time_Period == "CurrentConditions", 
  ]
  
  if (nrow(current_conditions) == 0) return(numeric(0))
  
  zone_min <- current_conditions$Zone.Min[1]
  zone_max <- current_conditions$Zone.Max[1]
  
  if (is.na(zone_min) || is.na(zone_max)) return(numeric(0))
  
  return(zone_min:zone_max)
}

#' Get all possible hardiness zones from available states
#' @param hardiness_data Hardiness zone dataset
#' @return Sorted numeric vector of all hardiness zones
get_all_hardiness_zones <- function(hardiness_data) {
  all_zones <- numeric(0)
  
  for (state in AVAILABLE_STATES) {
    state_zones <- get_state_hardiness_zones(state, hardiness_data)
    all_zones <- c(all_zones, state_zones)
  }
  
  return(sort(unique(all_zones)))
}

#' Create hierarchical tree structure for filter interface
#' @param filter_options Filter options data frame
#' @return List structure for tree input widget
create_filter_tree <- function(filter_options) {
  cat("Creating filter tree structure...\n")
  
  tree_structure <- list()
  node_id_counter <- 1
  
  for (i in seq_len(nrow(filter_options))) {
    category <- filter_options$display_name[i]
    values <- strsplit(filter_options$available_values[i], ",")[[1]]
    values <- str_trim(values)
    values <- values[values != "" & !is.na(values)]
    
    if (length(values) == 0) next
    
    # Create category node
    category_node <- list(
      text = category,
      id = paste0("category_", node_id_counter),
      children = list()
    )
    node_id_counter <- node_id_counter + 1
    
    # Add child nodes for each value
    for (value in values) {
      child_node <- list(
        text = value,
        id = paste0("item_", node_id_counter)
      )
      category_node$children <- append(category_node$children, list(child_node))
      node_id_counter <- node_id_counter + 1
    }
    
    tree_structure <- append(tree_structure, list(category_node))
  }
  
  cat(sprintf("Filter tree created with %d categories\n", length(tree_structure)))
  return(tree_structure)
}

#' Create mapping between tree node IDs and their corresponding values
#' @param tree_structure Tree structure from create_filter_tree
#' @return data.frame mapping node IDs to categories and values
create_tree_mapping <- function(tree_structure) {
  mapping <- data.frame(
    node_id = character(0),
    category = character(0),
    value = character(0),
    stringsAsFactors = FALSE
  )
  
  for (category_node in tree_structure) {
    category_name <- category_node$text
    
    if (!is.null(category_node$children)) {
      for (child_node in category_node$children) {
        mapping <- rbind(mapping, data.frame(
          node_id = child_node$id,
          category = category_name,
          value = child_node$text,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(mapping)
}

# Initialize Application Data =================================================
cat("Initializing application data...\n")

# Load all required data
app_data <- load_application_data()
hardiness_zones_data <- app_data$hardiness_zones
plant_data_raw <- app_data$plants

# Add state full names to hardiness zones data
hardiness_zones_data$state_full_name <- names(STATE_ABBREVIATIONS)[
  match(hardiness_zones_data$State, STATE_ABBREVIATIONS)
]

# Create filter configuration and clean plant data
filter_configuration <- create_filter_configuration()
cleaned_plant_data <- clean_plant_database(plant_data_raw, filter_configuration)

# Get all available hardiness zones
available_hardiness_zones <- get_all_hardiness_zones(hardiness_zones_data)

# Create comprehensive filter options
complete_filter_options <- create_filter_options(
  cleaned_plant_data, 
  filter_configuration, 
  available_hardiness_zones
)

# Create tree structure for UI
filter_tree_structure <- create_filter_tree(complete_filter_options)
tree_node_mapping <- create_tree_mapping(filter_tree_structure)

cat("Application initialization completed successfully!\n")

# User Interface Definition ===================================================
ui <- fluidPage(
  # External resources
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css", 
      rel = "stylesheet"
    )
  ),
  
  # Enable JavaScript functionality
  useShinyjs(),
  
  # Application header
  div(class = "app-header",
    div(class = "header-container",
      h1("Climate-Smart Plant Selection"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),
  
  # Main application container
  div(class = "main-container",
    # Information card
    div(class = "info-card",
      h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
      p(paste(
        "Select your state and desired site and plant characteristics below.",
        "The tool will generate a list of native and near-native plants likely",
        "to survive both current and future climate conditions. The plant list",
        "will be sorted based on a best match with the selection criteria you input.",
        "Plants with a higher match score meet more of your selection criteria.",
        "If you select five criteria, a plant with a match score of 5 is a perfect match."
      ))
    ),
    
    # Main layout with sidebar and results panel
    div(class = "main-layout",
      # Filter sidebar
      div(class = "sidebar",
        div(class = "filter-card",
          div(class = "filter-header",
            h5(tags$i(class = "fas fa-filter"), " Filter Options")
          ),
          div(class = "filter-body",
            # State selection
            div(class = "form-group",
              tags$label(`for` = "selected_state",
                tags$i(class = "fas fa-map-marker-alt"), " Choose your state"
              ),
              selectInput(
                inputId = "selected_state",
                label = NULL,
                choices = c("Select" = "", AVAILABLE_STATES),
                width = "100%"
              )
            ),
            
            # Plant characteristics section
            div(class = "characteristics-section",
              div(class = "characteristics-header",
                h6(class = "characteristics-title",
                  tags$i(class = "fas fa-seedling"), " Plant Characteristics"
                ),
                actionButton("clear_all_filters", "Clear All", 
                           class = "btn-clear", 
                           style = "margin-left: auto;")
              ),
              
              # Hierarchical filter tree
              treeInput(
                "plant_characteristic_tree",
                label = NULL,
                choices = filter_tree_structure,
                returnValue = "id",
                closeDepth = 0  # Start with all categories collapsed
              )
            )
          )
        )
      ),
      
      # Results panel
      div(class = "main-panel",
        div(class = "results-card",
          div(class = "results-header",
            h5(tags$i(class = "fas fa-list"), " Species List"),
            div(class = "results-count",
              textOutput("plant_results_count", inline = TRUE)
            )
          ),
          
          # Results data table
          div(class = "table-container",
            DT::dataTableOutput("plant_results_table")
          )
        )
      )
    )
  ),
  
  # Application footer
  tags$footer(class = "app-footer",
    div(class = "footer-container",
      p("Climate Resilient Plants Database - Helping you choose plants for future climate conditions")
    )
  )
)

# Server Logic =================================================================
server <- function(input, output, session) {
  
  # Debugging information
  observe({
    cat(sprintf("Application Status - Plants: %d, Hardiness Zones: %d, Tree Mapping: %d\n",
               nrow(cleaned_plant_data), nrow(hardiness_zones_data), nrow(tree_node_mapping)))
  })
  
  # Clear all filters
  observeEvent(input$clear_all_filters, {
    updateTreeInput(session, "plant_characteristic_tree", selected = character(0))
  })
  
  # Main reactive function for filtered plant list
  filtered_plant_list <- reactive({
    # Require state selection
    req(input$selected_state)
    
    tryCatch({
      cat(sprintf("Filtering plants for state: %s\n", input$selected_state))
      
      # Get state-specific hardiness zone constraints for future climate conditions
      state_abbreviation <- STATE_ABBREVIATIONS[input$selected_state]
      future_climate_data <- hardiness_zones_data[
        hardiness_zones_data$State == state_abbreviation & 
        hardiness_zones_data$Time_Period == "FutureWorst", 
      ]
      
      if (nrow(future_climate_data) == 0) {
        cat(sprintf("No future climate data found for %s\n", input$selected_state))
        return(data.frame())
      }
      
      # Extract hardiness zone constraints
      state_min_zone <- future_climate_data$Zone.Min[1]
      state_max_zone <- future_climate_data$Zone.Max[1]
      
      # Filter plants by state hardiness zones (plants must survive future climate)
      climate_suitable_plants <- cleaned_plant_data[
        cleaned_plant_data$max_hardiness_zone >= state_min_zone & 
        cleaned_plant_data$min_hardiness_zone <= state_max_zone, 
      ]
      
      if (nrow(climate_suitable_plants) == 0) {
        cat("No plants suitable for future climate conditions\n")
        return(data.frame())
      }
      
      # Process user-selected filter criteria
      selected_filter_ids <- input$plant_characteristic_tree
      if (is.null(selected_filter_ids)) {
        selected_filter_ids <- character(0)
      }
      
      # Parse selected criteria into categories
      selected_criteria <- parse_selected_criteria(selected_filter_ids)
      
      # Apply required filters (Growth Habit and Climate Status)
      filtered_plants <- apply_required_filters(climate_suitable_plants, selected_criteria)
      
      if (nrow(filtered_plants) == 0) {
        cat("No plants match required criteria\n")
        return(data.frame())
      }
      
      # Apply hardiness zone filter if specified
      filtered_plants <- apply_hardiness_zone_filter(filtered_plants, selected_criteria)
      
      # Calculate match scores for optional criteria
      filtered_plants <- calculate_match_scores(filtered_plants, selected_criteria)
      
      # Prepare output data table
      results_table <- prepare_results_table(filtered_plants, selected_criteria)
      
      cat(sprintf("Filtering completed: %d plants found\n", nrow(results_table)))
      return(results_table)
      
    }, error = function(e) {
      cat(sprintf("Error in filtered_plant_list: %s\n", e$message))
      return(data.frame())
    })
  })
  
  # Parse selected filter criteria from tree input
  parse_selected_criteria <- function(selected_ids) {
    criteria_by_category <- list()
    selected_hardiness_zones <- numeric(0)
    propagation_selected <- FALSE
    
    if (length(selected_ids) > 0 && nrow(tree_node_mapping) > 0) {
      for (node_id in selected_ids) {
        matching_nodes <- tree_node_mapping[tree_node_mapping$node_id == node_id, ]
        
        if (nrow(matching_nodes) > 0) {
          node_info <- matching_nodes[1, ]
          category <- as.character(node_info$category)
          value <- as.character(node_info$value)
          
          if (category == "Hardiness Zones") {
            selected_hardiness_zones <- c(selected_hardiness_zones, as.numeric(value))
          } else if (category == "Propagation Keywords") {
            propagation_selected <- TRUE
          } else {
            if (is.null(criteria_by_category[[category]])) {
              criteria_by_category[[category]] <- character(0)
            }
            criteria_by_category[[category]] <- c(criteria_by_category[[category]], value)
          }
        }
      }
    }
    
    return(list(
      by_category = criteria_by_category,
      hardiness_zones = selected_hardiness_zones,
      propagation_selected = propagation_selected
    ))
  }
  
  # Apply required filters (Growth Habit and Climate Status)
  apply_required_filters <- function(plant_data, criteria) {
    filtered_data <- plant_data
    
    for (required_category in REQUIRED_FILTER_CATEGORIES) {
      if (required_category %in% names(criteria$by_category)) {
        category_values <- criteria$by_category[[required_category]]
        column_name <- get_column_name_for_category(required_category)
        
        if (!is.null(column_name) && column_name %in% names(filtered_data)) {
          filtered_data <- filter_by_category_values(filtered_data, column_name, category_values)
        }
      }
    }
    
    return(filtered_data)
  }
  
  # Apply hardiness zone filtering if selected
  apply_hardiness_zone_filter <- function(plant_data, criteria) {
    if (length(criteria$hardiness_zones) > 0) {
      min_selected_zone <- min(criteria$hardiness_zones)
      max_selected_zone <- max(criteria$hardiness_zones)
      
      # Filter to plants that can survive in the selected zone range
      plant_data <- plant_data[
        plant_data$min_hardiness_zone <= max_selected_zone & 
        plant_data$max_hardiness_zone >= min_selected_zone, 
      ]
    }
    
    return(plant_data)
  }
  
  # Calculate match scores for optional criteria
  calculate_match_scores <- function(plant_data, criteria) {
    plant_data$match_score <- 0
    
    # Score optional plant characteristics
    optional_categories <- names(criteria$by_category)[
      !names(criteria$by_category) %in% REQUIRED_FILTER_CATEGORIES
    ]
    
    for (category in optional_categories) {
      category_values <- criteria$by_category[[category]]
      column_name <- get_column_name_for_category(category)
      
      if (!is.null(column_name) && column_name %in% names(plant_data)) {
        matches <- check_category_matches(plant_data, column_name, category_values)
        plant_data$match_score[matches] <- plant_data$match_score[matches] + 1
      }
    }
    
    # Bonus points for special criteria
    if (length(criteria$hardiness_zones) > 0) {
      plant_data$match_score <- plant_data$match_score + 1
    }
    
    if (criteria$propagation_selected) {
      plant_data$match_score <- plant_data$match_score + 1
    }
    
    return(plant_data)
  }
  
  # Get database column name for display category
  get_column_name_for_category <- function(category) {
    if (category == "Growth Habit") return("Growth.Habit")
    if (category == "Climate Status") return("Climate.Status")
    
    # Look up in filter configuration
    matching_config <- filter_configuration[filter_configuration$display_name == category, ]
    if (nrow(matching_config) > 0) {
      return(matching_config$column_name[1])
    }
    
    return(NULL)
  }
  
  # Filter plants by category values using optimized string matching
  filter_by_category_values <- function(plant_data, column_name, target_values) {
    if (length(target_values) == 0 || !column_name %in% names(plant_data)) {
      return(plant_data)
    }
    
    category_matches <- rep(FALSE, nrow(plant_data))
    
    for (target_value in target_values) {
      matches <- check_category_matches(plant_data, column_name, target_value)
      category_matches <- category_matches | matches
    }
    
    return(plant_data[category_matches, ])
  }
  
  # Check for matches in a category column using efficient string matching
  check_category_matches <- function(plant_data, column_name, target_values) {
    if (!column_name %in% names(plant_data)) {
      return(rep(FALSE, nrow(plant_data)))
    }
    
    column_values <- as.character(plant_data[[column_name]])
    column_values[is.na(column_values)] <- ""
    
    all_matches <- rep(FALSE, length(column_values))
    
    for (target_value in target_values) {
      if (is.na(target_value) || target_value == "") next
      
      # Escape special regex characters for exact matching
      escaped_target <- gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", target_value)
      
      # Try exact word boundary matching first
      exact_matches <- grepl(paste0("\\b", escaped_target, "\\b"), 
                           column_values, ignore.case = TRUE)
      
      # If no exact matches, try substring matching
      if (!any(exact_matches, na.rm = TRUE)) {
        substring_matches <- grepl(escaped_target, column_values, ignore.case = TRUE)
        all_matches <- all_matches | substring_matches
      } else {
        all_matches <- all_matches | exact_matches
      }
    }
    
    return(all_matches)
  }
  
  # Prepare final results table for display
  prepare_results_table <- function(plant_data, criteria) {
    # Start with basic columns
    results <- data.frame(
      "Match Score" = plant_data$match_score,
      "Scientific Name" = plant_data$scientific_name,
      "Common Name" = plant_data$common_name,
      "Min Zone" = plant_data$min_hardiness_zone,
      "Max Zone" = plant_data$max_hardiness_zone,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Add columns for selected characteristics
    selected_categories <- names(criteria$by_category)
    
    # Always include Growth Habit and Climate Status if they exist
    priority_categories <- c("Growth Habit", "Climate Status")
    for (category in priority_categories) {
      column_name <- get_column_name_for_category(category)
      if (!is.null(column_name) && column_name %in% names(plant_data)) {
        results[[category]] <- plant_data[[column_name]]
      }
    }
    
    # Add other selected characteristic columns
    other_categories <- selected_categories[!selected_categories %in% priority_categories]
    for (category in other_categories) {
      column_name <- get_column_name_for_category(category)
      if (!is.null(column_name) && column_name %in% names(plant_data)) {
        results[[category]] <- plant_data[[column_name]]
      }
    }
    
    # Add propagation information if selected
    if (criteria$propagation_selected && "propagation_methods" %in% names(plant_data)) {
      results[["Propagation Description"]] <- plant_data$propagation_methods
    }
    
    # Sort by match score (highest first), then by scientific name
    results <- results[order(-results$`Match Score`, results$`Scientific Name`), ]
    
    return(results)
  }
  
  # Results count output
  output$plant_results_count <- renderText({
    results <- filtered_plant_list()
    if (is.null(results) || nrow(results) == 0) {
      return("0 plants found")
    }
    return(paste(nrow(results), "plants found"))
  })
  
  # Main data table output with enhanced formatting
  output$plant_results_table <- DT::renderDataTable({
    tryCatch({
      results <- filtered_plant_list()
      
      # Handle empty results
      if (is.null(results) || nrow(results) == 0) {
        empty_message <- data.frame(
          "Message" = "No plants match your criteria. Try adjusting your filters or selecting a different state."
        )
        return(datatable(
          empty_message, 
          options = list(dom = 't', searching = FALSE),
          rownames = FALSE, 
          colnames = ""
        ))
      }
      
      # Create enhanced data table with improved performance
      datatable(
        results,
        extensions = c('Buttons'),
        options = list(
          # Basic table configuration
          dom = 'Bfrtip',
          pageLength = DEFAULT_PAGE_SIZE,
          
          # Scrolling configuration (optimized)
          scrollX = TRUE,
          scrollY = FALSE,
          
          # Column definitions for better formatting
          columnDefs = list(
            list(targets = 3, width = "80px", className = "dt-center"),   # Min Zone
            list(targets = 4, width = "80px", className = "dt-center"),   # Max Zone
            list(targets = "_all", width = "150px")                       # All other columns
          ),
          
          # Export functionality
          buttons = list(
            list(extend = 'csv', text = 'Download CSV'),
            list(extend = 'excel', text = 'Download Excel'),
            list(extend = 'pdf', text = 'Download PDF')
          ),
          
          # Table behavior settings
          responsive = FALSE,
          autoWidth = FALSE,
          searching = TRUE,
          lengthChange = TRUE,
          info = FALSE,
          paging = TRUE,
          
          # Default sorting: Match Score descending, then Scientific Name ascending
          order = list(list(0, 'desc'), list(1, 'asc')),
          
          # Performance optimizations
          deferRender = TRUE,
          processing = TRUE
        ),
        rownames = FALSE,
        class = 'table-hover table-striped compact',
        style = 'bootstrap4'
      ) %>%
        # Enhanced match score formatting with color gradient
        formatStyle(
          'Match Score',
          backgroundColor = styleInterval(
            cuts = seq(1, MAX_MATCH_SCORE),
            values = c('#ffffff', '#e8f5e9', '#c8e6c9', '#a5d6a7', '#81c784', '#66bb6a')
          ),
          fontWeight = 'bold',
          textAlign = 'center'
        ) %>%
        # Format hardiness zone columns
        formatStyle(
          c('Min Zone', 'Max Zone'),
          textAlign = 'center',
          fontWeight = '500'
        )
        
    }, error = function(e) {
      cat(sprintf("Error in renderDataTable: %s\n", e$message))
      error_message <- data.frame(
        "Error" = paste("Error loading data:", e$message)
      )
      return(datatable(
        error_message, 
        options = list(dom = 't', searching = FALSE),
        rownames = FALSE, 
        colnames = ""
      ))
    })
  })
}

# Application Execution ========================================================
#' Launch the Climate-Smart Plant Selection Application
#' @return Shiny application object
shinyApp(ui = ui, server = server)