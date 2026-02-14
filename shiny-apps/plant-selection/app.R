# ==============================================================================
# Climate-Smart Plant Selection Application - State Filter Version
# ==============================================================================

# Load Required Libraries =====================================================
# These libraries are essential for the application's functionality:
# - tidyverse: Data manipulation and visualization
# - shiny: Web application framework
# - shinyWidgets: Enhanced UI widgets (e.g., treeInput, noUiSliderInput)
# - DT: Interactive data tables
# - shinyjs: JavaScript operations in Shiny
# - data.table: Efficient data handling
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(data.table)

# Global Configuration ========================================================

# Constants
REQUIRED_FILTER_CATEGORIES <- c("Growth Habit", "Climate Status", "Sun Level", "Moisture Level")
DEFAULT_PAGE_SIZE <- 20
DEFAULT_COLUMNS <- c("Sun Level", "Moisture Level")

# Column Tooltips and Descriptions
COLUMN_TOOLTIPS <- list(
  "Match Score" = "The match score represents how many of your sorting criteria a given species meets. A higher match score is a better match to your preferences.",
  "Scientific Name" = "The official binomial nomenclature (genus and species) of the plant.",
  "Common Name" = "The widely recognized common or vernacular name of the plant species.",
  "Min Zone" = "The lowest USDA hardiness zone where this plant can survive winter.",
  "Max Zone" = "The highest USDA hardiness zone where this plant typically thrives.",
  "Growth Habit" = "The plant's growth form, such as annual, perennial, shrub, tree, vine, or grass.",
  "Climate Status" = "Classification indicating how well the plant adapts to climate change scenarios.",
  "Sun Level" = "Light requirements: Full Sun (6+ hours), Part Shade (3-6 hours), or Full Shade (less than 3 hours).",
  "Moisture Level" = "Water preference: Dry, Medium, Moist, or Wet soil conditions.",
  "Soil Type" = "Preferred soil characteristics: Sandy, Loam, Clay, Well-drained, or Moist.",
  "Bloom Period" = "The month or season when the plant produces flowers.",
  "Max Height" = "The maximum expected height the plant will reach at maturity.",
  "Color" = "The predominant color of the plant's flowers or foliage.",
  "Interesting Foliage" = "Indicates whether the plant has notable foliage characteristics or texture.",
  "Showy" = "Indicates whether the plant has visually striking or prominent flowers.",
  "Garden Aggressive" = "Indicates if the plant tends to self-seed or spread aggressively in gardens.",
  "Wildlife Services" = "Types of wildlife that benefit from this plant (e.g., birds, pollinators, mammals).",
  "Pollinators" = "Types of pollinators attracted to this plant (e.g., bees, butterflies, hummingbirds).",
  "Propagation Methods" = "Ways to reproduce the plant: from seed, cuttings, division, or other methods."
)

# State abbreviations mapping for data filtering
STATE_ABBREVIATIONS <- c(
  "New York" = "NY", "Connecticut" = "CT", "Rhode Island" = "RI",
  "Massachusetts" = "MA", "New Hampshire" = "NH", "Vermont" = "VT",
  "Maine" = "ME", "Ohio" = "OH", "West Virginia" = "WV",
  "Virginia" = "VA", "Maryland" = "MD", "New Jersey" = "NJ",
  "Delaware" = "DE", "Pennsylvania" = "PA", "Kentucky" = "KY"
)

AVAILABLE_STATES <- names(STATE_ABBREVIATIONS)

# Data Loading and Preprocessing ==============================================
load_application_data <- function() {
  cat("Loading application data...\n")
  tryCatch({
    list(
      plants = read.csv("data/ClimateSmart_Data_Cleaned.csv")
    )
  }, error = function(e) stop(paste("Failed to load data files:", e$message)))
}

create_filter_configuration <- function() {
  data.frame(
    column_name = c("Growth.Habit", "Sun.Level", "Moisture.Level", "Soil.Type", 
                   "Bloom.Period","max_height", "Color", "Interesting.Foliage", "Showy", 
                   "Garden.Aggressive",  "Wildlife.Services", "Pollinators", 
                   "Climate.Status"),
    display_name = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type", 
                    "Bloom Period", "Max Height", "Color", "Interesting Foliage", "Showy", 
                    "Garden Aggressive", "Wildlife Services", "Pollinators", 
                    "Climate Status"),
    stringsAsFactors = FALSE
  )
}

# Clean and preprocess the raw plant database
# Parameters:
#   raw_plant_data: Raw data frame from CSV
#   filter_config: Filter configuration data frame
# Returns: Cleaned data frame with standardized column names
clean_plant_database <- function(raw_plant_data, filter_config) {
  cat("Cleaning plant database...\n")
  
  raw_plant_data %>%
    select(State, Scientific.Name, Common.Name, Hardiness.Zone.Low, Hardiness.Zone.High,
           all_of(intersect(filter_config$column_name, names(.))), Propagation.Methods, 
           Propagation.Keywords, Climate.Status) %>%
    filter(!is.na(Hardiness.Zone.Low) & !is.na(Hardiness.Zone.High)) %>%
    rename(
      state = State,
      scientific_name = Scientific.Name,
      common_name = Common.Name,
      min_hardiness_zone = Hardiness.Zone.Low,
      max_hardiness_zone = Hardiness.Zone.High,
      propagation_methods = Propagation.Methods,
      propagation_keywords = Propagation.Keywords
    ) %>%
    mutate(
      match_score = 0,
      hardiness_zone_numeric = min_hardiness_zone
    )
}

# Unified ordering function with switch statement
# Orders values based on preferred order, placing unmatched values at the end
# Parameters:
#   values: Vector of values to order
#   preferred_order: Vector of preferred order
# Returns: Ordered vector
order_by_preference <- function(values, preferred_order) {
  matched_values <- preferred_order[preferred_order %in% values]
  unmatched_values <- sort(values[!values %in% preferred_order])
  c(matched_values, unmatched_values)
}

# Apply custom ordering based on category
# Uses predefined preferred orders for different plant characteristics
# Parameters:
#   values: Vector of values to order
#   category: Category name (e.g., "Growth Habit")
# Returns: Ordered vector of unique values
apply_custom_ordering <- function(values, category) {
  if (length(values) == 0) return(values)
  
  clean_values <- unique(values[!is.na(values) & values != "" & values != "NA"])
  if (length(clean_values) == 0) return(character(0))
  
  # Special handling for Hardiness Zone - numeric ordering
  if (category == "Hardiness Zone") {
    numeric_values <- as.numeric(clean_values)
    return(as.character(sort(numeric_values)))
  }
  
  preferred_order <- switch(category,
    "Growth Habit" = c("Annual", "Perennial Herb", "Grass", "Fern", "Vine", "Shrub", "Tree", "Other"),
    "Sun Level" = c("Full Sun", "Part Shade", "Full Shade", "Not Specified"),
    "Moisture Level" = c("Dry", "Medium", "Moist", "Wet", "Not Specified"),
    "Soil Type" = c("Sandy", "Loam", "Clay", "Well-drained", "Moist", "Other", "Not Specified"),
    "Bloom Period" = c("January", "February", "March", "April", "May", "June",
                      "July", "August", "September", "October", "November", "December",
                      "Indeterminate", "Non-flowering", "Rarely Flowers", "Not Specified"),
    "Max Height" = c("Very Small (0-2 ft)", "Small (2-5 ft)", "Medium (5-10 ft)", "Large (10-20 ft)", "Very Large (20-50 ft)", "Huge (50+ ft)"),
    "Color" = c("Red", "Orange", "Yellow", "Green", "Blue", "Purple", "Violet", 
               "Pink", "Brown", "White", "Not Applicable"),
    "Wildlife Services" = c("Birds", "Mammals", "Reptiles", "Amphibians", "Insects", "Not Specified"),
    # Default ordering for unspecified categories
    {
      special_endings <- c("Other", "None", "Not Specified", "Not Applicable", "Indeterminate")
      regular_values <- sort(clean_values[!clean_values %in% special_endings])
      special_values <- clean_values[clean_values %in% special_endings]
      special_sorted <- special_values[order(match(special_values, special_endings))]
      return(c(regular_values, special_sorted))
    }
  )
  
  # For Bloom Period and Wildlife Services, only return values that are in the preferred order list
  if (category == "Bloom Period" || category == "Wildlife Services") {
    clean_values <- clean_values[clean_values %in% preferred_order]
  }
  
  order_by_preference(clean_values, preferred_order)
}

# Extract categorical values from a data column
# Splits comma-separated values and applies custom ordering
# Parameters:
#   data: Data frame
#   column_name: Name of the column to extract values from
# Returns: Ordered vector of unique values
extract_categorical_values <- function(data, column_name) {
  if (!column_name %in% names(data)) return(character(0))
  
  all_values <- unlist(strsplit(paste(data[[column_name]], collapse = ","), "[,;/]"))
  cleaned_values <- all_values %>%
    str_trim() %>%
    .[. != "" & !is.na(.)] %>%
    unique()
  
  category <- get_display_name_for_column(column_name)
  apply_custom_ordering(cleaned_values, category)
}

# Get display name for a column name
# Maps internal column names to user-friendly display names
# Parameters:
#   column_name: Internal column name
# Returns: Display name string
get_display_name_for_column <- function(column_name) {
  display_mapping <- c(
    "Growth.Habit" = "Growth Habit", "Sun.Level" = "Sun Level",
    "Moisture.Level" = "Moisture Level", "Soil.Type" = "Soil Type",
    "Bloom.Period" = "Bloom Period", "max_height" = "Max Height", "Color" = "Color",
    "Interesting.Foliage" = "Interesting Foliage", "Showy" = "Showy",
    "Garden.Aggressive" = "Garden Aggressive", "Wildlife.Services" = "Wildlife Services",
    "Pollinators" = "Pollinators", "Climate.Status" = "Climate Status",
    "propagation_keywords" = "Propagation Keywords",
    "hardiness_zone_numeric" = "Hardiness Zone"
  )
  
  ifelse(column_name %in% names(display_mapping), display_mapping[column_name], column_name)
}

create_filter_options <- function(plant_data, filter_config) {
  cat("Creating filter options...\n")
  
  filter_options <- data.frame(
    column_name = character(0),
    display_name = character(0),
    available_values = character(0),
    is_filter = logical(0),
    stringsAsFactors = FALSE
  )
  
  # Process all columns including hardiness zone
  all_columns <- c(filter_config$column_name, "propagation_keywords", "hardiness_zone_numeric")
  all_display_names <- c(filter_config$display_name, "Propagation Keywords", "Hardiness Zone")
  
  for (i in seq_along(all_columns)) {
    # Special handling for hardiness zone
    if (all_columns[i] == "hardiness_zone_numeric") {
      values <- as.character(1:12)
    } else {
      values <- extract_categorical_values(plant_data, all_columns[i])
    }
    
    if (length(values) > 0) {
      is_filter_column <- all_display_names[i] %in% REQUIRED_FILTER_CATEGORIES
      filter_options <- rbind(filter_options, data.frame(
        column_name = all_columns[i],
        display_name = all_display_names[i],
        available_values = paste(values, collapse = ","),
        is_filter = is_filter_column,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Separate filter and sorting columns
  filter_columns <- filter_options[filter_options$is_filter, ]
  sorting_columns <- filter_options[!filter_options$is_filter, ]
  
  # Order filter columns by priority
  filter_priority_order <- c("Growth Habit", "Climate Status", "Sun Level", "Moisture Level")
  filter_columns <- filter_columns[match(filter_priority_order, filter_columns$display_name), ]
  filter_columns <- filter_columns[!is.na(filter_columns$column_name), ]
  
  # Order sorting columns by priority, then alphabetically
  sorting_priority_order <- c("Hardiness Zone")
  sorting_columns_priority <- sorting_columns[sorting_columns$display_name %in% sorting_priority_order, ]
  sorting_columns_priority <- sorting_columns_priority[match(sorting_priority_order, sorting_columns_priority$display_name), ]
  sorting_columns_priority <- sorting_columns_priority[!is.na(sorting_columns_priority$column_name), ]
  
  sorting_columns_remaining <- sorting_columns[!sorting_columns$display_name %in% sorting_priority_order, ]
  sorting_columns_remaining <- sorting_columns_remaining[order(sorting_columns_remaining$display_name), ]
  
  sorting_columns <- rbind(sorting_columns_priority, sorting_columns_remaining)
  
  # Combine back together
  rbind(filter_columns, sorting_columns)
}
# Create filter tree structure for UI
# Builds hierarchical tree structure for filter categories
# Parameters:
#   filter_options: Filter options data frame
# Returns: List representing tree structure
create_filter_tree <- function(filter_options) {
  cat("Creating filter tree structure...\n")
  
  tree_structure <- list()
  node_id_counter <- 1
  
  # Get only filter columns for the filter tree
  filter_columns <- filter_options[filter_options$is_filter, ]
  
  # Add filter columns as categories with their values as children
  for (i in seq_len(nrow(filter_columns))) {
    category <- filter_columns$display_name[i]
    values <- str_trim(strsplit(filter_columns$available_values[i], ",")[[1]])
    values <- values[values != "" & !is.na(values)]
    
    if (length(values) == 0) next
    
    category_node <- list(
      text = category,
      id = paste0("filter_category_", node_id_counter),
      children = lapply(values, function(value) {
        node_id_counter <<- node_id_counter + 1
        list(text = value, id = paste0("filter_item_", node_id_counter))
      })
    )
    node_id_counter <- node_id_counter + 1
    tree_structure <- append(tree_structure, list(category_node))
  }
  
  tree_structure
}

# Create sorting tree structure for UI
# Builds hierarchical tree structure for sorting categories
# Parameters:
#   filter_options: Filter options data frame
# Returns: List representing tree structure
create_sorting_tree <- function(filter_options) {
  cat("Creating sorting tree structure...\n")
  
  tree_structure <- list()
  node_id_counter <- 1000  # Start with higher numbers to avoid conflicts
  
  # Get only sorting columns for the sorting tree
  sorting_columns <- filter_options[!filter_options$is_filter, ]
  
  # Add sorting columns as categories with their values as children
  for (i in seq_len(nrow(sorting_columns))) {
    category <- sorting_columns$display_name[i]
    values <- str_trim(strsplit(sorting_columns$available_values[i], ",")[[1]])
    values <- values[values != "" & !is.na(values)]
    
    if (length(values) == 0) next
    
    category_node <- list(
      text = category,
      id = paste0("sorting_category_", node_id_counter),
      children = lapply(values, function(value) {
        node_id_counter <<- node_id_counter + 1
        list(text = value, id = paste0("sorting_item_", node_id_counter))
      })
    )
    node_id_counter <- node_id_counter + 1
    tree_structure <- append(tree_structure, list(category_node))
  }
  
  tree_structure
}

# Create tree node mapping for selected criteria parsing
# Maps tree node IDs to their corresponding categories and values
# Parameters:
#   filter_tree: Filter tree structure
#   sorting_tree: Sorting tree structure
#   filter_options: Filter options data frame
# Returns: Data frame mapping node IDs to categories and values
create_tree_mapping <- function(filter_tree, sorting_tree, filter_options) {
  mapping <- data.frame(
    node_id = character(0),
    category = character(0),
    value = character(0),
    is_filter = logical(0),
    stringsAsFactors = FALSE
  )
  
  # Process filter tree
  for (category_node in filter_tree) {
    if (!is.null(category_node$children)) {
      category <- category_node$text
      is_filter_category <- any(filter_options$display_name == category & filter_options$is_filter)
      
      for (child_node in category_node$children) {
        mapping <- rbind(mapping, data.frame(
          node_id = child_node$id,
          category = category,
          value = child_node$text,
          is_filter = is_filter_category,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Process sorting tree
  for (category_node in sorting_tree) {
    if (!is.null(category_node$children)) {
      category <- category_node$text
      is_filter_category <- any(filter_options$display_name == category & filter_options$is_filter)
      
      for (child_node in category_node$children) {
        mapping <- rbind(mapping, data.frame(
          node_id = child_node$id,
          category = category,
          value = child_node$text,
          is_filter = is_filter_category,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  mapping
}

# Initialize Application Data =================================================
app_data <- load_application_data()
plant_data_raw <- app_data$plants

filter_configuration <- create_filter_configuration()
cleaned_plant_data <- clean_plant_database(plant_data_raw, filter_configuration)
complete_filter_options <- create_filter_options(cleaned_plant_data, filter_configuration)
filter_tree_structure <- create_filter_tree(complete_filter_options)
sorting_tree_structure <- create_sorting_tree(complete_filter_options)
tree_node_mapping <- create_tree_mapping(filter_tree_structure, sorting_tree_structure, complete_filter_options)

cat("Application initialization completed successfully!\n")

# User Interface Definition ===================================================
ui <- fluidPage(
  # Include CSS and JavaScript dependencies
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css", 
      rel = "stylesheet"
    ),
  ),
  
  # Enable shinyjs for dynamic UI interactions
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
      p("Select your state and desired site and plant characteristics below. Filter columns must match for plants to appear in results. Sorting columns add to the match score to help rank plants by preference. This tool is still under construction. Resulting lists may not be fully correct."),
      p(tags$strong("Match score:"), " Each plant earns +1 point for matching a sorting preference. The score helps rank plants by how well they fit your desired characteristics."),
      p("Looking for sources of native plants? Try searching for vendors through directories ",
        tags$a(href = "https://nativegardendesigns.wildones.org/nursery-list/", "here"), ", ",
        tags$a(href = "https://www.audubon.org/native-plants", "here"), ", ",
        tags$a(href = "https://homegrownnationalpark.org/directory/", "here"), ", or ",
        tags$a(href = "https://xerces.org/pollinator-conservation/native-plant-nursery-and-seed-directory", "here"), ". ",
        "Or if you're looking specifically for nurseries that sell ", tags$em("only"),
        " native plants sourced from local/regional genotypes, find those ",
        tags$a(href = "https://beechhollowfarms.com/native-plant-nurseries/", "here"), "."
      ),
      p(class = "funding-acknowledgement",
        tags$strong("Funding Acknowledgement:"),
        " This tool was partially supported by the U.S. Geological Survey's Northeast Climate Adaptation Science Center through grants G23AC00614-00, G22AC00084-02, and G19AC00091 and NSF GRFP No. 1938059."
      )
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
            

            div(class = "characteristics-section",
              div(class = "characteristics-header",
                h6(class = "characteristics-title",
                  tags$i(class = "fas fa-seedling"), " Plant Characteristics"
                ),
                actionButton("clear_all_filters", "Clear All", 
                           class = "btn-clear", style = "margin-left: auto;")
              ),
              
              # Split into two separate sections for filters and sorting
              div(class = "filter-subsection",
                div(class = "subsection-title", "Filter Preferences (Filters dataset)"),
                treeInput("filter_tree", label = NULL,
                  choices = filter_tree_structure, returnValue = "id", closeDepth = 0)
              ),
              
              div(class = "sorting-subsection",
                div(class = "subsection-title", "Sorting Preferences (sorts by best match)"),
                treeInput("sorting_tree", label = NULL,
                  choices = sorting_tree_structure, returnValue = "id", closeDepth = 0)
              )
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
      p(
        tags$strong("Recommended Citation:"),
        " Singh, K., M.E. Fertakos, T.W.M. Nuhfer, J.M. Allen, E.M. Beaury, B.A. Bradley. C-SNaP Tools: Climate-smart native plant selection. URL: https://www.climatesmartnativeplants.org/plant-selection/, access date."
      ),
      p(
        tags$strong("Publication:"),
        " This tool is based on: Fertakos, M.E., T.W.M. Nuhfer, E.M. Beaury, S. Birch, K. Singh, B.A. Bradley, C. Marshner, and J.M. Allen. 2026. The climate smart gardening database: Native and near-native garden plants for the northeastern United States. Ecology."
      )
    )
  )
)

# Server Logic =================================================================
server <- function(input, output, session) {
  
  observeEvent(input$clear_all_filters, {
    tryCatch({
      updateTreeInput(session = session, inputId = "filter_tree", 
                     selected = character(0))
      updateTreeInput(session = session, inputId = "sorting_tree", 
                     selected = character(0))
    }, error = function(e) cat(sprintf("Error clearing filters: %s\n", e$message)))
  })
  
  # Get display name for a column name
# Maps internal column names to user-friendly display names
# Parameters:
#   column_name: Internal column name
# Returns: Display name string
get_display_name_for_column <- function(column_name) {
  display_mapping <- c(
    "Growth.Habit" = "Growth Habit", "Sun.Level" = "Sun Level",
    "Moisture.Level" = "Moisture Level", "Soil.Type" = "Soil Type",
    "Bloom.Period" = "Bloom Period", "max_height" = "Max Height", "Color" = "Color",
    "Interesting.Foliage" = "Interesting Foliage", "Showy" = "Showy",
    "Garden.Aggressive" = "Garden Aggressive", "Wildlife.Services" = "Wildlife Services",
    "Pollinators" = "Pollinators", "Climate.Status" = "Climate Status",
    "propagation_keywords" = "Propagation Keywords",
    "propagation_methods" = "Propagation Methods"
  )
  
  ifelse(column_name %in% names(display_mapping), display_mapping[column_name], column_name)
}

# Get tooltip for a column name
# Returns the descriptive tooltip for a column
# Parameters:
#   column_name: Column name to get tooltip for
# Returns: Tooltip string or empty string if not found
get_column_tooltip <- function(column_name) {
  tooltip <- COLUMN_TOOLTIPS[[column_name]]
  if (is.null(tooltip)) return("")
  tooltip
}

# Helper Functions for Data Processing ======================================
  
  # Helper function to expand bloom period ranges
  # Converts month ranges (e.g., "January, March") to full month lists
  # Parameters:
  #   bloom_str: Bloom period string (comma-separated months)
  # Returns: Vector of individual months or ranges expanded
  expand_bloom_period <- function(bloom_str) {
    months <- c("January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December")
    parts <- str_trim(strsplit(as.character(bloom_str), "-")[[1]])
    expanded <- c()
    
    if (length(parts) == 2) {
      # Assume two parts indicate a range (e.g., "January, March" = Jan, Feb, Mar)
      start <- match(parts[1], months)
      end <- match(parts[2], months)
      if (!is.na(start) && !is.na(end) && start <= end) {
        expanded <- months[start:end]
      } else {
        expanded <- parts  # Fallback if not a valid range
      }
    } else {
      # Single month or multiple non-range months
      expanded <- parts
    }
    
    unique(expanded)
  }
  
  # Parse selected criteria from tree inputs
  # Converts selected node IDs to filter and sorting criteria
  # Parameters:
  #   filter_ids: Selected filter tree node IDs
  #   sorting_ids: Selected sorting tree node IDs
  # Returns: List with filter_criteria, sorting_criteria, and propagation_selected
  parse_selected_criteria <- function(filter_ids, sorting_ids) {
    filter_criteria <- list()
    sorting_criteria <- list()
    propagation_selected <- FALSE
    
    all_selected_ids <- c(filter_ids, sorting_ids)
    
    if (length(all_selected_ids) > 0 && nrow(tree_node_mapping) > 0) {
      for (node_id in all_selected_ids) {
        matching_nodes <- tree_node_mapping[tree_node_mapping$node_id == node_id, ]
        
        if (nrow(matching_nodes) > 0) {
          node_info <- matching_nodes[1, ]
          category <- as.character(node_info$category)
          value <- as.character(node_info$value)
          is_filter <- node_info$is_filter
          
          if (category == "Propagation Keywords") {
            propagation_selected <- TRUE
          } else if (is_filter) {
            filter_criteria[[category]] <- c(filter_criteria[[category]], value)
          } else {
            sorting_criteria[[category]] <- c(sorting_criteria[[category]], value)
          }
        }
      }
    }
    
    list(
      filter_criteria = filter_criteria, 
      sorting_criteria = sorting_criteria, 
      propagation_selected = propagation_selected
    )
  }
  
  # Get column name for a given category
  # Maps display category names back to internal column names
  # Parameters:
  #   category: Display category name
  # Returns: Internal column name or NULL if not found
  get_column_name_for_category <- function(category) {
    column_mapping <- c("Growth Habit" = "Growth.Habit", "Climate Status" = "Climate.Status", "Max Height" = "max_height", "Hardiness Zone" = "hardiness_zone_numeric")
    if (category %in% names(column_mapping)) return(column_mapping[category])
    
    matching_config <- filter_configuration[filter_configuration$display_name == category, ]
    if (nrow(matching_config) > 0) return(matching_config$column_name[1])
    NULL
  }
  
  # Check if plants match category values
  # Performs exact and substring matching for filter criteria
  # For Bloom.Period, expands ranges to handle month ranges
  # For Hardiness Zone, checks if selected zone falls within plant's zone range
  # Parameters:
  #   plant_data: Plant data frame
  #   column_name: Column to check
  #   target_values: Values to match against
  # Returns: Logical vector indicating matches
  check_category_matches <- function(plant_data, column_name, target_values) {
    if (!column_name %in% names(plant_data)) return(rep(FALSE, nrow(plant_data)))
    
    all_matches <- rep(FALSE, nrow(plant_data))
    
    for (target_value in target_values) {
      if (is.na(target_value) || target_value == "") next
      
      for (i in seq_len(nrow(plant_data))) {
        plant_value <- as.character(plant_data[[column_name]][i])
        if (is.na(plant_value) || plant_value == "") next
        
        if (column_name == "hardiness_zone_numeric") {
          # For hardiness zone, check if selected zone is within plant's min/max range
          target_zone <- as.numeric(target_value)
          plant_min <- plant_data$min_hardiness_zone[i]
          plant_max <- plant_data$max_hardiness_zone[i]
          if (!is.na(target_zone) && !is.na(plant_min) && !is.na(plant_max)) {
            if (target_zone >= plant_min && target_zone <= plant_max) {
              all_matches[i] <- TRUE
            }
          }
        } else if (column_name == "Bloom.Period") {
          # For bloom period, expand ranges and check if target month is in the range
          expanded_months <- expand_bloom_period(plant_value)
          if (any(tolower(expanded_months) == tolower(target_value))) {
            all_matches[i] <- TRUE
          }
        } else {
          # For other columns, split by common delimiters and check each part
          plant_values <- str_trim(strsplit(plant_value, "[,;/]")[[1]])
          # Check for exact match (case-insensitive) with any of the split values
          if (any(tolower(plant_values) == tolower(target_value))) {
            all_matches[i] <- TRUE
          }
        }
      }
    }
    
    all_matches
  }
  
  # Count matches for sorting score calculation
  # Similar to check_category_matches but returns count instead of boolean
  # For Bloom.Period, expands ranges to handle month ranges
  # For Hardiness Zone, checks if selected zone falls within plant's zone range
  # Parameters:
  #   plant_data: Plant data frame
  #   column_name: Column to check
  #   target_values: Values to match against
  # Returns: Numeric vector with match counts
  count_category_matches <- function(plant_data, column_name, target_values) {
    if (!column_name %in% names(plant_data)) return(rep(0, nrow(plant_data)))
    
    match_counts <- rep(0, nrow(plant_data))
    
    for (target_value in target_values) {
      if (is.na(target_value) || target_value == "") next
      
      for (i in seq_len(nrow(plant_data))) {
        plant_value <- as.character(plant_data[[column_name]][i])
        if (is.na(plant_value) || plant_value == "") next
        
        if (column_name == "hardiness_zone_numeric") {
          # For hardiness zone, check if selected zone is within plant's min/max range
          target_zone <- as.numeric(target_value)
          plant_min <- plant_data$min_hardiness_zone[i]
          plant_max <- plant_data$max_hardiness_zone[i]
          if (!is.na(target_zone) && !is.na(plant_min) && !is.na(plant_max)) {
            if (target_zone >= plant_min && target_zone <= plant_max) {
              match_counts[i] <- match_counts[i] + 1
            }
          }
        } else if (column_name == "Bloom.Period") {
          # For bloom period, expand ranges and check if target month is in the range
          expanded_months <- expand_bloom_period(plant_value)
          if (any(tolower(expanded_months) == tolower(target_value))) {
            match_counts[i] <- match_counts[i] + 1
          }
        } else {
          # For other columns, split by common delimiters and check each part
          plant_values <- str_trim(strsplit(plant_value, "[,;/]")[[1]])
          # Check for exact match (case-insensitive) with any of the split values
          if (any(tolower(plant_values) == tolower(target_value))) {
            match_counts[i] <- match_counts[i] + 1
          }
        }
      }
    }
    
    match_counts
  }
  
  # Filter plants by category values
  # Applies filtering logic for a single category
  # Parameters:
  #   plant_data: Plant data frame
  #   column_name: Column to filter on
  #   target_values: Values to filter by
  # Returns: Filtered data frame
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
      # Get state abbreviation
      state_abbreviation <- STATE_ABBREVIATIONS[input$selected_state]
      
      # Filter by state
      state_plants <- cleaned_plant_data[
        cleaned_plant_data$state == state_abbreviation, 
      ]
      
      if (nrow(state_plants) == 0) return(data.frame())
      
      # Parse criteria from both trees
      filter_ids <- input$filter_tree
      sorting_ids <- input$sorting_tree
      if (is.null(filter_ids)) filter_ids <- character(0)
      if (is.null(sorting_ids)) sorting_ids <- character(0)
      
      criteria <- parse_selected_criteria(filter_ids, sorting_ids)
      
      # Apply ALL filter criteria (must match all selected filter categories)
      filtered_plants <- state_plants
      for (filter_category in names(criteria$filter_criteria)) {
        category_values <- criteria$filter_criteria[[filter_category]]
        column_name <- get_column_name_for_category(filter_category)
        
        if (!is.null(column_name) && column_name %in% names(filtered_plants)) {
          filtered_plants <- filter_by_category_values(filtered_plants, column_name, category_values)
        }
      }
      
      if (nrow(filtered_plants) == 0) return(data.frame())
      
      # Calculate match scores from sorting criteria
      filtered_plants$match_score <- 0
      total_possible_points <- 0
      
      # Count total possible points from sorting criteria
      for (sorting_category in names(criteria$sorting_criteria)) {
        total_possible_points <- total_possible_points + 1
      }
      
      # Add possible points for propagation
      if (criteria$propagation_selected) {
        total_possible_points <- total_possible_points + 1
      }
      
      # Set minimum of 1 to avoid division by zero
      if (total_possible_points == 0) {
        total_possible_points <- 1
      }
      
      # Calculate match score - each category that matches contributes 1 point
      for (sorting_category in names(criteria$sorting_criteria)) {
        category_values <- criteria$sorting_criteria[[sorting_category]]
        column_name <- get_column_name_for_category(sorting_category)
        
        if (!is.null(column_name) && column_name %in% names(filtered_plants)) {
          # Check if each plant matches ANY value in this category (0 or 1 point per category)
          matches <- check_category_matches(filtered_plants, column_name, category_values)
          filtered_plants$match_score <- filtered_plants$match_score + as.numeric(matches)
        }
      }
      
      if (criteria$propagation_selected) {
        filtered_plants$match_score <- filtered_plants$match_score + 1
      }
      
      # Convert raw score to percentage out of 100, capped at 100%
      filtered_plants$match_score <- pmin(round((filtered_plants$match_score / total_possible_points) * 100), 100)
      
      # Prepare results table with metadata for tooltips
      results <- data.frame(
        "Match Score" = filtered_plants$match_score,
        "Scientific Name" = filtered_plants$scientific_name,
        "Common Name" = filtered_plants$common_name,
        "Min Zone" = filtered_plants$min_hardiness_zone,
        "Max Zone" = filtered_plants$max_hardiness_zone,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Store column order for reference
      attr(results, 'column_order') <- names(results)
      
      # Add selected characteristic columns
      all_selected_categories <- c(names(criteria$filter_criteria), names(criteria$sorting_criteria))
      priority_categories <- c("Growth Habit", "Climate Status", "Sun Level", "Moisture Level")
      
      for (category in c(priority_categories, 
                        all_selected_categories[!all_selected_categories %in% priority_categories],
                        DEFAULT_COLUMNS[!DEFAULT_COLUMNS %in% all_selected_categories])) {
        column_name <- get_column_name_for_category(category)
        if (!is.null(column_name) && column_name %in% names(filtered_plants) && 
            !category %in% names(results)) {
          results[[category]] <- filtered_plants[[column_name]]
        }
      }
      
      # Add propagation if selected
      if (criteria$propagation_selected && "propagation_methods" %in% names(filtered_plants)) {
        results[["Propagation Description"]] <- filtered_plants$propagation_methods
      }
      
      # Sort results
      sorted_results <- results[order(-results$`Match Score`, results$`Scientific Name`), ]
      attr(sorted_results, 'column_order') <- attr(results, 'column_order')
      sorted_results
      
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

      # Build a safe base filename for downloads: include state abbreviation (if set) and date
      state_abbr <- if (!is.null(input$selected_state) && input$selected_state %in% names(STATE_ABBREVIATIONS)) {
        STATE_ABBREVIATIONS[input$selected_state]
      } else {
        "ALL"
      }
      filename_base <- paste0("ClimateSmart_Plants_", state_abbr, "_", format(Sys.Date(), "%Y%m%d"))

      # Create column definitions with proper widths and alignment
      column_defs <- list(
        list(targets = 0, width = "90px", className = "dt-center dt-match-score"),
        list(targets = 1, width = "180px", className = "dt-left"),
        list(targets = 2, width = "180px", className = "dt-left"),
        list(targets = 3, width = "75px", className = "dt-center"),
        list(targets = 4, width = "75px", className = "dt-center"),
        list(targets = "_all", width = "140px")
      )
      
      # Generate tooltip data structure
      col_tooltips_js <- paste(
        sprintf('"%s": "%s"', 
               colnames(results),
               sapply(colnames(results), function(col) {
                 tooltip <- get_column_tooltip(col)
                 # Escape special characters for JavaScript
                 gsub('"', '\\"', gsub('\n', ' ', tooltip))
               })),
        collapse = ", "
      )
      
      dt_table <- datatable(
        results,
        extensions = 'Buttons',
        options = list(
          dom = '<"dt-top-controls"Bfr>t<"dt-bottom-controls"ip>',
          pageLength = DEFAULT_PAGE_SIZE,
          scrollX = TRUE,
          columnDefs = column_defs,
          buttons = list(
            list(extend = 'csv', text = '<i class="fas fa-download"></i> CSV', 
                 filename = filename_base, className = 'btn-csv'),
            list(extend = 'excel', text = '<i class="fas fa-download"></i> Excel', 
                 filename = filename_base, className = 'btn-excel'),
            list(extend = 'pdf', text = '<i class="fas fa-download"></i> PDF', 
                 filename = filename_base, className = 'btn-pdf', orientation = 'landscape')
          ),
          responsive = FALSE,
          autoWidth = FALSE,
          searching = TRUE,
          lengthChange = TRUE,
          info = FALSE,
          paging = FALSE,
          order = list(list(0, 'desc'), list(1, 'asc')),
          deferRender = TRUE,
          processing = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "  var api = this.api();",
            "  var headers = api.columns().header();",
            "  var tooltips = {", col_tooltips_js, "};",
            "  $(headers).each(function(i) {",
            "    var headerText = $(this).text().trim();",
            "    var tooltip = tooltips[headerText];",
            "    if (tooltip && tooltip.length > 0) {",
            "      $(this).attr('title', tooltip).addClass('header-with-tooltip');",
            "      $(this).css({'cursor': 'help', 'border-bottom': '2px dotted rgba(76, 175, 80, 0.5)'});",
            "      $(this).on('mouseenter', function() {",
            "        var $tooltip = $('<div class=\"header-tooltip-popup\"></div>');",
            "        $tooltip.text(tooltip);",
            "        $('body').append($tooltip);",
            "        $tooltip.css({",
            "          'position': 'fixed',",
            "          'z-index': '9999',",
            "          'max-width': '300px',",
            "          'background': '#2c3e50',",
            "          'color': 'white',",
            "          'padding': '10px 12px',",
            "          'border-radius': '4px',",
            "          'font-size': '12px',",
            "          'line-height': '1.4',",
            "          'box-shadow': '0 4px 12px rgba(0, 0, 0, 0.3)',",
            "          'pointer-events': 'none'",
            "        });",
            "        var offset = $(this).offset();",
            "        var tooltipWidth = $tooltip.outerWidth();",
            "        var headerWidth = $(this).outerWidth();",
            "        var tooltipHeight = $tooltip.outerHeight();",
            "        $tooltip.css({",
            "          'left': (offset.left + headerWidth / 2 - tooltipWidth / 2) + 'px',",
            "          'top': (offset.top - tooltipHeight - 12) + 'px'",
            "        });",
            "      });",
            "      $(this).on('mouseleave', function() {",
            "        $('.header-tooltip-popup').remove();",
            "      });",
            "    }",
            "  });",
            "  $('.dataTables_filter input').attr('title', 'Searches only within the currently filtered results and visible columns.');",
            "}"
          )
        ),
        rownames = FALSE,
        class = 'table-modern stripe hover compact',
        style = 'bootstrap4'
      ) %>%
        formatStyle(
          'Match Score',
          backgroundColor = styleInterval(
            cuts = c(20, 40, 60, 80),
            values = c('#f5f5f5', '#e8f5e9', '#c8e6c9', '#a5d6a7', '#81c784')
          ),
          fontWeight = 'bold',
          textAlign = 'center',
          color = styleInterval(
            cuts = c(50),
            values = c('#555555', '#1b5e20')
          )
        ) %>%
        formatStyle(
          c('Min Zone', 'Max Zone'),
          textAlign = 'center',
          fontWeight = '500',
          backgroundColor = '#f8f9fa',
          borderRight = '1px solid #dee2e6'
        ) %>%
        formatStyle(
          'Scientific Name',
          fontStyle = 'italic',
          color = '#2c3e50'
        )
      
      dt_table
        
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