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

print("Printing Current Working Directory")
print(getwd())
print("Printing files in current working directory")
print(list.files())

#hardiness zone file from earlier riscc project
state.hz <- read.csv("data/state.hz.csv")
#zipcode file from USDA
zipcodes <- read.csv("data/zipcodes.csv")
#main dataset - updated to new format
data <- read.csv("data/ClimateSmart_Data_Cleaned.csv")

# Create reference sheet mapping from new dataset columns - exclude Propagation.Methods and Rare.Status
reference <- data.frame(
  col = c("Growth.Habit", "Sun.Level", "Moisture.Level", "Soil.Type", "Bloom.Period",
          "Color", "Interesting.Foliage", "Showy", "Garden.Aggressive", "Bird.Services",
          "Mammal.Services", "Insect.Services", "Reptile.Amphibian.Services", "Pollinators", "Climate.Status"),
  display = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type", "Bloom Period",
              "Color", "Interesting Foliage", "Showy", "Garden Aggressive", "Bird Services",
              "Mammal Services", "Insect Services", "Reptile/Amphibian Services", "Pollinators", "Climate Status"),
  sortable = rep(TRUE, 15)
)

#filter reference sheet to columns which we want to be filter options
reference.set <- subset(reference, subset = sortable == TRUE)

#consolidate and format data columns for querying
test <- data %>%
  dplyr::select(Scientific.Name, Common.Name, Hardiness.Zone.Low, Hardiness.Zone.High,
                one_of(reference.set$col), Propagation.Methods, Propagation.Keywords, Climate.Status) %>%
  # Remove rows with missing hardiness zone information
  filter(!is.na(Hardiness.Zone.Low) & !is.na(Hardiness.Zone.High))

reference.set$answers <- NA
answers <- list()
#get answers from dataset
for (i in 1:length(reference.set$col)) {
  x <- as.vector(unique(test[, i+4]))
  # Better handling of mixed data types and NA values
  x <- x[!is.na(x) & x != "" & x != "NA"]
  a <- paste(unique(unlist(strsplit(paste(x, collapse=","), "[,;/]"))), collapse=", ")
  answers <- append(answers, a)
}

#clean up answers to unique only
reference.set$answers <- answers
reference.set$answers <- as.character(reference.set$answers)

# Improved cleanup function
clean_answers <- function(text) {
  # Remove NA, empty strings, and clean punctuation
  text <- str_replace_all(text, "\\bNA\\b", "")
  text <- str_replace_all(text, "\\s*,\\s*", ",")
  text <- str_replace_all(text, "^,+|,+$", "")  # Remove leading/trailing commas
  text <- str_replace_all(text, ",+", ",")      # Remove multiple commas

  # Split, clean, and rejoin to remove duplicates
  items <- unlist(strsplit(text, ","))
  items <- trimws(items)
  items <- items[items != "" & !is.na(items)]
  items <- unique(items)

  return(paste(items, collapse = ","))
}

reference.set$answers <- sapply(reference.set$answers, clean_answers)

#make each option for each column a list element in a large list
reference.list <- reference.set %>%
  dplyr::select(1, 4)

answers.list <- function(i){
  if(reference.list$answers[i] == "" || is.na(reference.list$answers[i])) {
    return(list(""))
  }
  return(strsplit(reference.list$answers[i], ","))
}

total.choices <- lapply(1:length(reference.list$answers), answers.list)
rm(reference.list)
names(total.choices) <- reference.set$col

#deal with duplicates
test <- test[!duplicated(test$Scientific.Name), ]

#rename hardiness zone columns to match original code
names(test)[names(test) == 'Hardiness.Zone.Low'] <- 'MinZone'
names(test)[names(test) == 'Hardiness.Zone.High'] <- 'MaxZone'
names(test)[names(test) == 'Scientific.Name'] <- 'AcceptedName'
names(test)[names(test) == 'Common.Name'] <- 'CommonName.E'

#Connect state abbreviations to full state names
state.hz$Full.Name <- NA
state.hz$Full.Name[state.hz$State == "NY"] <- "New York"
state.hz$Full.Name[state.hz$State == "CT"] <- "Connecticut"
state.hz$Full.Name[state.hz$State == "RI"] <- "Rhode Island"
state.hz$Full.Name[state.hz$State == "MA"] <- "Massachusetts"
state.hz$Full.Name[state.hz$State == "NH"] <- "New Hampshire"
state.hz$Full.Name[state.hz$State == "VT"] <- "Vermont"
state.hz$Full.Name[state.hz$State == "ME"] <- "Maine"
state.hz$Full.Name[state.hz$State == "OH"] <- "Ohio"
state.hz$Full.Name[state.hz$State == "WV"] <- "West Virginia"
state.hz$Full.Name[state.hz$State == "VA"] <- "Virginia"
state.hz$Full.Name[state.hz$State == "MD"] <- "Maryland"
state.hz$Full.Name[state.hz$State == "NJ"] <- "New Jersey"
state.hz$Full.Name[state.hz$State == "DE"] <- "Delaware"
state.hz$Full.Name[state.hz$State == "PA"] <- "Pennsylvania"
state.hz$Full.Name[state.hz$State == "NC"] <- "North Carolina"
state.hz$Full.Name[state.hz$State == "KY"] <- "Kentucky"

#Add criteria column
test$Criteria <- rep(0, length.out = length(test$AcceptedName))

#set column choice numbers and names- updated for new dataset
choices <- c(1:length(reference.set$col))
names(choices) <- reference.set$col

# Add propagation keywords as a separate category for filtering
propagation_keywords <- unique(unlist(strsplit(paste(test$Propagation.Keywords[!is.na(test$Propagation.Keywords)], collapse=","), "[,;/]")))
propagation_keywords <- trimws(propagation_keywords)
propagation_keywords <- propagation_keywords[propagation_keywords != "" & !is.na(propagation_keywords) & propagation_keywords != "NA"]

# Get available states from data
available_states <- unique(state.hz$Full.Name)
available_states <- sort(available_states[!is.na(available_states)])

# Function to get hardiness zones for a given state
get_state_zones <- function(state_name) {
  state_data <- state.hz[state.hz$Full.Name == state_name & state.hz$Time_Period == "CurrentConditions", ]
  if(nrow(state_data) > 0) {
    zone_min <- state_data$Zone.Min[1]
    zone_max <- state_data$Zone.Max[1]
    return(zone_min:zone_max)
  }
  return(c())
}

# Get all possible hardiness zones from available states
all_zones <- c()
for(state in available_states) {
  state_zones <- get_state_zones(state)
  all_zones <- c(all_zones, state_zones)
}
all_zones <- sort(unique(all_zones))

# Create reference set with reordered items - Growth.Habit first, then Hardiness.Zones and Climate.Status
reference.set.extended <- data.frame(
  col = "Growth.Habit",
  display = "Growth Habit",
  sortable = TRUE,
  answers = reference.set$answers[reference.set$col == "Growth.Habit"],
  stringsAsFactors = FALSE
)

# Add Hardiness Zones second
reference.set.extended <- rbind(reference.set.extended,
  data.frame(
    col = "Hardiness.Zones",
    display = "Hardiness Zones",
    sortable = TRUE,
    answers = paste(all_zones, collapse = ","),
    stringsAsFactors = FALSE
  )
)

# Add Climate Status third
reference.set.extended <- rbind(reference.set.extended,
  data.frame(
    col = "Climate.Status",
    display = "Climate Status",
    sortable = TRUE,
    answers = reference.set$answers[reference.set$col == "Climate.Status"],
    stringsAsFactors = FALSE
  )
)

# Add remaining items (excluding Growth.Habit and Climate.Status since they're already added)
remaining_items <- reference.set[!(reference.set$col %in% c("Growth.Habit", "Climate.Status")), ]
reference.set.extended <- rbind(reference.set.extended, remaining_items)

# Add propagation keywords at the end
reference.set.extended <- rbind(reference.set.extended,
  data.frame(
    col = "Propagation.Keywords",
    display = "Propagation Keywords",
    sortable = TRUE,
    answers = paste(unique(propagation_keywords), collapse = ","),
    stringsAsFactors = FALSE
  )
)

tree_data <- data.frame()
for(i in 1:nrow(reference.set.extended)) {
  col_name <- reference.set.extended$col[i]
  options <- unlist(strsplit(reference.set.extended$answers[i], ","))
  options <- trimws(options)
  options <- options[options != "" & !is.na(options)]

  if(length(options) > 0) {
    temp_df <- data.frame(
      category = rep(col_name, length(options)),
      answer = options,
      stringsAsFactors = FALSE
    )
    tree_data <- rbind(tree_data, temp_df)
  }
}

# Create tree function
create_tree <- function(data) {
  categories <- unique(data$category)
  tree_list <- list()
  id_counter <- 1

  for(cat in categories) {
    cat_data <- data[data$category == cat, ]

    # Create category node
    cat_node <- list(
      text = reference.set.extended$display[reference.set.extended$col == cat][1],
      id = paste0("cat_", id_counter),
      children = list()
    )
    id_counter <- id_counter + 1

    # Add children
    for(j in 1:nrow(cat_data)) {
      child_node <- list(
        text = cat_data$answer[j],
        id = paste0("item_", id_counter)
      )
      cat_node$children <- append(cat_node$children, list(child_node))
      id_counter <- id_counter + 1
    }

    tree_list <- append(tree_list, list(cat_node))
  }

  return(tree_list)
}

tree_list <- create_tree(tree_data)

# Create a proper mapping of tree IDs to categories and values
tree_mapping <- data.frame()
extract_tree_info <- function(node, path = "", category = "", parent_id = "") {
  if(is.list(node)) {
    if(!is.null(names(node))) {
      for(name in names(node)) {
        if(name == "text") {
          # This is a leaf node with text
          current_text <- as.character(node$text)
          current_id <- as.character(node$id)

          # Determine if this is a category or value
          if(path == "") {
            # Top level - this is a category
            new_category <- current_text
            extract_tree_info(node, current_text, new_category, current_id)
          } else {
            # This is a value under a category
            tree_mapping <<- rbind(tree_mapping, data.frame(
              id = current_id,
              category = category,
              value = current_text,
              stringsAsFactors = FALSE
            ))
          }
        } else if(name == "children" && is.list(node$children)) {
          # Process children
          for(child in node$children) {
            extract_tree_info(child, path, category, parent_id)
          }
        } else {
          # Process other named elements
          extract_tree_info(node[[name]],
                           ifelse(path == "", name, paste(path, name, sep = "/")),
                           category, parent_id)
        }
      }
    } else {
      # Unnamed list - process each element
      for(item in node) {
        extract_tree_info(item, path, category, parent_id)
      }
    }
  }
}

# Extract the tree structure
extract_tree_info(tree_list)

# Get default selection for Growth Habit - look for first available option
default_ids <- c()
if(nrow(tree_mapping) > 0) {
  growth_habit_rows <- tree_mapping[tree_mapping$category == "Growth Habit", ]
  if(nrow(growth_habit_rows) > 0) {
    default_ids <- as.character(growth_habit_rows$id[1])
  }
}

# Create filtered tree list (excluding hardiness zones)
tree_list_filtered <- tree_list[!sapply(tree_list, function(x) x$text == "Hardiness Zones")]

# tree_mapping_filtered <- tree_mapping[tree_mapping$category, ]
tree_mapping_filtered <- tree_mapping[tree_mapping$category != "Hardiness Zones", ]

# Define UI with embedded HTML structure
ui <- fluidPage(
  # Include CSS files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shared-colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css", rel = "stylesheet")
  ),

  # Use shinyjs
  useShinyjs(),

  # Header Section
  div(class = "app-header",
    div(class = "header-container",
      h1("Climate-Smart Plant Selection"),
      a(href = "/", class = "home-btn",
        tags$i(class = "fas fa-home"), " Back to Home"
      )
    )
  ),

  # Main Container
  div(class = "main-container",
    # Info Card
    div(class = "info-card",
      h5(tags$i(class = "fas fa-info-circle"), " How to Use This Tool"),
      p("Select your state and desired site and plant characteristics below. The tool will generate a list of native and near-native plants likely to survive both current and future climate conditions. The plant list will be sorted based on a best match with the selection criteria you input. Plants with a higher match score meet more of your selection criteria. If you select five criteria, a plant with a match score of 5 is a perfect match.")
    ),

    # Main Layout
    div(class = "main-layout",
      # Sidebar Panel
      div(class = "sidebar",
        div(class = "filter-card",
          div(class = "filter-header",
            h5(tags$i(class = "fas fa-filter"), " Filter Options")
          ),
          div(class = "filter-body",
            # State Selection
            div(class = "form-group",
              tags$label(`for` = "state",
                tags$i(class = "fas fa-map-marker-alt"), " Choose your state"
              ),
              selectInput(
                inputId = "state",
                label = NULL,
                choices = c("Select" = "", "Connecticut", "Delaware", "Kentucky", "Maine", "Maryland",
                            "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania",
                            "Rhode Island", "Vermont", "Virginia", "West Virginia"),
                width = "100%"
              )
            ),
            # Plant Characteristics
            div(class = "characteristics-section",
              div(class = "characteristics-header",
                h6(class = "characteristics-title",
                  tags$i(class = "fas fa-seedling"), " Plant Characteristics"
                ),
                actionButton("clear_filters", "Clear All", 
                           class = "btn-clear", 
                           style = "margin-left: auto;")
              ),
              # Other Plant Characteristics (now includes hardiness zones)
              treeInput(
                "tree",
                label = NULL,
                choices = tree_list,  # Now includes hardiness zones
                returnValue = "id",
                closeDepth = 0  # All collapsed initially
              )
            )
          )
        )
      ),

      # Main Panel
      div(class = "main-panel",
        div(class = "results-card",
          div(class = "results-header",
            h5(tags$i(class = "fas fa-list"), " Species List"),
            div(class = "results-count",
              textOutput("results_count", inline = TRUE)
            )
          ),

          # # Loading Indicator
          # div(class = "loading-indicator", id = "loading", style = "display: none;",
          #   div(class = "spinner"),
          #   p("Loading plant data...")
          # ),

          # Data Table
          div(class = "table-container",
            DT::dataTableOutput("list")
          )
        )
      )
    )
  ),

  # Footer
  tags$footer(class = "app-footer",
    div(class = "footer-container",
      p("Climate Resilient Plants Database - Helping you choose plants for future climate conditions")
    )
  )
)

## defining data inputs and outputs
server <- function(input, output, session) {

  # Add debugging to check data loading
  observe({
    cat("Data loaded - test nrow:", nrow(test), "\n")
    cat("State.hz nrow:", nrow(state.hz), "\n")
    cat("Tree mapping nrow:", nrow(tree_mapping), "\n")
  })

  # Clear filters button
  observeEvent(input$clear_filters, {
    updateTreeInput(session, "tree", selected = character(0))
    # No need to clear separate zone selectors anymore
  })

  # Reactive function for the filtered species list
  listfortable <- reactive({
    tryCatch({
      req(input$state)
      cat("Selected state:", input$state, "\n")

      # Show loading indicator
      shinyjs::show("loading")

      # Get state data and zone information (same as before)
      state_data <- state.hz[state.hz$Full.Name == input$state, ]
      if(nrow(state_data) == 0) {
        cat("No data found for state:", input$state, "\n")
        shinyjs::hide("loading")
        return(data.frame())
      }

      abbrev <- state.hz$State[state.hz$Full.Name == input$state & state.hz$Time_Period == "FutureWorst"]
      if(length(abbrev) == 0) {
        cat("No abbreviation found for state:", input$state, "\n")
        shinyjs::hide("loading")
        return(data.frame())
      }

      zone_data <- state.hz[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst", ]
      if(nrow(zone_data) == 0) {
        cat("No zone data found for abbreviation:", abbrev, "\n")
        shinyjs::hide("loading")
        return(data.frame())
      }

      zone_min <- zone_data$Zone.Min[1]
      zone_max <- zone_data$Zone.Max[1]

      # Filter plants by state hardiness zones
      table.output <- test[test$MaxZone >= zone_min & test$MinZone <= zone_max, ]

      # Get selected criteria from tree
      selected_criteria <- input$tree
      if(is.null(selected_criteria)) {
        selected_criteria <- character(0)
      }

      # Process selected criteria including hardiness zones
      criteria_mapping <- data.frame()
      hardiness_zones_selected <- c()
      propagation_selected <- FALSE

      if(length(selected_criteria) > 0 && nrow(tree_mapping) > 0) {
        for(sel_id in selected_criteria) {
          matching_rows <- tree_mapping[tree_mapping$id == sel_id, ]

          if(nrow(matching_rows) > 0) {
            match_row <- matching_rows[1, ]
            category <- as.character(match_row$category)
            value <- as.character(match_row$value)

            if(category == "Hardiness Zones") {
              hardiness_zones_selected <- c(hardiness_zones_selected, as.numeric(value))
            } else if(category == "Propagation Keywords") {
              propagation_selected <- TRUE
            }

            criteria_mapping <- rbind(criteria_mapping, data.frame(
              category = category,
              value = value,
              stringsAsFactors = FALSE
            ))
          }
        }
      }

      # Apply hardiness zone filtering if selected
      if(length(hardiness_zones_selected) > 0) {
        min_selected_zone <- min(hardiness_zones_selected)
        max_selected_zone <- max(hardiness_zones_selected)
        
        # Filter to plants that can survive in the selected zone range
        table.output <- table.output[
          table.output$MinZone <= max_selected_zone & 
          table.output$MaxZone >= min_selected_zone, 
        ]
      }

      # Apply other filters (same logic as before for required and optional criteria)
      filtered_data <- table.output
      
      # Filter by required criteria: Growth Habit and Climate Status
      required_categories <- c("Growth Habit", "Climate Status")
      for(req_cat in required_categories) {
        req_criteria <- criteria_mapping[criteria_mapping$category == req_cat, ]
        if(nrow(req_criteria) > 0) {
          if(req_cat == "Growth Habit") {
            col_name <- "Growth.Habit"
          } else if(req_cat == "Climate Status") {
            col_name <- "Climate.Status"
          }

          category_filter <- rep(FALSE, nrow(filtered_data))
          for(i in 1:nrow(req_criteria)) {
            cat_value <- req_criteria$value[i]
            if(col_name %in% names(filtered_data) && !is.na(cat_value) && cat_value != "") {
              col_values <- as.character(filtered_data[[col_name]])
              col_values[is.na(col_values)] <- ""
              exact_matches <- grepl(paste0("\\b", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), "\\b"),
                                    col_values, ignore.case = TRUE)
              if(!any(exact_matches, na.rm = TRUE)) {
                contains_matches <- grepl(gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value),
                                        col_values, ignore.case = TRUE)
                matches <- contains_matches
              } else {
                matches <- exact_matches
              }
              category_filter <- category_filter | matches
            }
          }
          filtered_data <- filtered_data[category_filter, ]
        }
      }

      if(nrow(filtered_data) == 0) {
        shinyjs::hide("loading")
        return(data.frame())
      }

      # Calculate match scores for optional criteria
      filtered_data$Criteria <- 0
      optional_criteria <- criteria_mapping[!(criteria_mapping$category %in% 
                                            c(required_categories, "Propagation Keywords", "Hardiness Zones")), ]

      if(nrow(optional_criteria) > 0) {
        for(i in 1:nrow(optional_criteria)) {
          cat_name <- optional_criteria$category[i]
          cat_value <- optional_criteria$value[i]
          col_name <- reference.set$col[reference.set$display == cat_name]
          if(length(col_name) == 0) {
            col_name <- cat_name
          } else {
            col_name <- col_name[1]
          }

          if(col_name %in% names(filtered_data) && !is.na(cat_value) && cat_value != "") {
            col_values <- as.character(filtered_data[[col_name]])
            col_values[is.na(col_values)] <- ""
            exact_matches <- grepl(paste0("\\b", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), "\\b"),
                                  col_values, ignore.case = TRUE)
            if(!any(exact_matches, na.rm = TRUE)) {
              contains_matches <- grepl(gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value),
                                      col_values, ignore.case = TRUE)
              matches <- contains_matches
            } else {
              matches <- exact_matches
            }
            filtered_data$Criteria[matches] <- filtered_data$Criteria[matches] + 1
          }
        }
      }

      # Add bonus point for hardiness zone selection
      if(length(hardiness_zones_selected) > 0) {
        filtered_data$Criteria <- filtered_data$Criteria + 1
      }

      # Prepare output columns - ALWAYS include all selected characteristics
      cols_to_show <- unique(c("Growth.Habit", "Climate.Status",
                             criteria_mapping$category[!(criteria_mapping$category %in%
                             c("Propagation Keywords", "Growth Habit", "Climate Status"))]))

      # Create base output with all basic columns
      output_data <- data.frame(
        "Match Score" = filtered_data$Criteria,
        "Scientific Name" = filtered_data$AcceptedName,
        "Common Name" = filtered_data$CommonName.E,
        "Min Zone" = filtered_data$MinZone,
        "Max Zone" = filtered_data$MaxZone,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Add Growth Habit and Climate Status
      if("Growth.Habit" %in% names(filtered_data)) {
        output_data$"Growth Habit" <- filtered_data$Growth.Habit
      }
      if("Climate.Status" %in% names(filtered_data)) {
        output_data$"Climate Status" <- filtered_data$Climate.Status
      }

      # Add all other selected characteristic columns
      other_cols <- cols_to_show[!(cols_to_show %in% c("Growth.Habit", "Climate.Status", "Hardiness Zones"))]
      if(length(other_cols) > 0) {
        for(col in other_cols) {
          if(col %in% reference.set$col) {
            display_name <- reference.set$display[reference.set$col == col][1]
            output_data[[display_name]] <- filtered_data[[col]]
          } else {
            # Try to find matching column
            matching_col <- reference.set$col[reference.set$display == col]
            if(length(matching_col) > 0 && matching_col[1] %in% names(filtered_data)) {
              output_data[[col]] <- filtered_data[[matching_col[1]]]
            }
          }
        }
      }

      # Add propagation description if propagation keyword was selected
      if(propagation_selected && "Propagation.Methods" %in% names(filtered_data)) {
        output_data$"Propagation Description" <- filtered_data$Propagation.Methods
      }

      # Sort by match score (highest first)
      output_data <- output_data[order(-output_data$`Match Score`), ]

      shinyjs::hide("loading")
      return(output_data)

    }, error = function(e) {
      cat("Error in listfortable:", e$message, "\n")
      shinyjs::hide("loading")
      return(data.frame())
    })
  })

  # Results count
  output$results_count <- renderText({
    data <- listfortable()
    if(is.null(data) || nrow(data) == 0) {
      return("0 plants found")
    }
    paste(nrow(data), "plants found")
  })

  # Data table output with enhanced scrolling and match score highlighting
output$list <- DT::renderDataTable({
  tryCatch({
    data <- listfortable()

    if(is.null(data) || nrow(data) == 0) {
      empty_df <- data.frame("Message" = "No plants match your criteria. Try adjusting your filters or selecting a different state.")
      return(datatable(empty_df, options = list(dom = 't', searching = FALSE),
                      rownames = FALSE, colnames = ""))
    }

    # FIXED CONFIGURATION - REMOVES SCROLLING CONFLICTS
    datatable(
      data,
      extensions = c('Buttons'),
      options = list(
        # BASIC SETTINGS
        dom = 'Bfrtip',
        pageLength = 20,
        
        # CRITICAL FIX: Remove scrollX and scrollY to prevent conflicts
        scrollX = TRUE,
        scrollY = FALSE,
        
        # COLUMN DEFINITIONS - SIMPLIFIED
        columnDefs = list(
          # list(targets = 0, width = "90px", className = "dt-center"),   # Match Score
          # list(targets = 1, width = "200px"),                           # Scientific Name  
          # list(targets = 2, width = "180px"),                           # Common Name
          list(targets = 3, width = "80px", className = "dt-center"),   # Min Zone
          list(targets = 4, width = "80px", className = "dt-center"),   # Max Zone
          list(targets = "_all", width = "150px")                       # All other columns
        ),
        
        # EXPORT BUTTONS
        buttons = list(
          list(extend = 'csv', text = 'Download CSV'),
          list(extend = 'excel', text = 'Download Excel'),
          list(extend = 'pdf', text = 'Download PDF')
        ),
        
        # TABLE BEHAVIOR
        responsive = FALSE,
        autoWidth = FALSE,
        searching = TRUE,
        lengthChange = TRUE,
        info = FALSE,
        paging = TRUE,
        
        # SORTING
        order = list(list(0, 'desc'),list(1,'asc')),  # Sort by Match Score descending, then by Scientific Name ascending
        
        # PERFORMANCE SETTINGS
        deferRender = TRUE,
        processing = TRUE
      ),
      rownames = FALSE,
      class = 'table-hover table-striped compact',
      style = 'bootstrap4'
    ) %>%
      # MATCH SCORE FORMATTING
      formatStyle(
        'Match Score',
        backgroundColor = styleInterval(
          cuts = c(1, 2, 3, 4, 5),
          values = c('#ffffff', '#e8f5e9', '#c8e6c9', '#a5d6a7', '#81c784', '#66bb6a')
        ),
        fontWeight = 'bold',
        textAlign = 'center'
      ) %>%
      # ZONE COLUMNS FORMATTING
      formatStyle(
        c('Min Zone', 'Max Zone'),
        textAlign = 'center',
        fontWeight = '500'
      )
      
  }, error = function(e) {
    cat("Error in renderDataTable:", e$message, "\n")
    empty_df <- data.frame("Error" = paste("Error loading data:", e$message))
    return(datatable(empty_df, options = list(dom = 't', searching = FALSE),
                    rownames = FALSE, colnames = ""))
  })
})
}

## execute the app
shinyApp(ui = ui, server = server)