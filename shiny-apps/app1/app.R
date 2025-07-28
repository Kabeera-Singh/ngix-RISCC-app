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

#make tree - improved version
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

# Get default "Herb" selection for Growth Habit
herb_ids <- c()
if(nrow(tree_mapping) > 0) {
  herb_rows <- tree_mapping[tree_mapping$category == "Growth Habit" & 
                           grepl("Shrub", tree_mapping$value, ignore.case = TRUE), ]
  if(nrow(herb_rows) > 0) {
    herb_ids <- as.character(herb_rows$id[1])
  }
}

# If no herb found, get first Growth.Habit option
if(length(herb_ids) == 0) {
  growth_habit_rows <- tree_mapping[tree_mapping$category == "Growth Habit", ]
  if(nrow(growth_habit_rows) > 0) {
    herb_ids <- as.character(growth_habit_rows$id[1])
  }
}

#######################################

## specify user interface
ui <- fluidPage(
  # Modern theme using bslib
  theme = bs_theme(
    version = 4,
    bootswatch = "flatly",
    primary = "#2C5530",
    success = "#5A744F",
    info = "#8FA68E",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),
  
  # Include shinyjs
  useShinyjs(),
  
  # Modern header
  div(
    class = "bg-primary text-white py-4 mb-4",
    div(
      class = "container-fluid d-flex justify-content-between align-items-center",
      div(
        h1("Climate-Smart Plant Selection", class = "mb-0 h3")),
      actionButton(
        "homeBtn",
        label = "Back to Home",
        onclick = "window.open('/', '_self')",
        class = "btn btn-outline-light",
        icon = icon("home")
      )
    )
  ),
  
  # Info card
  fluidRow(
    class = "mb-4",
    column(
      width = 12,
      div(
        class = "card border-0 shadow-sm",
        div(
          class = "card-body",
          h5("How to Use This Tool", class = "card-title text-success"),
          p("Select your state and desired site and plant characteristics below. The tool will generate a list of native and near-native plants likely to survive both current and future climate conditions. The plant list will be sorted based on a best match with the selection criteria you input. Plants with a higher match score meet more of your selection criteria. If you select five criteria, a plant with a match score of 5 is a perfect match.", class = "card-text mb-0")
        )
      )
    )
  ),
  
  # Error display
  div(id = "error-display", style = "display: none;"),
  
  # Main layout
  fluidRow(
    # Sidebar panel with modern styling
    column(
      width = 4,
      div(
        class = "card border-0 shadow-sm sticky-top",
        style = "top: 20px;",
        div(
          class = "card-header bg-success text-white",
          h5("Filter Options", class = "mb-0")
        ),
        div(
          class = "card-body p-3",
          
          # State selection
          div(
            class = "mb-3",
            selectInput(
              inputId = "state", 
              label = tags$span(icon("map-marker-alt"), " Choose your state"),
              choices = c("Select" = "", "Connecticut", "Delaware", "Kentucky", "Maine", "Maryland", 
                        "Massachusetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", 
                        "Rhode Island", "Vermont", "Virginia", "West Virginia"),
              selected = "Massachusetts",
              width = "100%"
            )
          ),
          
          # Plant characteristics
          div(
            class = "mt-4",
            h6("Plant Characteristics", class = "text-success"),
            treeInput(
              "tree", 
              "Select desired characteristics:",
              tree_list, 
              selected = herb_ids,
              returnValue = "id", 
              closeDepth = 1
            )
          )
        )
      )
    ),
    
    # Main panel
    column(
      width = 8,
      div(
        class = "card border-0 shadow-sm",
        div(
          class = "card-header bg-info text-white d-flex justify-content-between align-items-center",
          h5("Species List", class = "mb-0"),
          div(
            class = "badge badge-light",
            textOutput("results_count", inline = TRUE)
          )
        ),
        div(
          class = "card-body p-0",
          # Simple loading indicator
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            div(
              class = "text-center p-5",
              HTML('<div class="spinner-border text-success" role="status">
                     <span class="sr-only">Loading...</span>
                   </div>
                   <p class="mt-3 text-muted">Loading plant data...</p>')
            )
          ),
          # Data table
          DT::dataTableOutput("list")
        )
      )
    )
  ),
  
  # Footer
  tags$footer(
    class = "bg-light text-muted text-center py-3 mt-5",
    p("Climate Resilient Plants Database - Helping you choose plants for future climate conditions", class = "mb-0 small")
  ),
  
  # Custom CSS for improved styling
  tags$head(
    tags$style(HTML("
      .content-wrapper { 
        padding-top: 20px; 
      }
      .dt-buttons { 
        margin-bottom: 10px; 
      }
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        margin-bottom: 10px;
      }
      .card {
        border-radius: 12px;
      }
      .btn {
        border-radius: 8px;
      }
      .form-control {
        border-radius: 6px;
      }
      #tree {
        max-height: 400px;
        overflow-y: auto;
      }
      .sticky-top {
        z-index: 1020;
      }
      .shiny-busy .recalculating {
        opacity: 0.5;
      }
    "))
  )
)

## defining data inputs and outputs
server <- function(input, output, session) {
  
  # Reactive function for the filtered species list
  listfortable <- reactive({
    req(input$state)
    
    # Get state abbreviation and zone information
    abbrev <- state.hz$State[state.hz$Full.Name == input$state & state.hz$Time_Period == "FutureWorst"]
    if(length(abbrev) == 0) return(data.frame())
    
    table.output <- test[test$MaxZone >= state.hz$Zone.Min[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"] & 
                         test$MinZone <= state.hz$Zone.Max[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"],]
    
    # Get selected criteria from tree
    selected_criteria <- input$tree
    if(is.null(selected_criteria) || length(selected_criteria) == 0) {
      selected_criteria <- herb_ids  # Use default
    }
    
    # Filter by selected hardiness zones and other criteria
    selected_zones <- c()
    filtered_data <- table.output
    
    # Create mapping of selected criteria to categories and values using tree_mapping
    criteria_mapping <- data.frame()
    propagation_selected <- FALSE
    
    if(length(selected_criteria) > 0 && nrow(tree_mapping) > 0) {
      for(sel_id in selected_criteria) {
        matching_rows <- tree_mapping[tree_mapping$id == sel_id, ]
        
        if(nrow(matching_rows) > 0) {
          match_row <- matching_rows[1, ]
          category <- as.character(match_row$category)
          value <- as.character(match_row$value)
          
          # Check if this is a hardiness zone selection
          if(category == "Hardiness Zones") {
            zone_num <- as.numeric(value)
            if(!is.na(zone_num)) {
              selected_zones <- c(selected_zones, zone_num)
            }
          }
          
          # Check if this is a propagation selection
          if(category == "Propagation Keywords") {
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
    
    # Filter by hardiness zones if selected
    if(length(selected_zones) > 0) {
      zone_filter <- rep(FALSE, nrow(filtered_data))
      for(zone in selected_zones) {
        zone_filter <- zone_filter | (filtered_data$MinZone <= zone & filtered_data$MaxZone >= zone)
      }
      filtered_data <- filtered_data[zone_filter, ]
    }
    
    # Filter by required criteria: Growth Habit and Climate Status
    required_categories <- c("Growth Habit", "Climate Status")
    for(req_cat in required_categories) {
      req_criteria <- criteria_mapping[criteria_mapping$category == req_cat, ]
      
      if(nrow(req_criteria) > 0) {
        # Map display name back to column name
        if(req_cat == "Growth Habit") {
          col_name <- "Growth.Habit"
        } else if(req_cat == "Climate Status") {
          col_name <- "Climate.Status"
        }
        
        # Apply filter for this required category
        category_filter <- rep(FALSE, nrow(filtered_data))
        
        for(i in 1:nrow(req_criteria)) {
          cat_value <- req_criteria$value[i]
          
          if(col_name %in% names(filtered_data) && !is.na(cat_value) && cat_value != "") {
            col_values <- as.character(filtered_data[[col_name]])
            col_values[is.na(col_values)] <- ""
            
            # Exact match (case insensitive)
            exact_matches <- grepl(paste0("\\b", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), "\\b"), 
                                  col_values, ignore.case = TRUE)
            
            # Contains match if no exact matches
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
    
    if(nrow(filtered_data) == 0) return(data.frame())
    
    # Calculate match scores for optional criteria (excluding required ones and hardiness zones)
    filtered_data$Criteria <- 0
    optional_criteria <- criteria_mapping[!(criteria_mapping$category %in% c(required_categories, "Hardiness Zones", "Propagation Keywords")), ]
    
    if(nrow(optional_criteria) > 0) {
      for(i in 1:nrow(optional_criteria)) {
        cat_name <- optional_criteria$category[i]
        cat_value <- optional_criteria$value[i]
        
        # Map display name back to column name
        col_name <- reference.set$col[reference.set$display == cat_name]
        if(length(col_name) == 0) {
          col_name <- cat_name
        } else {
          col_name <- col_name[1]
        }
        
        if(col_name %in% names(filtered_data) && !is.na(cat_value) && cat_value != "") {
          col_values <- as.character(filtered_data[[col_name]])
          col_values[is.na(col_values)] <- ""
          
          # Exact match (case insensitive)
          exact_matches <- grepl(paste0("\\b", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), "\\b"), 
                                col_values, ignore.case = TRUE)
          
          # Contains match if no exact matches
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
    
    # Prepare output columns - always include Growth Habit and Climate Status
    cols_to_show <- unique(c("Growth.Habit", "Climate.Status", 
                           criteria_mapping$category[!(criteria_mapping$category %in% 
                           c("Propagation Keywords", "Hardiness Zones", "Growth Habit", "Climate Status"))]))
    
    # Map display names back to column names
    actual_cols <- c()
    for(col in cols_to_show) {
      if(col == "Growth Habit") {
        actual_cols <- c(actual_cols, "Growth.Habit")
      } else if(col == "Climate Status") {
        actual_cols <- c(actual_cols, "Climate.Status")
      } else {
        mapped_col <- reference.set$col[reference.set$display == col]
        if(length(mapped_col) > 0) {
          actual_cols <- c(actual_cols, mapped_col[1])
        }
      }
    }
    
    actual_cols <- actual_cols[actual_cols %in% names(filtered_data)]
    
    # Create base output (removed Hardiness Zone column)
    output_data <- data.frame(
      "Scientific Name" = filtered_data$AcceptedName,
      "Common Name" = filtered_data$CommonName.E,
      "Match Score" = filtered_data$Criteria,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Add Growth Habit and Climate Status first
    if("Growth.Habit" %in% names(filtered_data)) {
      output_data$"Growth Habit" <- filtered_data$Growth.Habit
    }
    if("Climate.Status" %in% names(filtered_data)) {
      output_data$"Climate Status" <- filtered_data$Climate.Status
    }
    
    # Add other selected characteristic columns
    remaining_cols <- actual_cols[!(actual_cols %in% c("Growth.Habit", "Climate.Status"))]
    if(length(remaining_cols) > 0) {
      for(col in remaining_cols) {
        display_name <- reference.set$display[reference.set$col == col]
        if(length(display_name) > 0) {
          output_data[[display_name[1]]] <- filtered_data[[col]]
        } else {
          output_data[[col]] <- filtered_data[[col]]
        }
      }
    }
    
    # Add propagation description if propagation keyword was selected
    if(propagation_selected && "Propagation.Methods" %in% names(filtered_data)) {
      output_data$"Propagation Description" <- filtered_data$Propagation.Methods
    }
    
    # Sort by match score
    output_data <- output_data[order(-output_data$`Match Score`), ]
    
    return(output_data)
  })
  
  # Results count
  output$results_count <- renderText({
    data <- listfortable()
    if(is.null(data) || nrow(data) == 0) {
      return("0 plants found")
    }
    paste(nrow(data), "plants found")
  })
  
  # Data table output
  output$list <- DT::renderDataTable({
    data <- listfortable()
    
    if(is.null(data) || nrow(data) == 0) {
      empty_df <- data.frame("Message" = "No plants match your criteria. Try adjusting your filters or selecting a different state.")
      return(datatable(empty_df, options = list(dom = 't', searching = FALSE), 
                      rownames = FALSE, colnames = ""))
    }
    
    datatable(
      data,
      extensions = c('Buttons', 'Responsive'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', text = 'Download CSV'),
          list(extend = 'excel', text = 'Download Excel'),
          list(extend = 'pdf', text = 'Download PDF')
        ),
        responsive = TRUE,
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'table-hover'
    ) %>%
      formatStyle(
        'Match Score',
        backgroundColor = styleInterval(c(1, 3, 5), 
                                      c('#ffffff', '#e8f5e9', '#c8e6c9', '#a5d6a7')),
        fontWeight = 'bold'
      )
  })
}

## execute the app
shinyApp(ui = ui, server = server)
# app <- shinyApp(ui = ui, server = server)
# runApp(app, launch.browser = TRUE)