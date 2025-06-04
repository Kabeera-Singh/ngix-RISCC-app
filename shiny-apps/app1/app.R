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
          "Mammal.Services", "Insect.Services", "Reptile.Amphibian.Services", "Pollinators"),
  display = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type", "Bloom Period", 
              "Color", "Interesting Foliage", "Showy", "Garden Aggressive", "Bird Services", 
              "Mammal Services", "Insect Services", "Reptile/Amphibian Services", "Pollinators"),
  sortable = rep(TRUE, 14)
)

#filter reference sheet to columns which we want to be filter options
reference.set <- subset(reference, subset = sortable == TRUE)

#consolidate and format data columns for querying
test <- data %>%
  dplyr::select(Scientific.Name, Common.Name, Hardiness.Zone.Low, Hardiness.Zone.High, 
                one_of(reference.set$col), Propagation.Methods) %>%
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

# Add propagation methods as a separate category for filtering
propagation_options <- unique(unlist(strsplit(paste(test$Propagation.Methods[!is.na(test$Propagation.Methods)], collapse=","), "[,;/]")))
propagation_options <- trimws(propagation_options)
propagation_options <- propagation_options[propagation_options != "" & !is.na(propagation_options) & propagation_options != "NA"]

# Add propagation to reference set for tree but not for main filtering
reference.set.extended <- rbind(reference.set, data.frame(
  col = "Propagation.Methods",
  display = "Propagation Methods", 
  sortable = TRUE,
  answers = paste(unique(propagation_options), collapse = ",")
))

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

# Get default "Herb" selection
herb_ids <- c()
if(nrow(tree_mapping) > 0) {
  herb_rows <- tree_mapping[tree_mapping$category == "Growth.Habit" & 
                           grepl("Herb", tree_mapping$value, ignore.case = TRUE), ]
  if(nrow(herb_rows) > 0) {
    herb_ids <- as.character(herb_rows$id[1])
  }
}

# If no herb found, get first Growth.Habit option
if(length(herb_ids) == 0) {
  growth_habit_rows <- tree_mapping[tree_mapping$category == "Growth.Habit", ]
  if(nrow(growth_habit_rows) > 0) {
    herb_ids <- as.character(growth_habit_rows$id[1])
  }
}

# Get available states from data
available_states <- unique(state.hz$Full.Name)
available_states <- sort(available_states[!is.na(available_states)])

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
        h1("🌱 Climate Resilient Plants", class = "mb-0 h3"),
        p("Find native plants suitable for your climate future", class = "mb-0 small opacity-75")
      ),
      actionButton(
        "homeBtn",
        label = "Back to Home",
        onclick = "window.open('https://ngix-webpage-latest.onrender.com', '_self')",
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
          p("Select your state and current hardiness zone below. The tool will show climate-resilient plants that will thrive in your area's future climate conditions.", class = "card-text mb-0")
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
          
          # Hardiness zone input
          div(
            class = "mb-3",
            numericInput(
              inputId = "zones", 
              label = tags$span(icon("thermometer-half"), " Current hardiness zone"),
              value = 3, 
              min = 3, 
              max = 8, 
              step = 1,
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
    req(input$state, input$zones)
    
    # Get state abbreviation and zone information
    abbrev <- state.hz$State[state.hz$Full.Name == input$state & state.hz$Time_Period == "FutureWorst"]
    if(length(abbrev) == 0) return(data.frame())
    
    table.output <- test[test$MaxZone >= state.hz$Zone.Min[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"] & 
                         test$MinZone <= state.hz$Zone.Max[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"],]
    
    # Filter by hardiness zone
    filtered_data <- table.output[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones, ]
    
    if(nrow(filtered_data) == 0) return(data.frame())
    
    # Process tree input - fixed logic
    selected_criteria <- input$tree
    if(is.null(selected_criteria) || length(selected_criteria) == 0) {
      selected_criteria <- herb_ids  # Use default
    }
    
    # Create mapping of selected criteria to categories and values using tree_mapping
    criteria_mapping <- data.frame()
    propagation_selected <- FALSE
    
    if(length(selected_criteria) > 0 && nrow(tree_mapping) > 0) {
      # Debug: print selected criteria
      cat("Selected criteria:", selected_criteria, "\n")
      
      for(sel_id in selected_criteria) {
        # Find matching rows in tree_mapping
        matching_rows <- tree_mapping[tree_mapping$id == sel_id, ]
        
        if(nrow(matching_rows) > 0) {
          # Get the first matching row
          match_row <- matching_rows[1, ]
          
          # Extract category and value
          category <- as.character(match_row$category)
          value <- as.character(match_row$value)
          
          cat("Found match - Category:", category, "Value:", value, "\n")
          
          # Check if this is a propagation selection
          if(category == "Propagation.Methods") {
            propagation_selected <- TRUE
          }
          
          criteria_mapping <- rbind(criteria_mapping, data.frame(
            category = category,
            value = value,
            stringsAsFactors = FALSE
          ))
        } else {
          cat("No match found for ID:", sel_id, "\n")
        }
      }
    }
    
    # Debug: print criteria mapping
    if(nrow(criteria_mapping) > 0) {
      cat("Criteria mapping:\n")
      print(criteria_mapping)
    }
    
    # Calculate match scores
    filtered_data$Criteria <- 0
    
    if(nrow(criteria_mapping) > 0) {
      for(i in 1:nrow(criteria_mapping)) {
        cat_name <- criteria_mapping$category[i]
        cat_value <- criteria_mapping$value[i]
        
        # Skip propagation methods for scoring (only used for display)
        if(cat_name == "Propagation.Methods") next
        
        if(cat_name %in% names(filtered_data) && !is.na(cat_value) && cat_value != "") {
          # Get column values as character
          col_values <- as.character(filtered_data[[cat_name]])
          col_values[is.na(col_values)] <- ""
          
          cat("Matching", cat_value, "in column", cat_name, "\n")
          
          # Try different matching strategies
          # 1. Exact match (case insensitive)
          exact_matches <- grepl(paste0("\\b", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), "\\b"), 
                                col_values, ignore.case = TRUE)
          
          # 2. Contains match if no exact matches
          if(!any(exact_matches, na.rm = TRUE)) {
            contains_matches <- grepl(gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", cat_value), 
                                    col_values, ignore.case = TRUE)
            matches <- contains_matches
          } else {
            matches <- exact_matches
          }
          
          # Update scores
          match_count <- sum(matches, na.rm = TRUE)
          cat("Found", match_count, "matches\n")
          
          filtered_data$Criteria[matches] <- filtered_data$Criteria[matches] + 1
        }
      }
    }
    
    # Prepare output columns - only show selected characteristics (excluding propagation)
    cols_to_show <- unique(criteria_mapping$category[criteria_mapping$category != "Propagation.Methods"])
    cols_to_show <- cols_to_show[cols_to_show %in% names(filtered_data)]
    
    # Create base output
    output_data <- data.frame(
      "Scientific Name" = filtered_data$AcceptedName,
      "Common Name" = filtered_data$CommonName.E,
      "Match Score" = filtered_data$Criteria,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    # Add selected characteristic columns
    if(length(cols_to_show) > 0) {
      for(col in cols_to_show) {
        output_data[[col]] <- filtered_data[[col]]
      }
    }
    
    # Only add propagation description if propagation was selected
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
app <- shinyApp(ui = ui, server = server)
runApp(app, launch.browser = TRUE)