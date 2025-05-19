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
#subset of data - will change as we finish consolidating + cleaning
data <- read_excel("data/subsetdec6.xlsx")
#reference sheet with display column names
reference <- read.csv("data/reference.csv")
#cut down to columns of interest for display
reference <- reference[!is.na(reference$display),]

#filter reference sheet to columns which we want to be filter options
reference.set <- subset(reference, subset = sortable == TRUE)

#consolidate and format data columns for querying
test <- data %>%
  dplyr::select(AcceptedName, CommonName.E, Zone.From.Combo.x, Zone.To.Combo.x, one_of(reference.set$col))

reference.set$answers <- NA
answers <- list()
#get answers from dataset
for (i in 1:length(reference.set$col)) {
x <- as.vector(unique(test[, i+4]))
a <- (unlist(x)) %>% paste(collapse=", ")
answers <- append(answers, a)
}
#clean up answers to unique only
reference.set$answers <- answers
reference.set$answers <- as.character(reference.set$answers)
#remove extra spaces and NAs, clean up mix of punctuation (comma, semicolon, and slash
reference.set$answers <- str_replace_all(reference.set$answers, fixed("NA"), "")
reference.set$answers <- str_replace_all(reference.set$answers, fixed(", "), ",")
reference.set$answers <- str_replace_all(reference.set$answers, fixed(" ,"), ",")
reference.set$answers <- str_replace_all(reference.set$answers, fixed(" , "), ",")
reference.set$answers <- str_replace_all(reference.set$answers, fixed(",,"), ",")
reference.set$answers = sapply(strsplit(reference.set$answers, "/"), function(x) answers = paste(unique(x), collapse = ","))
reference.set$answers = sapply(strsplit(reference.set$answers, ";"), function(x) answers = paste(unique(x), collapse = ","))
reference.set$answers <- str_replace_all(reference.set$answers, fixed(",,"), ",")
reference.set$answers <- str_replace_all(reference.set$answers, fixed(",m,"), ",")
reference.set$answers = sapply(strsplit(reference.set$answers, ","), function(x) answers = paste(unique(x), collapse = ","))

#create logical column detecting strings beginning with commas
comma.fun <- function(i) {
b <- (gregexpr(",", reference.set$answers[i])[[1]][1]) == 1
}
logical <- sapply(1:28, comma.fun)
reference.set$temp <- logical
#remove commas at the beginning of strings
reference.set$answers[reference.set$temp == TRUE] <- (sub('.', '', reference.set$answers[reference.set$temp == TRUE]))
#clean up
reference.set$temp <- NULL
rm(logical)

#make each option for each column a list element in a large list. This will be referenced in the
#reactive selection code. Might not be strictly necessary but will be much less typing and
#make the app better able to be updated to full dataset
reference.list <- reference.set %>%
  dplyr::select(1, 4)
answers.list <- function(i){
  (strsplit(reference.list$answers[i],","))
}
total.choices <- lapply(1:length(reference.list$answers), answers.list)
rm(reference.list)
names(total.choices) <- reference.set$col

#deal with duplicates (ideally this code will be removed eventually as it will be redundant)
test <- test[!duplicated(test$AcceptedName), ]

#rename hardiness zone columns
names(test)[names(test) == 'Zone.From.Combo.x'] <- 'MinZone'
names(test)[names(test) == 'Zone.To.Combo.x'] <- 'MaxZone'

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

#Make sure all data has hardiness zone information
test <-test[!is.na(test$MinZone) & !is.na(test$MaxZone), ]

#Add criteria column
test$Criteria <- rep(0, length.out = length(test$AcceptedName))

#set column choice numbers and names- needs work
choices <- c(1:length(reference.set$col))
names(choices) <- c("Plant.Type", "Duration", "Lifespan", "Commercial.Availability", "Soil.Moisture", "Water.Use", "Drought.Tolerance",
                    "Sun.Exposure", "Growth.Rate", "Active.Growth.Season", "Blooms.From", "Blooms.To", "Bloom.Color", "Bloom.Length", "Showy.Flowers", "Plant.Shape", "Fall.Conspicuous",
                    "Leaf.Retention", "Interesting.Foliage", "Fragrant.Foliage", "Resprout.Ability", "Maintenance", "Hedge.Tolerance", "Fire.Tolerance", "Grazing.Palatability", "Salt.Tolerance",
                    "Pollinator.Value", "Pollinators")
reference.set$col <- names(choices)

#make tree https://rdrr.io/cran/shinyWidgets/man/treeInput.html - make this long, then skip viewing columns
#and bundle into filter options
tree <- reference.set %>% select(1, 4) %>%
 separate(2, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r"), sep = ",") %>% t() %>% as.data.frame() %>% slice( -(1))
names(tree) <- reference.set$col
tree <- tree %>% gather(key = "category", value = "answer", na.rm = TRUE)
tree_list <- create_tree(tree)

## find id in tree list
tree_id <- rrapply::rrapply(tree_list, how = "bind")

# Get all plant types from the tree list, defaulting to 'herb'
plant_type_ids <- tree_id[tree_id$col == "2" & tree_id$V1 == "Plant.Type", -c(1,2)]
herb_id <- plant_type_ids[grepl("herb", tree_id[tree_id$col == "2" & tree_id$V1 == "Plant.Type", -c(1,2)], ignore.case = TRUE)]
herb_id <- as.character(herb_id[!is.na(herb_id)])

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
              selected = 'herb',
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
    table.output <- test[test$MaxZone >= state.hz$Zone.Min[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"] & 
                         test$MinZone <= state.hz$Zone.Max[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"],]
    
    # Process tree input
    b <- input$tree
    j <- data.frame()
    for(i in 1:length(b)){
      j <- rbind(j, (which(tree_id == b[i], arr.ind = TRUE)))
    }
    pos <- as.data.frame(j)
    pos <- subset(pos, subset = col != "2")
    rownames(pos) <- NULL
    
    ans <- c()
    cat <- c()
    for (i in 1:length(pos$col)){
      r <- pos[i, 1]
      c <- (pos[i, 2] - 1)
      ans[i] <- tree_id[r, c]
      cat[i] <- tree_id[r, 1]
    }
    
    cols <- which(names(choices) %in% cat)
    key <- as.data.frame(cbind(ans, cat, match(cat, names(choices))))
    
    # Filter by hardiness zone
    filtered_data <- table.output[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones, ] # nolint
    
    # Create output list
    Scientific.Name <- filtered_data$AcceptedName
    Common.Name <- filtered_data$CommonName.E
    Criteria <- filtered_data$Criteria
    More <- filtered_data[, cols + 4]
    
    outputList <- as.data.frame(cbind(Scientific.Name, Common.Name, Criteria, More))
    outputList$Scientific.Name <- as.character(outputList$Scientific.Name)
    colnames(outputList) <- c("Scientific Name", "Common Name", "Match Score", names(choices)[cols])
    
    # Calculate criteria matching
    crit <- as.data.frame(outputList$`Match Score`)
    for(g in 1:length(cols)){
      pattern <- paste((key$ans[key$V3 == cols[g]]), collapse = '|')
      key_called <- unique(key$cat[key$V3 == cols[g]])
      outputList.crit <- outputList %>%
        mutate(yes = case_when(if_any(contains(key_called), ~ grepl(pattern, .x)) ~ + 1))
      crit[, g] <- outputList.crit$yes
    }
    
    outputList$`Match Score` <- rowSums(crit, na.rm = TRUE)
    outputList <- arrange(outputList, desc(`Match Score`))
    outputList
  })
  
  # Results count
  output$results_count <- renderText({
    data <- listfortable()
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