library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(stringr)
library(readxl)
library(shinyjs)
library(rrapply)
library(data.table)

print("Printing Current Working Directory")
print(getwd())
print("Printing files in current working directory")
print(list.files())

#hardiness zone file from earlier riscc project
state.hz <- read.csv("/srv/shiny-server/app1/data/state.hz.csv")
#zipcode file from USDA
zipcodes <- read.csv("/srv/shiny-server/app1/data/zipcodes.csv")
#subset of data - will change as we finish consolidating + cleaning
data <- read_excel("/srv/shiny-server/app1/data/subsetdec6.xlsx")
#reference sheet with display column names
reference <- read.csv("/srv/shiny-server/app1/data/reference.csv")
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
reference.set$answers = sapply(strsplit(reference.set$answers, "; "), function(x) answers = paste(unique(x), collapse = ","))
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
state.hz$Full.Name[state.hz$State == "MA"] <- "Massachussetts"
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

# Get all plant types from the tree list for pre-selection
plant_type_ids <- tree_id[tree_id$col == "2" & tree_id$V1 == "Plant.Type", -c(1,2)]
plant_type_ids <- plant_type_ids[!is.na(plant_type_ids)]
plant_type_ids <- unname(as.character(plant_type_ids))

#######################################

## specify user interface
ui<-fluidPage(theme = "bootstrap.min.css", #this is introducing bootswatch's Journal CSS (located in www folder of app files). Fluid page is so that the contents of the app adjust to the browser size, basicPage would mean the components are static

              titlePanel("Climate Resilient Plants"), #setting up the title
              tags$hr(), #adds a line between title Panel and the body of the app
              wellPanel(tags$i("Some text. Instructions for users. Select the state you live in and your current hardiness zone.")),
              wellPanel(tags$i("Disclaimers and more information")), #Well panel creates a grey box for the text of our app description. tags$i() is used to create italicized text
              sidebarLayout( #sidebarLayout is create a side panel for all of our input options
                #select box for state you want the list for
                sidebarPanel(selectInput(inputId="state", label="Choose your state", choices=c("Select", "Connecticut", "Delaware", "Kentucky", "Maine", "Maryland", "Massachussetts", "New Hampshire", "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont", "Virginia", "West Virginia"), selected="Massachussetts"),
                             #numeric input for your hardiness zone
                             numericInput(inputId="zones", label="Choose your current hardiness zone",
                                          value=3, min=3, max=8, step=1),

                              # columns and filters
                             treeInput("tree", "What would you like to see?", tree_list, selected = plant_type_ids, returnValue = "id", closeDepth = 0)),

                #Allocating output space
                #this section of code creates a tab for the output list
                mainPanel(tabsetPanel(
                  tabPanel("Species List", tags$br(),DT::dataTableOutput("list"))#add vertical space between top of tab panel and the data table with tags$br
                )))
)

## defining data inputs and outputs
server<-function(input,output){
  #using a reactive function to be able to access user inputs in order to set up the output list that will be used in renderdatatable
  listfortable <-reactive({
    # print(input$tree)
    state.hz #uploading a table that contains each state and their future hardiness zone
    abbrev<-state.hz$State[state.hz$Full.Name==input$state & state.hz$Time_Period == "FutureWorst"] #pulling the abbreviation for the state that was inputted by the user
    table.output <- test[test$MaxZone >= state.hz$Zone.Min[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"] & test$MinZone <= state.hz$Zone.Max[state.hz$State == abbrev & state.hz$Time_Period == "FutureWorst"],]
    #this all sets up a lot of reference objects based on the tree input that will be used in the reactive filtering
    b <- input$tree #for easier typing
    j <- data.frame() #create empty df to put index coordinates
    for(i in 1:length(b)){
      j <- rbind(j, (which(tree_id == b[i], arr.ind = TRUE))) #find index coordinates of each tree id
    }
    pos <- as.data.frame(j) #make dataframe of index coordinates of tree ids
    pos <- subset(pos, subset = col != "2") #removing inputs for parent categories, leaving only children (answers)
    rownames(pos) <- NULL #remove rownames, they were confusing
    ans <- c() #answer input
    cat<- c() #categories of each answer input
    for (i in 1:length(pos$col)){
      r <- pos[i, 1]
      c <- (pos[i, 2] - 1)
      ans[i] <- tree_id[r, c]
      cat[i] <- tree_id[r, 1]
    }
    cols <- which(names(choices) %in% cat) #this indicates which columns to include without duplicates
    key <- as.data.frame(cbind(ans, cat, match(cat, names(choices)))) #connects input answers with names and column numbers
    #creating cleaned up filtered vectors to make final input out of
    Scientific.Name <-table.output$AcceptedName[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones]
    Common.Name <-table.output$CommonName.E[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones]
    Criteria <- table.output$Criteria[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones]
    More <- table.output[table.output$MinZone <= input$zones & table.output$MaxZone >= input$zones, cols + 4]
    outputList <- as.data.frame(cbind(Scientific.Name, Common.Name, Criteria, More)) #add them all together
    outputList$Scientific.Name <- as.character(outputList$Scientific.Name)
    colnames(outputList)<-c("Scientific.Name", "Common.Name", "Criteria", names(choices)[cols]) #Make names for columns
    ##the following code sorts the dataset based on filtering criteria selection
    #text searches the database for input strings and sorts by highest # of matches (adding a criteria column)
    #will need to add to it for numerical range columns
    crit <- as.data.frame(outputList$Criteria) #make a dataframe length of dataset
    for(g in 1:length(cols)){
      pattern <- paste((key$ans[key$V3 == cols[g]]), collapse = '|')
      key_called <- unique(key$cat[key$V3 == cols[g]])
      outputList.crit <- outputList %>%
        mutate(yes = case_when(if_any(contains(key_called), ~ grepl(pattern, .x)) ~ + 1)) #search each answer in the correct column
      crit[, g] <- outputList.crit$yes #multiple columns, one for each criteria
    }
    # print(crit)
    outputList$Criteria <- rowSums(crit, na.rm = TRUE) #add column together to get total criteria number
    outputList <- arrange(outputList, desc(Criteria))
    outputList
  })
  #problem: this tells you if its currently hardy to your zone and will be hardy anywhere in your state
  #not necessarily in your zone. #Massachusetts zone 6 just won't display any plants.
  output$list<- DT::renderDataTable(datatable(listfortable())) #using the DT package to produce a data table that is searchable, has options for sorting output, and other functions that makes data output more appealing, the buttons extension allows us to export the data table as a csv or pdf, i think there is an internal problem though because it seems as though DT is not as updated enough?
  }

## execute the app, opens a new window
# shinyApp(ui=ui, server=server)

shinyApp(ui = ui, server = server)