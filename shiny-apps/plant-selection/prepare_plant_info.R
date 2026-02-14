# ==============================================================================
# Prepare Plant Data - Build-time script for pre-computed RDS files
# ==============================================================================
# Run this script from the plant-selection directory after updating
# ClimateSmart_Data_Cleaned.csv. Generates:
#   - data/plants_prepared.rds (full cleaned data with pre-expanded bloom)
#   - data/plants_by_state/*.rds (one file per state)
#   - data/filter_options.rds (filter options, trees, tree mapping)
# ==============================================================================

library(data.table)
library(dplyr)
library(stringr)

# Set working directory to plant-selection app root (script location's parent)
script_dir <- if (exists("Rscript")) {
  dirname(sys.frame(1)$ofile)
} else {
  getwd()
}
if (basename(script_dir) == "scripts") {
  setwd(dirname(script_dir))
} else if (!file.exists("data/ClimateSmart_Data_Cleaned.csv")) {
  stop("Run from plant-selection directory or ensure data/ClimateSmart_Data_Cleaned.csv exists")
}

# Constants (must match app.R)
REQUIRED_FILTER_CATEGORIES <- c("Growth Habit", "Climate Status", "Sun Level", "Moisture Level")
CSV_TO_INTERNAL_NAMES <- c(
  "Scientific Name" = "Scientific.Name", "Common Name" = "Common.Name",
  "Growth Habit" = "Growth.Habit", "Hardiness Zone Low" = "Hardiness.Zone.Low",
  "Hardiness Zone High" = "Hardiness.Zone.High", "Climate Status" = "Climate.Status",
  "Sun Level" = "Sun.Level", "Moisture Level" = "Moisture.Level",
  "Soil Type" = "Soil.Type", "Bloom Period" = "Bloom.Period",
  "Interesting Foliage" = "Interesting.Foliage", "Garden Aggressive" = "Garden.Aggressive",
  "Wildlife Services" = "Wildlife.Services", "Propagation Methods" = "Propagation.Methods",
  "Propagation Keywords" = "Propagation.Keywords"
)

create_filter_configuration <- function() {
  data.frame(
    column_name = c("Growth.Habit", "Sun.Level", "Moisture.Level", "Soil.Type",
                   "Bloom.Period", "max_height", "Color", "Interesting.Foliage", "Showy",
                   "Garden.Aggressive", "Wildlife.Services", "Pollinators", "Climate.Status"),
    display_name = c("Growth Habit", "Sun Level", "Moisture Level", "Soil Type",
                    "Bloom Period", "Max Height", "Color", "Interesting Foliage", "Showy",
                    "Garden Aggressive", "Wildlife Services", "Pollinators", "Climate Status"),
    stringsAsFactors = FALSE
  )
}

expand_bloom_period <- function(bloom_str) {
  months <- c("January", "February", "March", "April", "May", "June",
              "July", "August", "September", "October", "November", "December")
  parts <- str_trim(strsplit(as.character(bloom_str), "-")[[1]])
  expanded <- c()
  if (length(parts) == 2) {
    start <- match(parts[1], months)
    end <- match(parts[2], months)
    if (!is.na(start) && !is.na(end) && start <= end) {
      expanded <- months[start:end]
    } else {
      expanded <- parts
    }
  } else {
    expanded <- parts
  }
  unique(expanded)
}

# Vectorized expand for all bloom periods
expand_bloom_period_vectorized <- function(bloom_vec) {
  lapply(bloom_vec, function(x) expand_bloom_period(x))
}

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

order_by_preference <- function(values, preferred_order) {
  matched_values <- preferred_order[preferred_order %in% values]
  unmatched_values <- sort(values[!values %in% preferred_order])
  c(matched_values, unmatched_values)
}

apply_custom_ordering <- function(values, category) {
  if (length(values) == 0) return(values)
  clean_values <- unique(values[!is.na(values) & values != "" & values != "NA"])
  if (length(clean_values) == 0) return(character(0))
  if (category == "Hardiness Zone") {
    return(as.character(sort(as.numeric(clean_values))))
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
    "Color" = c("Red", "Orange", "Yellow", "Green", "Blue", "Purple", "Violet", "Pink", "Brown", "White", "Not Applicable"),
    "Wildlife Services" = c("Birds", "Mammals", "Reptiles", "Amphibians", "Insects", "Not Specified"),
    {
      special_endings <- c("Other", "None", "Not Specified", "Not Applicable", "Indeterminate")
      regular_values <- sort(clean_values[!clean_values %in% special_endings])
      special_values <- clean_values[clean_values %in% special_endings]
      special_sorted <- special_values[order(match(special_values, special_endings))]
      c(regular_values, special_sorted)
    }
  )
  if (category == "Bloom Period" || category == "Wildlife Services") {
    clean_values <- clean_values[clean_values %in% preferred_order]
  }
  order_by_preference(clean_values, preferred_order)
}

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

# create_filter_options using rbindlist (no rbind in loop)
create_filter_options <- function(plant_data, filter_config) {
  all_columns <- c(filter_config$column_name, "propagation_keywords", "hardiness_zone_numeric")
  all_display_names <- c(filter_config$display_name, "Propagation Keywords", "Hardiness Zone")
  rows <- list()
  for (i in seq_along(all_columns)) {
    if (all_columns[i] == "hardiness_zone_numeric") {
      values <- as.character(1:12)
    } else {
      values <- extract_categorical_values(plant_data, all_columns[i])
    }
    if (length(values) > 0) {
      is_filter_column <- all_display_names[i] %in% REQUIRED_FILTER_CATEGORIES
      rows[[length(rows) + 1]] <- data.frame(
        column_name = all_columns[i],
        display_name = all_display_names[i],
        available_values = paste(values, collapse = ","),
        is_filter = is_filter_column,
        stringsAsFactors = FALSE
      )
    }
  }
  filter_options <- do.call(rbind, rows)
  filter_columns <- filter_options[filter_options$is_filter, ]
  sorting_columns <- filter_options[!filter_options$is_filter, ]
  filter_priority_order <- c("Growth Habit", "Climate Status", "Sun Level", "Moisture Level")
  filter_columns <- filter_columns[match(filter_priority_order, filter_columns$display_name), ]
  filter_columns <- filter_columns[!is.na(filter_columns$column_name), ]
  sorting_priority_order <- c("Hardiness Zone")
  sorting_columns_priority <- sorting_columns[sorting_columns$display_name %in% sorting_priority_order, ]
  sorting_columns_priority <- sorting_columns_priority[match(sorting_priority_order, sorting_columns_priority$display_name), ]
  sorting_columns_priority <- sorting_columns_priority[!is.na(sorting_columns_priority$column_name), ]
  sorting_columns_remaining <- sorting_columns[!sorting_columns$display_name %in% sorting_priority_order, ]
  sorting_columns_remaining <- sorting_columns_remaining[order(sorting_columns_remaining$display_name), ]
  filter_options <- rbind(filter_columns, sorting_columns_priority, sorting_columns_remaining)
  filter_options
}

create_filter_tree <- function(filter_options) {
  tree_structure <- list()
  node_id_counter <- 1
  filter_columns <- filter_options[filter_options$is_filter, ]
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

create_sorting_tree <- function(filter_options) {
  tree_structure <- list()
  node_id_counter <- 1000
  sorting_columns <- filter_options[!filter_options$is_filter, ]
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

create_tree_mapping <- function(filter_tree, sorting_tree, filter_options) {
  mapping_rows <- list()
  for (category_node in filter_tree) {
    if (!is.null(category_node$children)) {
      category <- category_node$text
      is_filter_category <- any(filter_options$display_name == category & filter_options$is_filter)
      for (child_node in category_node$children) {
        mapping_rows[[length(mapping_rows) + 1]] <- data.frame(
          node_id = child_node$id,
          category = category,
          value = child_node$text,
          is_filter = is_filter_category,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  for (category_node in sorting_tree) {
    if (!is.null(category_node$children)) {
      category <- category_node$text
      is_filter_category <- any(filter_options$display_name == category & filter_options$is_filter)
      for (child_node in category_node$children) {
        mapping_rows[[length(mapping_rows) + 1]] <- data.frame(
          node_id = child_node$id,
          category = category,
          value = child_node$text,
          is_filter = is_filter_category,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, mapping_rows)
}

# Main execution
message("Loading ClimateSmart_Data_Cleaned.csv...")
plants <- data.table::fread("data/ClimateSmart_Data_Cleaned.csv")
for (old_name in names(CSV_TO_INTERNAL_NAMES)) {
  if (old_name %in% names(plants)) {
    data.table::setnames(plants, old_name, CSV_TO_INTERNAL_NAMES[old_name])
  }
}

filter_config <- create_filter_configuration()
select_cols <- c("State", "Scientific.Name", "Common.Name", "Hardiness.Zone.Low", "Hardiness.Zone.High",
                intersect(filter_config$column_name, names(plants)),
                "Propagation.Methods", "Propagation.Keywords", "Climate.Status")
select_cols <- unique(select_cols[select_cols %in% names(plants)])
cleaned <- plants %>%
  select(all_of(select_cols)) %>%
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

# Add pre-expanded bloom period column
if ("Bloom.Period" %in% names(cleaned)) {
  message("Pre-expanding bloom periods...")
  cleaned$bloom_months_expanded <- expand_bloom_period_vectorized(cleaned$Bloom.Period)
}

# Convert to data.table for efficient storage
cleaned_dt <- as.data.table(cleaned)

# Save full prepared data
message("Saving data/plants_prepared.rds...")
saveRDS(cleaned_dt, "data/plants_prepared.rds")

# Split by state and save
dir.create("data/plants_by_state", showWarnings = FALSE, recursive = TRUE)
states <- unique(cleaned_dt$state)
for (st in states) {
  if (!is.na(st) && nchar(st) > 0) {
    state_subset <- cleaned_dt[state == st]
    saveRDS(state_subset, file.path("data", "plants_by_state", paste0(st, ".rds")))
  }
}
message("Saved ", length(states), " state files to data/plants_by_state/")

# Create filter options
message("Creating filter options...")
complete_filter_options <- create_filter_options(cleaned, filter_config)
filter_tree_structure <- create_filter_tree(complete_filter_options)
sorting_tree_structure <- create_sorting_tree(complete_filter_options)
tree_node_mapping <- create_tree_mapping(filter_tree_structure, sorting_tree_structure, complete_filter_options)

filter_options_bundle <- list(
  complete_filter_options = complete_filter_options,
  filter_tree_structure = filter_tree_structure,
  sorting_tree_structure = sorting_tree_structure,
  tree_node_mapping = tree_node_mapping
)
message("Saving data/filter_options.rds...")
saveRDS(filter_options_bundle, "data/filter_options.rds")

message("Done. Run the plant-selection app to use pre-computed data.")
