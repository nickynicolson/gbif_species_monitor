# GBIF Species Monitor - Simplified Version
# This script handles GBIF occurrence data for species monitoring

# Load required packages
library(rgbif)
library(dplyr)

# Function to set up configuration for a specific species
setup_species_config <- function(species_key, species_name) {
  # Create species-specific directory
  species_dir <- file.path("data", gsub(" ", "_", species_name))

  # Create directory if it doesn't exist
  if (!dir.exists(species_dir)) {
    dir.create(species_dir, recursive = TRUE)
    message("Created directory for ", species_name, ": ", species_dir)
  }

  # Set up file paths
  current_file <- file.path(species_dir, "current_data.rds")
  previous_file <- file.path(species_dir, "previous_data.rds")
  log_file <- file.path(species_dir, "changes_log.csv")

  # Return configuration as a list
  return(list(
    species_key = species_key,
    species_name = species_name,
    data_dir = species_dir,
    current_file = current_file,
    previous_file = previous_file,
    log_file = log_file
  ))
}

# Function to fetch occurrence data from GBIF
fetch_gbif_data <- function(taxon_key) {
  message("Fetching data from GBIF...")

  # Fetch occurrence data
  occurrences <- occ_data(
    taxonKey = taxon_key,
    limit = 5000  # Adjust based on expected number of records
  )

  # Extract relevant fields
  if (is.null(occurrences$data)) {
    message("No data found for this taxon key.")
    return(NULL)
  }

  data <- occurrences$data %>%
    select(
      key, scientificName, decimalLatitude, decimalLongitude,
      country, locality, eventDate, basisOfRecord,
      coordinateUncertaintyInMeters, issues
    )

  message("Retrieved ", nrow(data), " records.")
  return(data)
}

# Function to compare current and previous data
compare_data <- function(current, previous) {
  if (is.null(previous)) {
    message("No previous data found for comparison.")
    return(list(
      is_changed = FALSE,
      new_records = 0,
      modified_records = 0,
      deleted_records = 0,
      new_records_data = data.frame(),
      modified_records_data = data.frame(),
      deleted_records_data = data.frame()
    ))
  }

  # Identify new, modified, and deleted records
  current_keys <- current$key
  previous_keys <- previous$key

  new_keys <- setdiff(current_keys, previous_keys)
  deleted_keys <- setdiff(previous_keys, current_keys)
  common_keys <- intersect(current_keys, previous_keys)

  new_records <- current %>% filter(key %in% new_keys)
  deleted_records <- previous %>% filter(key %in% deleted_keys)

  # Check for modifications in common records
  modified_records <- data.frame()
  for (k in common_keys) {
    current_row <- current %>% filter(key == k)
    previous_row <- previous %>% filter(key == k)

    # Skip if we have multiple rows with same key (shouldn't happen, but just in case)
    if (nrow(current_row) > 1 || nrow(previous_row) > 1) {
      message("Warning: Multiple rows found with key ", k, ". Skipping comparison.")
      next
    }

    # Compare relevant fields
    fields_to_compare <- c("decimalLatitude", "decimalLongitude", "country",
                           "locality", "coordinateUncertaintyInMeters")

    for (field in fields_to_compare) {
      # Safely extract values - ensure we're getting a single value
      current_value <- if (length(current_row[[field]]) == 1) as.character(current_row[[field]])[1] else NA
      previous_value <- if (length(previous_row[[field]]) == 1) as.character(previous_row[[field]])[1] else NA

      if (!identical(current_value, previous_value)) {
        modified_record <- current_row
        modified_record$modified_field <- field
        modified_record$previous_value <- previous_value
        modified_record$current_value <- current_value

        modified_records <- rbind(modified_records, modified_record)
        break
      }
    }
  }

  is_changed <- nrow(new_records) > 0 || nrow(deleted_records) > 0 || nrow(modified_records) > 0

  # Create summary
  message("Changes detected for ", species_name, ":")
  message("  New records: ", nrow(new_records))
  message("  Modified records: ", nrow(modified_records))
  message("  Deleted records: ", nrow(deleted_records))

  return(list(
    is_changed = is_changed,
    new_records = nrow(new_records),
    modified_records = nrow(modified_records),
    deleted_records = nrow(deleted_records),
    new_records_data = new_records,
    modified_records_data = modified_records,
    deleted_records_data = deleted_records
  ))
}

# Function to log changes to CSV
log_changes <- function(changes) {
  message("Logging changes...")

  log_entry <- data.frame(
    date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    species = species_name,
    new_records = changes$new_records,
    modified_records = changes$modified_records,
    deleted_records = changes$deleted_records
  )

  if (file.exists(log_file)) {
    previous_log <- read.csv(log_file)
    updated_log <- rbind(previous_log, log_entry)
  } else {
    updated_log <- log_entry
  }

  write.csv(updated_log, log_file, row.names = FALSE)
  message("Changes logged to ", log_file)
}

# STEP 1: Initial Setup - Establish baseline data
setup_baseline <- function() {
  message("\n=== SETUP BASELINE FOR ", species_name, " ===\n")

  # Check if baseline already exists
  if (file.exists(current_file)) {
    message("Baseline data already exists at ", current_file)
    message("To create a new baseline, delete or rename this file first.")
    return(FALSE)
  }

  # Fetch initial data
  data <- fetch_gbif_data(species_key)

  if (is.null(data) || nrow(data) == 0) {
    message("Failed to fetch data. Setup aborted.")
    return(FALSE)
  }

  # Save as current data
  message("Saving baseline data...")
  saveRDS(data, current_file)

  # Verify file was saved
  if (file.exists(current_file)) {
    message("Baseline setup complete. Found ", nrow(data), " records.")
    message("Baseline data saved to: ", normalizePath(current_file))
    return(TRUE)
  } else {
    message("Failed to save baseline data.")
    return(FALSE)
  }
}

# STEP 2: Check for Updates
# STEP 2: Check for Updates
check_for_updates <- function() {
  message("\n=== CHECKING FOR UPDATES: ", species_name, " ===\n")

  # Verify baseline exists
  if (!file.exists(current_file)) {
    message("No baseline data found. Please run setup_baseline() first.")
    return(FALSE)
  }

  # Load the current (baseline) data
  message("Loading baseline data...")
  current_data <- readRDS(current_file)
  message("Loaded ", nrow(current_data), " records from baseline.")

  # Fetch latest data from GBIF
  new_data <- fetch_gbif_data(species_key)
  #extra <- new_data[1,]
  #extra$key[extra$key=="5063364019"] <- "5063364010"
  #new_data <- bind_rows(extra, new_data)

  if (is.null(new_data) || nrow(new_data) == 0) {
    message("Failed to fetch new data. Update check aborted.")
    return(FALSE)
  }

  # Compare data sets
  message("Comparing data sets...")
  changes <- compare_data(new_data, current_data)

  # If changes exist, update files and log
  if (changes$is_changed) {
    message("\nChanges detected! Updating data files...")

    # First copy current to previous
    file.copy(current_file, previous_file, overwrite = TRUE)
    message("Previous baseline backed up to: ", normalizePath(previous_file))

    # Save new data as current
    saveRDS(new_data, current_file)
    message("New data saved to: ", normalizePath(current_file))

    # Log the changes
    log_changes(changes)

    # Generate timestamp for file names
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Save new records
    if (changes$new_records > 0) {
      new_records_file <- file.path(data_dir, paste0("new_records_", timestamp, ".rds"))
      saveRDS(changes$new_records_data, new_records_file)

      # Also save as CSV for easier viewing
      new_records_csv <- file.path(data_dir, paste0("new_records_", timestamp, ".csv"))
      write.csv(changes$new_records_data, new_records_csv, row.names = FALSE)

      message("New records saved to: ", normalizePath(new_records_file), " and ", normalizePath(new_records_csv))
    }

    # Save modified records
    if (changes$modified_records > 0) {
      modified_records_file <- file.path(data_dir, paste0("modified_records_", timestamp, ".rds"))
      saveRDS(changes$modified_records_data, modified_records_file)

      # Also save as CSV for easier viewing
      modified_records_csv <- file.path(data_dir, paste0("modified_records_", timestamp, ".csv"))
      write.csv(changes$modified_records_data, modified_records_csv, row.names = FALSE)

      message("Modified records saved to: ", normalizePath(modified_records_file), " and ", normalizePath(modified_records_csv))
    }

    # Save deleted records
    if (changes$deleted_records > 0) {
      deleted_records_file <- file.path(data_dir, paste0("deleted_records_", timestamp, ".rds"))
      saveRDS(changes$deleted_records_data, deleted_records_file)

      # Also save as CSV for easier viewing
      deleted_records_csv <- file.path(data_dir, paste0("deleted_records_", timestamp, ".csv"))
      write.csv(changes$deleted_records_data, deleted_records_csv, row.names = FALSE)

      message("Deleted records saved to: ", normalizePath(deleted_records_file), " and ", normalizePath(deleted_records_csv))
    }

    return(TRUE)
  } else {
    message("\nNo changes detected in GBIF data for ", species_name)
    return(FALSE)
  }
}



