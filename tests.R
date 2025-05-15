# Test function for GBIF species monitor
test_monitor_functions <- function() {
  message("\n=== TESTING GBIF SPECIES MONITOR FUNCTIONS ===\n")

  # Create a backup of original configuration
  original_species_key <- species_key
  original_species_name <- species_name
  original_data_dir <- data_dir
  original_current_file <- current_file
  original_previous_file <- previous_file
  original_log_file <- log_file

  # Set up test environment
  test_dir <- file.path(tempdir(), "gbif_test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  # Override global variables for testing
  species_key <- 123456
  species_name <- "Test Species"
  data_dir <- test_dir
  current_file <- file.path(test_dir, "current_data.rds")
  previous_file <- file.path(test_dir, "previous_data.rds")
  log_file <- file.path(test_dir, "changes_log.csv")

  # Generate simulated data
  create_test_data <- function(n_records = 100) {
    data.frame(
      key = 1:n_records,
      scientificName = rep("Test Species", n_records),
      decimalLatitude = runif(n_records, -90, 90),
      decimalLongitude = runif(n_records, -180, 180),
      country = sample(c("USA", "Canada", "Mexico", "Brazil", "France"), n_records, replace = TRUE),
      locality = paste("Locality", 1:n_records),
      eventDate = format(as.Date("2023-01-01") + sample(0:365, n_records, replace = TRUE), "%Y-%m-%d"),
      basisOfRecord = sample(c("PRESERVED_SPECIMEN", "HUMAN_OBSERVATION"), n_records, replace = TRUE),
      coordinateUncertaintyInMeters = sample(c(10, 100, 1000, NA), n_records, replace = TRUE),
      issues = rep("", n_records)
    )
  }

  # Create initial data
  original_data <- create_test_data(100)
  saveRDS(original_data, current_file)

  test_results <- list()

  # TEST CASE 1: No changes
  message("\nTEST CASE 1: No changes")
  saveRDS(original_data, previous_file)  # Previous = Current
  changes <- compare_data(original_data, original_data)
  test_results[["no_changes"]] <- list(
    expected = c(new = 0, modified = 0, deleted = 0),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: No changes")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["no_changes"]]$expected == test_results[["no_changes"]]$actual))

  # TEST CASE 2: New records only
  message("\nTEST CASE 2: New records")
  previous_data <- original_data[1:90, ]  # Keep only first 90 records
  saveRDS(previous_data, previous_file)
  changes <- compare_data(original_data, previous_data)
  test_results[["new_only"]] <- list(
    expected = c(new = 10, modified = 0, deleted = 0),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: 10 new records")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["new_only"]]$expected == test_results[["new_only"]]$actual))

  # TEST CASE 3: Deleted records only
  message("\nTEST CASE 3: Deleted records")
  current_data <- original_data[1:90, ]  # Remove last 10 records
  saveRDS(original_data, previous_file)
  changes <- compare_data(current_data, original_data)
  test_results[["deleted_only"]] <- list(
    expected = c(new = 0, modified = 0, deleted = 10),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: 10 deleted records")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["deleted_only"]]$expected == test_results[["deleted_only"]]$actual))

  # TEST CASE 4: Modified records only
  message("\nTEST CASE 4: Modified records")
  modified_data <- original_data
  # Modify latitude for 10 records
  modified_indices <- 41:50
  modified_data$decimalLatitude[modified_indices] <- modified_data$decimalLatitude[modified_indices] + 1
  saveRDS(original_data, previous_file)
  changes <- compare_data(modified_data, original_data)
  test_results[["modified_only"]] <- list(
    expected = c(new = 0, modified = 10, deleted = 0),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: 10 modified records")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["modified_only"]]$expected == test_results[["modified_only"]]$actual))

  # TEST CASE 5: Mixed changes
  message("\nTEST CASE 5: Mixed changes (new, modified, deleted)")
  # Start with original data
  mixed_data <- original_data
  # Remove records 1-10
  mixed_data <- mixed_data[-(1:10), ]
  # Add 15 new records (keys 101-115)
  new_records <- create_test_data(15)
  new_records$key <- 101:115
  mixed_data <- rbind(mixed_data, new_records)
  # Modify 8 records (keys 51-58)
  modified_indices <- which(mixed_data$key %in% 51:58)
  mixed_data$country[modified_indices] <- "Australia"

  saveRDS(original_data, previous_file)
  changes <- compare_data(mixed_data, original_data)
  test_results[["mixed_changes"]] <- list(
    expected = c(new = 15, modified = 8, deleted = 10),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: 15 new, 8 modified, 10 deleted records")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["mixed_changes"]]$expected == test_results[["mixed_changes"]]$actual))

  # TEST CASE 6: Edge case - Same key in both current and previous but different data
  message("\nTEST CASE 6: Edge case - Same key with completely different data")
  current_data <- data.frame(
    key = 1:5,
    scientificName = rep("Test Species", 5),
    decimalLatitude = runif(5, -90, 90),
    decimalLongitude = runif(5, -180, 180),
    country = rep("USA", 5),
    locality = paste("New Locality", 1:5),
    eventDate = rep("2023-01-01", 5),
    basisOfRecord = rep("HUMAN_OBSERVATION", 5),
    coordinateUncertaintyInMeters = rep(10, 5),
    issues = rep("", 5)
  )

  previous_data <- data.frame(
    key = 1:5,
    scientificName = rep("Test Species", 5),
    decimalLatitude = runif(5, -90, 90),  # Different coordinates
    decimalLongitude = runif(5, -180, 180),
    country = rep("Canada", 5),  # Different country
    locality = paste("Old Locality", 1:5),  # Different locality
    eventDate = rep("2022-01-01", 5),  # Different date
    basisOfRecord = rep("PRESERVED_SPECIMEN", 5),  # Different basis
    coordinateUncertaintyInMeters = rep(1000, 5),  # Different uncertainty
    issues = rep("", 5)
  )

  saveRDS(previous_data, previous_file)
  changes <- compare_data(current_data, previous_data)
  test_results[["edge_case"]] <- list(
    expected = c(new = 0, modified = 5, deleted = 0),
    actual = c(new = changes$new_records, modified = changes$modified_records, deleted = changes$deleted_records)
  )
  message("  Expected: 5 modified records")
  message("  Actual: New=", changes$new_records, ", Modified=", changes$modified_records, ", Deleted=", changes$deleted_records)
  message("  Test passed: ", all(test_results[["edge_case"]]$expected == test_results[["edge_case"]]$actual))

  # Clean up
  unlink(test_dir, recursive = TRUE)

  # Restore original configuration
  species_key <- original_species_key
  species_name <- original_species_name
  data_dir <- original_data_dir
  current_file <- original_current_file
  previous_file <- original_previous_file
  log_file <- original_log_file

  # Print summary
  message("\n=== TEST SUMMARY ===")
  all_passed <- TRUE
  for (test_name in names(test_results)) {
    passed <- all(test_results[[test_name]]$expected == test_results[[test_name]]$actual)
    message(sprintf("%-20s: %s", test_name, ifelse(passed, "PASSED", "FAILED")))
    if (!passed) all_passed <- FALSE
  }

  message("\nAll tests passed: ", all_passed)

  return(all_passed)
}
