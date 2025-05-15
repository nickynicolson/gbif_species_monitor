# run scripts

# TODO - add reports to show new points
# TODO - add scheduler to check regularly
# TODO - add analysis to show change between versions - e.g. EOO and AOO, # collections
# TODO - figure out how to maintain the top set e.g. flag records you want to use or exclude
# TODO - shiny app? check what sredlist have done with map and new gbif points
# TODO - name it? red_alert?

# Usage instructions
message("===== GBIF SPECIES MONITOR =====")
message("Three functions are available:")
message("1. setup_baseline() - Run first to establish baseline data")
message("2. check_for_updates() - Run periodically to check for changes")
message("3. test_monitor_functions() - Run periodically to check for consistency")

# source functions
source("fetch_compare.R")
source("tests.R")

# Configuration
species_key <- 7155386  # Dypsis decipiens (update this for your species)
species_name <-  "Pachypodium rosulatum"
data_dir <- "data"
current_file <- file.path(data_dir, "current_data.rds")
previous_file <- file.path(data_dir, "previous_data.rds")
log_file <- file.path(data_dir, "changes_log.csv")

# make config file
config <- setup_species_config(species_key, species_name)

# Make the configuration variables available in the global environment
species_key <- config$species_key
species_name <- config$species_name
data_dir <- config$data_dir
current_file <- config$current_file
previous_file <- config$previous_file
log_file <- config$log_fi

# run the scripts
#setup_baseline()
check_for_updates()

#generate_change_report(
#  species_name = "Trachelophorus giraffa",
#  current_file = "data/Trachelophorus_giraffa/current_data.rds",
#  data_dir = "data/Trachelophorus_giraffa"
#)

test_monitor_functions()

# species list
# Pachypodium rosulatum # 7155386
# Adansonia rubrostipa # 5663140
# Trachelophorus giraffa # 1173257

