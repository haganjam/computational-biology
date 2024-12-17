#' Clean and Process Seaweed Flies Experiment Data
#'
#' This function processes raw data from seaweed flies experiments, renaming variables, adding useful columns, and handling outliers.
#'
#' @param handle_outliers Character string specifying how to handle outliers. Options are:
#'   - "weight": Remove rows with outlier weights.
#'   - "scp": Remove rows with outlier SCP values.
#'   - "all": Remove rows with any type of outliers.
#'   - NA (default): Do not remove any outliers.
#'
#' @return A cleaned data frame with processed variables and optional outlier removal.
#' @export
#'
#' @examples
#' clean_data <- get_clean_data()
#' clean_data_no_outliers <- get_clean_data(handle_outliers = "all")
#' 
get_clean_data <- function(handle_outliers = NA) {
  
  # Load the raw data
  raw_data <- readr::read_csv(
    here::here("04-project-examples/seaweed-flies/raw-data/seaweed-flies-exp-raw-data.csv"),
    col_types = c("cnncn")
  )
  
  # Rename the variables
  raw_data <- raw_data |>
    dplyr::rename(
      experiment_n = Experiment,
      species_abb = Species,
      weight_mg = Weight,
      scp_id = SCP_ID,
      scp_c = SCP
    )
  
  # Add row_id and species_name variables
  raw_data <- raw_data |>
    dplyr::mutate(
      row_id = as.character(seq_len(nrow(raw_data))),
      species_name = dplyr::case_when(
        species_abb == "F" ~ "Coleopa frigida",
        TRUE ~ "Coleopa pilipes"
      )
    )
  
  # Remove scp_id, reorder variables
  raw_data <- raw_data |>
    dplyr::select(row_id, experiment_n, species_name, species_abb, weight_mg, scp_c)
  
  # Fix experiment_n
  raw_data <- raw_data |>
    dplyr::mutate(
      experiment_n = dplyr::case_when(
        experiment_n == "1" ~ "1",
        experiment_n == "3" ~ "2",
        experiment_n == "4" ~ "3",
        experiment_n == "5" ~ "4",
        experiment_n == "6" ~ "5",
        TRUE ~ experiment_n
      )
    )
  
  # Define outliers
  w_outliers <- c("19", "34", "35")
  scp_outliers <- c("17", "31")
  all_outliers <- unique(c(w_outliers, scp_outliers))
  
  # Handle outliers based on user input
  if (is.na(handle_outliers)) {
    
    return(raw_data)
    
  } else if (handle_outliers == "weight") {
    
    filtered_data <- raw_data |>
      dplyr::filter(!row_id %in% w_outliers)
    message("Removed weight outliers: ", paste(w_outliers, collapse = ", "))
    
  } else if (handle_outliers == "scp") {
    
    filtered_data <- raw_data |>
      dplyr::filter(!row_id %in% scp_outliers)
    message("Removed SCP outliers: ", paste(scp_outliers, collapse = ", "))
    
  } else if (handle_outliers == "all") {
    
    filtered_data <- raw_data |>
      dplyr::filter(!row_id %in% all_outliers)
    message("Removed all outliers: ", paste(all_outliers, collapse = ", "))
    
  } else {
    
    stop("Invalid handle_outliers option. Choose from 'weight', 'scp', 'all', or NA.")
    
  }
  
  return(filtered_data)
  
}

