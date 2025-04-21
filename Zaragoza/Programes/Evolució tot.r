rstudioapi::restartSession()

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(scales)

setwd('/Users/marceloppisuner/Documents/GitHub/Diputacions/Zaragoza/Programes')

# Define file path
file_paths <- list.files(path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Programes/_Evolució ProgramesTot", 
                         pattern = "*.csv", full.names = TRUE)

# Function to read and process each CSV without changing the original variable names
process_file <- function(file_path) {
  # Extract the year from the file name (assuming it's a 4-digit number)
  year <- str_extract(basename(file_path), "\\d{4}")
  
  # Read the CSV without renaming columns
  df <- read_csv2(file_path)
  
  # Add the extracted year as a new column and convert from wide to long format
  df <- df %>%
    mutate(Year = as.numeric(year)) %>%
    pivot_longer(cols = matches("^X\\d{1,2}$"), 
                 names_to = "Expenditure_Area", 
                 values_to = "Expenditure")
  
  return(df)
}

# Apply function to all datasets
datasets <- lapply(file_paths, process_file)

# Merge all datasets
merged_data <- bind_rows(datasets)

# Focus on the specific 'codente' value
data_subset <- merged_data %>% filter(codente == "50000DD000")

# Create graphs for each Expenditure_Area for codente 50000DD000
unique_vars <- unique(data_subset$Expenditure_Area)

for(var in unique_vars) {
  df_var <- data_subset %>% filter(Expenditure_Area == var)
  max_expenditure <- max(df_var$Expenditure, na.rm = TRUE) / 1000
  if(max_expenditure == 0) {
    max_expenditure <- 1
  }
  p <- ggplot(df_var, aes(x = Year, y = Expenditure/1000)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = sort(unique(df_var$Year))) +
    scale_y_continuous(labels = scales::comma, limits = c(0, max_expenditure)) +
    labs(subtitle = "Evolució de la despesa",
         x = "Any",
         y = "Despesa (en milers d'euros)") +
    theme_bw(base_family = "serif") +
    theme(
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.line = element_line(color = "black"),
      legend.position = "none",
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  # Save the plot with a file name based on the Expenditure_Area
  number <- str_extract(var, "\\d{1,3}$")
  ggsave(filename = paste0("zar_p_plot_", number, ".png"), plot = p, width = 8, height = 6)
}



# Define file path for the new folder (_Evolució Grups)
file_paths <- list.files(path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Programes/_Evolució Grups", 
                         pattern = "*.csv", full.names = TRUE)

# Function to read and process each CSV without changing the original variable names
process_file <- function(file_path) {
  # Extract the year from the file name (assuming it's a 4-digit number)
  year <- str_extract(basename(file_path), "\\d{4}")
  
  # Read the CSV without renaming columns
  df <- read_csv2(file_path)
  
  # Add the extracted year as a new column and convert from wide to long format for columns matching "z_\\d{3}"
  df <- df %>%
    mutate(Year = as.numeric(year)) %>%
    pivot_longer(cols = matches("^z_\\d{3}$"), 
                 names_to = "Variable", 
                 values_to = "Value") %>%
    replace_na(list(Value = 0))  # Treat NAs as 0
  
  return(df)
}

# Apply function to all datasets
datasets <- lapply(file_paths, process_file)

# Merge all datasets
merged_data <- bind_rows(datasets)

# Focus on the specific 'codente' value (if necessary)
data_subset <- merged_data %>% filter(codente == "50000DD000")

# Create graphs for each variable of interest
unique_vars <- unique(data_subset$Variable)

for(var in unique_vars) {
  df_var <- data_subset %>% filter(Variable == var)
  max_value <- max(df_var$Value, na.rm = TRUE) / 1000
  if(max_value == 0) {
    max_value <- 1
  }
  p <- ggplot(df_var, aes(x = Year, y = Value/1000)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = sort(unique(df_var$Year))) +
    scale_y_continuous(labels = scales::comma, limits = c(0, max_value)) +
    labs(subtitle = "Evolució de la despesa",
         x = "Any",
         y = "Valor (en milers)") +
    theme_bw(base_family = "serif") +
    theme(
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      axis.line = element_line(color = "black"),
      legend.position = "none",
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  
  # Save the plot with a file name based on the variable name
  number <- str_extract(var, "\\d{1,3}$")
  ggsave(filename = paste0("zar_p_plot_", number, ".png"), plot = p, width = 8, height = 6)
}
