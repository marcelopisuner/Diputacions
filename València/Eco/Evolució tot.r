##############################################
##Marcelo Pi-Suñer
##Evolució de la despesa econòmica diputacions
##############################################

rstudioapi::restartSession()

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(scales)



####Para guardar las visualizaciones
setwd('/Users/marceloppisuner/Documents/GitHub/Diputacions/València/Eco')





#####Clasificación por capítulos
# Define file path
file_paths <- list.files(path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Economica/Capitulo", 
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
    pivot_longer(cols = matches("^d\\.c\\.[1-9]$"), 
                 names_to = "Expenditure_Area", 
                 values_to = "Expenditure") %>% 
    replace_na(list(Expenditure = 0))
  
  return(df)
}

# Apply function to all datasets
datasets <- lapply(file_paths, process_file)

# Merge all datasets
merged_data <- bind_rows(datasets)

# Focus on the specific 'codente' value
data_subset <- merged_data %>% filter(codente == "46000DD000")

# Create graphs for each Expenditure_Area for codente 46000DD000
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
  ggsave(filename = paste0("val_e_plot_", str_extract(var, "\\d+$"), ".png"), plot = p, width = 8, height = 6)
}





# --- New Section: Individual plots for two-digit codes (d.c.xx) ---
file_paths_2digit <- list.files(
  path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Economica/Articulo",
  pattern = "*.csv",
  full.names = TRUE
)

process_file_2digit <- function(file_path) {
  year <- str_extract(basename(file_path), "\\d{4}")
  df <- read_csv2(file_path)
  df %>%
    mutate(Year = as.numeric(year)) %>%
    pivot_longer(
      cols = starts_with("d.c."),
      names_to = "Expenditure_Area",
      values_to = "Expenditure"
    ) %>%
    filter(str_detect(Expenditure_Area, "^d\\.c\\.[1-9][0-9]$")) %>% 
    replace_na(list(Expenditure = 0))
}

datasets_2digit <- lapply(file_paths_2digit, process_file_2digit)
merged_data_2digit <- bind_rows(datasets_2digit) %>% filter(codente == "46000DD000")
unique_vars_2digit <- unique(merged_data_2digit$Expenditure_Area)

for (var in unique_vars_2digit) {
  df_var <- merged_data_2digit %>% filter(Expenditure_Area == var)
  max_expenditure <- max(df_var$Expenditure, na.rm = TRUE) / 1000
  if (max_expenditure == 0) max_expenditure <- 1
  p <- ggplot(df_var, aes(x = Year, y = Expenditure / 1000)) +
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
  ggsave(filename = paste0("val_e_plot_", str_extract(var, "\\d+$"), ".png"), plot = p, width = 8, height = 6)
}







# --- New Section: Individual plots for three-digit codes (d.c.xxx) ---
file_paths_3digit <- list.files(
  path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Economica/Concepto",
  pattern = "*.csv",
  full.names = TRUE
)

process_file_3digit <- function(file_path) {
  year <- str_extract(basename(file_path), "\\d{4}")
  df <- read_csv2(file_path)
  df %>%
    mutate(Year = as.numeric(year)) %>%
    pivot_longer(
      cols = starts_with("d.c."),
      names_to = "Expenditure_Area",
      values_to = "Expenditure"
    ) %>%
    filter(str_detect(Expenditure_Area, "^d\\.c\\.[1-9][0-9]{2}$")) %>% 
    replace_na(list(Expenditure = 0))
}

datasets_3digit <- lapply(file_paths_3digit, process_file_3digit)
merged_data_3digit <- bind_rows(datasets_3digit) %>% filter(codente == "46000DD000")
unique_vars_3digit <- unique(merged_data_3digit$Expenditure_Area)

for (var in unique_vars_3digit) {
  df_var <- merged_data_3digit %>% filter(Expenditure_Area == var)
  max_expenditure <- max(df_var$Expenditure, na.rm = TRUE) / 1000
  if (max_expenditure == 0) max_expenditure <- 1
  p <- ggplot(df_var, aes(x = Year, y = Expenditure / 1000)) +
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
  ggsave(filename = paste0("val_e_plot_", str_extract(var, "\\d+$"), ".png"), plot = p, width = 8, height = 6)
}

# Generate data.js with list of all plot PNG filenames
png_files <- list.files(path = ".", pattern = "^plot_.*\\.png$")
js_lines <- c(
  "const images = [",
  paste0("  '", png_files, "'", collapse = ",\n"),
  "];",
  "export default images;"
)
writeLines(js_lines, "data.js")
