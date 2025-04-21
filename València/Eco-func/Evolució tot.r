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
library(readxl)

####Para guardar las visualizaciones
setwd('/Users/marceloppisuner/Documents/GitHub/Diputacions/Barcelona/Eco-func')

#####Clasificación por capítulos
# Define file path
file_paths <- list.files(
  path = "/Users/marceloppisuner/Documents/b) Trabajo/Fundació Pi i Sunyer/Dades/3.Dades diputacions/1.Evolució de la despesa/Eco-funcional",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Function to read and process each CSV without changing the original variable names
process_file <- function(file_path) {
  year <- str_extract(basename(file_path), "\\d{4}")
  df <- readxl::read_excel(file_path)
  df %>%
    mutate(Year = as.numeric(year)) %>%
    pivot_longer(
      cols = matches("^[1-9]_[0-9]{3}$"),
      names_to = "Expenditure_Area",
      values_to = "Expenditure"
    ) %>%
    replace_na(list(Expenditure = 0))
}

# Apply function to all datasets
datasets <- lapply(file_paths, process_file)

# Merge all datasets
merged_data <- bind_rows(datasets)

# Determine complete set of years
all_years <- sort(unique(merged_data$Year))
 
# Extract the three-digit suffixes from existing area codes
suffixes <- merged_data %>%
  pull(Expenditure_Area) %>%
  sub("^[1-9]_(\\d{3})$", "\\1", .) %>%
  unique() %>%
  sort()
 
# Define the required prefix digits (1–4 and 6–9)
prefixes <- c("1", "2", "3", "4", "6", "7", "8", "9")
 
# Combine each prefix with each suffix to form the full list of area codes
all_vars <- as.vector(outer(prefixes, suffixes, paste, sep = "_"))

# Build a template of every codente–area–year combination
template <- tidyr::expand_grid(
  codente = "08000DD000",
  Expenditure_Area = all_vars,
  Year = all_years
)

# Subset and then join to the full grid, filling missing expenditures with zero
data_subset <- merged_data %>%
  filter(codente == "08000DD000") %>%
  right_join(template, by = c("codente", "Expenditure_Area", "Year")) %>%
  replace_na(list(Expenditure = 0))

# Create graphs for each Expenditure_Area for codente 08000DD000
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
  ggsave(
    filename = paste0(
      "bcn_ef_plot_",
      str_replace(var, "^(\\d)_(\\d{3})$", "\\2_\\1"),
      ".png"
    ),
    plot = p,
    width = 8,
    height = 6
  )
}

# ------------------------------------------------------------------
# Generate data.js with the list of all plot PNG filenames
png_files <- list.files(path = getwd(), pattern = "^bcn_ef_plot_.*\\.png$", full.names = FALSE)
plotFiles_js <- paste0(
  "const plotFiles = [",
  paste(sprintf('\"%s\"', png_files), collapse = ", "),
  "];\n"
)
writeLines(plotFiles_js, file.path(getwd(), "data.js"))
# ------------------------------------------------------------------
