library(ggplot2)
library(stringr)
library(dplyr)


file_path <- "/cluster/scratch/zyuansheng/Hallow/"
setwd("/cluster/scratch/zyuansheng/Hallow/")



columns_to_modify<-c("baseline_nephrons","nom_Kf",
                     "nom_preafferent_arteriole_resistance",
                     "nom_afferent_diameter","nom_efferent_diameter",
                     "tubular_compliance",
                     "gamma",
                     "nom_water_intake","Na_intake_rate",
                     "glucose_concentration","nom_glucose_reabs_per_unit_length_s1","nom_glucose_reabs_per_unit_length_s2","nom_glucose_reabs_per_unit_length_s3")

columns_to_check<-c("CO","mean_arterial_pressure_MAP",
                    "LV_mass","EDV_h_wall","ejection_fraction",
                    "GFR_ml_min")

# List all CSV files starting with "Sens_" in the directory
file_list <- list.files(path = file_path, pattern = "^Sens_.*\\.csv$", full.names = TRUE)

# Define the function to read and modify data
read_modify <- function(file_name) {
  # Read the CSV file
  Sens1 <- read.csv(file_name)
  data<-Sens1[Sens1$x %in% c(1,24,24*7,24*30,24*365),]

  # seperate rowname into output and time
  df$output <- gsub("[0-9]", "", rownames(df))   # Remove digits
  df$time <- as.numeric(gsub("[^0-9]", "", rownames(df))  # Remove non-digits and convert to numeric
  df$time <- factor(df$time, levels = c("1 hour", "1 day", "1 week", "1 month", "1 year"))

  # Extract the last part of the file name after the last underscore
  data$source <- sub(".*_([^_]+)\\.csv$", "\\1", basename(file_name))
  
  # Return the modified dataframe
  return(data)
}

# Apply the function to each file and combine the results
data <- do.call(rbind, lapply(file_list, read_modify))

# View the combined dataframe
print(dim(data))


create_forest_plot <- function(data) {
  p <- ggplot(data, aes(x = Mean, y = reorder(interaction(source, time, sep = " - "), source), color = time)) +
    geom_point() +  # Points for means
    geom_errorbarh(aes(xmin = Mean - 1.96 * Sd, xmax = Mean + 1.96 * Sd), height = 0.2) +  # Error bars for CIs
    labs(x = "Mean Effect Size", y = "Source and Time") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8),
          strip.text.y = element_text(angle = 0, hjust = 1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    scale_color_brewer(palette = "Set1")  # Colors for different times
  return(p)
}


# Apply the plotting function by each output
output_plots <- lapply(split(df, df$output), create_forest_plot)

# If you need to print or inspect them separately


for (j in 1:len(columns_to_check)){
  pdf(file = paste0("ForestPlot_",columns_to_check[j],".pdf"),width = 12, height = 9 )
  # Create the forest plot
  print(output_plots[[j]])  # Example to print the first plot
  dev.off()
}

