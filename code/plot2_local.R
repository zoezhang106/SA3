library(ggplot2)
library(dplyr)

data<-read.csv("/Users/yuanshengzhang/Downloads/data.csv")

data<-data[,c(2:ncol(data))]
data$time<-ifelse(data$x == 1,"1 hour",
                  ifelse(data$x == 24,"1 day",
                         ifelse(data$x == 168,"1 week",
                                ifelse(data$x == 720,"1 month","1 year"
                                ))))
data$time <- factor(data$time, levels = c("1 hour", "1 day", "1 week", "1 month"))
data<- distinct(data)
# data$source <- sub("Sens_", "", data$source)
# data$source <- substr(data$source, 1, regexpr("\\.", data$source)-1)

#data<-data[!grepl("*diameter*",data$source ),]
# data2<-data[grepl("nom_glucose_reabs_per_unit_length_s1",data$source ),]
# data2$source<- "nom_glucose_reabs_per_unit_length_s2"
# data<-rbind(data,data2)

data<-data[!grepl("*nom_glucose_reabs_per_unit_length_s*",data$source ),]
data<-data[!grepl("*tubular_compliance*",data$source ),]
data<-data[!grepl("*intake*",data$source ),]
data<-data[!grepl("*gamma*",data$source ),]
data<-data[!grepl("*glucose*",data$source ),]



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

table(data$time)
data$time<-as.factor(data$time)
data$time<-factor(data$time, levels = c("1 month","1 week","1 day","1 hour"))

levels(data$time) <- rev(levels(data$time))

create_forest_plot <- function(data) {
  p <- ggplot(data, aes(x = Mean, y = (interaction(source, time, sep = " - ", source, lex.order=TRUE)), color = time)) +
    geom_point() +  # Points for means
    geom_errorbarh(aes(xmin = Mean - 1.96 * Sd, xmax = Mean + 1.96 * Sd), height = 0.1) +  # Error bars for CIs
    labs(x = "Mean Effect Size", y = "Source") +
    #theme_minimal() +
    theme(panel.background = element_rect(fill='transparent'), 
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = element_line(color = "grey", size = 0.5), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'),
          axis.text.y = element_text(size = 5),#
          strip.text.y = element_text(angle = 0, hjust = 1),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    scale_color_brewer(palette = "Set1")  # Colors for different times
  return(p)
}


# Apply the plotting function by each output
output_plots <- lapply(split(data, data$output), create_forest_plot)
output_plots[[1]]#co
output_plots[[2]]#mean_arterial_pressure_MAP
output_plots[[3]]#mass
output_plots[[4]]#EDV_h_wall
output_plots[[5]]#ejection_fraction
output_plots[[6]]#GFR_ml_min
# If you need to print or inspect them separately


for (j in 1:length(columns_to_check)){
  pdf(file = paste0("ForestPlot_",columns_to_check[j],".pdf"),width = 12, height = 9 )
  # Create the forest plot
  print(output_plots[[j]])  # Example to print the first plot
  dev.off()
}


