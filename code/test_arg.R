#!/usr/bin/Rscript

# Capturing arguments passed from the command line
args <- commandArgs(trailingOnly = TRUE)

# Assigning the arguments to variables
theta_change <- args[1]
scale <- as.numeric(args[2])

# Rest of your script here
# Example: Printing out the variables
print(paste("Theta Change: ", theta_change))
print(paste("Scale: ", scale))


#Load Libraries
library(rxode2)
####Load model structure
setwd("/cluster/scratch/zyuansheng/Hallow/")

source("modelfile_commented.R")

#Compile model
cvrsim <- RxODE(model = ode)

#Load Helper files
source("calcEndTime.R")
source("calcOutputs.R")

# #load basecase parameters
source("calcNomParams_timescale.R")
theta=calcNomParams()

#load basecase parameters
# source("getInits.R")
# inits=getInits()
load("inits_stable.RData")
#print(inits)

########################################## test #1 #######################################################
#When the system is perturbed, do cardiac output and sodium balance stabilize reasonably quickly and 
#with sufficient damping?
##########################################################################################################

endtime = calcEndTime(as.numeric(theta["HR_heart_rate"]), 24*365)
time0 <- Sys.time()
t = seq(0, endtime , .01)
ev = eventTable()
ev$add.sampling(t)
#print(length(t))

####Turn feedbacks back on by reloading parameters. All should now be at 1. 
#Reload parameters
# theta=calcNomParams()

#Increase cardiac output setpoint from 5 to 5.5 L and test response
# theta_change = "baseline_nephrons"
# scale = 0.1
theta[theta_change] = theta[theta_change]*scale
# theta["CO_nom"] = 5.5
#1 theta["baseline_nephrons"] = 2e6*0.1
#2 theta["tubular_compliance"] = 0.2#*0.1
#3 theta["nom_preafferent_arteriole_resistance"] = 14#*0.5
#4 theta["nom_Kf"] = 3.9#*0.5
#5 glucose_concentration = 5.5  #mmol/L
#6 Na_intake_rate=100/24/60   #mEq/min  - 100mmol/day or 2300 mg/day
#7 nom_water_intake = 2.1     #L/day
#8 nom_afferent_diameter=1.65e-5      #mmHg
#9 nom_efferent_diameter=1.1e-05    #mmHg
#10 gamma =  1.16667e-5*2;  #  viscosity of tubular fluid
print(theta[theta_change])

time1 <- Sys.time()
print("before change")
print(getRxThreads(verbose = FALSE))
#print(rxCores())
setRxThreads(threads = 9)
print("after change")
print(getRxThreads(verbose = FALSE))
#print(rxCores())
#x <- cvrsim$run(theta, ev, inits=inits)
x <- solve(cvrsim, theta, ev, inits=inits, cores=9, atol = 1e-6, rtol = 1e-4)
x = calcOutputs(x)
#Calculate outputs not directly calculated in model file
time2 <- Sys.time()
write.csv(x,paste0(theta_change,"_",scale,"_solution.csv"))

time3 <- Sys.time()
print(time1-time0)
print(time2-time1)
print(time3-time2)
# check out the pressure, volume, structure and function

########################################## Check #2 #######################################################
#Some variables have parameters that specify their setpoint under normal conditions. Are these variables at
#or very close to their setpoints?
##########################################################################################################
x <- read.csv(paste0(theta_change,"_",scale,"_solution.csv"))

compareWithNoms <- function(varname, nomval, range){
  nomval = as.numeric(theta[nomval])
  vallims= c(nomval*(1-range), nomval*(1+range)) # for red area +- range you set
  #minmax = c(nomval*(1-5*range), nomval*(1+5*range)) # for ylim
  minmax = c(min(min(x[,varname]),nomval)*(1-5*range), max(max(x[,varname]),nomval)*(1+5*range)) # for ylim
  print(varname)
  print(x[dim(x)[1],varname])
  print(x[1,varname])
  print(minmax)
  
  ggplot(x, aes(x = time / 24, y = .data[[varname]])) +
    geom_line(lwd = 2) +  # Set line width to 2
    annotate("rect", xmin = 0, xmax = max(x[,"time"]) / 24, ymin = vallims[1], ymax = vallims[2], 
             fill = rgb(1, 0, 0, 0.2), colour = NA) + 
    geom_hline(yintercept = nomval, color = "gray", linetype = 2, linewidth = 2) +  # Add a horizontal line
    labs(title = varname, x = "", y = "")+  # Set titles and axis labels
    ylim(minmax) +
    scale_x_continuous(limits = c(0, max(x[,"time"]) / 24), expand = c(0, 0)) + # xlim(0,max(x[,"time"]) / 24)+
    theme(plot.title = element_text(hjust = 0.5,size = 20#, face = "bold"
                                    ),
          #panel.background = element_rect(fill='transparent'), #transparent panel bg
          panel.background = element_rect(colour = "black", size=1, fill=NA),
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent'),
          axis.line = element_line(colour = "black"), 
          axis.text.x = element_text(size = 20),  # Font size for x-axis numbers
          axis.text.y = element_text(size = 20),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
          
}

pdf(file = paste0(theta_change,"_",scale,"_Fig1.pdf"),width = 12, height = 9 )

p1 <- compareWithNoms("CO_delayed", "CO_nom", 0.01)
p2 <- compareWithNoms("mean_arterial_pressure_MAP", "nominal_map_setpoint", 0.025)
p3 <- compareWithNoms("blood_volume_L", "blood_volume_nom", 0.025)
p4 <- compareWithNoms("interstitial_fluid_volume", "IF_nom", 0.025)
p5 <- compareWithNoms("renal_blood_flow_L_min", "nom_renal_blood_flow_L_min", 0.025)
p6 <- compareWithNoms("GFR_ml_min", "nom_GFR", 0.025)
p7 <- compareWithNoms("Na_concentration","ref_Na_concentration", 0.01)
p8 <- compareWithNoms("AT1_bound_AngII", "nominal_equilibrium_AT1_bound_AngII", 0.025)
p9 <- compareWithNoms("aldosterone_concentration", "nominal_aldosterone_concentration", 0.025)

# Combine the plots with patchwork
grid_layout <- p1 + p2 + p3 + 
  p4 + p5 + p6 + 
  p7 + p8 + p9 + 
  plot_layout(ncol = 3)  # Arrange in 3 columns

# Print the grid layout
print(grid_layout)
dev.off()

########################################## Check #3 #######################################################
#Some variables do not have setpoints, but have a known physiological range in healthy subjects
#Are these variables within that normal range?
##########################################################################################################


compareWithRange <- function(A, i, range){
  
  vallims= c(A$Min[i], A$Max[i])
  #minmax = c(A$Min[i]*(1-range), A$Max[i]*(1+range))

  varname = as.character(A$Variable[i])
  #minmax = c(min(x[,varname]*A$UnitScale[i])*(1-5*range), max(x[,varname]*A$UnitScale[i])*(1+5*range)) # for ylim
  minmax = c(min(min(x[,varname]*A$UnitScale[i]),A$Min[i])*(1-5*range), max(min(x[,varname]*A$UnitScale[i]),A$Max[i])*(1+5*range))
  print(varname)
  print(x[dim(x)[1],varname]*A$UnitScale[i])
  print(x[1,varname])
  print(minmax)
  plot(x[,"time"]/24,x[,varname]*A$UnitScale[i], type="l", main = varname, ylim = minmax, lwd=2,ylab=A$Units[i], xlab="Time (days)")
  polygon(c(0, max(x[,"time"]), max(x[,"time"]),0)/24, c(vallims[1], vallims[1], vallims[2], vallims[2]), col=rgb(1, 0, 0, 0.2), border= NA)
}


A = read.table("VariableCheckFile.csv",header=T, sep=",")
Nplots = dim(A)[1]
ncols = 6
nrows = ceiling(Nplots/ncols)

# dev.new()
pdf(file = paste0(theta_change,"_",scale,"_Fig2.pdf"),width = 12, height = 9 )
par(mfrow=c(nrows, ncols))

for (i in 1:Nplots) {
  compareWithRange(A,i,0.125)
  
}
dev.off()


########################################## Check #4 #######################################################
#When feedbacks are turned on, under the baseline steady state conditions, these feedbacks should all be at
#their operating point. They should only move away from zero when the system is perturbed from equilibrium
#Are all feedbacks at their operating point?
##########################################################################################################

compareWithOne <- function(varname, range, new_title){
  nomval = 1
  vallims= c(nomval*(1-range), nomval*(1+range))
  #minmax = c(nomval*(1-5*range), nomval*(1+5*range))
  minmax = c(min(min(x[,varname],nomval)*(1-5*range)), 
             max(max(x[,varname],nomval)*(1+5*range))) # for ylim
  print(varname)
  print(x[dim(x)[1],varname])
  print(x[1,varname])
  print(minmax)
  
  ggplot(x, aes(x = time / 24, y = .data[[varname]])) +
    geom_line(lwd = 2) +  # Set line width to 2
    annotate("rect", xmin = 0, xmax = max(x[,"time"]) / 24, ymin = vallims[1], ymax = vallims[2], 
             fill = rgb(1, 0, 0, 0.2), colour = NA) + 
    geom_hline(yintercept = nomval, color = "gray", linetype = 2, linewidth = 2) +  # Add a horizontal line
    labs(title = new_title, x = "", y = "normalized")+  # Set titles and axis labels
    ylim(minmax) +
    scale_x_continuous(limits = c(0, max(x[,"time"]) / 24), expand = c(0, 0)) + # xlim(0,max(x[,"time"]) / 24)+
    theme(plot.title = element_text(hjust = 0.5,size = 20#, face = "bold"
    ),
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size = 20),  # Font size for x-axis numbers
    axis.text.y = element_text(size = 20),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
  
  
  # plot(x[,"time"]/24,x[,varname], type="l", main = "", ylim = minmax, lwd=2, xlab = "time", ylab = "normalized")
  # polygon(c(0, max(x[,"time"]), max(x[,"time"]),0)/24, c(vallims[1], vallims[1], vallims[2], vallims[2]), col=rgb(1, 0, 0, 0.2), border= NA)
  # points(c(0, max(x[,"time"]))/24, c(nomval, nomval), type="l", col="gray", lty = 3, lwd=3)
}


compareWithZero <- function(varname, range, new_title){
  nomval = 0
  vallims= c(0-range, 0+range)
  #minmax = c(0-5*range, 0+5*range)
  minmax = c(min(min(x[,varname])*(1-5*range),0-5*range), 
             max(max(x[,varname])*(1+5*range),0+5*range)) # for ylim
  print(varname)
  print(x[dim(x)[1],varname])
  print(x[1,varname])
  print(minmax)
  
  ggplot(x, aes(x = time / 24, y = .data[[varname]])) +
    geom_line(lwd = 2) +  # Set line width to 2
    annotate("rect", xmin = 0, xmax = max(x[,"time"]) / 24, ymin = vallims[1], ymax = vallims[2], 
             fill = rgb(1, 0, 0, 0.2), colour = NA) + 
    geom_hline(yintercept = nomval, color = "gray", linetype = 2, linewidth = 2) +  # Add a horizontal line
    labs(title = new_title, x = "", y = "normalized")+  # Set titles and axis labels
    ylim(minmax) +
    scale_x_continuous(limits = c(0, max(x[,"time"]) / 24), expand = c(0, 0)) + # xlim(0,max(x[,"time"]) / 24)+
    theme(plot.title = element_text(hjust = 0.5,size = 20#, face = "bold"
    ),
    #panel.background = element_rect(fill='transparent'), #transparent panel bg
    panel.background = element_rect(colour = "black", size=1, fill=NA),
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.line = element_line(colour = "black"), 
    axis.text.x = element_text(size = 20),  # Font size for x-axis numbers
    axis.text.y = element_text(size = 20),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
  # plot(x[,"time"]/24,x[,varname], type="l", main = "", ylim = minmax, lwd=2, xlab = "time", ylab = "microns")
  # polygon(c(0, max(x[,"time"]), max(x[,"time"]),0)/24, c(vallims[1], vallims[1], vallims[2], vallims[2]), col=rgb(1, 0, 0, 0.2), border= NA)
  # points(c(0, max(x[,"time"]))/24, c(nomval, nomval), type="l", col="gray", lty = 3, lwd=3)
}

pdf(file = paste0(theta_change,"_",scale,"_Fig3.pdf"),width = 12, height = 9 )

p1 <- compareWithOne("tissue_autoregulation_signal", 0.05, "CO - TPR controller")
p2 <- compareWithOne("tubulo_glomerular_feedback_effect", 0.05, "Tubuloglomerular \n Feedback Signal")
p3 <- compareWithOne("preafferent_pressure_autoreg_signal", 0.05, "Myogenic autoregulation \n signal")
p4 <- compareWithOne("pressure_natriuresis_PT_effect", 0.05, "Pressure Natriuresis Signal")
p5 <- compareWithOne("normalized_aldosterone_level", 0.05, "Vasopressin signal")
p6 <- compareWithOne("AT1_bound_AngII_effect_on_PRA", 0.05,"At1-bound AngII \nfeedback on PRA")
p7 <- compareWithOne("aldo_effect_on_CD", 0.05,"Aldosterone effect \non Na+ reabsorption")

p8 <- compareWithZero("change_in_myocyte_diameter", 2e-7,"Myocyte hypertrophy\nDiameter change")
p9 <- compareWithZero("change_in_myocyte_length", 2e-7,"Myocyte Hypertrophy\nLength Change")

# Combine the plots with patchwork
grid_layout <- p1 + p2 + p3 + 
  p4 + p5 + p6 + 
  p7 + p8 + p9 + 
  plot_layout(ncol = 3)  # Arrange in 3 columns

# Print the grid layout
print(grid_layout)
dev.off()

