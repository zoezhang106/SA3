library(FME)
library(deSolve)
library(rxode2)

# library(dplyr)

####Load model structure
# Capturing arguments passed from the command line
args <- commandArgs(trailingOnly = TRUE)

#Assigning the arguments to variables
id <- args[1]
id = as.numeric(id)
print(id)
days = 30
number = 50

setwd("/cluster/scratch/zyuansheng/Hallow/")


source("utilities.R")
source("senRange_v2.R")
source("modelfile_commented.R")

#Compile model
cvrsim <- RxODE(model = ode)

#Load Helper files
source("calcEndTime.R")
source("calcOutputs.R")

#load basecase parameters
source("calcNomParams_timescale.R")
theta <- calcNomParams()

#load basecase parameters
# source("getInits.R")
# inits <- getInits()
load("inits_stable.RData")
inits <- unlist(inits)


################################### Set time intervals ####################################
##########################################################################################################

endtime = calcEndTime(as.numeric(theta["HR_heart_rate"]), 24*days)
# endtime = calcEndTime(as.numeric(theta["HR_heart_rate"]), 24*365)
t = seq(0, endtime , .01)# time granularity 0.1s
ev <- eventTable()
ev$add.sampling(t)

############################# Run to initial steady-state with no feedbacks ##########################################
##########################################################################################################


solveCvrsim <- function(theta) {
  
  ## ode solves the model by integration ...
  #x = cvrsim$run(theta, ev, inits=inits, atol = 1e-6, rtol = 1e-4)
  x = solve(cvrsim, theta, ev, inits=inits, cores=9, atol = 1e-6, rtol = 1e-4)
  x = calcOutputs(x)
  return(as.data.frame(x))
}


# ## the sensitivity parameters
# what to change
# columns_to_modify<-c("baseline_nephrons","nom_Kf")
columns_to_modify<-c("baseline_nephrons","nom_Kf",
                     "nom_preafferent_arteriole_resistance",
                     "nom_afferent_diameter","nom_efferent_diameter",
                     "tubular_compliance",
                     "gamma",
                     "nom_water_intake","Na_intake_rate",
                     "glucose_concentration","nom_glucose_reabs_per_unit_length_s1","nom_glucose_reabs_per_unit_length_s2","nom_glucose_reabs_per_unit_length_s3")
# what to check and plot
columns_to_check<-c("CO","mean_arterial_pressure_MAP",
                    "LV_mass","EDV_h_wall","ejection_fraction",
                    "GFR_ml_min")

theta_Ranges <- as.data.frame(data.frame(min = c(as.numeric(theta[,columns_to_modify] * 0.990)), 
                                         max = c(as.numeric(theta[,columns_to_modify] * 1.010))))

rownames(theta_Ranges)<- columns_to_modify
theta_Ranges

print("start")
SensR1 <- sensRange(func = solveCvrsim, parms = theta, dist = "grid",
                    sensvar = columns_to_check, parRange = theta_Ranges[id,], 
                    num = number)#the number of times the model has to be run = the number of random parameter sets to generate
print(dim(SensR1))
write.csv(SensR1,paste0("SensR_",rownames(theta_Ranges)[id],".csv"))
Sens1  <-summary(SensR1)
write.csv(Sens1,paste0("Sens_",rownames(theta_Ranges)[id],".csv"))
print("finish Sens1")
print(dim(Sens1))

# Grid(theta_Ranges, 50)
# pairs(Grid(theta_Ranges, 500), main = "Grid")
# The number of parameter sets generated with Grid
# library(metafor)

# for (i in 1:nrow(theta_Ranges)){
#     ## sensitivity to one constant parameter; equally-spaced parameters ("grid")
#     print("start")
#     SensR1 <- sensRange(func = solveCvrsim, parms = theta, dist = "grid",
#                         sensvar = columns_to_check, parRange = theta_Ranges[i,], 
#                         num = 50)#the number of times the model has to be run
#     print(dim(SensR1))
#     write.csv(SensR1,paste0("SensR_",rownames(theta_Ranges)[i],".csv"))
#     Sens1  <-summary(SensR1)
#     print("finish Sens1")
#     print(dim(Sens1))

#     pattern <- paste(columns_to_check, collapse = "|")

#     data<-Sens1[Sens1$x %in% c(1,24) & grepl(pattern, rownames(Sens1)),]
#     #data<-Sens1[Sens1$x %in% c(1,24,24*7,24*30,24*365) & grepl(j, rownames(Sens1)),]
#     rownames(data)<-c("1 hour","1 day")#,"1 week","1 month","1 year")

#     # plot(Sens1,which = "CO", legpos = "bottomleft", xlab = "time (s)", ylab = "mL",
#     #      main = "nom_peritubular_resistance CO", mfrow = NULL,col = c("red", "blue"))
#     # plot(Sens1, which = "mean_arterial_pressure_MAP",legpos = "bottomright", xlab = "time (s)", ylab = "mL",
#     #      main = "mean_arterial_pressure_MAP", mfrow = NULL,col = c("red", "blue"))


#     # for (j in columns_to_check){
#     #     print(j)
#         # # Convert mean and sd to log odds and its variance (required for forest plot)
#         # pattern <- paste(columns_to_check, collapse = "|")

#         # data<-Sens1[Sens1$x %in% c(1,24) & grepl(pattern, rownames(Sens1)),]
#         # #data<-Sens1[Sens1$x %in% c(1,24,24*7,24*30,24*365) & grepl(j, rownames(Sens1)),]
#         # rownames(data)<-c("1 hour","1 day")#,"1 week","1 month","1 year")

#         # # Convert mean and sd to log odds and its variance (required for forest plot)
#         # data$var <- (data$Sd / data$Mean) ^ 2

#         # # Define colors for each time interval
#         # colors <- c("red", "blue")#, "green", "orange", "purple")


#         # pdf(file = paste0("ForestPlot_",rownames(theta_Ranges)[i],"_",j,".pdf"),width = 12, height = 9 )
#         # # Create the forest plot
#         # forest(data$Mean, sei=sqrt(data$var), slab=data$x, xlab="Mean", 
#         #        alim=c(min(data$Mean) - 1, max(data$Mean) + 1), col=colors)

#         # dev.off()
#     # }
# }



#         # Convert mean and sd to log odds and its variance (required for forest plot)
#         data$var <- (data$Sd / data$Mean) ^ 2

#         # Define colors for each time interval
#         colors <- c("red", "blue")#, "green", "orange", "purple")


#         pdf(file = paste0("ForestPlot_",rownames(theta_Ranges)[i],"_",j,".pdf"),width = 12, height = 9 )
#         # Create the forest plot
#         forest(data$Mean, sei=sqrt(data$var), slab=data$x, xlab="Mean", 
#                alim=c(min(data$Mean) - 1, max(data$Mean) + 1), col=colors)

#         dev.off()







