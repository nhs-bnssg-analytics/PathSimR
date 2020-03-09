# install.packages("shiny")
# install.packages("DiagrammeR")
# install.packages("magrittr")
# install.packages("readr")
# install.packages("DT")
# install.packages("openxlsx")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("parallel")
# install.packages("data.table")
# install.packages("tidyverse")
# install.packages("shinyalert")
# install.packages("shinyMatrix")
# install.packages("fitdistrplus")
# install.packages("shinyBS")
# install.packages("shinyjs")
# install.packages("shinythemes")

library(shiny)
library(DiagrammeR) #used to draw the network diagrams from user inputs
library(magrittr)
library(readr)
library(DT)
library(openxlsx)
library(grid)
library(gridExtra)
library(parallel)
library(data.table)
library(tidyverse)
library(shinyalert) #used to create some of the popups/modals (e.g. while simulation is running)
library(shinyMatrix)
library(fitdistrplus)
library(shinyBS) #used to create some of the modals/popups (e.g. on sidepanel help buttons)
library(shinyjs)

options(shiny.maxRequestSize = 30 * 1024 ^ 2) # Sets the Shiny file upload size limit to 30MB

#### Creating the starting name matricies ####
m1 <- matrix(nrow = 1,
             ncol = 1,
             data = c("A"))
colnames(m1) <- c("Service Points")
rownames(m1) <- c("Enter Names in Right Column")


m2 = matrix(nrow = 1,
            ncol = 1,
            data = c("B"))
colnames(m2) <- c("Exits")
rownames(m2) <- c("Enter Names in Right Column")


#1. UI pages ####

#1.0.1 helper function to format text/layout on glossary pages ####
source("./PathSimR_Shiny/code/ui_functions/overview_item_format.R")

#1.1 Introduction ####
introduction_sidebar <- source("./PathSimR_Shiny/code/ui_functions/introduction_sidebar.R")$value
introduction_main <- source("./PathSimR_Shiny/code/ui_functions/introduction_main.R")$value

#1.2 Overview & Glossary ####
overview_overview <- source("./PathSimR_Shiny/code/ui_functions/overview_overview.R")$value
overview_wizard <- source("./PathSimR_Shiny/code/ui_functions/overview_wizard.R")$value
overview_output <- source("./PathSimR_Shiny/code/ui_functions/overview_output.R")$value

#1.3 Input pathway wizard ####
#1.3.1 W1 setup ####
wizard_w1_setup_sidebar <- source("./PathSimR_Shiny/code/ui_functions/wizard_w1_setup_sidebar.R")$value
wizard_w1_setup_main <- source("./PathSimR_Shiny/code/ui_functions/wizard_w1_setup_main.R")$value
#1.3.2 W2 data entry ####
wizard_w2_data_entry_sidebar <- source("./PathSimR_Shiny/code/ui_functions/wizard_w2_data_entry_sidebar.R")$value
wizard_w2_data_entry_main <- source("./PathSimR_Shiny/code/ui_functions/wizard_w2_data_entry_main.R")$value
#1.3.3 W3 tables and template download ####
wizard_w3_tables_and_download_sidebar <- source("./PathSimR_Shiny/code/ui_functions/wizard_w3_tables_and_download_sidebar.R")$value
wizard_w3_tables_and_download_main <- source("./PathSimR_Shiny/code/ui_functions/wizard_w3_tables_and_download_main.R")$value

#1.4 Service Distribution Tool ####
service_distribution_sidebar <- source("./PathSimR_Shiny/code/ui_functions/service_distribution_sidebar.R")$value
service_distribution_main <- source("./PathSimR_Shiny/code/ui_functions/service_distribution_main.R")$value

#1.4 Simulation Tool ####
#1.4.1 Network import ####
network_import_sidebar <- source("./PathSimR_Shiny/code/ui_functions/network_import_sidebar.R")$value
network_import_main <- source("./PathSimR_Shiny/code/ui_functions/network_import_main.R")$value
#1.4.2 Simulation setup ####
simulation_setup_sidebar <- source("./PathSimR_Shiny/code/ui_functions/simulation_setup_sidebar.R")$value
simulation_setup_main <- source("./PathSimR_Shiny/code/ui_functions/simulation_setup_main.R")$value
#1.4.3 simulation outputs ####
o1_interpretation <- source("./PathSimR_Shiny/code/ui_functions/outputs_o1_interpretation.R")$value
o2_warm_up <- source("./PathSimR_Shiny/code/ui_functions/outputs_o2_warm_up.R")$value
o3_through_time <- source("./PathSimR_Shiny/code/ui_functions/outputs_o3_average_through_time.R")$value
o4_service_point <- source("./PathSimR_Shiny/code/ui_functions/outputs_o4_service_point_statistics.R")$value
o5_pathway <- source("./PathSimR_Shiny/code/ui_functions/outputs_o5_pathway_statistics.R")$value
o6_patient_occupancy <- source("./PathSimR_Shiny/code/ui_functions/outputs_o6_patient_occupancy.R")$value
o7_bed_occupancy <- source("./PathSimR_Shiny/code/ui_functions/outputs_o7_bed_occupancy.R")$value
o8_capacity_delay <- source("./PathSimR_Shiny/code/ui_functions/outputs_o8_capacity_driven_delay.R")$value
o9_transition_delay <- source("./PathSimR_Shiny/code/ui_functions/outputs_o9_transition_delay.R")$value
o10_queueing_summary <- source("./PathSimR_Shiny/code/ui_functions/outputs_o10_queueing_summary.R")$value

#1.4.4 Download outputs and report ####
download_outputs_sidebar <- source("./PathSimR_Shiny/code/ui_functions/download_outputs_sidebar.R")$value

runApp("./PathSimR_Shiny/code")
