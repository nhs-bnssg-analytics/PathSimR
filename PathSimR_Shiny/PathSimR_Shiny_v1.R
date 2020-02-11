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
library(DiagrammeR)
library(magrittr)
library(readr)
library(DT)
library(openxlsx)
library(grid)
library(gridExtra)
library(parallel)
library(data.table)
library(tidyverse)
library(shinyalert)
library(shinyMatrix)
library(fitdistrplus)
library(shinyBS)
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


introduction_sidebar <- source("./PathSimR_Shiny/code/ui_functions/introduction_sidebar.R")

introduction_main <- source("./PathSimR_Shiny/code/ui_functions/introduction_main.R")
#source("./PathSimR_Shiny/code/ui.R")
#source("./PathSimR_Shiny/code/server.R")


#shinyApp(ui = ui, server = server)

runApp("./PathSimR_Shiny/code")
