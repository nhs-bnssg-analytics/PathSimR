download_outputs <- list(
  
  # File Downloader --
  h4(strong("Details")),
  h5(
    "A description of each of the files can be found in the",
    strong("Output Library Document.")
  ),
  
  
  hr(),
  h4("Data Tables"),
  p(
    "PathSimR produces an excel workbook which contains all metrics produced within the tool at both replicate level and simulation level. Each tab is clearly labelled,
                 with the first half of the tabs relating to patient level metrics (e.g. wait times, Length of Stays etc) and the second half containing information regarding the through
                 time metrics (e.g. Occupancy, Queues etc). The final tab contains all the data required to recreate the 'Average Through Time' plot."
  ),
  fluidRow(column(
    12,
    align = "center",
    downloadButton(
      outputId = "downloadtables",
      label = "Download Tables",
      icon = icon("download"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  
  hr(),
  h4("Simulation Plots"),
  p(
    "All plots created in PathSimR are saved down in a single PDF that can then be manipulated as needed. All the figures shown in PathSimR can be recreated from the data
                 provided in the Data Tables (download button above)."
  ),
  fluidRow(column(
    12,
    align = "center",
    downloadButton(
      outputId = "downloadplot",
      label = "Download Plots",
      icon = icon("download"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  
  hr(),
  h4("Automated Report"),
  p(
    "An automated word document is produced which includes a collection of figures and data tables from the simulation. These have been sorted into sections and a brief description of
                 the metrics and figures is included. This report is designed to speed up the summary process and provides easy manipulation for the user."
  ),
  fluidRow(column(
    12,
    align = "center",
    downloadButton(
      outputId = "downloadreport",
      label = "Download Report",
      icon = icon("download"),
      style = 'padding:10px; font-size:125%'
    )
  )),
  hr()
  
)