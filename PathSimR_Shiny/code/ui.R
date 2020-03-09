##### SHINY UI CODE #####
ui <- 
    
  navbarPage(
    #style - read in from a bootstrap v3 (n.b. current version is 4, but doesn't work with Shiny) CSS
  theme="mod.cerulean.css",
  
  title = c(tagList(icon("compass"), "Navigation Bar")),
  id = "navbar",
  
  #### 1. INTRODUCTION TAB ####
  tabPanel("Introduction",
           sidebarLayout(
             introduction_sidebar,
             introduction_main
          )),
  
  #### 2. OVERVIEW AND GLOSSARY TAB ####
  tabPanel(
    "Overview & Glossary",
    navlistPanel(
      #2.1 overview subtab####
      tabPanel(
        "Overview",
        overview_overview
        ),
      #2.2 wizard subtab ####
      tabPanel(
        "Wizard & Setup Terms",
        overview_wizard
      ),
      #2.3 outputs subtab ####
      tabPanel(
        "Output Terms",
        overview_output
      ),
       widths = c(2, 10),
       well = TRUE
      ) 
  ),
  
  ####3. WIZARD 1 - SETUP TAB ####
  
  tabPanel("W1. Setup",
           
           sidebarLayout(
             sidebarPanel(
               
               wizard_w1_setup_sidebar,
               
               width = 3
             ),
             
             mainPanel(
               
               wizard_w1_setup_main,
               
             )
           )),
  
  ####4. WIZARD 2 - DATA ENTRY TAB ####
  
  tabPanel("W2. Data Entry",
           
           sidebarLayout(
              sidebarPanel(
                
                wizard_w2_data_entry_sidebar,
                
                width = 3
              ),
             mainPanel(
               # uiOutput("tabs")
               wizard_w2_data_entry_main
               )
           )),
  
  ####5. WIZARD 3 - FINAL WIZARD TABLES & DOWNLOAD TAB ####
  
  tabPanel(
    "W3. Final Wizard Tables & Download",
    
    sidebarLayout(
      sidebarPanel(
        
        wizard_w3_tables_and_download_sidebar,
        
        width = 3
      ),
      mainPanel(
        
        wizard_w3_tables_and_download_main
      )
    )
    
  ),
  
  ####6. SERVICE DISTRIBUTION TOOL TAB ####
  
  tabPanel(
    title = "Service Distribution Tool",
    icon = icon("chart-area"),
    sidebarLayout(
      sidebarPanel(
        
        service_distribution_sidebar,
        width = 3
      ),
      
      mainPanel(
        
        service_distribution_main,
        
      )
    )
  ),
  
  ####7. TOOL 1 - NETWORK IMPORT & VISUALISATION TAB ####
  
  tabPanel(
    "1. Network Import & Visualisation",
    sidebarLayout(
      # Sidebar panel for inputs -
      sidebarPanel(
        
        network_import_sidebar,
        
        width = 3
      ),
      
      
      mainPanel(
        
        network_import_main
       
      )
      
    )
  ),
  
  ####8. TOOL 2 - SIMULATION SETUP & RUN TAB ####
  
  tabPanel(
    "2. Simulation Setup & Run",
    sidebarLayout(
      # Sidebar panel for inputs --
      sidebarPanel(
        
        simulation_setup_sidebar,

        width = 3
      ),
      
      # Main panel for displaying outputs -
      mainPanel(
        
        simulation_setup_main
        
      )
      
    )
  ),
  
  
  ####9. TOOL 3 - SIMULATION OUTPUTS TAB ####
  
  tabPanel(
    "3. Simulation Outputs",
    navlistPanel(
      id = "Simulation Outputs",
      
      #9.1 outputs 1 - interpretation ####
      tabPanel(
        title = "Output Interpretation",
        icon = icon("book"),
        o1_interpretation
        ),
      
      #9.2 outputs 2 - warm up ####
      tabPanel(
        "Warm-Up Period Assistance",
        icon = icon("chart-line"),
        o2_warm_up
        ),

      #9.3 outputs 3 - average through time plot ####
      tabPanel(
        "Average Through Time Plot",
        icon = icon("chart-line"),
        o3_through_time
        ),
      
      #9.4 outputs 4 - service point statistics ####
      tabPanel(
        "Service Point Statistics",
        icon = icon("table"),
        o4_service_point
        ),
    
      #9.5 outputs 5 - pathway statistics ####
      tabPanel(
        "Pathway Statistics",
        icon = icon("table"),
        o5_pathway
      ),
      
      #9.6 outputs 6 - patient occupancy summary ####
      tabPanel(
        "Patient Occupancy Summary",
        icon = icon("user"),
        o6_patient_occupancy
        ),
      #9.7 outputs 7 - bed occupancy summary ####
      tabPanel(
        "Bed Occupancy Summary",
        icon = icon("bed"),
        o7_bed_occupancy
        ),
      
      #9.8 outputs 8 - capacity driven delay summary ####
      tabPanel(
        "Capacity Driven Delay Summary",
        icon = icon("door-closed"),
        o8_capacity_delay
      ),
      
      
      #9.9 outputs 9 - transition delay summary ####
      tabPanel(
        "Transition Delay Summary",
        icon = icon("expand-arrows-alt"),
        h2(strong("Transition Delay Summary")),
        o9_transition_delay
      ),
      #9.10 outputs 10 - queueing summary ####
      tabPanel(
        "Queueing Summary",
        icon = icon("clock"),
        o10_queueing_summary
      ),
      
      widths = c(3, 9),
      well = TRUE
      
    )
  ),
  
  ####10. TOOL 4 - DOWNLOAD OUTPUTS TAB ####
  
  tabPanel(
    "4. Download Outputs",
    sidebarLayout(
      # Sidebar panel for buttons --
      sidebarPanel(
        download_outputs_sidebar,
        # # File Downloader --
        # h4(strong("Details")),
        # h5(
        #   "A description of each of the files can be found in the",
        #   strong("Output Library Document.")
        # ),
        # 
        # 
        # hr(),
        # h4("Data Tables"),
        # p(
        #   "PathSimR produces an excel workbook which contains all metrics produced within the tool at both replicate level and simulation level. Each tab is clearly labelled,
        #          with the first half of the tabs relating to patient level metrics (e.g. wait times, Length of Stays etc) and the second half containing information regarding the through
        #          time metrics (e.g. Occupancy, Queues etc). The final tab contains all the data required to recreate the 'Average Through Time' plot."
        # ),
        # fluidRow(column(
        #   12,
        #   align = "center",
        #   downloadButton(
        #     outputId = "downloadtables",
        #     label = "Download Tables",
        #     icon = icon("download"),
        #     style = 'padding:10px; font-size:125%'
        #   )
        # )),
        # 
        # hr(),
        # h4("Simulation Plots"),
        # p(
        #   "All plots created in PathSimR are saved down in a single PDF that can then be manipulated as needed. All the figures shown in PathSimR can be recreated from the data
        #          provided in the Data Tables (download button above)."
        # ),
        # fluidRow(column(
        #   12,
        #   align = "center",
        #   downloadButton(
        #     outputId = "downloadplot",
        #     label = "Download Plots",
        #     icon = icon("download"),
        #     style = 'padding:10px; font-size:125%'
        #   )
        # )),
        # 
        # hr(),
        # h4("Automated Report"),
        # p(
        #   "An automated word document is produced which includes a collection of figures and data tables from the simulation. These have been sorted into sections and a brief description of
        #          the metrics and figures is included. This report is designed to speed up the summary process and provides easy manipulation for the user."
        # ),
        # fluidRow(column(
        #   12,
        #   align = "center",
        #   downloadButton(
        #     outputId = "downloadreport",
        #     label = "Download Report",
        #     icon = icon("download"),
        #     style = 'padding:10px; font-size:125%'
        #   )
        # )),
        # hr(),
        
        
        width = 3
      ),
      
      # Main panel for displaying outputs -
      mainPanel()
      
    ),
    useShinyalert()
  )
  
  
)



