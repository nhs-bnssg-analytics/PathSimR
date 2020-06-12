#### NETWORK VISUALISATION ####

#inputs, the network diagrams, and some tables are all created inside this "viz" object ####
#is a shiny thing that is listening for input
#the input$go_vis object is in the network_inport_sidebar script
#it is a shiny action button
viz_fun <- function(thing1=input$w_temp,
                    inputfile1=input$file1,
                    inputfile2=input$file2,
                    thing2 = input$disp3,
                    var=var(),
                    cal=cal(),
                    output_next_button = output$next_button
                    
                    ){

  
  
    if (thing1 == 0) {
    req(inputfile1)
    req(inputfile2)
    
    var_input <-
      read.csv(inputfile1$datapath,
               header = TRUE,
               sep = ",")
    rownames(var_input) <- var_input[, 1]
    var_input <- var_input[, -1]
    
    cal_input <-
      read.csv(inputfile2$datapath,
               header = TRUE,
               sep = ",")
    
    issues <- issues()
    
    req(issues[1, 1] == "Complete")
    
  } else{
    var_input <- var
    
    var_input <- as.data.frame(var_input)
    
    f <- var_input
    indx <- sapply(f, is.factor)
    f[indx] <- lapply(f[indx], function(x)
      as.character(x))
    var_input <- as.data.frame(f)
    
    f <- var_input[, 1:nrow(var_input)]
    indx <- 1:nrow(var_input)
    f[, indx] <- lapply(f[indx], function(x)
      as.numeric(x))
    var_input[, 1:nrow(var_input)] <- f
    
    
    
    var_input$ext_queue <- as.numeric(var_input$ext_queue)
    
    var_input$int_queue <- as.numeric(var_input$int_queue)
    
    
    
    cal_input <- cal
    cal_input <- as.data.frame(cal_input)
    cal_input$metric <- as.character(cal_input$metric)
    cal_input$node <- as.character(cal_input$node)
    cal_input$start <- as.numeric(as.character(cal_input$start))
    cal_input$end <- as.numeric(as.character(cal_input$end))
    cal_input$value <- as.numeric(as.character(cal_input$value))
    
    
    
    
  }
  
  nodes <-
    rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                           "serv_dist") - 1], na.rm = T) != 0), ])
  exits <-
    rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
                                                           "serv_dist") - 1], na.rm = T) == 0), ])
  #ext_arr<-rownames(var_input[which(var_input$ext_arr>0),])
  
  ext_arr <-
    unique(cal_input$node[which(cal_input$metric == "ext_arr" &
                                  cal_input$value > 0)])
  
  
  delay_dist <-
    var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
  rownames(delay_dist) <- rownames(var_input)
  colnames(delay_dist) <- rownames(var_input)
  delay_dist[which(delay_dist == "", arr.ind = T)] <- NA
  
  delay_param <-
    var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
  rownames(delay_param) <- rownames(var_input)
  colnames(delay_param)[1:nrow(delay_param)] <- rownames(var_input)
  delay_param[which(delay_param == "", arr.ind = T)] <- NA
  
  
  from <- c(0)
  to <- c(0)
  
  
  for (i in 1:nrow(delay_dist)) {
    for (j in 1:nrow(delay_dist)) {
      if (!is.na(delay_dist[i, j])) {
        from <- c(from, i)
        to <- c(to, j)
      }
    }
  }
  
  delay_list <- cbind(from, to)
  
  
  tmp <- rownames(delay_dist)
  delay_exits <-
    tmp[c(delay_list[, 2])][!tmp[c(delay_list[, 2])] %in% nodes]
  
  
  var_input$serv_dist[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
    NA
  var_input$serv_dist_param[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
    NA
  
  cap_min <- vector()
  for (i in nodes) {
    cap_min <-
      c(cap_min, min(cal_input$value[which(cal_input$node == i &
                                             cal_input$metric == "cap")]))
  }
  
  
  cap_max <- vector()
  for (i in nodes) {
    cap_max <-
      c(cap_max, max(cal_input$value[which(cal_input$node == i &
                                             cal_input$metric == "cap")]))
  }
  
  
  cal_tooltip <- vector()
  
  for (i in nodes) {
    tmp <- cal_input[which(cal_input$node == i &
                             cal_input$metric == "cap"), ]
    tmp2 <- vector()
    
    for (j in 1:nrow(tmp)) {
      tmp3 <-
        paste("\n",
              "Start:",
              tmp[j, 3],
              "End:",
              tmp[j, 4],
              "Capacity:",
              tmp[j, 5],
              "//")
      
      
      tmp2 <- c(tmp2, tmp3)
    }
    tmp2 <- paste(tmp2, collapse = "")
    tmp2 <- paste("Capacity Calendar //", tmp2)
    cal_tooltip <- c(cal_tooltip, tmp2)
  }
  
  
  
  # Create a node data frame (ndf)
  
  ndf1 <- create_node_df(
    n = length(nodes),
    type = "lower",
    label = c(nodes),
    fillcolor = "deepskyblue1",
    color = "black",
    fontcolor = "black",
    shape = "square",
    tooltip = cal_tooltip
  )
  
  
  
  ndf2 <- create_node_df(
    n = length(exits),
    type = "lower",
    label = c(exits),
    fillcolor = "green",
    color = "black",
    fontcolor = "black",
    shape = "diamond",
    tooltip = "Exit"
  )
  
  
  
  
  ndf3 <- create_node_df(
    n = length(ext_arr),
    type = "lower",
    label = as.numeric(c(length(c(
      nodes, exits
    )) + 1):(length(c(
      nodes, exits
    )) + length(ext_arr))) ,
    fillcolor = "white",
    fontcolor = "white",
    shape = "square",
    color = "white"
  )
  
  ndf <- combine_ndfs(ndf1, ndf2, ndf3)
  
  # Create an edge data frame (edf)
  f <- vector()
  t <- vector()
  l <- vector()
  edge_col <- vector()
  edge_tip <- vector()
  
  for (i in 1:length(nodes)) {
    for (j in 1:length(c(nodes, exits))) {
      if (var_input[i, j] > 0) {
        f <- c(f, i)
        t <- c(t, j)
        
        if (!is.na(delay_dist[i, j])) {
          l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
          edge_col <- c(edge_col, "sienna2")
          
          if (delay_dist[i, j] == "exp") {
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            delay_mean <- 1 / pars[1]
            
          } else if (delay_dist[i, j] == "unif") {
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            delay_mean <- (pars[1] + pars[2]) / 2
            
          } else if (delay_dist[i, j] == "lnorm") {
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            delay_mean <- exp(pars[1] + 0.5 * (pars[2]) ^ 2)
            
          } else if (delay_dist[i, j] == "weibull") {
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            delay_mean <- pars[2] * (gamma(1 + (1 / pars[1])))
            
          } else if (delay_dist[i, j] == "gamma") {
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            delay_mean <- pars[1] / pars[2]
            
          } else{
            pars <-
              as.numeric(unlist(strsplit(
                x = as.character(delay_param[i, j]), split = ";"
              )))
            tmp2 <-
              do.call(get(paste0("r", delay_dist[i, j])), as.list(c(10 ^ 7, pars)))  #Creates a service time
            delay_mean <- mean(tmp2)
            
          }
          
          edge_tip <-
            c(
              edge_tip,
              paste0(
                "Mean Delay: ",
                delay_mean,
                " (Delay Dist: ",
                delay_dist[i, j],
                ")"
              )
            )
        }
        else{
          l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
          edge_col <- c(edge_col, "black")
          edge_tip <- c(edge_tip, paste0("No Delay"))
        }
        
      }
      
    }
  }
  
  
  edf1 <- create_edge_df(
    from = f,
    to = t,
    #rel = c("leading_to"),
    label = l,
    color = edge_col,
    fontcolor = edge_col,
    tooltip = edge_tip
  )
  
  
  edf2 <-
    create_edge_df(
      from = c(length(c(nodes, exits)) + 1):(length(c(nodes, exits)) + length(ext_arr)),
      to = as.numeric(which(rownames(var_input) %in% ext_arr)),
      #rel = c("leading_to"),
      label = as.character("Arrivals"),
      color = "red",
      fontcolor = "red",
      tooltip = "Arrival"
    )
  
  edf <- combine_edfs(edf1, edf2)
  
  
  
  
  #Create a list of average LOS
  LOS <- vector()
  
  
  for (i in nodes) {
    arr.dist <- var_input$serv_dist[which(rownames(var_input) == i)]
    pars <-
      as.numeric(unlist(strsplit(
        as.character(var_input$serv_dist_param[which(rownames(var_input) == i)]), ";"
      )))
    
    
    if (arr.dist == "exp") {
      tmp3 <- 1 / pars
      LOS <- c(LOS, tmp3)
      
    } else if (arr.dist == "unif") {
      tmp3 <- (pars[1] + pars[2]) / 2
      LOS <- c(LOS, tmp3)
      
    } else if (arr.dist == "lnorm") {
      tmp3 <- exp(pars[1] + 0.5 * (pars[2]) ^ 2)
      LOS <- c(LOS, tmp3)
      
    } else if (arr.dist == "weibull") {
      tmp3 <- pars[2] * (gamma(1 + (1 / pars[1])))
      LOS <- c(LOS, tmp3)
      
      
    } else if (arr.dist == "gamma") {
      tmp3 <- pars[1] / pars[2]
      LOS <- c(LOS, tmp3)
      
      
      
    } else{
      tmp2 <-
        do.call(get(paste0("r", arr.dist)), as.list(c(10 ^ 7, pars)))  #Creates a service time
      tmp3 <- mean(tmp2)
      
      LOS <- c(LOS, tmp3)
      
      
    }
  }
  LOS <- round(LOS, digits = 2)
  
  
  TAC <- vector()
  
  for (i in nodes) {
    tmp <- cal_input[which(cal_input$node == i &
                             cal_input$metric == "cap"), ]
    
    if (nrow(tmp) == 1) {
      TAC <- c(TAC, tmp$value)
    }
    if (nrow(tmp) > 1) {
      tmp2 <- sum(tmp$value * (tmp$end - tmp$start)) / max(tmp$end)
      TAC <- c(TAC, tmp2)
    }
  }
  
  TAC <- ceiling(TAC)
  
  
  node_labels <- vector()
  
  for (i in 1:length(nodes)) {
    tmp1 <-
      paste0(
        nodes[i],
        "\n",
        " LOS: ",
        LOS[i],
        "\n",
        "Av Cap: ",
        TAC[i],
        "\n",
        "IQC: ",
        var_input$int_queue[i],
        "\n",
        "EQC: ",
        var_input$ext_queue[i]
      )
    
    node_labels <- c(node_labels, tmp1)
    
  }
  
  if (thing2 == TRUE) {
    ndf$label[1:length(nodes)] <- node_labels
  }
  
  
  # Create a graph with the ndf and edf
  graph <-
    create_graph(nodes_df = ndf,
                 edges_df = edf)
  
  # graph$global_attrs[1, "value"] <- "dot"
  # graph$global_attrs[4, "value"] <- 20
  # graph$global_attrs[6, "value"] <- "false"
  # graph$global_attrs[14, "value"] <- 20
  # graph$global_attrs[17, "value"] <- 1
  
  graph$global_attrs[graph$global_attrs$attr=="layout" &
                       graph$global_attrs$attr_type=="graph",]$value <- "dot"
  
  graph$global_attrs[graph$global_attrs$attr=="fontsize" &
                       graph$global_attrs$attr_type=="node",]$value <- 10
  graph$global_attrs[graph$global_attrs$attr=="fixedsize" &
                       graph$global_attrs$attr_type=="node",]$value <- "false"
  graph$global_attrs[graph$global_attrs$attr=="fontsize" &
                       graph$global_attrs$attr_type=="edge",]$value <- 10
  graph$global_attrs[graph$global_attrs$attr=="arrowsize" &
                       graph$global_attrs$attr_type=="edge",]$value <- 1
  
  
  
  
  graph$global_attrs <-
    rbind(graph$global_attrs, c("rankdir", "LR", "graph"))
  graph$global_attrs <-
    rbind(graph$global_attrs, c("splines", "true", "graph"))
  
  showTab(inputId = "navbar", target = "2. Simulation Setup & Run")
  
  #output$next_button
  output_next_button <- renderUI({
    column(6, align = "center", actionButton(inputId = "j2PSR2", label = c(tagList(
      "Next", icon("arrow-right")
    ))))
    
  })
  
  
  
  
  
  render_graph(graph)

}

  
  
# #show the things created in "viz" ####
# output$network <- renderGrViz({
#   viz()
# })
# 
# checklist_viz <- eventReactive(input$checklist, {
#   viz()
# })
# 
# output$cl_viz <- renderGrViz({
#   checklist_viz()
# })