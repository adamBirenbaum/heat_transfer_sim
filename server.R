server <- function(input,output){
  
  
  observeEvent(input$sim_type,{
    
    if (input$sim_type == "Transient"){
      output$ui_transient <- renderUI({
        tagList(
          textInput("total_time",label = "Total Time",value = 10),
          textInput("time_step",label = "Time Step",value = 1),
          textInput("initial_temp",label = "Initial Temp", value = 22)
          
        )
        
      })
      
    }
    
  })
  
  output$material_conduct <- renderText({
    

    k <- material_df$k[material_df$material == input$material]
    paste0("k = ", k, " W/m C")
    
  })
  
  
  deg <- function(x) as.character(expression(paste0(x)*degree*'C'))
  
  deg <- function(x) paste0(x,"~degree*C")
  deg2 <- function(x) paste0(x,"~W/m^2*degree*C")
  
  output$draw_setup <- renderPlot({

    x <- input$dim_x
    y <- input$dim_y
    larger_dim <- max(x,y)
    x <- x / larger_dim
    y <- y / larger_dim
    
    x_sep <- .1
    y_sep <- .1
    
    k <- material_df$k[material_df$material == input$material]
    k <- paste0(k,"~W/m*degree*C")
    
    material <- gsub(",","",input$material,fixed = T)
    material <- gsub(" ","~",material,fixed = T)
    
    df <- data.frame(xmin = 0, xmax = x, ymin = 0, ymax = y)
    text_df <- data.frame(x = c(-x_sep,x/2,x + x_sep,x/2,-x_sep,x/2,x + x_sep,x/2,x/2,x/2),
                          y = c(y/2 + y_sep,y + y_sep,y/2 + y_sep,-.05,
                                y/2 + y_sep/2, y + y_sep/2,y/2 + y_sep/2,-.05 - .05,y/2 + y_sep,y/2 + y_sep/2),
                          label = c(deg(input$T_left),deg(input$T_up),deg(input$T_right),deg(input$T_down),
                                    deg2(input$h_left),deg2(input$h_up),deg2(input$h_right),deg2(input$h_down),
                                    material,k))
    
    ggplot(df,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + geom_rect(fill = "lightblue",alpha = 0.5,color = "black") + 
    annotate("text",x =text_df$x, y = text_df$y, label = text_df$label,parse = T) + coord_equal() +theme_void() + labs(title = "Setup") + theme(title = element_text(size = 20))
    
    
    
    
    
    
  })
  
  
  make_initial_grid <- function(x,y,n){
    
    a <- -(x+y) / (2*(1-n))
    b <- sqrt((x + y)^2 - 4 * (1-n)*y*x) / (2*(1-n))
    if (a > b){
      dx <- a - b
    }else{
      dx <- a + b
    } 
    
    possible_x <- x / 1:n
    possible_y <- y / 1:n
    in_both <- intersect(possible_x,possible_y)
    smallest_diff <- min(abs(in_both - dx))
    
    dx <- in_both[abs(in_both-dx) == smallest_diff]
    n <- (y / dx + 1)*(x / dx + 1)
    m <- matrix(1:n,nrow = y/dx + 1,ncol = x / dx + 1)
    list(m = m, dx = dx)
  }
  
  
  make_neighboring_matrix <- function(m){
    
    mat.pad = rbind(NA, cbind(NA, m, NA), NA)
    
    indx <- 2:(ncol(m)+1) # row/column indices of the "middle"
    indy <- 2:(nrow(m)+1)
    neigh <- rbind(N  = as.vector(mat.pad[indy - 1, indx    ]),
                   E  = as.vector(mat.pad[indy    , indx + 1]),
                   S  = as.vector(mat.pad[indy + 1, indx    ]),
                   W  = as.vector(mat.pad[indy    , indx - 1]))
    

    
  }
  
  output$steady_state <- renderPlot({

    
    x <- input$dim_x / 100
    y <- input$dim_y / 100
    n <- input$n_nodes
    
    grid_list <- make_initial_grid(x,y,n)
    m <- grid_list$m
    dx <- grid_list$dx
    
    neighbor_m <- make_neighboring_matrix(m)
    
    num_neighbors <- apply(neighbor_m,2,function(x) sum(!is.na(x)))
    is_corner <- num_neighbors == 2
    is_one_side <- num_neighbors == 3
    
    
    h_top <- input$h_up
    h_bottom <- input$h_down
    h_right <- input$h_right
    h_left <- input$h_left
    
    T_left <- input$T_left
    T_right <- input$T_right
    T_top <- input$T_up
    T_bottom <- input$T_down
    
    A = dx
    As = dx
    
    k <- as.numeric(material_df$k[material_df$material == input$material])
    
    n <- length(m)
    
    sim_type <- input$sim_type
 
   
    
    M <- matrix(0,nrow = n, ncol = n)
    B <- matrix(0, nrow = n, ncol = 1)
    

    for (i in 1:n){
      current_neighbors <-  neighbor_m[,i]
      current_neighbors <- current_neighbors[!is.na(current_neighbors)]
      neigh_direct <- names(current_neighbors)
      
      if (is_one_side[i]){
        
        missing_direct <- paste0(c("N","E","S","W")[!c("N","E","S","W") %in% neigh_direct],collapse = "")
        
        h_total <- switch(missing_direct,"N" = h_top ,"W" =  h_left,"E" = h_right,"S" = h_bottom)
        b_total <- switch(missing_direct,"N" = h_top * T_top ,"W" =  h_left * T_left,
                          "E" = h_right * T_right ,"S" = h_bottom * T_bottom )
        
        
        M[i,i] <- -(As*h_total + 2*k*A/dx)
        
        inside_node <- switch(missing_direct,"N" = "S","W" = "E","S" = "N", "E" = "W")

        ## I BELIEVE THE TRUE TERM SHOULDN'T HAVE A /2 term, but its rendering weird solutions
        M[i,unname(current_neighbors)] <- ifelse(names(current_neighbors) == inside_node,k*A/dx,k*A/2/dx)
        # for (j in current_neighbors){
        #   #M[i,j] <- k*A/2/dx
        # }
        
        B[i,1] <- -As * b_total
        
        
      }else if (is_corner[i]){
        
        missing_direct <- paste0(c("N","E","S","W")[!c("N","E","S","W") %in% neigh_direct],collapse = "")
        
        h_total <- switch(missing_direct,"NE" = h_top + h_right,"NW" = h_top + h_left,"ES" = h_right + h_bottom,"SW" = h_bottom + h_left)
        b_total <- switch(missing_direct,"NE" = h_top * T_top + h_right * T_right,"NW" = h_top * T_top + h_left * T_left,
                          "ES" = h_right * T_right + h_bottom * T_bottom,"SW" = h_bottom * T_bottom + h_left * T_left)
        
        M[i,i] <- -(As/2*h_total + k*A/dx)
        for (j in current_neighbors){
          M[i,j] <- k*A/2/dx
        }
        
        B[i,1] <- -As/2 * b_total
        
        
      }else{
        
        M[i,i] <- -4
        
        for (j in current_neighbors){
          M[i,j] <- 1
        }
        
      }
      
      
    }
    
    Temps <- solve(M) %*% B
    
    

    df <- data.frame(x = as.numeric(col(m))*dx*100,
                     y = rev(as.numeric(row(m)))*dx*100,
                     temp = Temps[,1])
    

    ggplot(df, aes(x = x, y =y, z = temp)) + geom_raster(aes(fill = temp))+geom_contour(color = "black",size = .2) +
      scale_fill_distiller(expression("Temp."~"("*degree*"C)"),palette = "Spectral") + 
      coord_equal() + theme_minimal() + labs(x = "Width (cm)", y = "Height (cm)",title = "Steady State") + theme(panel.grid = element_blank()) +theme(plot.title = element_text(size = 20))
    
    
    
  })
  

  is_error <- function(x) inherits(x,'try-error') || is.na(x)
  
  observe({
    if (input$sim_type == "Transient"){
      
      
      n <- input$n_nodes
      x <- input$dim_x / 100
      y <- input$dim_y / 100
      grid_list <- make_initial_grid(x,y,n)
      m <- grid_list$m
      n <- length(m)
      
      total_time <- try(as.numeric(input$total_time))
      dt <- try(as.numeric(input$time_step))
     initial_temp <- try(as.numeric(input$initial_temp))
     
     if (length(total_time) == 0){
       total_time <- 10
       dt = 1
       initial_temp <- 22
     }
     
     
      if (!(is_error(total_time) || is_error(dt) || is_error(initial_temp))){
    
        n_time_steps <- (total_time / dt + 1)
        total_eqs <- n*(n_time_steps - 1)
        
        
        if (total_eqs < 5000){
          output$ui_transient_button <- renderUI({
            tagList(
              fluidRow(
                column(width = 6,
                       tags$button(id = "get_transient", type = "button", class = "btn btn-primary action-button btn-lg", "Calculate Transient")
                ),
                column(width = 6,
                       h4(paste0("# Equations to solve: ", total_eqs))
                )
              )
              
            )
            
          })
          
        }else{
          output$ui_transient_button <- renderUI({
            tagList(
              fluidRow(
                column(width = 12,
                       h4(paste0("Too many equations, will crash system.  Equations to solve: ", total_eqs))
                )
              )
              
            )
            
          })
        }
        
         
      }else{
        output$ui_transient_button <- renderUI({
      NULL
        })
      }
      

      
    }
    
  })
  
  observeEvent(input$get_transient,{
    
    output$ui_transient_image <- renderUI({
      imageOutput("transient",width = "100%")
      
      })
 
    
    
    x <- input$dim_x / 100
    y <- input$dim_y / 100
    n <- input$n_nodes
    
    grid_list <- make_initial_grid(x,y,n)
    m <- grid_list$m
    dx <- grid_list$dx
    
    neighbor_m <- make_neighboring_matrix(m)
    
    num_neighbors <- apply(neighbor_m,2,function(x) sum(!is.na(x)))
    is_corner <- num_neighbors == 2
    is_one_side <- num_neighbors == 3
    
    
    h_top <- input$h_up
    h_bottom <- input$h_down
    h_right <- input$h_right
    h_left <- input$h_left
    
    T_left <- input$T_left
    T_right <- input$T_right
    T_top <- input$T_up
    T_bottom <- input$T_down
    
    A = dx
    As = dx
    
    k <- as.numeric(material_df$k[material_df$material == input$material])
    
    n <- length(m)
    
    
    total_time <- as.numeric(input$total_time)
    dt <- as.numeric(input$time_step)
    n_time_steps <- (total_time / dt + 1)
    
    
    #total_time <- 30
    #dt <- 1
    #n_time_steps <- (total_time / dt + 1)
    
    
    
    M <- matrix(0,nrow = n*(n_time_steps - 1), ncol = n*(n_time_steps - 1))
    B <- matrix(0, nrow = n*(n_time_steps - 1), ncol = 1)
    
    print(dim(M))
    if (dim(M)[1] > 5000 ) stop("TOO BIG")
    
    initial_temp <- as.numeric(input$initial_temp)
    
    
    alpha <- 9.7E-5
    tau <- alpha*dt/dx^2
    print(n_time_steps)
    
    for (t in 0:(n_time_steps - 2)){
      for (i in 1:n){
        
        current_neighbors <-  neighbor_m[,i]
        current_neighbors <- current_neighbors[!is.na(current_neighbors)]
        neigh_direct <- names(current_neighbors)
        
        if (is_one_side[i]){
          
          missing_direct <- paste0(c("N","E","S","W")[!c("N","E","S","W") %in% neigh_direct],collapse = "")
          
          h_total <- switch(missing_direct,"N" = h_top ,"W" =  h_left,"E" = h_right,"S" = h_bottom)
          b_total <- switch(missing_direct,"N" = h_top * T_top ,"W" =  h_left * T_left,
                            "E" = h_right * T_right ,"S" = h_bottom * T_bottom )
          
          
          inside_node <- switch(missing_direct,"N" = "S","W" = "E","S" = "N", "E" = "W")
          
          
          
          # for (j in current_neighbors){
          #   #M[i,j] <- k*A/2/dx
          # }
          row_ind <- (n*(t) + i)
          
          
          if (t == 0){
            B[row_ind,1] <- -2*dx/k* b_total - initial_temp/tau
            
          }else{
            B[row_ind,1] <- -2*dx/k* b_total
            prev_ind <- (n*(t-1) + i)
            M[row_ind,prev_ind] <- 1/tau
            
          }
          
          M[row_ind,row_ind] <- -(dx*2/k*h_total + 4 + 1/tau)
          
          M[row_ind,(n*t + current_neighbors)] <- ifelse(names(current_neighbors) == inside_node,2,1)
          
          
          
        }else if (is_corner[i]){
          
          missing_direct <- paste0(c("N","E","S","W")[!c("N","E","S","W") %in% neigh_direct],collapse = "")
          
          h_total <- switch(missing_direct,"NE" = h_top + h_right,"NW" = h_top + h_left,"ES" = h_right + h_bottom,"SW" = h_bottom + h_left)
          b_total <- switch(missing_direct,"NE" = h_top * T_top + h_right * T_right,"NW" = h_top * T_top + h_left * T_left,
                            "ES" = h_right * T_right + h_bottom * T_bottom,"SW" = h_bottom * T_bottom + h_left * T_left)
          
          
          row_ind <- (n*t + i)
          
          
          if (t == 0){
            B[row_ind,1] <- -2*dx/k* b_total - initial_temp/tau
            
          }else{
            B[row_ind,1] <- -2*dx/k* b_total
            prev_ind <- (n*(t-1) + i)
            M[row_ind,prev_ind] <- 1/tau
            
          }
          
          M[row_ind,row_ind] <- -(dx*2/k*h_total + 4 + 1/tau)
          
          M[row_ind,(n*t + current_neighbors)] <- 2
          
          
          
          
          
        }else{
          
          row_ind <- (n*t + i)
          
          
          if (t == 0){
            B[row_ind,1] <- -initial_temp/tau
            
          }else{
            prev_ind <- (n*(t-1) + i)
            M[row_ind,prev_ind] <- 1/tau
          }
          
          M[row_ind,row_ind] <- -(4 + 1/tau)
          
          M[row_ind,(n*t + current_neighbors)] <- 1
          
          
          
        }
        
        
      }
      
    }
    
    Temps <- solve(M) %*% B
    
    df <- data.frame(x = as.numeric(col(m))*dx*100,
                     y = rev(as.numeric(row(m)))*dx*100,
                     temp = Temps[,1],
                     state = round(rep(c(1:(n_time_steps-1)),each = n) * dt,digits = 2))

    g <- ggplot(df, aes(x = x, y =y, z = temp)) + geom_raster(aes(fill = temp),show.legend = F)+    coord_equal() +theme_void()+
      transition_states(state,transition_length = 1,state_length = 100, wrap = F) + ease_aes('linear') + scale_fill_distiller(palette = "Spectral") + 
      labs(title = 'Transient - Time: {closest_state} s.') + theme(plot.title = element_text(size = 20))
    if (n_time_steps < 50){
      gg <- animate(g,nframes = 100, fps = 20)
    }else if (n_time_steps < 100){
      gg <- animate(g,nframes = 200, fps = 25)
      
    }else{
      gg <- animate(g,nframes = 400, fps = 25)
      
    }
   
    
    anim_save(paste0(path_to_main,"transient.gif"),animation = gg)
    output$transient <- renderImage(list(src = paste0(path_to_main,"transient.gif"),contentType = 'image/gif'),deleteFile = T)
    
    
    # geom_contour(color = "black",size = .2) +
    # scale_fill_distiller(expression("Temp."~"("*degree*"C)"),palette = "Spectral") + 
    # coord_equal() + theme_minimal() + labs(x = "Width (cm)", y = "Height (cm)") + theme(panel.grid = element_blank()) + 
    # transition_states(state,transition_length = 1,state_length = 10) + ease_aes('linear')
    
    
    
  })
  
  
  
  
  
  
  
  
}

