server <- function(input,output){
  
  
  
  
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
    annotate("text",x =text_df$x, y = text_df$y, label = text_df$label,parse = T) + coord_equal() +theme_void()
    
    
    
    
    
    
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
        M[i,i] <- -(As/2*h_total + 3/2*k*A/dx)
        for (j in current_neighbors){
          M[i,j] <- k*A/2/dx
        }
        
        B[i,1] <- -As/2 * b_total
        
        
        
        
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
      coord_equal() + theme_minimal() + labs(x = "Width (cm)", y = "Height (cm)") + theme(panel.grid = element_blank())
    
  })
  
  
  
  
  
  
  
  
  
  
}

