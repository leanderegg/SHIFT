plotPercentBars.new = function(varPart, col = c(ggColorHue(ncol(varPart)-1), "grey85") ){
  
  if( !is.matrix(varPart) && !is.data.frame(varPart)){
    stop("Argument must be a matrix or data.frame")
  } 
  
  if( length(col) < ncol(varPart) ){
    stop("Number of colors is less than number of variables")
  }
  
  # check row sums
  if( any(abs(rowSums(as.matrix(varPart)) -1) > 1e-4)){
    warning("Variance fractions don't sum to 100%: This plot may not be meaningful")
  }
  
  # convert matrix to tall data.frame
  df = reshape2::melt(varPart, id.vars=NULL)
  
  # assign gene names
  df$gene = rep(rownames(varPart), ncol(varPart))
  
  # convert gene names to factors sorted so first gene is 
  # plotted on top
  df$gene = factor(df$gene, rev(rownames(varPart)))
  
  # plot residuals on right
  df$variable = factor(df$variable, colnames(varPart))
  
  # convert values from [0-1] to [0-100]
  df$value = 100*df$value
  
  # Initialize variables to satisfy R CMD check
  gene = value = variable = 0
  
  # Flip order of columns for use with ggplot2 2.2.0
  # Nov 17, 2016
  fig = ggplot(df, aes(x = gene, 
                       y = value, 
                       fill = variable,
                       # alpha = 1
  ), 
  ) +
    geom_bar(stat = "identity", 
             position = position_fill(reverse = TRUE)
    ) +
    #theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    #coord_flip() +
    xlab("") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  fig = fig + theme(
    text = element_text(size=13),
    axis.text.x = element_text(angle=90, hjust=1, vjust = 1),
    axis.line = element_line(colour = "transparent"),
    # axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key = element_blank(),
    plot.margin = unit(c(0, .5, 0, 0), "cm")
  ) +
    guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = col) +
    scale_x_discrete(
      breaks = seq(0, 100, by = 20),
      label = seq(100, 0, by = -20),
      expand = c(0, .3)
    ) +
    labs(y = "", 
         x = "")
  
  fig = fig + theme(text = element_text(colour="black"), 
                    axis.text 	= element_text(colour="black"),
                    legend.text = element_text(colour="black")) 
  
  fig	
}

plotPercentBars.legend = function( varPart, col = c(ggColorHue(ncol(varPart)-1), "grey85") ){
  
  if( !is.matrix(varPart) && !is.data.frame(varPart)){
    stop("Argument must be a matrix or data.frame")
  } 
  
  if( length(col) < ncol(varPart) ){
    stop("Number of colors is less than number of variables")
  }
  
  # check row sums
  if( any(abs(rowSums(as.matrix(varPart)) -1) > 1e-4)){
    warning("Variance fractions don't sum to 100%: This plot may not be meaningful")
  }
  
  # convert matrix to tall data.frame
  df = reshape2::melt(varPart, id.vars=NULL)
  
  # assign gene names
  df$gene = rep(rownames(varPart), ncol(varPart))
  
  # convert gene names to factors sorted so first gene is 
  # plotted on top
  df$gene = factor(df$gene, rev(rownames(varPart)))
  
  # plot residuals on right
  df$variable = factor(df$variable, colnames(varPart))
  
  # convert values from [0-1] to [0-100]
  df$value = 100*df$value
  
  # Initialize variables to satisfy R CMD check
  gene = value = variable = 0
  
  # Flip order of columns for use with ggplot2 2.2.0
  # Nov 17, 2016
  fig = ggplot(df, aes(x = gene, 
                       y = value, 
                       fill = variable,
                       # alpha = 1
  ), 
  ) +
    geom_bar(stat = "identity", 
             position = position_fill(reverse = TRUE)
    ) +
    #theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    #coord_flip() +
    xlab("") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  fig = fig + theme(
    text = element_text(size=13),
    axis.text.x = element_text(angle=90, hjust=1, vjust = 1),
    axis.line = element_line(colour = "transparent"),
    # axis.line.x = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks.y = element_blank(),
    legend.key = element_blank(),
    plot.margin = unit(c(0, .5, 0, 0), "cm")
  ) +
    #guides(fill = guide_legend(title = NULL)) +
    scale_fill_manual(values = col) +
    scale_x_discrete(
      breaks = seq(0, 100, by = 20),
      label = seq(100, 0, by = -20),
      expand = c(0, .3)
    ) +
    labs(y = "", 
         x = "", 
         fill = "Predictor")
  
  fig = fig + theme(text = element_text(colour="black"), 
                    axis.text 	= element_text(colour="black"),
                    # legend.text = element_text(colour="black", size = 13),
                    # legend.title = element_text(face = 'bold', size = 16
  )
  
  fig	
}

