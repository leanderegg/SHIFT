library(calecopal)
library(MetBrewer)
#make ggplot prettier

ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank())  # Hide minor gridlines
}

nb.cols <- 58
pal <- met.brewer("Cross")
mycolors <- colorRampPalette(pal)(nb.cols)

color_very_many <- scale_color_manual(values = mycolors) 

nb.cols.10 <- 13
pal.10 <- met.brewer("Cross")
mycolors.10 <- colorRampPalette(pal)(nb.cols.10)

color_many <- scale_color_manual(values = mycolors.10) 
  
color_two <- scale_color_manual(values = met.brewer("Cross", 4)) 

color_fill <- scale_fill_manual(values = met.brewer("Cross"), 4, #direction = -1
                                )

color_grad <- scale_color_gradientn(colors=met.brewer("Cross" #direction = -1)
                                                      ))

#Other color options: 

#install.packages("rcartocolor")
# or
# install.packages("remotes")
#remotes::install_github("Nowosad/rcartocolor")
#library(rcartocolor)


