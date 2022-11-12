library(calecopal)
library(MetBrewer)
#make ggplot prettier

ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank())  # Hide minor gridlines
}


color_many <- scale_color_manual(values = met.brewer("Tiepolo", direction = -1, type = "discrete")) 
  
color_two <- scale_color_manual(values = met.brewer("Tiepolo", 4 ,direction = -1)) 


color_fill <- scale_fill_manual(values = met.brewer("Tiepolo"))

color_grad <- scale_color_gradientn(colors=met.brewer("Tiepolo", direction = -1))

#Other color options: 

#install.packages("rcartocolor")
# or
# install.packages("remotes")
#remotes::install_github("Nowosad/rcartocolor")
#library(rcartocolor)

#nColor <- 12
#scales::show_col(carto_pal(nColor, "Safe"))

