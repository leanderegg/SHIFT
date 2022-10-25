library(calecopal)

#make ggplot prettier

ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank()) +  # Hide minor gridlines
    scale_color_manual(values = cal_palette("superbloom3"))
}