library(calecopal)
library(MetBrewer)
#make ggplot prettier

ggplot <- function(...) { ggplot2::ggplot(...) + 
    theme(panel.background = element_rect(fill='white', colour='black'), # Make background white and border black
          panel.grid.major = element_blank(),  # Hide major gridlines
          panel.grid.minor = element_blank())  # Hide minor gridlines
}


greys <- list(c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000"))


Cross = list(c("#c969a1", "#ce4441", "#ee8577", "#eb7926", "#ffbb44", "#859b6c", "#62929a", "#004f63", "#122451"), 
             c(4, 7, 1, 8, 2, 6, 3, 5, 9), 
             colorblind=FALSE)

Cross_rev = c("#122451", "#063E5C", "#165E6F", "#528791", "#729684", "#8E9D68", "#D9B150", "#F8A63A" ,"#EC7E28", "#EC7F51", "#E97B6E", "#D5534D", "#CC5265", "#C969A1")


#Many colors: 

nb.cols <- 58
pal <- met.brewer("Cross")
mycolors <- colorRampPalette(pal)(nb.cols)

color_very_many <- scale_color_manual(values = mycolors) 

#Colors for weeks: 

      #Weeks = 9 10 11 12 13 14 15 17 18 19 21 29 33 37

nb.cols.14 <- 14
pal.14 <- met.brewer("Cross")
mycolors.14 <- colorRampPalette(pal)(nb.cols.14)

color_week_all <- scale_color_manual(values = mycolors.14)

# color_week_all <- scale_color_manual(values = c(
#   37 = "#C969A9",
#   33 = "#C969A1",
#   29 = "#CC5265",
#   21 =  "#D5534D",
#   19 =  "#E97B6E" ,
#   18 = "#EC7F51",
#   17 = "#EC7E28",
#   16 = "#F8A63A",
#   15 = "#D9B150" ,
#   14 = "#8E9D68",
#   13 = "#729684",
#   12 =  "#528791",
#   11 = "#165E6F",
#   10 = "#063E5C",
#   9 = "#122451" 
# ))

#For variance decmp: 
color_fill_vdc <- scale_fill_manual(values = c("Residuals" = "#D9D9D9",
                                               "Site" = "#252525",
                                               "Tree" =  "#525252",
                                               "LWC" = "#969696" ,
                                               "Week" = "#62929a"))

nb.cols.10 <- 13
pal.10 <- met.brewer("Cross")
mycolors.10 <- colorRampPalette(pal)(nb.cols.10)
mycolors.10

color_many_2 <- scale_color_manual(values = c("#122451", "#165E6F", "#528791",  "#8E9D68",
                                              "#D9B150", "#F8A63A" ,"#EC7E28", "#E97B6E",  
                                              "#CC5265", "#C969A1"))

color_many <- scale_color_manual(values = c(Cross_more))

cross_discrete =  MetBrewer::met.brewer(name="Cross", type="discrete")

color_weeks <- scale_color_manual(values = cross_discrete)
  
color_two <- scale_color_manual(values = met.brewer("Cross", 4)) 

color_fill <- scale_fill_manual(values = met.brewer("Cross"), 4) #direction = -1

color_grad <- scale_color_gradientn(colors=met.brewer("Cross", direction = -1)
                                                      )

