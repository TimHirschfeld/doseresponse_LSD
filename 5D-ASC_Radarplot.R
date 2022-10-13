library(readxl)
library(fmsb)

#preliminaries
setwd("C:/PC fix/META-analysis/RStudio_Analyse/5D-ASC")

data <- read_excel("5D-ASC_Radarplot.xlsx", col_names = TRUE, col_types = "numeric")

#######################################################################
# define that all data numeric values
spider_data <- lapply(data, as.numeric)

# create dataframe
spider_data <- data.frame(spider_data$Auditory_Alterations,
                          spider_data$Vigilance_Reduction,
                          spider_data$Visionary_Restructuralization,
                          spider_data$Oceanic_Boundlessness,
                          spider_data$Dread_of_Ego_Dissolution)

# column names are added later
colnames(spider_data)=c("",
                        "",
                        "",
                        "",
                        "")

# To use the fmsb package, one has to add 2 rows to the dataframe: the max and min of each dimension
spider_data <- rbind(rep(100,5), rep(0,5), spider_data)

# define colors 
colors_border=c( "#9ecae1", "#6baed6" , "#4292c6", "#2171b5" )

# plotting
tiff(filename = "5D-ASC_Radarplot.tiff", units="in", width=30, height=15, res=600) 
radarchart(spider_data, axistype=0, 
           #custom polygon
           pcol=colors_border, plwd=7 , plty=1,
           #custom the grid
           cglcol="Black", cglty=1, axislabcol="Black", caxislabels=NULL, calcex=1.56, cglwd=0.8, vlcex=2.5
)
dev.off()
