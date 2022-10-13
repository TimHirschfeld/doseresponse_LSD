library(readxl)
library(fmsb)

#preliminaries
setwd("C:/PC fix/META-analysis/RStudio_Analyse/MEQ30")

data <- read_excel("MEQ30_Radarplot.xlsx", col_names = TRUE, col_types = "numeric")

#######################################################################
# define that all data numeric values
spider_data <- lapply(data, as.numeric)

#create dataframe
spider_data <- data.frame(spider_data$Ineff,
                          spider_data$Myst,
                          spider_data$Pos,
                          spider_data$Tran)

colnames(spider_data)=c("",
                        "",
                        "",
                        "")

# To use the fmsb package, one has to add 2 rows to the dataframe: the max and min of each dimension
spider_data <- rbind(rep(100,4), rep(0,4), spider_data)

# define colors 
colors_border=c("#9ecae1", "#6baed6" , "#4292c6", "#2171b5", "#08519C", "#08306B")

# plotting
tiff(filename = "MEQ30_Radarplot.tiff", units="in", width=30, height=15, res=600)
radarchart(spider_data, axistype=0, 
           #custom polygon
           pcol=colors_border, plwd=7 , plty=1,
           #custom the grid
           cglcol="Black", cglty=1, axislabcol="Black", caxislabels=NULL , calcex=1.56, cglwd=0.8,
           vlcex=2.5
)
dev.off()
