library(readxl)
library(fmsb)
library(robumeta)

#preliminaries
setwd("C:/PC fix/META-analysis/RStudio_Analyse/5D-ASC")

data <- read_excel("5D-ASC_AA.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

##############################################################################
# Individuelle Regressionsanalyse  #

#defining variables as numeric
Scale_std <- as.numeric(data$Auditory_Alterations_std)        # standard deviation
effect_sizes <- as.numeric(data$Auditory_Alterations_mean)    # mean
dosages <- as.numeric(data$dosage_quantity)                   # dosages
participants <- as.numeric(data$nr_of_subjects)               # Nr of study participants

#transform std to variance 
variance <- Scale_std^2/participants

#defining studynum (required for model; each study has a specific number)
studynum <- data$studynum

#analysis:
model <- robu(effect_sizes ~ dosages,
              data = data,
              studynum = studynum,
              var.eff.size = variance,
              rho = 0.8,
              small=TRUE)
# results:
model

# sensitivity analysis
sensitivity(model)

########################################################################
# Scatterplot #

#In "$r.weights" the weights are ordered alphabetically depending on the study name
weights <- model$data.full$r.weights 

#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  

#defining colors; number corresponds to the number of effect sizes (different doses) for each study)
#colors <-   c(rep("deeppink", 2),        #27714429
 #             rep("orangered4", 1),      #25575620
  #            rep("yellowgreen", 1),     #31733631
   #           rep("red", 4),             #33059356
    #          rep("darkorange", 1),      #28940312
     #         rep("blue", 2),            #35217796
      #        rep("cyan", 3)             #35253516
#)
colors <-   c(rep("#6a3d9a", 1),        #27714429
              rep("#b2df8a", 1),      #25575620
              rep("#33a02c", 1),     #31733631
              rep("#e31a1c", 4),             #33059356
              rep("darkorange", 1),      #28940312
              rep("#1f78b4", 2),            #35217796
              rep("cyan", 3)             #35253516
)

#plotting
#(put # before tiff to see plot in R)

tiff(filename = "5D-ASC_AA.tiff", units="cm", width=14.55, height=10.184, res=600)
par(mar=c(2.1,2.5,0.6,0.8))
plot(dosages, effect_sizes, cex=weights_norm*30, bty="n",pch=1, col=colors, yaxs="i", xaxs="i",
     lwd=3, 
     main=paste0(""), #("5D-ASC Auditory Alterations"),
     xlab="",
     ylab="",
     xlim=c(0, 250),
     ylim=c(0, 100),
     las=1
     )
axis(side = 1, at = c(0, 50, 100, 150, 200, 250))
#title(ylab="(%)", mgp=c(2.5,1,0), cex.lab=1)
#title(xlab= "??g LSD", mgp=c(2.1,1,0), cex.lab=1)
lines(seq(0,250,0.001), model$b.r[1]+model$b.r[2]*seq(0,250,0.001), lwd=2)
# color legend: legend(x = 1,y = 100, legend=colors,fill = colors,col = colors,border = colors)
dev.off() #??g