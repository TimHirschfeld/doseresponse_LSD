library(readxl)
library(fmsb)
library(ggplot2)
library(plotly)
library(clubSandwich)
library(metafor)
library(clubSandwich)
library(rms)
library(scales)
library(ggthemes)
library(echarts4r)

########################################################################################################################
data <- read_excel("C:/5D-ASC.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

data$Scale_std <- as.numeric(data$Oceanic_Boundlessness_std)        
data$effect_sizes <- as.numeric(data$Oceanic_Boundlessness_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots1 <- attr(rcs(model.matrix(robustres1)[,2], 3), "parms")
OB20 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(20, knots1, inclx=TRUE), addx = TRUE))
OB40 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(40, knots1, inclx=TRUE), addx = TRUE))
OB60 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(60, knots1, inclx=TRUE), addx = TRUE))
OB80 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(80, knots1, inclx=TRUE), addx = TRUE))
OB100 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(100, knots1, inclx=TRUE), addx = TRUE))
OB120 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(120, knots1, inclx=TRUE), addx = TRUE))
OB140 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(140, knots1, inclx=TRUE), addx = TRUE))
OB160 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(160, knots1, inclx=TRUE), addx = TRUE))
OB180 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(180, knots1, inclx=TRUE), addx = TRUE))
OB200 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(200, knots1, inclx=TRUE), addx = TRUE))
OB10 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(10, knots1, inclx=TRUE), addx = TRUE))
OB30 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(30, knots1, inclx=TRUE), addx = TRUE))
OB50 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(50, knots1, inclx=TRUE), addx = TRUE))
OB70 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(70, knots1, inclx=TRUE), addx = TRUE))
OB90 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(90, knots1, inclx=TRUE), addx = TRUE))
OB110 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(110, knots1, inclx=TRUE), addx = TRUE))
OB130 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(130, knots1, inclx=TRUE), addx = TRUE))
OB150 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(150, knots1, inclx=TRUE), addx = TRUE))
OB170 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(170, knots1, inclx=TRUE), addx = TRUE))
OB190 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(190, knots1, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Dread_of_Ego_Dissolution_std)        
data$effect_sizes <- as.numeric(data$Dread_of_Ego_Dissolution_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),random = ~ (1|main_author),data=data)
robustres3 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots3 <- attr(rcs(model.matrix(robustres3)[,2], 3), "parms")
DED20 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(20, knots3, inclx=TRUE), addx = TRUE))
DED40 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(40, knots3, inclx=TRUE), addx = TRUE))
DED60 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(60, knots3, inclx=TRUE), addx = TRUE))
DED80 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(80, knots3, inclx=TRUE), addx = TRUE))
DED100 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(100, knots3, inclx=TRUE), addx = TRUE))
DED120 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(120, knots3, inclx=TRUE), addx = TRUE))
DED140 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(140, knots3, inclx=TRUE), addx = TRUE))
DED160 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(160, knots3, inclx=TRUE), addx = TRUE))
DED180 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(180, knots3, inclx=TRUE), addx = TRUE))
DED200 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(200, knots3, inclx=TRUE), addx = TRUE))
DED10 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(10, knots3, inclx=TRUE), addx = TRUE))
DED30 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(30, knots3, inclx=TRUE), addx = TRUE))
DED50 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(50, knots3, inclx=TRUE), addx = TRUE))
DED70 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(70, knots3, inclx=TRUE), addx = TRUE))
DED90 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(90, knots3, inclx=TRUE), addx = TRUE))
DED110 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(110, knots3, inclx=TRUE), addx = TRUE))
DED130 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(130, knots3, inclx=TRUE), addx = TRUE))
DED150 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(150, knots3, inclx=TRUE), addx = TRUE))
DED170 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(170, knots3, inclx=TRUE), addx = TRUE))
DED190 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(190, knots3, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Visionary_Restructuralization_std)        
data$effect_sizes <- as.numeric(data$Visionary_Restructuralization_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),random = ~ (1|main_author),data=data)
robustres4 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots4 <- attr(rcs(model.matrix(robustres4)[,2], 3), "parms")
VisR20 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(20, knots4, inclx=TRUE), addx = TRUE))
VisR40 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(40, knots4, inclx=TRUE), addx = TRUE))
VisR60 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(60, knots4, inclx=TRUE), addx = TRUE))
VisR80 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(80, knots4, inclx=TRUE), addx = TRUE))
VisR100 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(100, knots4, inclx=TRUE), addx = TRUE))
VisR120 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(120, knots4, inclx=TRUE), addx = TRUE))
VisR140 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(140, knots4, inclx=TRUE), addx = TRUE))
VisR160 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(160, knots4, inclx=TRUE), addx = TRUE))
VisR180 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(180, knots4, inclx=TRUE), addx = TRUE))
VisR200 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(200, knots4, inclx=TRUE), addx = TRUE))
VisR10 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(10, knots4, inclx=TRUE), addx = TRUE))
VisR30 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(30, knots4, inclx=TRUE), addx = TRUE))
VisR50 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(50, knots4, inclx=TRUE), addx = TRUE))
VisR70 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(70, knots4, inclx=TRUE), addx = TRUE))
VisR90 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(90, knots4, inclx=TRUE), addx = TRUE))
VisR110 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(110, knots4, inclx=TRUE), addx = TRUE))
VisR130 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(130, knots4, inclx=TRUE), addx = TRUE))
VisR150 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(150, knots4, inclx=TRUE), addx = TRUE))
VisR170 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(170, knots4, inclx=TRUE), addx = TRUE))
VisR190 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(190, knots4, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Vigilance_Reduction_std)        
data$effect_sizes <- as.numeric(data$Vigilance_Reduction_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),random = ~ (1|main_author),data=data)
robustres2 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots2 <- attr(rcs(model.matrix(robustres2)[,2], 3), "parms")
VigR20 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(20, knots2, inclx=TRUE), addx = TRUE))
VigR40 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(40, knots2, inclx=TRUE), addx = TRUE))
VigR60 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(60, knots2, inclx=TRUE), addx = TRUE))
VigR80 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(80, knots2, inclx=TRUE), addx = TRUE))
VigR100 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(100, knots2, inclx=TRUE), addx = TRUE))
VigR120 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(120, knots2, inclx=TRUE), addx = TRUE))
VigR140 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(140, knots2, inclx=TRUE), addx = TRUE))
VigR160 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(160, knots2, inclx=TRUE), addx = TRUE))
VigR180 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(180, knots2, inclx=TRUE), addx = TRUE))
VigR200 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(200, knots2, inclx=TRUE), addx = TRUE))
VigR10 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(10, knots2, inclx=TRUE), addx = TRUE))
VigR30 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(30, knots2, inclx=TRUE), addx = TRUE))
VigR50 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(50, knots2, inclx=TRUE), addx = TRUE))
VigR70 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(70, knots2, inclx=TRUE), addx = TRUE))
VigR90 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(90, knots2, inclx=TRUE), addx = TRUE))
VigR110 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(110, knots2, inclx=TRUE), addx = TRUE))
VigR130 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(130, knots2, inclx=TRUE), addx = TRUE))
VigR150 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(150, knots2, inclx=TRUE), addx = TRUE))
VigR170 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(170, knots2, inclx=TRUE), addx = TRUE))
VigR190 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(190, knots2, inclx=TRUE), addx = TRUE))


data$Scale_std <- as.numeric(data$Auditory_Alterations_std)        
data$effect_sizes <- as.numeric(data$Auditory_Alterations_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres5 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots5 <- attr(rcs(model.matrix(robustres5)[,2], 3), "parms")
AA20 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(20, knots5, inclx=TRUE), addx = TRUE))
AA40 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(40, knots5, inclx=TRUE), addx = TRUE))
AA60 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(60, knots5, inclx=TRUE), addx = TRUE))
AA80 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(80, knots5, inclx=TRUE), addx = TRUE))
AA100 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(100, knots5, inclx=TRUE), addx = TRUE))
AA120 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(120, knots5, inclx=TRUE), addx = TRUE))
AA140 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(140, knots5, inclx=TRUE), addx = TRUE))
AA160 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(160, knots5, inclx=TRUE), addx = TRUE))
AA180 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(180, knots5, inclx=TRUE), addx = TRUE))
AA200 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(200, knots5, inclx=TRUE), addx = TRUE))
AA10 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(10, knots5, inclx=TRUE), addx = TRUE))
AA30 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(30, knots5, inclx=TRUE), addx = TRUE))
AA50 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(50, knots5, inclx=TRUE), addx = TRUE))
AA70 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(70, knots5, inclx=TRUE), addx = TRUE))
AA90 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(90, knots5, inclx=TRUE), addx = TRUE))
AA110 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(110, knots5, inclx=TRUE), addx = TRUE))
AA130 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(130, knots5, inclx=TRUE), addx = TRUE))
AA150 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(150, knots5, inclx=TRUE), addx = TRUE))
AA170 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(170, knots5, inclx=TRUE), addx = TRUE))
AA190 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(190, knots5, inclx=TRUE), addx = TRUE))

df<-structure(list(AA=c(AA10[1], AA20[1], AA30[1], AA40[1], AA50[1], AA60[1], AA70[1], AA80[1], AA90[1], AA100[1], AA110[1], AA120[1], AA130[1], AA140[1], AA150[1], AA160[1], AA170[1], AA180[1], AA190[1], AA200[1]),
                   VigR=c(VigR10[1], VigR20[1], VigR30[1], VigR40[1], VigR50[1], VigR60[1], VigR70[1], VigR80[1], VigR90[1], VigR100[1], VigR110[1], VigR120[1], VigR130[1], VigR140[1], VigR150[1], VigR160[1], VigR170[1], VigR180[1], VigR190[1], VigR200[1]),
                   VisR=c(VisR10[1], VisR20[1], VisR30[1], VisR40[1], VisR50[1], VisR60[1], VisR70[1], VisR80[1], VisR90[1], VisR100[1], VisR110[1], VisR120[1], VisR130[1], VisR140[1], VisR150[1], VisR160[1], VisR170[1], VisR180[1], VisR190[1], VisR200[1]),
                   OB=c(OB10[1], OB20[1], OB30[1], OB40[1], OB50[1], OB60[1], OB70[1], OB80[1], OB90[1], OB100[1], OB110[1], OB120[1], OB130[1], OB140[1], OB150[1], OB160[1], OB170[1], OB180[1], OB190[1], OB200[1]),
                   DED=c(DED10[1], DED20[1], DED30[1], DED40[1], DED50[1], DED60[1], DED70[1], DED80[1], DED90[1], DED100[1], DED110[1], DED120[1], DED130[1], DED140[1], DED150[1], DED160[1], DED170[1], DED180[1], DED190[1], DED200[1])),
              .Names=c("AA","VigR","VisR","OB","DED"),row.names=c(NA, -20L),class="data.frame")

# To use the fmsb package, one has to add 2 rows to the dataframe: the max and min of each dimension
spider_data <- rbind(rep(100,5), rep(0,5), df)
spider_data <- spider_data %>%
  mutate(AA = as.numeric(AA),
         VigR = as.numeric(VigR),
         VisR = as.numeric(VisR),
         OB = as.numeric(OB),
         DED = as.numeric(DED))

colors_border=c("#CAE1EFFF","#C1DAECFF","#B6D3E8FF","#ACCCE4FF","#A1C4E0FF","#97BDDCFF","#8CB5D8FF",
                           "#81ACD4FF","#76A4CFFF","#6A9BCBFF","#5F92C7FF","#5389C2FF","#4780BEFF",
                           "#3A76BAFF","#316DB3FF","#3263A7FF","#31599BFF","#2F4F8FFF","#2D4682FF", "#2B417CFF")

par("mar")
par(mar=c(1,1,1,1))

tiff(filename = "Spider_5D-ASC_reanalysis.tiff", units="cm", width=15, height=15, res=600)
radarchart(spider_data, axistype=0, 
           #custom polygon
           pcol=colors_border, plwd=c(1,1,1,1,3,1,1,1,1,3,1,1,1,1,3,1,1,1,1,3) , plty=1,
           #custom the grid
           cglcol="Black", cglty=1, axislabcol="Black", caxislabels=NULL , calcex=1.56, cglwd=0.8,
           #custom labels
           vlcex=1.5
)
#ggsave(file="test.svg", plot=image, width=10, height=8)
dev.off()


#####################################################
# 11-ASC

data <- read_excel("C:/11-ASC.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

data$Scale_std <- as.numeric(data$Elementary_imagery_std)        
data$effect_sizes <- as.numeric(data$Elementary_imagery_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots1 <- attr(rcs(model.matrix(robustres1)[,2], 4), "parms")
EI20 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(20, knots1, inclx=TRUE), addx = TRUE))
EI40 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(40, knots1, inclx=TRUE), addx = TRUE))
EI60 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(60, knots1, inclx=TRUE), addx = TRUE))
EI80 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(80, knots1, inclx=TRUE), addx = TRUE))
EI100 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(100, knots1, inclx=TRUE), addx = TRUE))
EI120 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(120, knots1, inclx=TRUE), addx = TRUE))
EI140 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(140, knots1, inclx=TRUE), addx = TRUE))
EI160 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(160, knots1, inclx=TRUE), addx = TRUE))
EI180 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(180, knots1, inclx=TRUE), addx = TRUE))
EI200 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(200, knots1, inclx=TRUE), addx = TRUE))
EI10 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(10, knots1, inclx=TRUE), addx = TRUE))
EI30 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(30, knots1, inclx=TRUE), addx = TRUE))
EI50 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(50, knots1, inclx=TRUE), addx = TRUE))
EI70 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(70, knots1, inclx=TRUE), addx = TRUE))
EI90 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(90, knots1, inclx=TRUE), addx = TRUE))
EI110 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(110, knots1, inclx=TRUE), addx = TRUE))
EI130 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(130, knots1, inclx=TRUE), addx = TRUE))
EI150 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(150, knots1, inclx=TRUE), addx = TRUE))
EI170 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(170, knots1, inclx=TRUE), addx = TRUE))
EI190 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(190, knots1, inclx=TRUE), addx = TRUE))


data$Scale_std <- as.numeric(data$Complex_imagery_std)        
data$effect_sizes <- as.numeric(data$Complex_imagery_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres2 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots2 <- attr(rcs(model.matrix(robustres2)[,2], 4), "parms")
CI20 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(20, knots2, inclx=TRUE), addx = TRUE))
CI40 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(40, knots2, inclx=TRUE), addx = TRUE))
CI60 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(60, knots2, inclx=TRUE), addx = TRUE))
CI80 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(80, knots2, inclx=TRUE), addx = TRUE))
CI100 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(100, knots2, inclx=TRUE), addx = TRUE))
CI120 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(120, knots2, inclx=TRUE), addx = TRUE))
CI140 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(140, knots2, inclx=TRUE), addx = TRUE))
CI160 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(160, knots2, inclx=TRUE), addx = TRUE))
CI180 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(180, knots2, inclx=TRUE), addx = TRUE))
CI200 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(200, knots2, inclx=TRUE), addx = TRUE))
CI10 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(10, knots2, inclx=TRUE), addx = TRUE))
CI30 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(30, knots2, inclx=TRUE), addx = TRUE))
CI50 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(50, knots2, inclx=TRUE), addx = TRUE))
CI70 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(70, knots2, inclx=TRUE), addx = TRUE))
CI90 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(90, knots2, inclx=TRUE), addx = TRUE))
CI110 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(110, knots2, inclx=TRUE), addx = TRUE))
CI130 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(130, knots2, inclx=TRUE), addx = TRUE))
CI150 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(150, knots2, inclx=TRUE), addx = TRUE))
CI170 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(170, knots2, inclx=TRUE), addx = TRUE))
CI190 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(190, knots2, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Audio_visual_synesthesia_std)        
data$effect_sizes <- as.numeric(data$Audio_visual_synesthesia_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres3 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots3 <- attr(rcs(model.matrix(robustres3)[,2], 4), "parms")
AVS20 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(20, knots3, inclx=TRUE), addx = TRUE))
AVS40 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(40, knots3, inclx=TRUE), addx = TRUE))
AVS60 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(60, knots3, inclx=TRUE), addx = TRUE))
AVS80 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(80, knots3, inclx=TRUE), addx = TRUE))
AVS100 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(100, knots3, inclx=TRUE), addx = TRUE))
AVS120 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(120, knots3, inclx=TRUE), addx = TRUE))
AVS140 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(140, knots3, inclx=TRUE), addx = TRUE))
AVS160 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(160, knots3, inclx=TRUE), addx = TRUE))
AVS180 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(180, knots3, inclx=TRUE), addx = TRUE))
AVS200 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(200, knots3, inclx=TRUE), addx = TRUE))
AVS10 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(10, knots3, inclx=TRUE), addx = TRUE))
AVS30 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(30, knots3, inclx=TRUE), addx = TRUE))
AVS50 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(50, knots3, inclx=TRUE), addx = TRUE))
AVS70 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(70, knots3, inclx=TRUE), addx = TRUE))
AVS90 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(90, knots3, inclx=TRUE), addx = TRUE))
AVS110 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(110, knots3, inclx=TRUE), addx = TRUE))
AVS130 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(130, knots3, inclx=TRUE), addx = TRUE))
AVS150 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(150, knots3, inclx=TRUE), addx = TRUE))
AVS170 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(170, knots3, inclx=TRUE), addx = TRUE))
AVS190 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(190, knots3, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Changed_meaning_of_percepts_std)        
data$effect_sizes <- as.numeric(data$Changed_meaning_of_percepts_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres4 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots4 <- attr(rcs(model.matrix(robustres4)[,2], 4), "parms")
CMP20 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(20, knots4, inclx=TRUE), addx = TRUE))
CMP40 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(40, knots4, inclx=TRUE), addx = TRUE))
CMP60 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(60, knots4, inclx=TRUE), addx = TRUE))
CMP80 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(80, knots4, inclx=TRUE), addx = TRUE))
CMP100 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(100, knots4, inclx=TRUE), addx = TRUE))
CMP120 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(120, knots4, inclx=TRUE), addx = TRUE))
CMP140 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(140, knots4, inclx=TRUE), addx = TRUE))
CMP160 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(160, knots4, inclx=TRUE), addx = TRUE))
CMP180 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(180, knots4, inclx=TRUE), addx = TRUE))
CMP200 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(200, knots4, inclx=TRUE), addx = TRUE))
CMP10 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(10, knots4, inclx=TRUE), addx = TRUE))
CMP30 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(30, knots4, inclx=TRUE), addx = TRUE))
CMP50 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(50, knots4, inclx=TRUE), addx = TRUE))
CMP70 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(70, knots4, inclx=TRUE), addx = TRUE))
CMP90 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(90, knots4, inclx=TRUE), addx = TRUE))
CMP110 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(110, knots4, inclx=TRUE), addx = TRUE))
CMP130 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(130, knots4, inclx=TRUE), addx = TRUE))
CMP150 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(150, knots4, inclx=TRUE), addx = TRUE))
CMP170 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(170, knots4, inclx=TRUE), addx = TRUE))
CMP190 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(190, knots4, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Insightfulness_std)        
data$effect_sizes <- as.numeric(data$Insightfulness_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres5 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots5 <- attr(rcs(model.matrix(robustres5)[,2], 4), "parms")
I20 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(20, knots5, inclx=TRUE), addx = TRUE))
I40 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(40, knots5, inclx=TRUE), addx = TRUE))
I60 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(60, knots5, inclx=TRUE), addx = TRUE))
I80 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(80, knots5, inclx=TRUE), addx = TRUE))
I100 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(100, knots5, inclx=TRUE), addx = TRUE))
I120 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(120, knots5, inclx=TRUE), addx = TRUE))
I140 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(140, knots5, inclx=TRUE), addx = TRUE))
I160 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(160, knots5, inclx=TRUE), addx = TRUE))
I180 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(180, knots5, inclx=TRUE), addx = TRUE))
I200 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(200, knots5, inclx=TRUE), addx = TRUE))
I10 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(10, knots5, inclx=TRUE), addx = TRUE))
I30 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(30, knots5, inclx=TRUE), addx = TRUE))
I50 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(50, knots5, inclx=TRUE), addx = TRUE))
I70 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(70, knots5, inclx=TRUE), addx = TRUE))
I90 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(90, knots5, inclx=TRUE), addx = TRUE))
I110 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(110, knots5, inclx=TRUE), addx = TRUE))
I130 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(130, knots5, inclx=TRUE), addx = TRUE))
I150 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(150, knots5, inclx=TRUE), addx = TRUE))
I170 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(170, knots5, inclx=TRUE), addx = TRUE))
I190 <- as.data.frame(predict(robustres5, newmods=rcspline.eval(190, knots5, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Spiritual_experience_std)        
data$effect_sizes <- as.numeric(data$Spiritual_experience_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres6 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots6 <- attr(rcs(model.matrix(robustres6)[,2], 4), "parms")
SE20 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(20, knots6, inclx=TRUE), addx = TRUE))
SE40 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(40, knots6, inclx=TRUE), addx = TRUE))
SE60 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(60, knots6, inclx=TRUE), addx = TRUE))
SE80 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(80, knots6, inclx=TRUE), addx = TRUE))
SE100 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(100, knots6, inclx=TRUE), addx = TRUE))
SE120 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(120, knots6, inclx=TRUE), addx = TRUE))
SE140 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(140, knots6, inclx=TRUE), addx = TRUE))
SE160 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(160, knots6, inclx=TRUE), addx = TRUE))
SE180 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(180, knots6, inclx=TRUE), addx = TRUE))
SE200 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(200, knots6, inclx=TRUE), addx = TRUE))
SE10 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(10, knots6, inclx=TRUE), addx = TRUE))
SE30 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(30, knots6, inclx=TRUE), addx = TRUE))
SE50 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(50, knots6, inclx=TRUE), addx = TRUE))
SE70 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(70, knots6, inclx=TRUE), addx = TRUE))
SE90 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(90, knots6, inclx=TRUE), addx = TRUE))
SE110 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(110, knots6, inclx=TRUE), addx = TRUE))
SE130 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(130, knots6, inclx=TRUE), addx = TRUE))
SE150 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(150, knots6, inclx=TRUE), addx = TRUE))
SE170 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(170, knots6, inclx=TRUE), addx = TRUE))
SE190 <- as.data.frame(predict(robustres6, newmods=rcspline.eval(190, knots6, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Experience_of_unity_std)        
data$effect_sizes <- as.numeric(data$Experience_of_unity_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres7 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots7 <- attr(rcs(model.matrix(robustres7)[,2], 4), "parms")
EU20 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(20, knots7, inclx=TRUE), addx = TRUE))
EU40 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(40, knots7, inclx=TRUE), addx = TRUE))
EU60 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(60, knots7, inclx=TRUE), addx = TRUE))
EU80 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(80, knots7, inclx=TRUE), addx = TRUE))
EU100 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(100, knots7, inclx=TRUE), addx = TRUE))
EU120 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(120, knots7, inclx=TRUE), addx = TRUE))
EU140 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(140, knots7, inclx=TRUE), addx = TRUE))
EU160 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(160, knots7, inclx=TRUE), addx = TRUE))
EU180 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(180, knots7, inclx=TRUE), addx = TRUE))
EU200 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(200, knots7, inclx=TRUE), addx = TRUE))
EU10 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(10, knots7, inclx=TRUE), addx = TRUE))
EU30 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(30, knots7, inclx=TRUE), addx = TRUE))
EU50 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(50, knots7, inclx=TRUE), addx = TRUE))
EU70 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(70, knots7, inclx=TRUE), addx = TRUE))
EU90 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(90, knots7, inclx=TRUE), addx = TRUE))
EU110 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(110, knots7, inclx=TRUE), addx = TRUE))
EU130 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(130, knots7, inclx=TRUE), addx = TRUE))
EU150 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(150, knots7, inclx=TRUE), addx = TRUE))
EU170 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(170, knots7, inclx=TRUE), addx = TRUE))
EU190 <- as.data.frame(predict(robustres7, newmods=rcspline.eval(190, knots7, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Blissful_state_std)        
data$effect_sizes <- as.numeric(data$Blissful_state_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres8 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots8 <- attr(rcs(model.matrix(robustres8)[,2], 4), "parms")
BS20 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(20, knots8, inclx=TRUE), addx = TRUE))
BS40 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(40, knots8, inclx=TRUE), addx = TRUE))
BS60 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(60, knots8, inclx=TRUE), addx = TRUE))
BS80 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(80, knots8, inclx=TRUE), addx = TRUE))
BS100 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(100, knots8, inclx=TRUE), addx = TRUE))
BS120 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(120, knots8, inclx=TRUE), addx = TRUE))
BS140 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(140, knots8, inclx=TRUE), addx = TRUE))
BS160 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(160, knots8, inclx=TRUE), addx = TRUE))
BS180 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(180, knots8, inclx=TRUE), addx = TRUE))
BS200 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(200, knots8, inclx=TRUE), addx = TRUE))
BS10 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(10, knots8, inclx=TRUE), addx = TRUE))
BS30 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(30, knots8, inclx=TRUE), addx = TRUE))
BS50 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(50, knots8, inclx=TRUE), addx = TRUE))
BS70 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(70, knots8, inclx=TRUE), addx = TRUE))
BS90 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(90, knots8, inclx=TRUE), addx = TRUE))
BS110 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(110, knots8, inclx=TRUE), addx = TRUE))
BS130 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(130, knots8, inclx=TRUE), addx = TRUE))
BS150 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(150, knots8, inclx=TRUE), addx = TRUE))
BS170 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(170, knots8, inclx=TRUE), addx = TRUE))
BS190 <- as.data.frame(predict(robustres8, newmods=rcspline.eval(190, knots8, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Disembodiment_std)        
data$effect_sizes <- as.numeric(data$Disembodiment_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres9 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots9 <- attr(rcs(model.matrix(robustres9)[,2], 4), "parms")
D20 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(20, knots9, inclx=TRUE), addx = TRUE))
D40 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(40, knots9, inclx=TRUE), addx = TRUE))
D60 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(60, knots9, inclx=TRUE), addx = TRUE))
D80 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(80, knots9, inclx=TRUE), addx = TRUE))
D100 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(100, knots9, inclx=TRUE), addx = TRUE))
D120 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(120, knots9, inclx=TRUE), addx = TRUE))
D140 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(140, knots9, inclx=TRUE), addx = TRUE))
D160 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(160, knots9, inclx=TRUE), addx = TRUE))
D180 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(180, knots9, inclx=TRUE), addx = TRUE))
D200 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(200, knots9, inclx=TRUE), addx = TRUE))
D10 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(10, knots9, inclx=TRUE), addx = TRUE))
D30 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(30, knots9, inclx=TRUE), addx = TRUE))
D50 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(50, knots9, inclx=TRUE), addx = TRUE))
D70 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(70, knots9, inclx=TRUE), addx = TRUE))
D90 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(90, knots9, inclx=TRUE), addx = TRUE))
D110 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(110, knots9, inclx=TRUE), addx = TRUE))
D130 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(130, knots9, inclx=TRUE), addx = TRUE))
D150 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(150, knots9, inclx=TRUE), addx = TRUE))
D170 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(170, knots9, inclx=TRUE), addx = TRUE))
D190 <- as.data.frame(predict(robustres9, newmods=rcspline.eval(190, knots9, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Impaired_control_and_cognition_std)        
data$effect_sizes <- as.numeric(data$Impaired_control_and_cognition_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres10 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots10 <- attr(rcs(model.matrix(robustres10)[,2], 4), "parms")
ICC20 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(20, knots10, inclx=TRUE), addx = TRUE))
ICC40 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(40, knots10, inclx=TRUE), addx = TRUE))
ICC60 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(60, knots10, inclx=TRUE), addx = TRUE))
ICC80 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(80, knots10, inclx=TRUE), addx = TRUE))
ICC100 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(100, knots10, inclx=TRUE), addx = TRUE))
ICC120 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(120, knots10, inclx=TRUE), addx = TRUE))
ICC140 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(140, knots10, inclx=TRUE), addx = TRUE))
ICC160 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(160, knots10, inclx=TRUE), addx = TRUE))
ICC180 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(180, knots10, inclx=TRUE), addx = TRUE))
ICC200 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(200, knots10, inclx=TRUE), addx = TRUE))
ICC10 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(10, knots10, inclx=TRUE), addx = TRUE))
ICC30 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(30, knots10, inclx=TRUE), addx = TRUE))
ICC50 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(50, knots10, inclx=TRUE), addx = TRUE))
ICC70 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(70, knots10, inclx=TRUE), addx = TRUE))
ICC90 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(90, knots10, inclx=TRUE), addx = TRUE))
ICC110 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(110, knots10, inclx=TRUE), addx = TRUE))
ICC130 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(130, knots10, inclx=TRUE), addx = TRUE))
ICC150 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(150, knots10, inclx=TRUE), addx = TRUE))
ICC170 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(170, knots10, inclx=TRUE), addx = TRUE))
ICC190 <- as.data.frame(predict(robustres10, newmods=rcspline.eval(190, knots10, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Anxiety_std)        
data$effect_sizes <- as.numeric(data$Anxiety_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4),                   random = ~ (1|main_author),data=data)
robustres11 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots11 <- attr(rcs(model.matrix(robustres11)[,2], 4), "parms")
A20 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(20, knots11, inclx=TRUE), addx = TRUE))
A40 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(40, knots11, inclx=TRUE), addx = TRUE))
A60 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(60, knots11, inclx=TRUE), addx = TRUE))
A80 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(80, knots11, inclx=TRUE), addx = TRUE))
A100 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(100, knots11, inclx=TRUE), addx = TRUE))
A120 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(120, knots11, inclx=TRUE), addx = TRUE))
A140 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(140, knots11, inclx=TRUE), addx = TRUE))
A160 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(160, knots11, inclx=TRUE), addx = TRUE))
A180 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(180, knots11, inclx=TRUE), addx = TRUE))
A200 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(200, knots11, inclx=TRUE), addx = TRUE))
A10 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(10, knots11, inclx=TRUE), addx = TRUE))
A30 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(30, knots11, inclx=TRUE), addx = TRUE))
A50 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(50, knots11, inclx=TRUE), addx = TRUE))
A70 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(70, knots11, inclx=TRUE), addx = TRUE))
A90 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(90, knots11, inclx=TRUE), addx = TRUE))
A110 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(110, knots11, inclx=TRUE), addx = TRUE))
A130 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(130, knots11, inclx=TRUE), addx = TRUE))
A150 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(150, knots11, inclx=TRUE), addx = TRUE))
A170 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(170, knots11, inclx=TRUE), addx = TRUE))
A190 <- as.data.frame(predict(robustres11, newmods=rcspline.eval(190, knots11, inclx=TRUE), addx = TRUE))

df<-structure(list(
EI=c(EI10[1], EI20[1], EI30[1], EI40[1], EI50[1], EI60[1], EI70[1], EI80[1], EI90[1], EI100[1], EI110[1], EI120[1], EI130[1], EI140[1], EI150[1], EI160[1], EI170[1], EI180[1], EI190[1], EI200[1]),
CI=c(CI10[1], CI20[1], CI30[1], CI40[1], CI50[1], CI60[1], CI70[1], CI80[1], CI90[1], CI100[1], CI110[1], CI120[1], CI130[1], CI140[1], CI150[1], CI160[1], CI170[1], CI180[1], CI190[1], CI200[1]),
AVS=c(AVS10[1], AVS20[1], AVS30[1], AVS40[1], AVS50[1], AVS60[1], AVS70[1], AVS80[1], AVS90[1], AVS100[1], AVS110[1], AVS120[1], AVS130[1], AVS140[1], AVS150[1], AVS160[1], AVS170[1], AVS180[1], AVS190[1], AVS200[1]),
CMP=c(CMP10[1], CMP20[1], CMP30[1], CMP40[1], CMP50[1], CMP60[1], CMP70[1], CMP80[1], CMP90[1], CMP100[1], CMP110[1], CMP120[1], CMP130[1], CMP140[1], CMP150[1], CMP160[1], CMP170[1], CMP180[1], CMP190[1], CMP200[1]),
I=c(I10[1], I20[1], I30[1], I40[1], I50[1], I60[1], I70[1], I80[1], I90[1], I100[1], I110[1], I120[1], I130[1], I140[1], I150[1], I160[1], I170[1], I180[1], I190[1], I200[1]),
SE=c(SE10[1], SE20[1], SE30[1], SE40[1], SE50[1], SE60[1], SE70[1], SE80[1], SE90[1], SE100[1], SE110[1], SE120[1], SE130[1], SE140[1], SE150[1], SE160[1], SE170[1], SE180[1], SE190[1], SE200[1]),
EU=c(EU10[1], EU20[1], EU30[1], EU40[1], EU50[1], EU60[1], EU70[1], EU80[1], EU90[1], EU100[1], EU110[1], EU120[1], EU130[1], EU140[1], EU150[1], EU160[1], EU170[1], EU180[1], EU190[1], EU200[1]),
BS=c(BS10[1], BS20[1], BS30[1], BS40[1], BS50[1], BS60[1], BS70[1], BS80[1], BS90[1], BS100[1], BS110[1], BS120[1], BS130[1], BS140[1], BS150[1], BS160[1], BS170[1], BS180[1], BS190[1], BS200[1]),
D=c(D10[1], D20[1], D30[1], D40[1], D50[1], D60[1], D70[1], D80[1], D90[1], D100[1], D110[1], D120[1], D130[1], D140[1], D150[1], D160[1], D170[1], D180[1], D190[1], D200[1]),
ICC=c(ICC10[1], ICC20[1], ICC30[1], ICC40[1], ICC50[1], ICC60[1], ICC70[1], ICC80[1], ICC90[1], ICC100[1], ICC110[1], ICC120[1], ICC130[1], ICC140[1], ICC150[1], ICC160[1], ICC170[1], ICC180[1], ICC190[1], ICC200[1]),
A=c(A10[1], A20[1], A30[1], A40[1], A50[1], A60[1], A70[1], A80[1], A90[1], A100[1], A110[1], A120[1], A130[1], A140[1], A150[1], A160[1], A170[1], A180[1], A190[1], A200[1])),
.Names=c("EI","CI","AVS","CMP","I", "SE", "EU", "BS", "D", "ICC", "A"),row.names=c(NA, -20L),class="data.frame")
# To use the fmsb package, one has to add 2 rows to the dataframe: the max and min of each dimension
spider_data <- rbind(rep(100,11), rep(0,11), df)
spider_data <- spider_data %>%
  mutate(EI = as.numeric(EI),
         CI = as.numeric(CI),
         AVS = as.numeric(AVS),
         CMP = as.numeric(CMP),
         I = as.numeric(I),
         SE = as.numeric(SE),
         EU = as.numeric(EU),
         BS = as.numeric(BS),
         D = as.numeric(D),
         ICC = as.numeric(ICC),
         A = as.numeric(A))
#View(spider_data)
colors_border=c("#CAE1EFFF","#C1DAECFF","#B6D3E8FF","#ACCCE4FF","#A1C4E0FF","#97BDDCFF","#8CB5D8FF",
                           "#81ACD4FF","#76A4CFFF","#6A9BCBFF","#5F92C7FF","#5389C2FF","#4780BEFF",
                           "#3A76BAFF","#316DB3FF","#3263A7FF","#31599BFF","#2F4F8FFF","#2D4682FF", "#2B417CFF")
                           
par("mar")
par(mar=c(1,1,1,1))

tiff(filename = "Spider_11-ASC_reanalysis.tiff", units="cm", width=15, height=15, res=600)
radarchart(spider_data, axistype=0, 
           #custom polygon
           pcol=colors_border, plwd=c(1,1,1,1,3,1,1,1,1,3,1,1,1,1,3,1,1,1,1,3) , plty=1,
           #custom the grid
           cglcol="Black", cglty=1, axislabcol="Black", caxislabels=NULL , calcex=1.56, cglwd=0.8,
           #custom labels
           vlcex=1.5
)
dev.off()


#################################################################################################################################################################################
# MEQ30

data <- read_excel("C:/MEQ30.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

data$Scale_std <- as.numeric(data$Ineffability_std)        
data$effect_sizes <- as.numeric(data$Ineffability_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots1 <- attr(rcs(model.matrix(robustres1)[,2], 3), "parms")
I20 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(20, knots1, inclx=TRUE), addx = TRUE))
I40 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(40, knots1, inclx=TRUE), addx = TRUE))
I60 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(60, knots1, inclx=TRUE), addx = TRUE))
I80 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(80, knots1, inclx=TRUE), addx = TRUE))
I100 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(100, knots1, inclx=TRUE), addx = TRUE))
I120 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(120, knots1, inclx=TRUE), addx = TRUE))
I140 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(140, knots1, inclx=TRUE), addx = TRUE))
I160 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(160, knots1, inclx=TRUE), addx = TRUE))
I180 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(180, knots1, inclx=TRUE), addx = TRUE))
I200 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(200, knots1, inclx=TRUE), addx = TRUE))
I10 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(10, knots1, inclx=TRUE), addx = TRUE))
I30 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(30, knots1, inclx=TRUE), addx = TRUE))
I50 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(50, knots1, inclx=TRUE), addx = TRUE))
I70 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(70, knots1, inclx=TRUE), addx = TRUE))
I90 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(90, knots1, inclx=TRUE), addx = TRUE))
I110 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(110, knots1, inclx=TRUE), addx = TRUE))
I130 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(130, knots1, inclx=TRUE), addx = TRUE))
I150 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(150, knots1, inclx=TRUE), addx = TRUE))
I170 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(170, knots1, inclx=TRUE), addx = TRUE))
I190 <- as.data.frame(predict(robustres1, newmods=rcspline.eval(190, knots1, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Mystical_std)        
data$effect_sizes <- as.numeric(data$Mystical_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres2 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots2 <- attr(rcs(model.matrix(robustres2)[,2], 3), "parms")
M20 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(20, knots2, inclx=TRUE), addx = TRUE))
M40 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(40, knots2, inclx=TRUE), addx = TRUE))
M60 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(60, knots2, inclx=TRUE), addx = TRUE))
M80 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(80, knots2, inclx=TRUE), addx = TRUE))
M100 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(100, knots2, inclx=TRUE), addx = TRUE))
M120 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(120, knots2, inclx=TRUE), addx = TRUE))
M140 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(140, knots2, inclx=TRUE), addx = TRUE))
M160 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(160, knots2, inclx=TRUE), addx = TRUE))
M180 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(180, knots2, inclx=TRUE), addx = TRUE))
M200 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(200, knots2, inclx=TRUE), addx = TRUE))
M10 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(10, knots2, inclx=TRUE), addx = TRUE))
M30 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(30, knots2, inclx=TRUE), addx = TRUE))
M50 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(50, knots2, inclx=TRUE), addx = TRUE))
M70 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(70, knots2, inclx=TRUE), addx = TRUE))
M90 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(90, knots2, inclx=TRUE), addx = TRUE))
M110 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(110, knots2, inclx=TRUE), addx = TRUE))
M130 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(130, knots2, inclx=TRUE), addx = TRUE))
M150 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(150, knots2, inclx=TRUE), addx = TRUE))
M170 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(170, knots2, inclx=TRUE), addx = TRUE))
M190 <- as.data.frame(predict(robustres2, newmods=rcspline.eval(190, knots2, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Positive_mood_std)        
data$effect_sizes <- as.numeric(data$Positive_mood_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres3 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots3 <- attr(rcs(model.matrix(robustres3)[,2], 3), "parms")
P20 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(20, knots3, inclx=TRUE), addx = TRUE))
P40 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(40, knots3, inclx=TRUE), addx = TRUE))
P60 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(60, knots3, inclx=TRUE), addx = TRUE))
P80 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(80, knots3, inclx=TRUE), addx = TRUE))
P100 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(100, knots3, inclx=TRUE), addx = TRUE))
P120 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(120, knots3, inclx=TRUE), addx = TRUE))
P140 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(140, knots3, inclx=TRUE), addx = TRUE))
P160 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(160, knots3, inclx=TRUE), addx = TRUE))
P180 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(180, knots3, inclx=TRUE), addx = TRUE))
P200 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(200, knots3, inclx=TRUE), addx = TRUE))
P10 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(10, knots3, inclx=TRUE), addx = TRUE))
P30 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(30, knots3, inclx=TRUE), addx = TRUE))
P50 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(50, knots3, inclx=TRUE), addx = TRUE))
P70 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(70, knots3, inclx=TRUE), addx = TRUE))
P90 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(90, knots3, inclx=TRUE), addx = TRUE))
P110 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(110, knots3, inclx=TRUE), addx = TRUE))
P130 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(130, knots3, inclx=TRUE), addx = TRUE))
P150 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(150, knots3, inclx=TRUE), addx = TRUE))
P170 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(170, knots3, inclx=TRUE), addx = TRUE))
P190 <- as.data.frame(predict(robustres3, newmods=rcspline.eval(190, knots3, inclx=TRUE), addx = TRUE))

data$Scale_std <- as.numeric(data$Transcendence_of_time_and_space_std)        
data$effect_sizes <- as.numeric(data$Transcendence_of_time_and_space_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               
data$variance <- data$Scale_std^2
V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)
res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3),                   random = ~ (1|main_author),data=data)
robustres4 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)
knots4 <- attr(rcs(model.matrix(robustres4)[,2], 3), "parms")
TTS20 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(20, knots4, inclx=TRUE), addx = TRUE))
TTS40 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(40, knots4, inclx=TRUE), addx = TRUE))
TTS60 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(60, knots4, inclx=TRUE), addx = TRUE))
TTS80 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(80, knots4, inclx=TRUE), addx = TRUE))
TTS100 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(100, knots4, inclx=TRUE), addx = TRUE))
TTS120 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(120, knots4, inclx=TRUE), addx = TRUE))
TTS140 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(140, knots4, inclx=TRUE), addx = TRUE))
TTS160 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(160, knots4, inclx=TRUE), addx = TRUE))
TTS180 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(180, knots4, inclx=TRUE), addx = TRUE))
TTS200 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(200, knots4, inclx=TRUE), addx = TRUE))
TTS10 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(10, knots4, inclx=TRUE), addx = TRUE))
TTS30 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(30, knots4, inclx=TRUE), addx = TRUE))
TTS50 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(50, knots4, inclx=TRUE), addx = TRUE))
TTS70 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(70, knots4, inclx=TRUE), addx = TRUE))
TTS90 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(90, knots4, inclx=TRUE), addx = TRUE))
TTS110 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(110, knots4, inclx=TRUE), addx = TRUE))
TTS130 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(130, knots4, inclx=TRUE), addx = TRUE))
TTS150 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(150, knots4, inclx=TRUE), addx = TRUE))
TTS170 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(170, knots4, inclx=TRUE), addx = TRUE))
TTS190 <- as.data.frame(predict(robustres4, newmods=rcspline.eval(190, knots4, inclx=TRUE), addx = TRUE))


df<-structure(list(
  I=c(I10[1], I20[1], I30[1], I40[1], I50[1], I60[1], I70[1], I80[1], I90[1], I100[1], I110[1], I120[1], I130[1], I140[1], I150[1], I160[1], I170[1], I180[1], I190[1], I200[1]),
  M=c(M10[1], M20[1], M30[1], M40[1], M50[1], M60[1], M70[1], M80[1], M90[1], M100[1], M110[1], M120[1], M130[1], M140[1], M150[1], M160[1], M170[1], M180[1], M190[1], M200[1]),
  P=c(P10[1], P20[1], P30[1], P40[1], P50[1], P60[1], P70[1], P80[1], P90[1], P100[1], P110[1], P120[1], P130[1], P140[1], P150[1], P160[1], P170[1], P180[1], P190[1], P200[1]),
  TTS=c(TTS10[1], TTS20[1], TTS30[1], TTS40[1], TTS50[1], TTS60[1], TTS70[1], TTS80[1], TTS90[1], TTS100[1], TTS110[1], TTS120[1], TTS130[1], TTS140[1], TTS150[1], TTS160[1], TTS170[1], TTS180[1], TTS190[1], TTS200[1])),
  .Names=c("I","M","P","TTS"),row.names=c(NA, -20L),class="data.frame")
# To use the fmsb package, one has to add 2 rows to the dataframe: the max and min of each dimension
spider_data <- rbind(rep(100,4), rep(0,4), df)
spider_data <- spider_data %>%
  mutate(I = as.numeric(I),
         M = as.numeric(M),
         P = as.numeric(P),
         TTS = as.numeric(TTS))
         
#View(spider_data)
colors_border=c("#CAE1EFFF","#C1DAECFF","#B6D3E8FF","#ACCCE4FF","#A1C4E0FF","#97BDDCFF","#8CB5D8FF",
                           "#81ACD4FF","#76A4CFFF","#6A9BCBFF","#5F92C7FF","#5389C2FF","#4780BEFF",
                           "#3A76BAFF","#316DB3FF","#3263A7FF","#31599BFF","#2F4F8FFF","#2D4682FF", "#2B417CFF")
                           
par("mar")
par(mar=c(1,1,1,1))

tiff(filename = "Spider_MEQ_reanalysis.tiff", units="cm", width=15, height=15, res=600)
radarchart(spider_data, axistype=0, 
           #custom polygon
           pcol=colors_border, plwd=c(1,1,1,1,3,1,1,1,1,3,1,1,1,1,3,1,1,1,1,3) , plty=1,
           #custom the grid
           cglcol="Black", cglty=1, axislabcol="Black", caxislabels=NULL , calcex=1.56, cglwd=0.8,
           #custom labels
           vlcex=1.5
)
dev.off()






########################################################################################################################################################################################################
########################################################################################################################################################################################################
# Plots
########################################################################################################################################################################################################
########################################################################################################################################################################################################

# 5D-ASC

data <- read_excel("C:/5D-ASC.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

data$Scale_std <- as.numeric(data$Oceanic_Boundlessness_std)        # standard deviation
data$effect_sizes <- as.numeric(data$Oceanic_Boundlessness_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               # Nr of study participants

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#9D7660FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 4),
              rep("#59A14FFF", 2),
              rep("#8CD17DFF", 3),
              rep("#B6992DFF", 3),
              rep("#499894FF", 2),
              rep("#EF6F6A", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 
cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
                                           axis.title.y = element_blank(),
                                           panel.background = element_blank(),
                                           aspect.ratio = 1/1.61,
                                           axis.text.y=element_text(size = 18, margin=margin(r=0)),
                                          axis.text.x=element_text(size = 18, margin=margin(r=0)),
                                           axis.line = element_line(),
                                           axis.ticks = element_line()) 

ggsave("5D-ASC_OB.svg", width = 4.5, height = 3)

#View(data)

#plot(data$dosages, data$effect_sizes, cex=data$weights_norm*50, bty="n", pch=1,col=colors, yaxs="i", xaxs="i",
#     lwd=3, 
#     main=paste0(""),
#     xlab="",
#     ylab="",
#     xlim=c(0,220),
#     ylim=c(0, 100),
#     las=1)

#title(ylab="Dread of Ego Dissolution (%)", mgp=c(2.5,1,0), cex.lab=1)
#title(xlab="??g/kg body weight", mgp=c(2.1,1,0), cex.lab=1)
#lines(seq(0,450,0.001), model$b.r[1]+model$b.r[2]*seq(0,450,0.001), lwd=2)
#

#####################################################################################################################
# Vigilance Reduction

data$Scale_std <- as.numeric(data$Vigilance_Reduction_std)       
data$effect_sizes <- as.numeric(data$Vigilance_Reduction_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#9D7660FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 4),
              rep("#59A14FFF", 2),
              rep("#8CD17DFF", 3),
              rep("#B6992DFF", 3),
              rep("#499894FF", 2),
              rep("#EF6F6A", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
 scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("5D-ASC_VG.svg", width = 4.5, height = 3)
label = scales::label_number(scale_cut = cut_si("%"))
###############################################################################################################
# Dread of ego dissolution

data$Scale_std <- as.numeric(data$Dread_of_Ego_Dissolution_std)        
data$effect_sizes <- as.numeric(data$Dread_of_Ego_Dissolution_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#9D7660FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 4),
              rep("#59A14FFF", 2),
              rep("#8CD17DFF", 3),
              rep("#B6992DFF", 3),
              rep("#499894FF", 2),
              rep("#EF6F6A", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("5D-ASC_DED.svg", width = 4.5, height = 3)

##################################################################################################################
# VisR

data$Scale_std <- as.numeric(data$Visionary_Restructuralization_std)        
data$effect_sizes <- as.numeric(data$Visionary_Restructuralization_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#9D7660FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 4),
              rep("#59A14FFF", 2),
              rep("#8CD17DFF", 3),
              rep("#B6992DFF", 3),
              rep("#499894FF", 2),
              rep("#EF6F6A", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("5D-ASC_VisR.svg", width = 4.5, height = 3)

##################################################################################################################
# Auditory Alterations

data$Scale_std <- as.numeric(data$Auditory_Alterations_std)        
data$effect_sizes <- as.numeric(data$Auditory_Alterations_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#9D7660FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 4),
              rep("#59A14FFF", 2),
              rep("#8CD17DFF", 3),
              rep("#B6992DFF", 3),
              rep("#499894FF", 2),
              rep("#EF6F6A", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("5D-ASC_AA.svg", width = 4.5, height = 3)

##################################################################################################################
##################################################################################################################
# 11-ASC
##################################################################################################################
##################################################################################################################

# Experience of unity

data <- read_excel("C:/11-ASC.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

data$Scale_std <- as.numeric(data$Experience_of_unity_std)        
data$effect_sizes <- as.numeric(data$Experience_of_unity_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 8),
              rep("#EF6F6A", 3),
              rep("#B07AA1FF", 2),
              rep("#D4A6C8FF", 1)) #14

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line())

ggsave("11-ASC_EU.svg", width = 4.5, height = 3)

########################################################################################################
#Spiritual_experience

data$Scale_std <- as.numeric(data$Spiritual_experience_std)        
data$effect_sizes <- as.numeric(data$Spiritual_experience_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_SE.svg", width = 4.5, height = 3)

########################################################################################################
# Blissful_state

data$Scale_std <- as.numeric(data$Blissful_state_std)        
data$effect_sizes <- as.numeric(data$Blissful_state_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_BS.svg", width = 4.5, height = 3)

########################################################################################################
# Insightfulness

data$Scale_std <- as.numeric(data$Insightfulness_std)        
data$effect_sizes <- as.numeric(data$Insightfulness_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_I.svg", width = 4.5, height = 3)

########################################################################################################
# Disembodiment

data$Scale_std <- as.numeric(data$Disembodiment_std)        
data$effect_sizes <- as.numeric(data$Disembodiment_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_D.svg", width = 4.5, height = 3)

########################################################################################################
# Impaired_control_and_cognition

data$Scale_std <- as.numeric(data$Impaired_control_and_cognition_std)        
data$effect_sizes <- as.numeric(data$Impaired_control_and_cognition_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_IMP.svg", width = 4.5, height = 3)

########################################################################################################
# Anxiety

data$Scale_std <- as.numeric(data$Anxiety_std)        
data$effect_sizes <- as.numeric(data$Anxiety_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 
cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_A.svg", width = 4.5, height = 3)

########################################################################################################
# Elementary_imagery

data$Scale_std <- as.numeric(data$Elementary_imagery_std)        
data$effect_sizes <- as.numeric(data$Elementary_imagery_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_EI.svg", width = 4.5, height = 3)

########################################################################################################
# Complex_imagery

data$Scale_std <- as.numeric(data$Complex_imagery_std)        
data$effect_sizes <- as.numeric(data$Complex_imagery_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_CI.svg", width = 4.5, height = 3)

########################################################################################################
# Audio_visual_synesthesia

data$Scale_std <- as.numeric(data$Audio_visual_synesthesia_std)        
data$effect_sizes <- as.numeric(data$Audio_visual_synesthesia_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_AVS.svg", width = 4.5, height = 3)

########################################################################################################
# Changed_meaning_of_percepts

data$Scale_std <- as.numeric(data$Changed_meaning_of_percepts_std)        
data$effect_sizes <- as.numeric(data$Changed_meaning_of_percepts_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 4), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 4), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..4.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#A0CBE8FF", 2),
              rep("#F28E2BFF", 1),
              rep("#FFBE7DFF", 3),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 1),
              rep("#B6992DFF", 1),
              rep("#F1CE63FF", 1),
              rep("#499894FF", 4),
              rep("#86BCB6FF", 2),
              rep("#D37295FF", 4),
              rep("#EF6F6A", 4),
              rep("#B07AA1FF", 3),
              rep("#D4A6C8FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("11-ASC_CMP.svg", width = 4.5, height = 3)

########################################################################################################
########################################################################################################
# MEQ30
########################################################################################################
########################################################################################################
data <- read_excel("C:/MEQ30.xlsx", col_names = TRUE, col_types = "guess", na = "NULL")

# Mystical

data$Scale_std <- as.numeric(data$Mystical_std)        
data$effect_sizes <- as.numeric(data$Mystical_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 1),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 4),
              rep("#B6992DFF", 2),
              rep("#499894FF", 3),
              rep("#B07AA1FF", 2),
              rep("black", 1))
                  
data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("MEQ_M.svg", width = 4.5, height = 3)

############################################################################################################
# Transcendence_of_time_and_space

data$Scale_std <- as.numeric(data$Transcendence_of_time_and_space_std)        
data$effect_sizes <- as.numeric(data$Transcendence_of_time_and_space_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 1),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 4),
              rep("#B6992DFF", 2),
              rep("#499894FF", 3),
              rep("#B07AA1FF", 2),
              rep("black", 1))

       
data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("MEQ_T.svg", width = 4.5, height = 3)

############################################################################################################
# Ineffability

data$Scale_std <- as.numeric(data$Ineffability_std)        
data$effect_sizes <- as.numeric(data$Ineffability_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 1),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 4),
              rep("#B6992DFF", 2),
              rep("#499894FF", 3),
              rep("#B07AA1FF", 2),
              rep("black", 1))

      
data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("MEQ_I.svg", width = 4.5, height = 3)

############################################################################################################
# Positive_mood

data$Scale_std <- as.numeric(data$Positive_mood_std)        
data$effect_sizes <- as.numeric(data$Positive_mood_mean)
data$dosages <- as.numeric(data$dosage_quantity) 
data$participants <- as.numeric(data$nr_of_subjects)               

#transform std to variance 
data$variance <- data$Scale_std^2

xs <- seq(0,1000, length = 220)

V_mat <- impute_covariance_matrix(vi = data$variance, cluster = data$main_author, r = 0.8, smooth_vi = TRUE)

res.rcs <- rma.mv(effect_sizes, V= V_mat, mods = ~ rcs(dosages, 3), random = ~ (1|main_author),data=data)

robustres1 <- robust(res.rcs, cluster = data$main_author , adjust=TRUE, clubSandwich=TRUE)

knots <- attr(rcs(model.matrix(res.rcs)[,2], 3), "parms")

sav <- as.data.frame(predict(robustres1, newmods=rcspline.eval(xs, knots, inclx=TRUE), addx = TRUE))

weights <- weights(robustres1) 
data$weights <- weights
#here the weights are normalized to 1 to make them comparable across different meta-regressions
weights_norm <- weights/sum(weights)  
data$weights_norm <- weights_norm

savv2 <- sav %>%
  mutate(dosages = round(X.rcs.dosages..3.dosages, 1),
         effect = round(pred, 1))

colors <-   c(rep("#4E79A7FF", 1),
              rep("#D7B5A6FF", 1),
              rep("#FFBE7DFF", 1),
              rep("#59A14FFF", 1),
              rep("#8CD17DFF", 4),
              rep("#B6992DFF", 2),
              rep("#499894FF", 3),
              rep("#B07AA1FF", 2),
              rep("black", 1))

data <- data %>%
  mutate(ww = as.numeric(round(scales::rescale(data$weights_norm, to = c(8, 12)),2))) 

cc <- ggplot(savv2) +
  geom_ribbon(aes(x = dosages, ymin = ci.lb, ymax = ci.ub), alpha = 0.1)  +
  geom_point(data = data, aes(dosages, effect_sizes), colour = data$color, size = data$ww, alpha = 0.7) +
  geom_line(data = savv2, aes(dosages, effect), linewidth = 0.8) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 220),breaks = c(50,100,150,200), oob = rescale_none, label = c("50µg", "100µg", "150µg", "200µg")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = c(0, 20,40,60,80,100), oob = rescale_none, label= scales::label_percent(scale = 1)) 

cc + theme(axis.title.x = element_blank(),
           axis.title.y = element_blank(),
           panel.background = element_blank(),
           aspect.ratio = 1/1.61,
           axis.text.y=element_text(size = 18, margin=margin(r=0)),
          axis.text.x=element_text(size = 18, margin=margin(r=0)),
           axis.line = element_line(),
           axis.ticks = element_line()) 

ggsave("MEQ_PM.svg", width = 4.5, height = 3)