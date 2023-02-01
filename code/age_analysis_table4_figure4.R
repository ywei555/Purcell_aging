# ## install necessary packages if not already installed
# install.packages("dfoptim")
# install.packages("optimx")
# install.packages("effects")
# install.packages("allEffects")
# install.packages("MuMIn")
# install.packages("jtools")
# install.packages("sjPlot")
# install.packages("ggplot2")
# install.packages("see")
# install.packages("patchwork")
# install.packages("see", dependencies = TRUE)

## load packages
library(lmerTest)
library(MuMIn)
library(effects)
library(jtools)
library(sjPlot)
library(ggplot2)
library(optimx)
library(ggpubr)
library(png)

rm(list=ls())
## setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load data p20
wkdir = "../ROI_MLM_data/p20_signal"
files = c("3_Cerebellum-CrusI_ROIdata_cond-type-region-hemi_20p.csv",
          "5_Stark_Cerebellum-Crus2_cond-type-region-hemi_mean.csv",
          "4_Cerebellum-VI_ROIdata_cond-type-region-hemi_20p.csv",
          "6_Stark_Cerebellum-vermis8_cond-type-region-hemi_mean.csv",
          "2_Stark_PHC_cond-type-region-hemi_20p.csv",
          "1_Stark_Hippocampus_cond-type-region-hemi_20p.csv");
agedata = list()
regions = c("Cerebellum_CrusI", "Cerebellum-Crus2", "Cerebellum_VI", "Vermis", "PHG/EC", "Hippocampus")
for (fi in 1:length(files)){
  agedata[[regions[fi]]] = read.csv(file.path(wkdir, files[fi]))
}


## data cleaning
for (ri in 1:length(regions)) {
  #set region (region name) and condition (Rest vs Exercise) to be factors
  agedata[[ri]]$Rest_Ex <- as.factor(agedata[[ri]]$Rest_Ex)
  agedata[[ri]]$hemisphere<- as.factor(agedata[[ri]]$hemisphere)
  #set contrasts for the factors to sum to zero
  contrasts(agedata[[ri]]$Rest_Ex)<-"contr.sum"
  contrasts(agedata[[ri]]$hemisphere)<-"contr.sum"
  #center age and totalvol
  agedata[[ri]]$age_centered <- scale(agedata[[ri]]$age_yrs, center = T, scale = F)
  agedata[[ri]]$totalvol <- scale(agedata[[ri]]$totalvol, center = T, scale = T)
  
}


## model analysis
lmer_obj_6regions = matrix(list(),6)
for (ri in 1:length(regions)) {
  print(sprintf("processing %s", regions[ri]))
  lmer_obj_6regions [[ri]] = lmer(Hreg ~Rest_Ex *age_centered * hemisphere + totalvol + WB_Hreg
                                  + (1 + Rest_Ex + hemisphere |  idno) ,
                                  data =  agedata[[ri]],
                                  control=lmerControl(optimizer = "nmkbw", optCtrl = list(maxfeval = 200000)))
}



#summary table for 6 regions
tb_summary = matrix(list(),6)
for (ri in 1:length(regions)) {
  tb_summary[[ri]] = summary(lmer_obj_6regions[[ri]])
}


#get r sqr
tb_r2 = matrix(NA,6)
for (ri in 1:length(regions)) {
  tb_r2[[ri]] = r.squaredGLMM(lmer_obj_6regions[[ri]])[2]
}


## table 4
table_4 = data.frame()
for (i in 1:length(regions)){
  ttab = tb_summary[[i]]$coefficients[c(3,7,10),]
  ttab = as.data.frame(ttab)
  ttab$region = regions[i]
  ttab$r2 = tb_r2[i]
  table_4= rbind(table_4, ttab)
}
table_4


##summary table for Cerebellum_CrusI
tb_summary[[1]]
##summary table for Cerebellum-Crus2
tb_summary[[2]]
##summary table for Cerebellum_VI
tb_summary[[3]]
##summary table for Vermis
tb_summary[[4]]
##summary table for PHG/EC
tb_summary[[5]]
##summary table for Hippocampus
tb_summary[[6]]




##figure4
#visualization of model estimates with region:
output_plot<- function(lmer_obj){
  output_plot = plot_model(lmer_obj, show.legend = FALSE, type = "pred", terms = c("age_centered","Rest_Ex","hemisphere"), title = "",
                           show.data = TRUE,dot.size = 1.3,axis.title = "", axis.labels = "", 
                           axis.lim(tlim)) + xlab('') + theme_minimal() + theme(text = element_text(size=10))
  
}

#figure for Cerebellum_CrusI
rplot_1 = output_plot(lmer_obj_6regions[[1]])
#figure for Cerebellum-Crus
rplot_2 = output_plot(lmer_obj_6regions[[2]])
#figure for Cerebellum_VI
rplot_3 = output_plot(lmer_obj_6regions[[3]])
#figure for Vermis
rplot_4 = output_plot(lmer_obj_6regions[[4]])
#figure for PHG/EC
rplot_5 = output_plot(lmer_obj_6regions[[5]])
#figure for Hippocampus
rplot_6 = output_plot(lmer_obj_6regions[[6]])

#figure4

ggarrange(rplot_1, rplot_2, rplot_3, rplot_4, rplot_5, rplot_6, labels = regions,
               ncol = 2, nrow = 3, font.label = list(size = 8),common.legend = TRUE)
