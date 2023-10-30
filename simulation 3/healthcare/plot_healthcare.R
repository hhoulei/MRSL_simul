

library(ggplot2)
library(RColorBrewer)
D=7
mname <- c("each_g",
           paste0("MRSL_remove_collider_XM_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_open_path_XM_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_min_suffi_set_XM_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
          
           paste0("MRPC_sigSNP_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("HC_sigSNP_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("MRPC_score_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("HC_score_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           #paste0("BIMMER_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("cGAUGE.ivw_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.egger_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.mrpresso_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           
           #paste0("R2_D",1:D),
           #paste0("F_D",1:D),
           
           "time11","time12","time13",
           "time2","time21","time3","time31",
           #"time4",
           "time51","time52","time53"
)
dttime <- NULL

gg <- c(5,seq(10,50,10))
dt1 <- NULL
for(i in 1:length(gg)){
  dt1_once <- read.csv(paste0("healthcare_each_g_",gg[i],"_dlB_0_duB_0.25.csv"),header =F)
  dt1_once <- apply(dt1_once,2,function(x) mean(x,na.rm=T))
  dt1 <- rbind(dt1,dt1_once)
}
dt1 <- as.data.frame(dt1)
colnames(dt1) <- mname
dttime <- rbind(dttime,dt1)
dt21 <- data.frame(
  method=rep(c("MRSL_remove_collider",
               "MRSL_open_path",
               "MRSL_min_sep_set",
               
               "MRPC_sigSNP","HC+prior_sigSNP",
               "MRPC_score","HC+prior_score",
               #'BIMMER',
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"
               ),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_XM_F1,
             dt1$MRSL_open_path_XM_F1,
             dt1$MRSL_min_suffi_set_XM_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1
             ),
  
  Presion=c(dt1$MRSL_remove_collider_XM_precision,
            dt1$MRSL_open_path_XM_precision,
            dt1$MRSL_min_suffi_set_XM_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision
            ),
  
  Recall=c(dt1$MRSL_remove_collider_XM_recall,
           dt1$MRSL_open_path_XM_recall,
           dt1$MRSL_min_suffi_set_XM_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall
           ),
  
  FPR = c(dt1$MRSL_remove_collider_XM_FPR,
          dt1$MRSL_open_path_XM_FPR,
          dt1$MRSL_min_suffi_set_XM_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR
          ),
  
  TPR = c(dt1$MRSL_remove_collider_XM_TPR,
          dt1$MRSL_open_path_XM_TPR,
          dt1$MRSL_min_suffi_set_XM_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR
          ),
  
  timee = c(dt1$time11,
            dt1$time12,
            dt1$time13,
            dt1$time2,
            dt1$time21,
            dt1$time3,
            dt1$time31,
            #dt1$time4,
            dt1$time51,
            dt1$time52,
            dt1$time53
            ),
  
  eachg=rep(dt1$each_g,10)
)

gg <-  c(5,seq(10,50,10))
dt1 <- NULL
for(i in 1:length(gg)){
  dt1_once <- read.csv(paste0("healthcare_each_g_",gg[i],"_dlB_0.25_duB_0.5.csv"),header =F)
  dt1_once <- apply(dt1_once,2,function(x) mean(x,na.rm=T))
  dt1 <- rbind(dt1,dt1_once)
}
dt1 <- as.data.frame(dt1)
colnames(dt1) <- mname
dttime <- rbind(dttime,dt1)
dt22 <- data.frame(
  method=rep(c("MRSL_remove_collider",
               "MRSL_open_path",
               "MRSL_min_sep_set",
               
               "MRPC_sigSNP","HC+prior_sigSNP",
               "MRPC_score","HC+prior_score",
               #'BIMMER',
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"
  ),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_XM_F1,
             dt1$MRSL_open_path_XM_F1,
             dt1$MRSL_min_suffi_set_XM_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1
  ),
  
  Presion=c(dt1$MRSL_remove_collider_XM_precision,
            dt1$MRSL_open_path_XM_precision,
            dt1$MRSL_min_suffi_set_XM_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision
  ),
  
  Recall=c(dt1$MRSL_remove_collider_XM_recall,
           dt1$MRSL_open_path_XM_recall,
           dt1$MRSL_min_suffi_set_XM_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall
  ),
  
  FPR = c(dt1$MRSL_remove_collider_XM_FPR,
          dt1$MRSL_open_path_XM_FPR,
          dt1$MRSL_min_suffi_set_XM_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR
  ),
  
  TPR = c(dt1$MRSL_remove_collider_XM_TPR,
          dt1$MRSL_open_path_XM_TPR,
          dt1$MRSL_min_suffi_set_XM_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR
  ),
  
  timee = c(dt1$time11,
            dt1$time12,
            dt1$time13,
            dt1$time2,
            dt1$time21,
            dt1$time3,
            dt1$time31,
            #dt1$time4,
            dt1$time51,
            dt1$time52,
            dt1$time53
  ),
  
  eachg=rep(dt1$each_g,10)
)

gg <-  c(5,seq(10,50,10))
dt1 <- NULL
for(i in 1:length(gg)){
  dt1_once <- read.csv(paste0("healthcare_each_g_",gg[i],"_dlB_0.5_duB_0.75.csv"),header =F)
  dt1_once <- apply(dt1_once,2,function(x) mean(x,na.rm=T))
  dt1 <- rbind(dt1,dt1_once)
}
dt1 <- as.data.frame(dt1)
colnames(dt1) <- mname
dttime <- rbind(dttime,dt1)
dt23 <- data.frame(
  method=rep(c("MRSL_remove_collider",
               "MRSL_open_path",
               "MRSL_min_sep_set",
               
               "MRPC_sigSNP","HC+prior_sigSNP",
               "MRPC_score","HC+prior_score",
               #'BIMMER',
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"
  ),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_XM_F1,
             dt1$MRSL_open_path_XM_F1,
             dt1$MRSL_min_suffi_set_XM_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1
  ),
  
  Presion=c(dt1$MRSL_remove_collider_XM_precision,
            dt1$MRSL_open_path_XM_precision,
            dt1$MRSL_min_suffi_set_XM_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision
  ),
  
  Recall=c(dt1$MRSL_remove_collider_XM_recall,
           dt1$MRSL_open_path_XM_recall,
           dt1$MRSL_min_suffi_set_XM_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall
  ),
  
  FPR = c(dt1$MRSL_remove_collider_XM_FPR,
          dt1$MRSL_open_path_XM_FPR,
          dt1$MRSL_min_suffi_set_XM_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR
  ),
  
  TPR = c(dt1$MRSL_remove_collider_XM_TPR,
          dt1$MRSL_open_path_XM_TPR,
          dt1$MRSL_min_suffi_set_XM_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR
  ),
  
  timee = c(dt1$time11,
            dt1$time12,
            dt1$time13,
            dt1$time2,
            dt1$time21,
            dt1$time3,
            dt1$time31,
            #dt1$time4,
            dt1$time51,
            dt1$time52,
            dt1$time53
  ),
  
  eachg=rep(dt1$each_g,10)
)

gg <- c(5,seq(10,50,10))
dt1 <- NULL
for(i in 1:length(gg)){
  dt1_once <- read.csv(paste0("healthcare_each_g_",gg[i],"_dlB_0.75_duB_1.csv"),header =F)
  dt1_once <- apply(dt1_once,2,function(x) mean(x,na.rm=T))
  dt1 <- rbind(dt1,dt1_once)
}
dt1 <- as.data.frame(dt1)
colnames(dt1) <- mname
dttime <- rbind(dttime,dt1)
dt24 <- data.frame(
  method=rep(c("MRSL_remove_collider",
               "MRSL_open_path",
               "MRSL_min_sep_set",
               
               "MRPC_sigSNP","HC+prior_sigSNP",
               "MRPC_score","HC+prior_score",
               #'BIMMER',
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"
  ),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_XM_F1,
             dt1$MRSL_open_path_XM_F1,
             dt1$MRSL_min_suffi_set_XM_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1
  ),
  
  Presion=c(dt1$MRSL_remove_collider_XM_precision,
            dt1$MRSL_open_path_XM_precision,
            dt1$MRSL_min_suffi_set_XM_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision
  ),
  
  Recall=c(dt1$MRSL_remove_collider_XM_recall,
           dt1$MRSL_open_path_XM_recall,
           dt1$MRSL_min_suffi_set_XM_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall
  ),
  
  FPR = c(dt1$MRSL_remove_collider_XM_FPR,
          dt1$MRSL_open_path_XM_FPR,
          dt1$MRSL_min_suffi_set_XM_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR
  ),
  
  TPR = c(dt1$MRSL_remove_collider_XM_TPR,
          dt1$MRSL_open_path_XM_TPR,
          dt1$MRSL_min_suffi_set_XM_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR
  ),
  
  timee = c(dt1$time11,
            dt1$time12,
            dt1$time13,
            dt1$time2,
            dt1$time21,
            dt1$time3,
            dt1$time31,
            #dt1$time4,
            dt1$time51,
            dt1$time52,
            dt1$time53
  ),
  
  eachg=rep(dt1$each_g,10)
)


dttime <- cbind(rep(c("0-0.25","0.25-0.5","0.5-0.75","0.75-1"),each=6),dttime)
dttime1 <- dttime[,c(1:2,113:122)]
colnames(dttime1) <- c('edge effect','each g',
                       'MRSL_remove_collider','MRSL_open_path','MRSL_min_sep_set',
                       'MRPC_sigSNP','HC_sigSNP','MRPC_score','HC_score',
                       'cGAUGE.ivw','cGAUGE.egger','cGAUGE.mrpresso')
write.csv(dttime1,"healthcare_computing_time.csv")


dt2 <- as.data.frame(rbind(dt21,dt22,dt23,dt24))
dt2$edge_effect <- c(rep("edges_effect 0-0.25",60),rep(c("edges_effect 0.25-0.5",
                                                         "edges_effect 0.5-0.75",
                         "edges_effect 0.75-1"),each=60))

shapes <- c(15,15,15,17,17,5,5,rep(8,3))
#coloo <- c(brewer.pal(11, "Paired"))
coloo <- c("#084081", "#2B8CBE", "#7BCCC4",
            "#FDAE6B","#F16913",
            "#A6D854","#33A02C",
            "#FC8D62","#E7298A","#E31A1C"
)

pyt <- ggplot(data=dt2, aes(x=eachg, y=F1_score,colour=method,group=method)) +
  geom_point(size=2,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  geom_line(size=0.7,aes(colour=method)) + 
  scale_colour_manual(values=coloo)+
  #scale_colour_brewer(palette = "Paired") +
  facet_wrap(~edge_effect,nrow=2,scales = "free_x") +
  ggtitle("F1 score with healthcare graph") +
  xlab("The number of SNPs") +
  ylab("F1 score") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5)) +
  ylim(0,1)

pyt

pdf(file="healthcare_F1score.pdf",height = 6,width = 7.5)  
pyt 
dev.off()


dt2$timee <- ifelse(dt2$timee>50,50,dt2$timee)
pyt <- ggplot(data=dt2, aes(x=eachg, y=timee,colour=method,group=method)) +
  geom_point(size=2,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  geom_line(size=0.7,aes(colour=method)) + 
  scale_colour_manual(values=coloo)+
  #scale_colour_brewer(palette = "Paired") +
  facet_wrap(~edge_effect,nrow=2,scales = "free_x") +
  ggtitle("Computing time with healthcare graph") +
  xlab("The number of SNPs") +
  ylab("Time") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5)) 

pyt

pdf(file="healthcare_time.pdf",height = 6,width = 7.5)  
pyt 
dev.off()


dt2$eachg_name <- paste0('eachg=',dt2$eachg)
dt2$eachg_name <- factor(dt2$eachg_name,levels=unique(dt2$eachg_name),ordered = T)

dt2221 <- dt2[dt2$edge_effect=='edges_effect 0-0.25',]
pyt <- ggplot(data=dt2221, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  facet_wrap(~eachg_name,nrow=2) +
  #facet_grid(edge_effect~eachg_name) +
  ggtitle("Precision-Recall with healthcare graph when edges' effect between 0-0.25") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.2,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)

pyt

pdf(file="healthcare_PRcurve_0_0.25.pdf",height = 6,width = 10)  
pyt 
dev.off()


dt2222 <- dt2[dt2$edge_effect=='edges_effect 0.25-0.5',]
pyt <- ggplot(data=dt2222, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  facet_wrap(~eachg_name,nrow=2) +
  #facet_grid(edge_effect~eachg_name) +
  ggtitle("Precision-Recall with healthcare graph when edges' effect between 0.25-0.5") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.2,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)

pyt

pdf(file="healthcare_PRcurve_0.25_0.5.pdf",height = 6,width = 10)  
pyt 
dev.off()

dt2223 <- dt2[dt2$edge_effect=='edges_effect 0.5-0.75',]
pyt <- ggplot(data=dt2223, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  facet_wrap(~eachg_name,nrow=2) +
  #facet_grid(edge_effect~eachg_name) +
  ggtitle("Precision-Recall with healthcare graph when edges' effect between 0.5-0.75") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.2,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)

pyt

pdf(file="healthcare_PRcurve_0.5_0.75.pdf",height =6,width = 10)  
pyt 
dev.off()


dt2224 <- dt2[dt2$edge_effect=='edges_effect 0.75-1',]
pyt <- ggplot(data=dt2224, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  facet_wrap(~eachg_name,nrow=2) +
  #facet_grid(edge_effect~eachg_name) +
  ggtitle("Precision-Recall with healthcare graph when edges' effect between 0.75-1") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.2,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)

pyt

pdf(file="healthcare_PRcurve_0.75_1.pdf",height = 6,width = 10)  
pyt 
dev.off()




  