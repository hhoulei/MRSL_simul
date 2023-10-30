

library(ggplot2)
library(RColorBrewer)
D=10
mname <- c("each_g","prob",
           paste0("MRSL_remove_collider_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_open_path_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_min_suffi_set_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
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
           "time2","time21","time3","time31",#"time4",
           "time51","time52","time53"
)
dttime <- NULL


dt1 <- NULL
for(i in 1:30){
  dt1_once <- read.csv(paste0("D10/D_10_dlB_0_duB_0.41_",i,".csv"),header =F)
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
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_F1,
             dt1$MRSL_open_path_F1,
             dt1$MRSL_min_suffi_set_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1),
  
  Presion=c(dt1$MRSL_remove_collider_precision,
            dt1$MRSL_open_path_precision,
            dt1$MRSL_min_suffi_set_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision),
  
  Recall=c(dt1$MRSL_remove_collider_recall,
           dt1$MRSL_open_path_recall,
           dt1$MRSL_min_suffi_set_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall),
  
  FPR = c(dt1$MRSL_remove_collider_FPR,
          dt1$MRSL_open_path_FPR,
          dt1$MRSL_min_suffi_set_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR),
  
  TPR = c(dt1$MRSL_remove_collider_TPR,
          dt1$MRSL_open_path_TPR,
          dt1$MRSL_min_suffi_set_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR),
  
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
            dt1$time53),
  
  eachg=rep(dt1$each_g,10),
  p=paste0("Prob=",rep(dt1$prob,10))
)


dt1 <- NULL
for(i in 1:18){
  dt1_once <- read.csv(paste0("D10/D_10_dlB_0.41_duB_0.69_",i,".csv"),header =F)
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
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_F1,
             dt1$MRSL_open_path_F1,
             dt1$MRSL_min_suffi_set_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1),
  
  Presion=c(dt1$MRSL_remove_collider_precision,
            dt1$MRSL_open_path_precision,
            dt1$MRSL_min_suffi_set_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision),
  
  Recall=c(dt1$MRSL_remove_collider_recall,
           dt1$MRSL_open_path_recall,
           dt1$MRSL_min_suffi_set_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall),
  
  FPR = c(dt1$MRSL_remove_collider_FPR,
          dt1$MRSL_open_path_FPR,
          dt1$MRSL_min_suffi_set_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR),
  
  TPR = c(dt1$MRSL_remove_collider_TPR,
          dt1$MRSL_open_path_TPR,
          dt1$MRSL_min_suffi_set_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR),
  
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
            dt1$time53),
  
  eachg=rep(dt1$each_g,10),
  p=paste0("Prob=",rep(dt1$prob,10))
)


dt1 <- NULL
for(i in 1:18){
  dt1_once <- read.csv(paste0("D10/D_10_dlB_0.69_duB_0.92_",i,".csv"),header =F)
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
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_F1,
             dt1$MRSL_open_path_F1,
             dt1$MRSL_min_suffi_set_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1),
  
  Presion=c(dt1$MRSL_remove_collider_precision,
            dt1$MRSL_open_path_precision,
            dt1$MRSL_min_suffi_set_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision),
  
  Recall=c(dt1$MRSL_remove_collider_recall,
           dt1$MRSL_open_path_recall,
           dt1$MRSL_min_suffi_set_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall),
  
  FPR = c(dt1$MRSL_remove_collider_FPR,
          dt1$MRSL_open_path_FPR,
          dt1$MRSL_min_suffi_set_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR),
  
  TPR = c(dt1$MRSL_remove_collider_TPR,
          dt1$MRSL_open_path_TPR,
          dt1$MRSL_min_suffi_set_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR),
  
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
            dt1$time53),
  
  eachg=rep(dt1$each_g,10),
  p=paste0("Prob=",rep(dt1$prob,10))
)
dt23$F1_score[which(is.nan(dt23$F1_score))] <- 0


dt1 <- NULL
for(i in 1:18){
  dt1_once <- read.csv(paste0("D10/D_10_dlB_0.92_duB_1.1_",i,".csv"),header =F)
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
               
               "cGAUGE-ivw","cGAUGE-egger","cGAUGE-mrpresso"),each=nrow(dt1)),
  
  F1_score=c(dt1$MRSL_remove_collider_F1,
             dt1$MRSL_open_path_F1,
             dt1$MRSL_min_suffi_set_F1,
             dt1$MRPC_sigSNP_F1,
             dt1$HC_sigSNP_F1,
             dt1$MRPC_score_F1,
             dt1$HC_score_F1,
             #dt1$BIMMER_F1,
             dt1$cGAUGE.ivw_F1,
             dt1$cGAUGE.egger_F1,
             dt1$cGAUGE.mrpresso_F1),
  
  Presion=c(dt1$MRSL_remove_collider_precision,
            dt1$MRSL_open_path_precision,
            dt1$MRSL_min_suffi_set_precision,
            dt1$MRPC_sigSNP_precision,
            dt1$HC_sigSNP_precision,
            dt1$MRPC_score_precision,
            dt1$HC_score_precision,
            #dt1$BIMMER_precision,
            dt1$cGAUGE.ivw_precision,
            dt1$cGAUGE.egger_precision,
            dt1$cGAUGE.mrpresso_precision),
  
  Recall=c(dt1$MRSL_remove_collider_recall,
           dt1$MRSL_open_path_recall,
           dt1$MRSL_min_suffi_set_recall,
           dt1$MRPC_sigSNP_recall,
           dt1$HC_sigSNP_recall,
           dt1$MRPC_score_recall,
           dt1$HC_score_recall,
           #dt1$BIMMER_recall,
           dt1$cGAUGE.ivw_recall,
           dt1$cGAUGE.egger_recall,
           dt1$cGAUGE.mrpresso_recall),
  
  FPR = c(dt1$MRSL_remove_collider_FPR,
          dt1$MRSL_open_path_FPR,
          dt1$MRSL_min_suffi_set_FPR,
          dt1$MRPC_sigSNP_FPR,
          dt1$HC_sigSNP_FPR,
          dt1$MRPC_score_FPR,
          dt1$HC_score_FPR,
          #dt1$BIMMER_FPR,
          dt1$cGAUGE.ivw_FPR,
          dt1$cGAUGE.egger_FPR,
          dt1$cGAUGE.mrpresso_FPR),
  
  TPR = c(dt1$MRSL_remove_collider_TPR,
          dt1$MRSL_open_path_TPR,
          dt1$MRSL_min_suffi_set_TPR,
          dt1$MRPC_sigSNP_TPR,
          dt1$HC_sigSNP_TPR,
          dt1$MRPC_score_TPR,
          dt1$HC_score_TPR,
          #dt1$BIMMER_TPR,
          dt1$cGAUGE.ivw_TPR,
          dt1$cGAUGE.egger_TPR,
          dt1$cGAUGE.mrpresso_TPR),
  
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
            dt1$time53),
  
  eachg=rep(dt1$each_g,10),
  p=paste0("Prob=",rep(dt1$prob,10))
)
dt24$F1_score[which(is.nan(dt24$F1_score))] <- 0


dttime <- cbind(c(rep("OR 1-1.5",30),rep(c("OR 1.5-2","OR 2-2.5","OR 2.5-3"),each=18)),dttime)
colnames(dttime)
dttime1 <- dttime[,c(1:3,114:123)]
colnames(dttime1) <- c('edge effect','each g','prob',
                       'MRSL_remove_collider','MRSL_open_path','MRSL_min_sep_set',
                       'MRPC_sigSNP','HC_sigSNP','MRPC_score','HC_score',
                       'cGAUGE.ivw','cGAUGE.egger','cGAUGE.mrpresso')
write.csv(dttime1,"D10_computing_time-1.csv")


dt2 <- as.data.frame(rbind(dt21,dt22,dt23,dt24))
dt2$edge_effect <- c(rep("OR 1-1.5",300),
                     rep(c("OR 1.5-2",
                           "OR 2-2.5","OR 2.5-3"),each=180))

shapes <- c(15,15,15,17,17,5,5,rep(8,3))
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
  facet_grid( vars(p), vars(edge_effect),scales="free_x")+
  ylab("F1 score") +
  ggtitle("F1 score with 10 binary nodes") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5)) +
  ylim(0,1)

  
pyt

pdf(file="simulation_D10_F1score-1.pdf",height = 8,width = 12)  
pyt 
dev.off()



dt2$eachg_name <- paste0('eachg=',dt2$eachg)
dt2$eachg_name <- factor(dt2$eachg_name,levels=unique(dt2$eachg_name),ordered = T)

dt2221 <- dt2[dt2$edge_effect=='OR 1-1.5',]

pyt <- ggplot(data=dt2221, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  #facet_wrap(~eachg_name,nrow=2) +
  facet_grid(vars(p), vars(eachg_name),scales="free_x")+
  ggtitle("Precision-Recall with 10 binary nodes when OR between 1-1.5") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)


pyt

pdf(file="simulation_D10_PRcurve_1_1.5-1.pdf",height = 6,width = 13)  
pyt 
dev.off()

dt2222 <- dt2[dt2$edge_effect=='OR 1.5-2',]

pyt <- ggplot(data=dt2222, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  #facet_wrap(~eachg_name,nrow=2) +
  facet_grid(vars(p), vars(eachg_name),scales="free_x")+
  ggtitle("Precision-Recall with 10 binary nodes when OR between 1.5-2") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)


pyt

pdf(file="simulation_D10_PRcurve_1.5_2.pdf",height = 6,width = 13)  
pyt 
dev.off()

dt2223 <- dt2[dt2$edge_effect=='OR 2-2.5',]

pyt <- ggplot(data=dt2223, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  #facet_wrap(~eachg_name,nrow=2) +
  facet_grid(vars(p), vars(eachg_name),scales="free_x")+
  ggtitle("Precision-Recall with 10 binary nodes when OR 2-2.5") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)


pyt

pdf(file="simulation_D10_PRcurve_2_2.5.pdf",height = 6,width = 13)  
pyt 
dev.off()


dt2224 <- dt2[dt2$edge_effect=='OR 2.5-3',]

pyt <- ggplot(data=dt2224, aes(x=Recall, y=Presion,colour=method,group=method)) +
  geom_point(size=4,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  # geom_line(size=0.7,aes(colour=method)) + 
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=coloo)+
  #facet_wrap(~eachg_name,nrow=2) +
  facet_grid(vars(p), vars(eachg_name),scales="free_x")+
  ggtitle("Precision-Recall with 10 binary nodes when OR between 2.5-3") +
  xlab("Recall") +
  ylab("Presion") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5))+
  xlim(0,1)+
  ylim(0,1)


pyt

pdf(file="simulation_D10_PRcurve_2.5_3.pdf",height = 6,width = 13)  
pyt 
dev.off()


  