

library(ggplot2)
library(RColorBrewer)
D=10
me1 <- c('SpearF','kenDcor_statistic','kenDcor_p.value','kenDcor_estimate')
mname <- c(paste0("MRSL_remove_collider_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_open_path_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("MRSL_min_suffi_set_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("MRPC_sigSNP_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("HC_sigSNP_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("MRPC_score_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("HC_score_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("BIMMER_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("cGAUGE.ivw_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.egger_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.mrpresso_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           
           paste0("R2_D",1:D),
           paste0("F_D",1:D),
           
           "time11","time12","time13",
           "time2","time21","time3","time31","time4","time51","time52","time53",
           
           paste0('MRSL_remove_collider_',me1),
           paste0('MRSL_open_path_',me1),
           paste0('MRSL_min_suffi_set_',me1)
)

prob=c(0.2,0.5,0.8)
dt1 <- NULL
for(i in 1:length(prob)){
  load(paste0('D_10_dlB_0.25_duB_0.5_prob_',prob[i],'.Rdata'))
  
  res_all <- NULL
  for(i in 1:length(tt1)){
    tt10 <- tt1[[i]]
    res_all <- rbind(res_all,
                     c(unlist(tt10)))
  }
  dt1_once <- apply(res_all,2,function(x) mean(x,na.rm=T))
  dt1 <- rbind(dt1,dt1_once)
}
dt1 <- as.data.frame(dt1)
colnames(dt1) <- mname


dt_SpearF <- dt1[,grep('SpearF',colnames(dt1))]
dt_KenD <- dt1[,grep('kenDcor_estimate',colnames(dt1))]

write.csv(dt_SpearF,'dt_SpearF.csv')
write.csv(dt_KenD,'dt_KenD.csv')

dt_all <- data.frame(time=c(dt1$time11,
                            dt1$time12,
                            dt1$time13,
                            dt1$time2,
                            dt1$time21,
                            dt1$time3,
                            dt1$time31,
                            dt1$time4,
                            dt1$time51,
                            dt1$time52,
                            dt1$time53),
                     prob=rep(c(0.2,0.5,0.8),11),
                     method=rep(c("MRSL_remove_collider",
                                "MRSL_open_path",
                                "MRSL_min_suffi_set",
                                'MRPC_sigSNP',
                                'MRPC_score',
                                'HC_sigSNP',
                                'HC_score',
                                'BIMMER',
                                'cGAUGE.ivw',
                                'cGAUGE.egger',
                                'cGAUGE.mrpresso'),each=nrow(dt1)))


shapes <- c(16,15,15,15,17,17,5,5,rep(8,3))
coloo <- c( "#A6761D",
            "#084081", "#2B8CBE", "#7BCCC4",
            "#FDAE6B","#F16913",
            "#A6D854","#33A02C",
            "#FC8D62","#E7298A","#E31A1C"
)


pyt <- ggplot(data=dt_all, aes(x=prob, y=time,colour=method,group=method)) +
  geom_point(size=2,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  geom_line(size=0.7,aes(colour=method)) + 
  scale_colour_manual(values=coloo)+
  #scale_colour_brewer(palette = "Paired") +
  facet_wrap( vars(p))+
  ylab("F1 score") +
  ggtitle("F1 score with 10 continuous nodes") +
  theme_bw() +
  theme(plot.title = element_text(size = 15,hjust = 0.5,vjust = 0.5)) +
  ylim(0,1)


pyt

pdf(file="simulation_D10_F1score.pdf",height = 8,width = 12)  
pyt 
dev.off()
