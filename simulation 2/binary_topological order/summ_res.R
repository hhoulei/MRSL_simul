

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
           
           #paste0("BIMMER_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           paste0("cGAUGE.ivw_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.egger_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           paste0("cGAUGE.mrpresso_",c("TP","FP","FN","TN","precision","recall","F1","acc","TPR","TNR","FPR")),
           
           
           #paste0("R2_D",1:D),
           #paste0("F_D",1:D),
           
           "time11","time12","time13",
           "time2","time21","time3","time31",
           #"time4",
           "time51","time52","time53",
           
           paste0('MRSL_remove_collider_',me1),
           paste0('MRSL_open_path_',me1),
           paste0('MRSL_min_suffi_set_',me1)
)

prob=c(0.2,0.5,0.8)
dt1 <- NULL
for(i in 1:length(prob)){
  load(paste0('D_10_dlB_0.41_duB_0.69_prob_',prob[i],'.Rdata'))
  
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

# dt_all <- data.frame(SpearF=c(dt1$MRSL_remove_collider_SpearF,
#                               dt1$MRSL_open_path_SpearF,
#                               dt1$MRSL_min_suffi_set_SpearF)/90,
#                      prob=rep(c(0.2,0.5,0.8),nrow(dt1)),
#                      kenDcor_estimate=c(dt1$MRSL_remove_collider_kenDcor_estimate,
#                                         dt1$MRSL_open_path_kenDcor_estimate,
#                                         dt1$MRSL_min_suffi_set_kenDcor_estimate),
#                      method=rep(c("MRSL_remove_collider",
#                                 "MRSL_open_path",
#                                 "MRSL_min_suffi_set"),each=nrow(dt1)))
# 
# 
# coloo <- c("#FC8D62","#E7298A","#E31A1C")
# 
# p1 <- ggplot(data=dt_all,mapping=aes(x=prob,y=SpearF,fill=method))+
#   geom_bar(stat = 'identity',position = position_dodge(0.75))+
#   scale_fill_manual(values=coloo)+
#   theme_bw() +
#   ggtitle("Spearman's footrule (10 continuous variables)") +
#   xlab("Complexity of netowrk") +
#   ylab("Spearman's footrule")+
#   theme(plot.title = element_text(size = 20,hjust = 0.5,vjust = 0.5),
#         axis.text = element_text(size = 15),
#         axis.title = element_text(size = 15))
# p1  
# 
# pdf("SpearF.pdf",width = 15,height = 7)
# p1
# dev.off()
