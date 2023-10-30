

me <- c('TP','FP','FN','TN','precision','recall','F1','acc','TPR','TNR','FPR')

method_name1 <- c('ivw','egger','weighted median','Mode based',
                 #'MR.Robust',
                 #'contamination mixture',
                 'MR.Mix',
                 'MR.RAPS',
                 #'mrcML.DP',
                 'CDcML.DP.S',
                 #'CD.Ratio',
                 'CD.Egger')
method_name2 <- c('MRPC_sigSNP','HC+prior_sigSNP',
                  'MRPC_score','HC+prior_score',
                  'BIMMER',
                  'cGAUGE.ivw','cGAUGE.egger','cGAUGE.mrpresso')
method_name <- c(method_name1,
                 method_name2)

D=7
name1 <- NULL
for(i in 1:length(method_name)){
  name1 <- c(name1,
             paste0(me,'_',method_name[i]))
}
name1 <- c(name1,paste0('R2',1:D),paste0('Fsta',1:D))

name2 <- paste0('time_',method_name)


name3 <- NULL
name4 <- NULL
for(i in 1:length(method_name1)){
  name3 <- c(name3,
             paste0(me,'_',method_name1[i]))
  name4 <- c(name4,
             paste0(c('SpearF','kenDcor_statistic',
                      'kenDcor_pvalue','kenDcor_estimate'),'_',method_name1[i]))
}



prob <- c(0,0.3,0.5)
res_F1 <- NULL
res_precision <- NULL
res_recall <- NULL
res_step1_all <- NULL
for(i in 1:length(prob)){
  tt1 <- NULL
  load(paste0('Magic__prob_weakIV_',prob[i],'_addorder.Rdata'))
  
  # final results
  res_all <- NULL
  for(i in 1:length(tt1)){
    tt10 <- tt1[[i]]
    res_all <- rbind(res_all,
                     c(unlist(tt10[[1]]),unlist(tt10[[2]]),
                       unlist(tt10[[3]]),unlist(tt10[[4]])))
  }
  res_all <- as.data.frame(res_all)
  colnames(res_all) <- c(name1,name2)

  
  res_all1 <- res_all[,paste0('F1_',method_name)]
  res_F1 <- rbind(res_F1,
                  apply(res_all1,2,function(x) mean(x,na.rm = T)))
  
  res_all2 <- res_all[,paste0('precision_',method_name)]
  res_precision <- rbind(res_precision,
                  apply(res_all2,2,function(x) mean(x,na.rm = T)))
  
  res_all3 <- res_all[,paste0('recall_',method_name)]
  res_recall <- rbind(res_recall,
                         apply(res_all3,2,function(x) mean(x,na.rm = T)))
  
  
  # step 1 result
  res_step1 <- NULL
  for(i in 1:length(tt1)){
    tt10 <- tt1[[i]]
    res_step1 <- rbind(res_step1,
                       c(unlist(tt10[[5]]),unlist(tt10[[6]])))
  }
  res_step1 <- as.data.frame(res_step1)
  colnames(res_step1) <- c(name4,name3)
  
  res_step1_all <- rbind(res_step1_all,
                         apply(res_step1,2,function(x) mean(x,na.rm = T)))
}


res_step1_F1 <- res_step1_all[,grep(pattern = 'F1',colnames(res_step1_all))]
res_step1_precision <- res_step1_all[,grep(pattern = 'precision',colnames(res_step1_all))]
res_step1_recall <- res_step1_all[,grep(pattern = 'recall',colnames(res_step1_all))]

res_step1_spearF <- res_step1_all[,grep(pattern = 'SpearF',colnames(res_step1_all))]
res_step1_kenDcor <- res_step1_all[,grep(pattern = 'kenDcor_estimate',colnames(res_step1_all))]

# load(paste0('Magic__prob_corp_0.1_addorder.Rdata'))
# tt10 <- tt1[[1]]
# tt10[[7]][1]
# tt10[[7]][7]
# tt10[[7]][8]


### plot
library(ggplot2)
res_F1 <- as.data.frame(res_F1)
res_precision <- as.data.frame(res_precision)
res_recall <- as.data.frame(res_recall)
datares <- data.frame(F1score=c(res_F1$F1_ivw,
                                res_F1$F1_egger,
                                res_F1$`F1_weighted median`,
                                res_F1$`F1_Mode based`,
                                res_F1$F1_MR.Mix,
                                res_F1$F1_MR.RAPS,
                                res_F1$F1_CDcML.DP.S,
                                res_F1$F1_CD.Egger,
                                res_F1$F1_MRPC_sigSNP,
                                res_F1$`F1_HC+prior_sigSNP`,
                                res_F1$`F1_HC+prior_score`,
                                res_F1$F1_MRPC_score,
                                res_F1$F1_BIMMER,
                                res_F1$F1_cGAUGE.ivw,
                                res_F1$F1_CD.Egger,
                                res_F1$F1_cGAUGE.mrpresso),
                      method=rep(c(paste0('MRSL_',method_name1),
                               method_name2),
                               each=nrow(res_F1)),
                      precision=c(res_precision$precision_ivw,
                                  res_precision$precision_egger,
                                  res_precision$`precision_weighted median`,
                                  res_precision$`precision_Mode based`,
                                  res_precision$precision_MR.Mix,
                                  res_precision$precision_MR.RAPS,
                                  res_precision$precision_CDcML.DP.S,
                                  res_precision$precision_CD.Egger,
                                  res_precision$precision_MRPC_sigSNP,
                                  res_precision$`precision_HC+prior_sigSNP`,
                                  res_precision$`precision_HC+prior_score`,
                                  res_precision$precision_MRPC_score,
                                  res_precision$precision_BIMMER,
                                  res_precision$precision_cGAUGE.ivw,
                                  res_precision$precision_CD.Egger,
                                  res_precision$precision_cGAUGE.mrpresso),
                      recall=c(res_recall$recall_ivw,
                               res_recall$recall_egger,
                               res_recall$`recall_weighted median`,
                               res_recall$`recall_Mode based`,
                               res_recall$recall_MR.Mix,
                               res_recall$recall_MR.RAPS,
                               res_recall$recall_CDcML.DP.S,
                               res_recall$recall_CD.Egger,
                               res_recall$recall_MRPC_sigSNP,
                               res_recall$`recall_HC+prior_sigSNP`,
                               res_recall$`recall_HC+prior_score`,
                               res_recall$recall_MRPC_score,
                               res_recall$recall_BIMMER,
                               res_recall$recall_cGAUGE.ivw,
                               res_recall$recall_CD.Egger,
                               res_recall$recall_cGAUGE.mrpresso),
                      prob=rep(c('0%','30%','50%'),16)
                      )

#datares$method <- 

# library(RColorBrewer)
# display.brewer.pal(9, 'Set1')
# display.brewer.pal(6, 'Set2')
# display.brewer.pal(9, 'Set3')
# display.brewer.pal(12, 'Paired')
# #cloo <- c(brewer.pal(9, 'Set3'),brewer.pal(9, 'Set1')[c(3,2,1,4:8)])
# cloo <- c(brewer.pal(6, 'Set2'),brewer.pal(12, 'Paired'))
# cloo <- cloo[c(6,3,7,8,1,5,9,10,11,12,13,14,2,4,15,16)]

cloo <- c("#8DA0CB", "#A6CEE3" ,"#1F78B4",
  "#66C2A5","#A6D854" ,"#B2DF8A" ,"#33A02C",
   "#FFD92F","#FC8D62","#FDBF6F", "#FF7F00", "#E31A1C",
  "#FB9A99", "#E78AC3", "#CAB2D6", "#6A3D9A")
p1 <- ggplot(data=datares,mapping=aes(x=prob,y=F1score,fill=method))+
  geom_bar(stat = 'identity',position = position_dodge(0.75))+
  scale_fill_manual(values=cloo)+
  theme_bw() +
  ggtitle("F1 score (weak IVs)") +
  xlab("Proportion of weak IVs") +
  ylab("F1 score")+
  ylim(0,1)+
  theme(plot.title = element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))
p1  

pdf("F1 score (weak IVs).pdf",width = 15,height = 7)
p1
dev.off()


shapes <- c(16,15,15,15,17,17,5,5,rep(8,8))

p1 <- ggplot(data=datares,mapping=aes(x=recall,y=precision,
                                      colour=method,group=method))+
  geom_point(size=5,aes(shape=method,colour=method)) +
  scale_shape_manual(values=shapes) +
  geom_abline(slope=1,intercept = 0,color="gray") +
  scale_colour_manual(values=cloo)+
  theme_bw() +
  facet_wrap(~prob,nrow=1) +
  ggtitle("Precision-Recall (weak IV)") +
  xlab("Recall") +
  ylab("Presion") +
  xlim(0,1)+
  ylim(0,1)+
  theme(plot.title = element_text(size = 20,hjust = 0.5,vjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))
p1  

pdf("Precision-Recall (weak IVs).pdf",width = 15,height = 5)
p1
dev.off()







# p1 <- ggplot(data=datares,mapping=aes(x=prob,y=precision,fill=method))+
#   geom_bar(stat = 'identity',position = position_dodge(0.75))+
#   scale_fill_manual(values=cloo)+
#   theme_bw() +
#   ggtitle("Precision (correlated pleiotropy)") +
#   xlab("Proportion of correlated pleiotropic SNPs") +
#   ylab("Precision")+
#   ylim(0,1)+
#   theme(plot.title = element_text(size = 20,hjust = 0.5,vjust = 0.5),
#         axis.text = element_text(size = 15),
#         axis.title = element_text(size = 15))
# p1  
# 
# pdf("Precision (correlated pleiotropy).pdf",width = 15,height = 7)
# p1
# dev.off()
# 
# 
# p1 <- ggplot(data=datares,mapping=aes(x=prob,y=recall,fill=method))+
#   geom_bar(stat = 'identity',position = position_dodge(0.75))+
#   scale_fill_manual(values=cloo)+
#   theme_bw() +
#   ggtitle("Recall (correlated pleiotropy)") +
#   xlab("Proportion of correlated pleiotropic SNPs") +
#   ylab("Recall")+
#   ylim(0,1)+
#   theme(plot.title = element_text(size = 20,hjust = 0.5,vjust = 0.5),
#         axis.text = element_text(size = 15),
#         axis.title = element_text(size = 15))
# p1  
# 
# pdf("Recall (correlated pleiotropy).pdf",width = 15,height = 7)
# p1
# dev.off()
