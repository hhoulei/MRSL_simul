
library(ggplot2)
library(RColorBrewer)

mme <- 1:7
edge_effect <- seq(0.1,0.9,0.2)

######################################################################

dt_allA <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('A_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allA <- rbind(dt_allA,file_once)
  }
}
colnames(dt_allA) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allA1 <- dt_allA[dt_allA$g==100,]

dt_allB <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('B_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allB <- rbind(dt_allB,file_once)
  }
}
colnames(dt_allB) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allB1 <- dt_allB[dt_allB$g==100,]

dt_allC <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('C_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allC <- rbind(dt_allC,file_once)
  }
}
colnames(dt_allC) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allC1 <- dt_allC[dt_allC$g==100,]


dt_all1 <- rbind(dt_allA,dt_allB,dt_allC)
dt_all1 <- as.data.frame(dt_all1)
dt_all1 <- dt_all1[dt_all1$g ==100,]
dt_all1$graph<- rep(c('Graph A (collider)','Graph B (mediator)','Graph C (confounder)'),each=35000)

dt_all1$effect <- factor(dt_all1$effect)
dt_all1$method <- factor(dt_all1$method,
                         labels = c('G1 only','G2 only','G3 only',
                                    'G1+G3','G1+G2','G2+G3','G1+G2+G3'),
                         ordered = T)
dt_all1A <- dt_all1[dt_all1$graph=='Graph A (collider)',]
dt_all1B <- dt_all1[dt_all1$graph=='Graph B (mediator)',]
dt_all1C <- dt_all1[dt_all1$graph=='Graph C (confounder)',]

dt_all1A <- dt_all1A[dt_all1A$method %in% c('G1+G2+G3','G1+G2'),]
dt_all1B <- dt_all1B[dt_all1B$method %in% c('G1+G2+G3','G1+G2'),]
dt_all1C <- dt_all1C[dt_all1C$method %in% c('G1+G2+G3','G1+G3'),]


coloo <- c(brewer.pal(7, "Set2"))

P1<- ggplot(data=dt_all1A,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  #ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph A collider (beta=0)', tag = "A)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection")) +
  ylim(-0.25,0.25)+
  theme(legend.position="none")

P1

P2<- ggplot(data=dt_all1B,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  # ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph B mediator (beta=0)', tag = "B)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection"))+
  ylim(-0.25,0.25)+
  theme(legend.position="none")

P2

P3<- ggplot(data=dt_all1C,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo,labels=c('Intersection','Union'))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray")+
  # ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph C confounder (beta=0)', tag = "C)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection"))+
  ylim(-0.25,0.25)

P3

######################################################################

dtA <- read.csv('A_mv_ivw_0.csv')
dtB <- read.csv('B_mv_ivw_0.csv')
dtC <- read.csv('C_mv_ivw_0.csv')


dt_all1 <- rbind(dtA,dtB,dtC)
dt_all1 <- as.data.frame(dt_all1)

dtabc <- data.frame(typrI=c(dt_all1$typeI1_1,
                            dt_all1$typeI1_2,
                            dt_all1$typeI1_3,
                            dt_all1$typeI1_4,
                            dt_all1$typeI1_5,
                            dt_all1$typeI1_6,
                            dt_all1$typeI1_7),
                    g=rep(dt_all1$g,7),
                    effect=rep(dt_all1$elow,7),
                    method=rep(1:7,each=nrow(dt_all1)),
                    graph=rep(rep(c('Graph A (collider)','Graph B (mediator)','Graph C (confounder)'),each=35),7)
)

dtabc$effect <- factor(dtabc$effect )
dtabc$method <- factor(dtabc$method,
                       labels = c('G1 only','G2 only','G3 only',
                                  'G1+G3','G1+G2','G2+G3','G1+G2+G3') )
dtabc$typrI[dtabc$typrI>0.5] <- 0.5
dtabc <- dtabc[dtabc$g==100,]
coloo <- c(brewer.pal(7, "Set2"))

dtabcA <- dtabc[dtabc$graph=='Graph A (collider)',]
dtabcB <- dtabc[dtabc$graph=='Graph B (mediator)',]
dtabcC <- dtabc[dtabc$graph=='Graph C (confounder)',]

dtabcA <- dtabcA[dtabcA$method %in% c('G1+G2+G3','G1+G2'),]
dtabcB <- dtabcB[dtabcB$method %in% c('G1+G2+G3','G1+G2'),]
dtabcC <- dtabcC[dtabcC$method %in% c('G1+G2+G3','G1+G3'),]

P4<- ggplot(data=dtabcA,aes(fill=method, x = effect, y = typrI)) +
  geom_bar(position = 'dodge',stat = 'identity') +
  scale_fill_manual(values=coloo,labels=c('Intersection','Union'))+
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray")+
  #ylim(0,0.5)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph A collider (beta=0)', tag = "D)")+
  xlab("Other edges' Effects") +
  ylab("Type I error rates") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none")

P4

P5<- ggplot(data=dtabcB,aes(fill=method, x = effect, y = typrI)) +
  geom_bar(position = 'dodge',stat = 'identity') +
  scale_fill_manual(values=coloo,labels=c('Intersection','Union'))+
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray")+
  #ylim(0,0.5)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph B mediator (beta=0)', tag = "E)")+
  xlab("Other edges' Effects") +
  ylab("Type I error rates") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none")+
  ylim(0,0.5)

P5

P6<- ggplot(data=dtabcC,aes(fill=method, x = effect, y = typrI)) +
  geom_bar(position = 'dodge',stat = 'identity') +
  scale_fill_manual(values=coloo,labels=c('Intersection','Union'))+
  geom_hline(yintercept=0.05, linetype="dashed", color = "gray")+
  #ylim(0,0.5)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph C confounder (beta=0)', tag = "F)")+
  xlab("Other edges' Effects") +
  ylab("Type I error rates") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection"))+
  ylim(0,0.5)

P6

#####################################################################

mme <- 1:7
edge_effect <- seq(0.1,0.9,0.2)


dt_allA <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('A_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.1.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allA <- rbind(dt_allA,file_once)
  }
}
colnames(dt_allA) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allA1 <- dt_allA[dt_allA$g==100,]

dt_allB <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('B_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.1.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allB <- rbind(dt_allB,file_once)
  }
}
colnames(dt_allB) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allB1 <- dt_allB[dt_allB$g==100,]

dt_allC <- NULL
for(i in 1:length(mme)){
  for(j in 1:length(edge_effect)){
    filename <- paste0('C_elow_',edge_effect[j],'_eup_',edge_effect[j],'method_',mme[i],'_0.1.csv')
    file_once <- read.csv(filename)
    file_once <- file_once[,-1]
    file_once <- cbind(mme[i],file_once)
    file_once <- cbind(edge_effect[j],file_once)
    dt_allC <- rbind(dt_allC,file_once)
  }
}
colnames(dt_allC) <- c('effect','method','g','beta','se','tvalue','pvalue')
#dt_allC1 <- dt_allC[dt_allC$g==100,]


dt_all1 <- rbind(dt_allA,dt_allB,dt_allC)
dt_all1 <- as.data.frame(dt_all1)
dt_all1 <- dt_all1[dt_all1$g %in% c(100),]
dt_all1$graph<- rep(c('Graph A (collider)','Graph B (mediator)','Graph C (confounder)'),each=35000)
dt_all1$effect <- factor(dt_all1$effect )
dt_all1$method <- factor(dt_all1$method,
                         labels = c('G1 only','G2 only','G3 only',
                                    'G1+G3','G1+G2','G2+G3','G1+G2+G3'))

dt_all1A <- dt_all1[dt_all1$graph=='Graph A (collider)',]
dt_all1B <- dt_all1[dt_all1$graph=='Graph B (mediator)',]
dt_all1C <- dt_all1[dt_all1$graph=='Graph C (confounder)',]

dt_all1A <- dt_all1A[dt_all1A$method %in% c('G1+G2+G3','G1+G2'),]
dt_all1B <- dt_all1B[dt_all1B$method %in% c('G1+G2+G3','G1+G2'),]
dt_all1C <- dt_all1C[dt_all1C$method %in% c('G1+G2+G3','G1+G3'),]

coloo <- c(brewer.pal(7, "Set2"))

P7<- ggplot(data=dt_all1A,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo)+
  geom_hline(yintercept=0.1, linetype="dashed", color = "gray")+
  # ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph A collider (beta=0.1)', tag = "G)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none") +
  ylim(-0.2,0.4)

P7

P8<- ggplot(data=dt_all1B,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo)+
  geom_hline(yintercept=0.1, linetype="dashed", color = "gray")+
  # ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph B mediator (beta=0.1)', tag = "H)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none")+
  ylim(-0.2,0.4)

P8

P9<- ggplot(data=dt_all1C,aes(x = effect, y = beta)) +
  geom_boxplot(aes(fill = method),outlier.alpha=0.5) +
  scale_fill_manual(values=coloo,labels=c('Intersection','Union'))+
  geom_hline(yintercept=0.11, linetype="dashed", color = "gray")+
  # ylim(-0.6,0.8)+
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph C confounder (beta=0.1)', tag = "I)")+
  xlab("Other edges' Effects") +
  ylab("Estimation") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection"))+
  ylim(-0.2,0.4)

P9
#####################################################################


dt_allA <- read.csv('A_mv_ivw_0.1.csv')
dt_allB <- read.csv('B_mv_ivw_0.1.csv')
dt_allC <- read.csv('C_mv_ivw_0.1.csv')

dt_all <- rbind(dt_allA,dt_allB,dt_allC)
dt_all <- as.data.frame(dt_all)
dt_all$graph <- rep(c('Graph A (collider)',
                      'Graph B (mediator)',
                      'Graph C (confounder)'),each=35)

dt_all1 <- data.frame(power=c(dt_all$typeI1_1,
                              dt_all$typeI1_2,
                              dt_all$typeI1_3,
                              dt_all$typeI1_4,
                              dt_all$typeI1_5,
                              dt_all$typeI1_6,
                              dt_all$typeI1_7),
                      effect=rep(dt_all$elow,7),
                      g=rep(dt_all$g,7),
                      graph=rep(dt_all$graph,7),
                      method=rep(1:7,each=nrow(dt_all)))


dt_all1 <- dt_all1[dt_all1$g %in% c(100),]
dt_all1$effect <- factor(dt_all1$effect)
dt_all1$method <- factor(dt_all1$method,
                         labels = c('G1 only','G2 only','G3 only',
                                    'G1+G3','G1+G2','G2+G3','G1+G2+G3'))

shapes <- c(15,16,17,18,13,6,7)
coloo <- c(brewer.pal(7, "Set2"))

dtabcA <- dt_all1[dt_all1$graph=='Graph A (collider)',]
dtabcB <- dt_all1[dt_all1$graph=='Graph B (mediator)',]
dtabcC <- dt_all1[dt_all1$graph=='Graph C (confounder)',]

dtabcA <- dtabcA[dtabcA$method %in% c('G1+G2+G3','G1+G2'),]
dtabcB <- dtabcB[dtabcB$method %in% c('G1+G2+G3','G1+G2'),]
dtabcC <- dtabcC[dtabcC$method %in% c('G1+G2+G3','G1+G3'),]

#linline <- ifelse(dtabcA$method %in% c('G1+G2+G3'),'solid','dashed')


P10<- ggplot(data=dtabcA,aes(x = effect, y = power,colour=method,group=method)) +
  geom_point(size=2)+
  scale_colour_manual(values=coloo)+
  scale_shape_manual(values=shapes) +
  geom_line(linetype='dashed',size=0.5,aes(colour=method)) + 
  #scale_fill_manual(values=coloo)+
  #geom_abline(slope=1,intercept = 0,color="black") +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph A collider (beta=0.1)', tag = "J)")+
  labs(x = "Other edges' Effects", 
       y = "Power") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none")+
  ylim(0,1)
P10

linetype1 <- ifelse(dtabcB$method %in% c('G2 only','G1+G2','G1+G2+G3'),'typeI stable','typeI stable (not)')
P11<- ggplot(data=dtabcB,aes(x = effect, y = power,colour=method,group=method)) +
  geom_point(size=2)+
  scale_colour_manual(values=coloo)+
  scale_shape_manual(values=shapes) +
  geom_line(size=0.7,aes(linetype=linetype1,colour=method)) + 
  #scale_fill_manual(values=coloo)+
  #geom_abline(slope=1,intercept = 0,color="black") +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph B mediator (beta=0.1)', tag = "K)")+
  labs(x = "Other edges' Effects", 
       y = "Power") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  theme(legend.position="none")+
  ylim(0,1)
P11

linetype <- ifelse(dtabcC$method %in% c('G1 only','G2 only','G1+G2','G1+G2+G3'),'typeI stable','typeI stable (not)')
P12<- ggplot(data=dtabcC,aes(x = effect, y = power,colour=method,group=method)) +
  geom_point(size=2)+
  scale_colour_manual(values=coloo,labels=c('Intersection','Union'))+
  scale_shape_manual(values=shapes) +
  geom_line(size=0.7,aes(linetype=linetype,colour=method)) + 
  #scale_fill_manual(values=coloo)+
  #geom_abline(slope=1,intercept = 0,color="black") +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  labs(title='Graph C confounder (beta=0.1)', tag = "L)")+
  labs(x = "Other edges' Effects", 
       y = "Power") +
  theme(strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18)) +
  theme(plot.title = element_text(size = 18),
        plot.tag = element_text(size = 18),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 15))+
  guides(fill=guide_legend(title="IV selection"))+
  ylim(0,1)

P12

#####################################################################

source('multiplot.R')


pdf("Figure 3 major revision 0115.pdf",width = 15, height = 13)
MM <- c(1,1,1,1,2,2,2,2,3,3,3,3,3,
        4,4,4,4,5,5,5,5,6,6,6,6,6,
        7,7,7,7,8,8,8,8,9,9,9,9,9,
        10,10,10,10,11,11,11,11,12,12,12,12,12)
multiplot(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,
          layout=matrix(MM, nrow=4, byrow=TRUE))
dev.off()



