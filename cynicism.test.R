getwd()
setwd('~/Downloads')
psy_test<-read_excel("p_test.xlsx")
install.packages("psychometric")
library(psychometric)
#(2, 5, 11, 14, 18, 20, 21, 22, 27, 29, 30, 31, 32, 34, 36)反向
psy_test$LC2<-6-psy_test$LC2
psy_test$LC5<-6-psy_test$LC5
psy_test$OC11<-6-psy_test$OC11
psy_test$OC14<-6-psy_test$OC14
psy_test$OC18<-6-psy_test$OC18
psy_test$OC20<-6-psy_test$OC20
psy_test$PC21<-6-psy_test$PC21
psy_test$PC22<-6-psy_test$PC22
psy_test$SC27<-6-psy_test$SC27
psy_test$SC29<-6-psy_test$SC29
psy_test$SC30<-6-psy_test$SC30
psy_test$SC31<-6-psy_test$SC31
psy_test$SC32<-6-psy_test$SC32
psy_test$EC34<-6-psy_test$EC34
psy_test$EC36<-6-psy_test$EC36
alpha(psy_test[,c(6:46)])
psy_test_items<-psy_test[,c(6:42)]
psy_test_items<-tbl_df(psy_test_items)
names(psy_test_items)
psy_test_items<-rbind(psy_test_items,
                      mean=summarize_each(psy_test_items,
                                          funs(mean)))
psy_test_items<-mutate(psy_test_items,
 LC_total_mean=rowSums(psy_test_items[,1:9])/9,
 OC_total_mean=rowSums(psy_test_items[,10:20])/11,
 PC_total_mean=rowSums(psy_test_items[,21:26])/6,
 SC_total_mean=rowSums(psy_test_items[,27:33])/7,
 EC_total_mean=rowSums(psy_test_items[,34:37])/4,
 total_mean=rowSums(psy_test_items[,1:37])/37)

psy_test_items<-psy_test_items[-315,]
ggplot(psy_test_items, aes(x=LC_total_mean, y=LC1))+
  geom_jitter()+
  geom_abline()
LC_cor<-as.vector(apply(psy_test_items[,1:9], 2,
      function(x)cor(x, psy_test_items$LC_total_mean, method="spearman")))
OC_cor<-as.vector(apply(psy_test_items[,10:20], 2,
      function(x)cor(x, psy_test_items$OC_total_mean, method="spearman")))
PC_cor<-as.vector(apply(psy_test_items[,21:26], 2,
      function(x)cor(x, psy_test_items$PC_total_mean, method="spearman")))
SC_cor<-as.vector(apply(psy_test_items[,27:33], 2,
      function(x)cor(x, psy_test_items$SC_total_mean, method="spearman")))
EC_cor<-as.vector(apply(psy_test_items[,34:41], 2, 
      function(x)cor(x, psy_test_items$EC_total_mean, method="spearman")))
sub_cor<-c(LC_cor, OC_cor, PC_cor, SC_cor, EC_cor)
total_cor<-as.vector(apply(psy_test_items[,1:37], 2, 
      function(x)cor(x, psy_test_items$total_mean, method="spearman")))

alpha(psy_test_items)

write.xlsx(psy_test_items, "cor_test.xlsx")
raw<-psy_test[,c(6:46)]
item.exam(raw)
cronbach(raw)
alpha(raw)
discrimination<-function(x,data){
  dta<-data[,x]
  total<-0
  for(i in 1:nrow(dta)){
    total[i]<-sum(dta[i, 1:ncol(dta)])
  }
  dta$total<-total
  qt<-quantile(dta$total)
  dta.u<-filter(dta, total>=qt[4])
  dta.l<-filter(dta, total<=qt[2])
  dis<-unlist(lapply(1:ncol(dta),function(x){t.test(dta.u[,x],dta.l[,x])$p.value}))
  dis.posit<-list(dis, which(dis>=0.05))
  print(dis.posit)
}
discrimination(c(1:41),psy_test_items)
item.exam(raw)
psy.test<-read_excel("信度分析.xlsx")
which(n.test[316,]<0.3)
psy<-psy_test_items[, c(1:37)]
cronbach(psy[, -c(2, 11, 16, 33, 34, 36)])
cronbach(psy[, -c(2, 33, 34, 36)])
factanal(psy, 5)
ggpairs(psy[, 1:9])
r<-as.vector(r)
which(r<0.2)
r<-as.vector(r)
item.exam(psy)
item<-c(1:37)
factanal(psy, 5)
QQ<-data.frame(item, r, sub, sub_r)
which(QQ$r<0.3)
ggplot(QQ, aes(x=item, y=r))+
  geom_point()+
  geom_hline(yintercept = 0.25, linetype=4, color="red")+
  scale_x_continuous(breaks=seq(1,37,1))+
  scale_y_continuous(breaks=seq(0, 0.7, 0.05))+
  ggtitle("Spearman R")
sub<-c("LC","LC","LC","LC","LC","LC","LC","LC","LC","OC","OC","OC","OC","OC","OC",
       "OC","OC","OC","OC","OC","PC","PC","PC","PC","PC","PC","SC","SC","SC","SC",
       "SC","SC","SC","EC","EC","EC","EC")
sub_r=c(0.6051483,0.2740123,0.5648314,0.5303208,0.5431106,0.5091417,
          0.6463705,0.6374921,0.5571147,0.6578371,0.4172129,0.4817859,
          0.6750965,0.5116052,0.5243680,0.4101521,0.5753795,0.4572722,
          0.6296870,0.5243579,0.7523344,0.7256517,0.5826666,0.7546909,
          0.5697302,0.6499846,0.7026265,0.6606159,0.7709996,0.7705100,
          0.6388435,0.7358045,0.3468496,0.3091063,0.4594041,0.2489855,
          0.4104843)
sub_r<-round(sub_r, 3)
ggplot(QQ, aes(x=item, y=sub_r, col=sub))+
  geom_point()+
  geom_hline(yintercept = 0.4, linetype=4, color="red")+
  scale_x_continuous(breaks=seq(1,37,1))+
  scale_y_continuous(breaks=seq(0, 0.7, 0.05))+
  ggtitle("Spearman R")
psy_test_items$total_mean<-round(rowSums(psy_test_items)/37, 3)
psy_test_items$learning_mean<-round(rowSums(psy_test_items[, c(1:9)]/9), 3)
psy_test_items$organizational_mean<-round(rowSums(psy_test_items[, c(10:20)]/11), 3)
psy_test_items$policy_mean<-round(rowSums(psy_test_items[, c(21:26)]/6), 3)
psy_test_items$social_mean<-round(rowSums(psy_test_items[,c(27:33)]/7), 3)
psy_test_items$environmental<-round(rowSums(psy_test_items[, c(34:37)]/4), 3)
spearman_correlation<-data.frame(item, sub_r, r)
