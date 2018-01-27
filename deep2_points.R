##point source
library(dplyr)
library(ggplot2)
library(reshape)
dir<-getSrcDirectory(function(x) {x})
setwd(dir)
df<-read.csv('data_deep.csv')
df_opening<-filter(df,sim_id%in%c(14,15,16,17))
df_delay<-df_opening[,c(1,2,6)]
df_delay$sim_id<-as.factor(df_delay$sim_id)
df_final<-melt(df_delay)
#S1 individual near-far
df_final2<-filter(df_final,variable=='delay_s1')
df_final3<-df_opening[,c(1,5,6)]
df_final3$sim_id<-as.factor(df_final3$sim_id)
p<-ggplot(data=df_final2) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
df_final3$gp<-c(1,2)[df_final3$sim_id %in% c(16,17)+1L]
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final3,aes(x=sim_id,y=delay_s1,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
   # geom_line(data=df_final3,aes(x=sim_id,y=delay_s1,color=bipole,group=bipole))+
 # ylab('Delay S1 (ms)')
geom_line(data=df_final3,aes(x=sim_id,y=delay_s1,color=bipole,group=interaction(bipole,gp),shape=bipole))+
ylab('Delay S1 (ms)')
ggsave("deep_near_far_point_del_s1.png")
#S2 individual near-far
df_final4<-filter(df_final,variable=='delay_s2')
df_final5<-df_opening[,c(2,5,6)]
df_final5$sim_id<-as.factor(df_final5$sim_id)
p<-ggplot(data=df_final4) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
df_final5$gp<-c(1,2)[df_final3$sim_id %in% c(16,17)+1L]
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final5,aes(x=sim_id,y=delay_s2,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final5,aes(x=sim_id,y=delay_s2,color=bipole,group=interaction(bipole,gp)))+
  ylab('Delay S2 (ms)')
ggsave("deep_near_far_point_del_s2.png")
##S1 individual near-far entropy
df_opening<-filter(df,sim_id%in%c(14,15,16,17))
df_ent<-df_opening[,c(3,4,6)]
df_ent$sim_id<-as.factor(df_ent$sim_id)
df_final<-melt(df_ent)
df_final2<-filter(df_final,variable=='Entropy_s1')
df_final3<-df_opening[,c(3,5,6)]
df_final3$sim_id<-as.factor(df_final3$sim_id)
p<-ggplot(data=df_final2) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
df_final3$gp<-c(1,2)[df_final3$sim_id %in% c(16,17)+1L]
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final3,aes(x=sim_id,y=Entropy_s1,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final3,aes(x=sim_id,y=Entropy_s1,color=bipole,group=interaction(bipole,gp)))+
  ylab('Entropy S1')
ggsave("deep_near_far_point_ent_S1.png")
###S2 individual near-far Entropy
df_final4<-filter(df_final,variable=='Entropy_s2')
df_final5<-df_opening[,c(4,5,6)]
df_final5$sim_id<-as.factor(df_final3$sim_id)
df_final5$gp<-c(1,2)[df_final3$sim_id %in% c(16,17)+1L]
p<-ggplot(data=df_final4) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final5,aes(x=sim_id,y=Entropy_s2,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final5,aes(x=sim_id,y=Entropy_s2,color=bipole,group=interaction(bipole,gp)))+
  ylab('Entropy S2')
ggsave("deep_near_far_point_ent_S2.png")
########length-Width
##s1 delay width length
df_opening<-filter(df,sim_id%in%c(3,4,10,11,12,13,18,19))
df_delay<-df_opening[,c(1,2,6)]
df_delay$sim_id<-as.factor(df_delay$sim_id)
df_final<-melt(df_delay)
df_final2<-filter(df_final,variable=='delay_s1')
df_final3<-df_opening[,c(1,5,6)]
df_final3$sim_id<-as.factor(df_final3$sim_id)
df_final3$gp<-as.numeric(df_final3$sim_id)
df_final3$gp[df_final3$gp%in%c(1,2)]=1
df_final3$gp[df_final3$gp%in%c(3,4)]=2
df_final3$gp[df_final3$gp%in%c(5,6)]=3
df_final3$gp[df_final3$gp%in%c(7,8)]=4
p<-ggplot(data=df_final2) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final3,aes(x=sim_id,y=delay_s1,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final3,aes(x=sim_id,y=delay_s1,color=bipole,group=interaction(bipole,gp)))+
  ylab('Delay S1 (ms)')
ggsave("deep_length_width_delay_s1.png")
##s2 delay width length
df_final4<-filter(df_final,variable=='delay_s2')
df_final5<-df_opening[,c(2,5,6)]
df_final5$sim_id<-as.factor(df_final5$sim_id)
df_final5$gp<-as.numeric(df_final5$sim_id)
df_final5$gp[df_final5$gp%in%c(1,2)]=1
df_final5$gp[df_final5$gp%in%c(3,4)]=2
df_final5$gp[df_final5$gp%in%c(5,6)]=3
df_final5$gp[df_final5$gp%in%c(7,8)]=4
p<-ggplot(data=df_final4) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final5,aes(x=sim_id,y=delay_s2,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final5,aes(x=sim_id,y=delay_s2,color=bipole,group=interaction(bipole,gp)))+
  ylab('Delay S2 (ms)')
ggsave("deep_length_width_delay_s2.png")
##s1 entropy width length
df_opening<-filter(df,sim_id%in%c(3,4,10,11,12,13,18,19))
df_final2<-filter(df_final,variable=='Entropy_s1')
df_final3<-df_opening[,c(3,5,6)]
df_final3$sim_id<-as.factor(df_final3$sim_id)
df_final3$gp<-as.numeric(df_final3$sim_id)
df_final3$gp[df_final3$gp%in%c(1,2)]=1
df_final3$gp[df_final3$gp%in%c(3,4)]=2
df_final3$gp[df_final3$gp%in%c(5,6)]=3
df_final3$gp[df_final3$gp%in%c(7,8)]=4
p<-ggplot(data=df_final2) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final3,aes(x=sim_id,y=Entropy_s1,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final3,aes(x=sim_id,y=Entropy_s1,color=bipole,group=interaction(bipole,gp)))+
  ylab('Entropy S1')
ggsave("deep_length_width_ent_s1.png")
df_ent<-df_opening[,c(3,4,6)]
df_ent$sim_id<-as.factor(df_ent$sim_id)
df_final<-melt(df_ent)
df_final4<-filter(df_final,variable=='Entropy_s2')
df_final5<-df_opening[,c(4,5,6)]
df_final5$sim_id<-as.factor(df_final3$sim_id)
df_final5$gp<-as.numeric(df_final5$sim_id)
df_final5$gp[df_final5$gp%in%c(1,2)]=1
df_final5$gp[df_final5$gp%in%c(3,4)]=2
df_final5$gp[df_final5$gp%in%c(5,6)]=3
df_final5$gp[df_final5$gp%in%c(7,8)]=4
p<-ggplot(data=df_final4) 
#p+geom_boxplot(aes(x=sim_id, y=value, fill=variable))+ylab('Delay (ms)')
p+geom_point(aes(x=sim_id, y=value))+
  geom_point(data=df_final5,aes(x=sim_id,y=Entropy_s2,color=bipole,shape=bipole),size=4)+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  geom_line(data=df_final5,aes(x=sim_id,y=Entropy_s2,color=bipole,group=interaction(bipole,gp)))+
  ylab('Entropy S2')
ggsave("deep_length_width_ent_s2.png")
