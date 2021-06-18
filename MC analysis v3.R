dataG=df_Q3SAR
dig=2
r<-function(x){format(round(as.numeric(x),2),dig=2)}
r2<-function(x){format(round(as.numeric(x),4),dig=2)}
dataG$label=ifelse(dataG$type==2,paste0("   ",dataG$label),dataG$label)
dataG$fontface=ifelse(dataG$type==1, "italic","plain")
dataG$fontface=ifelse(dataG$type==3, "bold",dataG$fontface)
dataG$label_studyCI=ifelse(!is.na(dataG$h), paste0("[",r(dataG$l),";",r2(dataG$h),"]"),NA)
#dataG$label_studyCI=ifelse(!is.na(dataG$Pl), paste0("[",r(dataG$Pl),";",r(dataG$Ph),"]"),dataG$label_studyCI)
#dataG$label_studyCI=ifelse(!is.na(dataG$Rl), paste0("[",r(dataG$Rl),";",r(dataG$Rh),"]"),dataG$label_studyCI)
dataG$label_study=ifelse(!is.na(dataG$p),r(dataG$p),NA)
dataG$label_studyC=ifelse(!is.na(dataG$nc),paste0(dataG$event.c,"/",dataG$nc),NA)
dataG$label_studyE=ifelse(!is.na(dataG$ne),paste0(dataG$event.e,"/",dataG$ne),NA)

breaks=c(0.01,0.1,1,10)


dataG = dataG %>% mutate(Rp=ifelse(type==3, p, NA),
                         Rl=ifelse(type==3, l, NA),
                         Rh=ifelse(type==3, h, NA),
                         p=ifelse(type!=3, p, NA),
                         l=ifelse(type!=3, l, NA),
                         h=ifelse(type!=3, h, NA))

tmp=dataG[!is.na(dataG$Rl),]

p=ggplot()+ 
  geom_point(data=dataG,aes(y=dataG$line, x=log(dataG$p)),fill="gray", shape=22, color="black",size=4)+
  geom_errorbarh(data=dataG,aes(y=dataG$line, xmin=log(dataG$l), xmax=log(dataG$h)), height=0.3)+
  
  
  geom_text(aes(y=rep(-1,5), x=c(-18,-11,-9,5,6.5), label=c("Study","E/N","E/N (symptomatic)", "RR","95% CI")), hjust = 0, fontface = "bold")+
  
  geom_text(data=dataG,aes(y=dataG$line, x=5, label=label_study), hjust = 0, fontface=dataG$fontface)+
  geom_text(data=dataG,aes(y=dataG$line, x=6.5, label=label_studyCI), hjust = 0, fontface=dataG$fontface)+
  geom_text(data=dataG,aes(y=line, x=-10, label=label_studyE), hjust = 1)+
  geom_text(data=dataG,aes(y=line, x=-7.5, label=label_studyC), hjust = 1)+
  
  geom_text(data=dataG,aes(y=dataG$line, x=-18, label=dataG$label), hjust = 0, fontface=dataG$fontface)+
  #geom_rect(data=dataG,aes(xmin=dataG$Pl,xmax=dataG$Ph,ymin=dataG$line-0.1,ymax=dataG$line+0.1),color="black", fill="red")+
  
  theme_void() + 
  scale_y_reverse()+
  geom_segment(aes(y=max(dataG$line)+1, x=log(breaks[1]), xend=log(breaks[length(breaks)]), yend=max(dataG$line)+1))+
  geom_segment(aes(y=max(dataG$line)+1,yend=max(dataG$line)+1.3,x=log(breaks),xend=log(breaks)))+
  
  geom_segment(aes(y=1,yend=max(dataG$line)+1,x=log(1),xend=log(1)), linetype="dashed")+
  geom_text(aes(y=rep(max(dataG$line)+2,length(breaks)),x=log(breaks), label=breaks))+
  #geom_segment(data=dataG,aes(x=vlineloc, xend=vlineloc, y=0, yend=max(dataG$line)+1), linetype=3)+
  
  xlim(c(-18,8))+
  theme(legend.position = "none")
p

# add diamonds:
x=vector()
y=vector()
f=vector()
for(n in 1:nrow(tmp)){
  x=c(x,tmp$Rl[n],tmp$Rp[n],tmp$Rh[n],tmp$Rp[n])
  y=c(y,tmp$line[n],tmp$line[n]+0.3,tmp$line[n],tmp$line[n]-0.3)
  f=c(f,rep(n,4))
}
p=p+geom_polygon(aes(log(x),y,group=f))

#png(file="forest_ggplotQ3SAR_20200717.png", res=300, height=7.5, width=25, units="cm")
pdf(file="Figure3.pdf", height=9/3, width=25/3)
p
dev.off()

