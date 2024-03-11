require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern);library(stringr);library(dplyr)

#s=read.csv('gdl_100gt_all_branches_fixed.csv')
#s=read.csv('gdl_100gt_all_branches_vary_dl.csv')
#s=read.csv('gdl_100gt_all_branches_vary_ils.csv')
s=read.csv('gdl_all_branches_gdl_20.csv')
#s=read.csv('gdl_all_branches_gtrees.csv')
s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$Condition <- gsub('default', 'gdl_5e-10_1', s$Condition)
s$duprate = gsub(".*_(.+)_.*","\\1",s$Condition)
s$duprate <- factor(s$duprate, levels=c("1e-10", "5e-10", "1e-9"))
#levels(s$duprate) = list("1e-10", "5e-10", "1e-9")
s$duploss = gsub(".*_(.+)","\\1",s$Condition)
s$duploss = gsub('05', '0.5', s$duploss)

i=read.csv('gdl_all_branches_ils.csv')
i$Condition =  factor(i$Condition)#, levels=c("50bp", "100bp", "500bp", "truegt")) 
i$l.est = ifelse(i$l.est <=0, 1e-6, i$l.est)
i$log10err = log10(i$l.est / i$l.true )
i$abserr = abs(i$l.true - i$l.est)
i$se = (i$l.est - i$l.true)^2 

t=read.csv('gdl_gtrees_time.csv')
t$genes_num =  factor(t$genes_num)
t$Method=factor(t$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))

ggplot(aes(x=genes_num,y=time_s/60,color=Method,group=Method),
       data=t)+
  stat_summary(geom="line")+
  stat_summary()+
  scale_fill_brewer(palette = "Dark2",name="")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_y_continuous(trans="log10",name="Running time (minutes)" )+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",)+
  scale_x_discrete(name="Number of genes")+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-time.pdf",width=4,height=3)

ggplot(aes(x=Method,y=time_s/60,fill=Method),
       data=t[t$genes_num==10000,])+
  #geom_bar(stat = "identify")+
  stat_summary(fun.y = "mean", geom = "bar")+
  #stat_summary(geom="line")+
  #stat_summary()+
  scale_fill_brewer(palette = "Set2",name="")+
  scale_color_brewer(palette = "Set2",name="")+
  scale_y_continuous(name="Running time (minutes)" )+
  scale_x_discrete(name="")+labs(x = NULL)+
  theme_bw()+
  guides(color=guide_legend(nrow=3, byrow=TRUE),
         linetype=guide_legend(nrow=3, byrow=TRUE))+
  theme(legend.position = "none", legend.direction = "horizontal",
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank())
ggsave("gdl-time-bar.pdf",width=2.5,height=2.5)



ggplot(aes(y=mem,x=time,color=Method,group=Method,shape=genes_num),
       data=cbind(dcast(t,genes_num+Method~"time",fun.aggregate = mean,value.var = "time_s"), 
                  dcast(t,genes_num+Method~"mem",fun.aggregate = mean,value.var = "mem_gb"))[,c(1,2,3,6)])+
  geom_point()+
  geom_line()+
  scale_fill_brewer(palette = "Dark2",name="")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_y_continuous(trans="log10",name="Running time (minutes)" )+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0)
  )+scale_x_continuous(trans = "log10") +
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))

ggplot(aes(x=genes_num,y=mem_gb,color=Method,group=Method),
       data=t)+
  stat_summary(geom="line")+
  stat_summary()+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_y_continuous(trans="log10",name="Peak memory usage (GB)" )+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  #coord_cartesian(xlim=c(1,5),clip="off") +
  scale_x_discrete(label=function(x) gsub(" ","\n",x,fixed=T))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-memory.pdf",width=4,height =3)

ggplot(aes(x=Method,y=mem_gb,fill=Method),
       data=t[t$genes_num==10000,])+
  #geom_bar(stat = "identify")+
  stat_summary(fun.y = "mean", geom = "bar")+
  #stat_summary(geom="line")+
  #stat_summary()+
  scale_fill_brewer(palette = "Set2",name="")+
  scale_color_brewer(palette = "Set2",name="")+
  scale_y_continuous(name="Peak memory usage (GB)" )+
  scale_x_discrete(name="Method")+labs(x = NULL)+
  theme_bw()+
  guides(color=guide_legend(nrow=3, byrow=TRUE),
         linetype=guide_legend(nrow=3, byrow=TRUE))+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank())
ggsave("gdl-mem-bar.pdf",width=2.5,height=2.5)


ggplot(aes(x=as.factor(num_genes),y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+num_genes~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="Number of genes")+
  #facet_wrap(~num_genes,ncol=4)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-gtrees.pdf",width=7,height = 3.5)

ggplot(aes(x=as.factor(num_genes),y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+num_genes~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Absolute error")+
  scale_x_discrete(name="Number of genes")+
  #facet_wrap(~num_genes,ncol=4)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-abserr-gtrees.pdf",width=7,height = 3.5)

ggplot(aes(x=as.factor(num_genes),y=l.est-l.true,color=Method),
       data=dcast(data=s,Condition+Method+replicate+num_genes~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Absolute error")+
  scale_x_discrete(name="Number of genes")+
  #facet_wrap(~num_genes,ncol=4)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-bias-gtrees.pdf",width=7,height = 3.5)

ggplot(aes(x=as.factor(num_genes),y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #scale_y_continuous(trans="identity",name="Bias (est-true length)")+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  coord_cartesian(ylim=c(-0.05,0.05))
ggsave("gdl-bias_overall_vary_gtrees.pdf",width=7,height =3.5)

ggplot(aes(x=Condition,y=log10err,color=Method),
       data=dcast(data=i,Condition+Method+replicate+num_genes~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="ILS level")+
  #facet_wrap(~num_genes,ncol=4)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-ils.pdf",width=3,height = 3.5)


ggplot(aes(x=Condition,y=abserr,fill=Method),
       data=dcast(data=s,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2",name="")+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank())+
  theme_bw()
#coord_cartesian(ylim=c(0,0.2),xlim=c(1,5),clip="off")
ggsave("gdl-100estgt-error-perrep_vary_seqlen.pdf",width=7,height = 3.5)


ggplot(aes(x=Condition,y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()
ggsave("gdl-estgt-error-abserr-seqlen.pdf",width=9,height = 3.5)

ggplot(aes(x=Condition,y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="Sequence length")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-seqlen.pdf",width=3,height = 3.5)

ggplot(aes(x=duploss,y=log10err,color=Method),
       data=dcast(data=s,duprate+duploss+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="Loss/Dup ratio")+
  facet_wrap(~duprate,ncol=3)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-overall.pdf",width=7,height = 3.5)


ggplot(aes(x= Condition,y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  facet_wrap(~Branch.Type,ncol=2)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #scale_y_continuous(trans="identity",name="Bias (est-true length)")+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  coord_cartesian(ylim=c(-0.1,0.1))
ggsave("gdl-bias_overall_vary_ils.pdf",width=7,height =3.5)

h = read.csv('hgt_estgt_all_branches.csv')
head(h)
nrow(h)
unique(h$Method)
h$se = (h$l.est - h$l.true)^2 
#h$Method = factor(h$Method, levels=c("CASTLES" ,"CASTLES-II", "CASTLES-II(lambert)", "CASTLES-II(lambert+1/s)", "CASTLES-II(lambert+1/2s)"))#, "ERaBLE", 
                                     #"Patristic(AVG)+FastME", "Concat+RAxML"))
h$Method = factor(h$Method, levels=c("CASTLES-Pro" , "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)"))
h$Condition = factor(h$Condition, levels=c('0 (0)', '2e-09 (0.08)', '5e-09 (0.2)', '2e-08 (0.8)', '2e-07 (8)', '5e-07 (20)'))
h %>% drop_na(Branch.Type)

h$Condition =  factor(h$Condition) 
levels(h$Condition) = list('0\n(0)' = '0 (0)',
                           '2e-09\n(0.08)' = '2e-09 (0.08)', 
                           '5e-09\n(0.2)' = '5e-09 (0.2)',
                           '2e-08\n(0.8)'= '2e-08 (0.8)',
                           '2e-07\n(8)' = '2e-07 (8)',
                           '5e-07\n(20)' = '5e-07 (20)')



h$l.est = ifelse(h$l.est <=0, 1e-6, h$l.est)
h$log10err = log10(h$l.est / h$l.true )
h$abserr = abs(h$l.true - h$l.est)
h$se = (h$l.est - h$l.true)^2 

ggplot(aes(x= Condition,
           y=l.true-l.est,color=Method),
       data=h)+
  facet_wrap(~reorder(Branch.Type,-l.true),ncol=1)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("True" - "Estimated length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("hgt-bias.pdf",width=6,height =  10)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))
ggsave("hgt-error-perrep.pdf",width=7,height = 4.7)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~Branch.Type)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))
ggsave("hgt-error-perrep-broken.pdf",width=7,height = 4.7)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~Branch.Type)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))
ggsave("hgt-error-perrep.pdf",width=7,height = 4.7)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = c(.4,.8), legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))
ggsave("hgt-error-perrep-line.pdf",width=5,height = 3.2)


ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_x_discrete(name="HGT rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        #axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))
ggsave("hgt-error-perrep-line-main.pdf",width=3.5,height = 3.2)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="HGT rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  coord_cartesian(ylim=c(0.15,0.5))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("hgt-logerror-perrep-line-main.pdf",width=3.5,height = 3.2)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h,Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~Branch.Type)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("hgt-error-perrep-line-broken.pdf",width=8,height = 4.5)


ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))
ggsave("hgt-logerror-perrep.pdf",width=7,height = 4.7)


ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=h,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="HGT rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  coord_cartesian(ylim=c(0.15,0.5))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("hgt-logerror-perrep-line.pdf",width=5,height = 3.3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=h,Condition+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~Branch.Type)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="HGT rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("hgt-logerror-perrep-line-broken.pdf",width=7,height = 4)


m = read.csv('mvroot_all_branches_estgt.csv')
head(m)
nrow(m)
unique(m$Method)
m$se = (m$l.est - m$l.true)^2 
mvariants = m$Method %in% c("Naive")
m$outgroup = factor(grepl("outgroup.1", m$Condition))
m$ratevar =  unique(sub(".genes.*","",sub("outgroup.*.species.","",m$Condition)))
m$Method = factor(m$Method, levels=c("CASTLES-Pro" , "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)"))

summary(with(m[m$Method =="CASTLES" ,],l.est < 0))
m$l.est = ifelse(m$l.est <=0, 1e-6, m$l.est)
m$log10err = log10(m$l.est / m$l.true )
m$abserr = abs(m$l.true - m$l.est)
m$se = (m$l.est - m$l.true)^2

ggplot(aes(color=Method, y=log10err,x=cut(AD,c(0,25,35,50,60,70,85)/100)), na.rm = TRUE, 
       data=merge(
         dcast(data=m[!is.na(m$Method) & m$outgroup ==TRUE & !grepl("NA",m$Method),],
               outgroup+ratevar+Method+replicate+Branch.Type~'log10err' ,
               value.var = "log10err",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==TRUE,], outgroup+replicate+ratevar~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="ILS level")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Set1",name="")+#,labels=c("CASTLES","CASTLES-II","Concat (RAxML)"),values =c("#2E8B57" , "#6A5ACD" , "#D2691E"))+
  theme_bw()+
  theme(legend.position =  c(.4,.75), 
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))
ggsave("MV-logerr-ILS-talk_estgt.pdf",width=5,height = 3.5)

ggplot(aes(color=Method, y=abserr,x=cut(AD,4)),
       data=merge(
         dcast(data=m[!mvariants & m$outgroup ==FALSE,],
               outgroup+ratevar+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==FALSE,], outgroup+replicate+ratevar~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
  geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary(position = position_dodge(width=0.8))+
  #geom_boxplot(outlier.size = 0)+
  scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  coord_cartesian()+
  coord_cartesian(ylim=c(0,0.075))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("MV-abserror-perrep-ILS-bymethod.pdf",width=6.2*0.95,height = 3.5)

ggplot(aes(color=Method, y=log10err,x=cut(AD,4)),
       data=merge(
         dcast(data=m[!mvariants & m$outgroup ==FALSE,],
               outgroup+ratevar+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==FALSE,], outgroup+replicate+ratevar~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #facet_wrap(~Branch.Type,ncol=2)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
  geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary(position = position_dodge(width=0.8))+
  scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
  ggsave("MV-logerr-perrep-ILS.pdf",width=6.2*0.95,height = 4.3*0.95)
  
ggplot(aes(color=Method, y=l.est-l.true,x=cut(AD,4)), na.rm = TRUE,data=m[!mvariants,])+
    scale_y_continuous(trans="identity",name="Mean log10 error")+
    #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
    #facet_wrap(~Branch.Type,ncol=2)+
    scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
    stat_summary(position = position_dodge(width=0.9),size=0.8,fun.data = mean_sdl)+
    #geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
    #stat_summary()+
    #stat_summary(aes(group=Method),geom="line")+
    #geom_boxplot(outlier.size = 0)+
    scale_shape(name="",labels=c("With outgroup","No outgroup"))+
    scale_color_brewer(palette = "Dark2",name="")+#,labels=c("CASTLES","CASTLES-II","Concat (RAxML)"),values =c("#2E8B57" , "#6A5ACD" , "#D2691E"))+
    theme_bw()+
    theme(legend.position =  c(.5,.8), 
          legend.box.margin = margin(0), legend.margin = margin(0),
          axis.text.x = element_text(angle=0,size=11))
  ggsave("MV-bias-ILS-talk_estgt.pdf",width=6,height = 4)

# DISCO+QR datasets
s=read.csv('discoqr_all_branches_dup.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$highILS = factor(grepl("hILS", s$Condition))
s$highILS = ifelse(s$highILS==FALSE,"Low ILS","High ILS")
levels(s$Condition) <- gsub("_hILS", "", levels(s$Condition), fixed=TRUE)
s$duprate = gsub("20_gdl.*_(.+)_.*","\\1",s$Condition)
s$duprate <- factor(s$duprate, levels=c("1e-13", "1e-12", "1e-11", "1e-10", "5e-10", "1e-9"))

ggplot(aes(x=duprate,
           y=log10err,color=Method),
       data=dcast(data=s,duprate+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Duplication rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-dup-error-logerr.pdf",width=7,height = 3.1)

ggplot(aes(x=duprate,
           y=abserr,color=Method),
       data=dcast(data=s,duprate+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Duplication rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-error-perrep-line.pdf",width=7,height = 4)


ggplot(aes(x= duprate,
           y=l.true-l.est,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("True" - "Estimated length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-dup-bias.pdf",width=6,height =  10)


s=read.csv('gdl_all_branches_species.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$highILS = factor(grepl("hILS", s$Condition))
s$highILS = ifelse(s$highILS==FALSE,"Low ILS","High ILS")
levels(s$Condition) <- gsub("_hILS", "", levels(s$Condition), fixed=TRUE)
s$Condition =  factor(s$Condition, levels=c("20", "50", "100"))

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2,scales = "free_x")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of species")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-species-error-logerr.pdf",width=4.5,height = 3)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of species")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-species-error-perrep-line-main.pdf",width=4,height = 2.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-species-bias-main2.pdf",width=5.5,height =5)


ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-legend.pdf",width=8,height =5)



s=read.csv('discoqr_all_branches_seqlen.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$highILS = factor(grepl("hILS", s$Condition))
s$highILS = ifelse(s$highILS==FALSE,"Low ILS","High ILS")
levels(s$Condition) <- gsub("_hILS", "", levels(s$Condition), fixed=TRUE)
s$Condition =  factor(s$Condition, levels=c("50bp", "100bp", "500bp"))

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Sequence length")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-seqlen-error-logerr-main.pdf",width=4,height = 2.5)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Sequence length")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-seqlen-error-perrep-line-main.pdf",width=4,height = 2.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_x_discrete(name="Sequence length")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-seqlen-bias.pdf",width=7,height =  6)


s=read.csv('discoqr_all_branches_genetrees.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$highILS = factor(grepl("hILS", s$Condition))
s$highILS = ifelse(s$highILS==FALSE,"Low ILS","High ILS")
levels(s$Condition) <- gsub("_hILS", "", levels(s$Condition), fixed=TRUE)
s$Condition =  factor(s$Condition)

ggplot(aes(x= as.factor(num_genes),
           y=l.est-l.true,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_x_discrete(name="Number of genes")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-genetrees-bias.pdf",width=7,height =  6)

ggplot(aes(x=as.factor(num_genes),
           y=log10err,color=Method),
       data=dcast(data=s,num_genes+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of genes")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-genetrees-error-logerr-main.pdf",width=4,height = 2.5)

# ggsave("gdl-discoqr-genetrees-error-logerr.pdf",width=4.5,height = 3)
ggplot(aes(x=as.factor(num_genes),
           y=abserr,color=Method),
       data=dcast(data=s,num_genes+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of genes")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-genetrees-error-perrep-line-main.pdf",width=4,height = 2.5)


# ILS S100 dataset

s = read.csv('s100_all_branches.csv')
head(s)
unique(s$Method)
s$se = (s$l.est - s$l.true)^2 

names(s)=c("X"             ,      "Condition"         ,  "Method"        ,      "replicate"    ,       "Branch.Type"  ,
           "l.true"      ,        "l.est")
#s$Method=factor(s$Method,levels=c("CASTLES" , "CASTLES-Pro(taylor)", "CASTLES-Pro(lambert)", "CASTLES-Pro(lambert+1/s)", "CASTLES-Pro(lambert+1/2s)"))
s$Method = factor(s$Method, levels=c("CASTLES-Pro" , "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)"))
s$Condition =  factor(s$Condition) 
levels(s$Condition) = list("200bp" = "fasttree_genetrees_200_non", 
                           "400bp" = "fasttree_genetrees_400_non", 
                           "800bp" = "fasttree_genetrees_800_non",
                           "1600bp" = "fasttree_genetrees_1600_non",
                           "true gene trees" = "truegenetrees")

s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep.pdf",width=6,height = 3)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))
ggsave("S100-error-perrep-variants-line.pdf",width=5,height = 4.5)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  #stat_summary()+
  #stat_summary(aes(group=Method),geom="line")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep.pdf",width=6,height = 4.3)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  #facet_wrap(~reorder(Branch.Type,-l.true),ncol=2)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sd,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  #stat_summary()+
  #stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.05))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point.pdf",width=6,height =  3.5)

