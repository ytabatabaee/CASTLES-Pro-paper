require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern);library(stringr);library(dplyr)

#s=read.csv('gdl_100gt_all_branches_fixed.csv')
#s=read.csv('gdl_100gt_all_branches_vary_dl.csv')
#s=read.csv('gdl_100gt_all_branches_vary_gdl.csv')
s=read.csv('gdl_all_branches.csv')
s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp", "truegt")) 
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


ggplot(aes(x=Condition,y=log10err,color=Method),
       data=dcast(data=i,Condition+Method+replicate+num_genes~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="True gene tree discordance (ILS)")+
  facet_wrap(~num_genes,ncol=4)+
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
ggsave("gdl-estgt-error-logabs-ils.pdf",width=8,height = 3.5)


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
ggsave("gdl-estgt-error-abserr-overall_vs.pdf",width=9,height = 3.5)

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
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-seqlen.pdf",width=7,height = 3.5)

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
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #scale_y_continuous(trans="identity",name="Bias (est-true length)")+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()
#coord_cartesian(ylim=c(-0.2,0.2))
ggsave("gdl-bias_overall_vary_ils.pdf",width=7,height =3.5)

h = read.csv('hgt_estgt_all_branches.csv')
head(h)
nrow(h)
unique(h$Method)
h$se = (h$l.est - h$l.true)^2 
h$Method = factor(h$Method, levels=c("CASTLES" ,"CASTLES-II", "ERaBLE", 
                                     "Patristic(AVG)+FastME", "Concat+RAxML"))
h$Condition = factor(h$Condition, levels=c('0, 0', '2e-09, 0.08', '5e-09, 0.2', '2e-08, 0.8', '2e-07, 8', '5e-07, 20'))
h %>% drop_na(Branch.Type)


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
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))
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
  coord_cartesian(ylim=c(0.15,0.5))+
ggsave("hgt-logerror-perrep-line.pdf",width=6,height = 3.5)
