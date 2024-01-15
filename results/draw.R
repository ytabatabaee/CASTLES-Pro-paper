require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern)

#s=read.csv('gdl_100gt_all_branches_fixed.csv')
#s=read.csv('gdl_100gt_all_branches_vary_dl.csv')
s=read.csv('gdl_100gt_all_branches_vary_ils.csv')
s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp")) 
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 


ggplot(aes(x=Condition,y=abserr,fill=Method),
       data=dcast(data=s,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  #facet_wrap(~Branch.Type,ncol=2)+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2",name="")+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank())+
  theme_bw()
#coord_cartesian(ylim=c(0,0.2),xlim=c(1,5),clip="off")
ggsave("gdl-100estgt-error-perrep_vary_ils.pdf",width=7,height = 3.5)


ggplot(aes(x=Condition,y=log10err,fill=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()
ggsave("gdl-estgt-error-logabs-overall_vary_ils.pdf",width=7,height = 3.5)


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
h$Method = factor(h$Method, levels=c("CASTLES" ,"CASTLES-II", "ERaBLE", "Naive" , 
                                     "Patristic(AVG)+FastME" ,"Patristic(MIN)+FastME", "Concat+RAxML"))
h$Condition = factor(h$Condition, levels=c('0, 0', '2e-09, 0.08', '5e-09, 0.2', '2e-08, 0.8', '2e-07, 8', '5e-07, 20'))
h %>% drop_na(Branch.Type)


h$l.est = ifelse(h$l.est <=0, 1e-6, h$l.est)
h$log10err = log10(h$l.est / h$l.true )
h$abserr = abs(h$l.true - h$l.est)
h$se = (h$l.est - h$l.true)^2 


ggplot(aes(x= Condition,
           y=l.true-l.est,color=Method),
       data=h[!variants,])+
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
ggsave("hgt-bias_pc3.pdf",width=6,height =  10)


ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=h[!variants,],Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
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
ggsave("hgt-error-perrep_pc3.pdf",width=7,height = 4.7)


ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=h[!variants,],Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))
ggsave("hgt-logerror-perrep_pc3.pdf",width=7,height = 4.7)



