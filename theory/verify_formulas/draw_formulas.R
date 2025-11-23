require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern)

pop_size <- 800
q_no_variation= read.csv('no_variation_formulas.csv')
q_only_hs= read.csv('only_hs_formulas.csv')
q_only_hl= read.csv('only_hl_formulas.csv')
q_hs_hl= read.csv('hs_hl_formulas.csv')
q_hs_hl_hg= read.csv('hs_hl_hg_formulas.csv')
q_hs_hl_hg_highr= read.csv('hs_hl_hg_highr_formulas.csv')
q_hs_hl_hg_highr_rates= read.csv('hs_hl_hg_high_rates_formulas.csv')
q = rbind(q_hs_hl_hg_highr_rates)
# q = rbind(q_no_variation, q_only_hs, q_only_hl, q_hs_hl, q_hs_hl_hg, q_hs_hl_hg_highr_rates)

q$log10err = log10(q$bl_true / q$bl_est )
q$abserr = abs(q$bl_true - q$bl_est)

# comment to see negative branches
q$bl_est = ifelse(q$bl_est <=0, 1e-6, q$bl_est)
q$bl_formula = ifelse(q$bl_formula <=0, 1e-6, q$bl_formula)

ggplot(aes(x=bl_true,y=bl_lambert,color=quartet_type,linetype), data=q_hs_hl_hg_highr[q_hs_hl_hg_highr$branch=='internal',])+
  facet_grid(~branch)+
  scale_x_continuous(trans="log10",name="True length")+
  scale_y_continuous(trans="log10",name="Estimated length")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-4.2,0.5),ylim=c(10^-4.2,0.5))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "bottom", panel.spacing.x = unit(5, "mm"))
ggsave("q_hs_hl_hg_highr_lambert.pdf",width=10,height = 2.8) # 'bl_lambert', 'bl_coal', 'bl_taylor0_1', 'bl_taylor0_2'


ggplot(aes(x=l_theory,y=l_simplified,color=quartet_type,linetype), data=q_hs_hl_hg_highr)+
  facet_grid(gene_type~branch)+
  scale_x_continuous(trans="log10",name="Exact formula")+
  scale_y_continuous(trans="log10",name="Simplified formula")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-4,0.9),ylim=c(10^-4,0.9))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "none", panel.spacing.x = unit(5, "mm"))
ggsave("hs_hl_hg_highr_expected_formulas.pdf",width=10,height = 3.6)

ggplot(aes(x=l_theory,y=l_empirical,color=quartet_type,linetype), data=q_hs_hl_hg_highr)+
  facet_grid(gene_type~branch)+
  scale_x_continuous(trans="log10",name="Exact formula")+
  scale_y_continuous(trans="log10",name="Estimated formula")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-4,1),ylim=c(10^-4,1))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "None", panel.spacing.x = unit(5, "mm"))
ggsave("hs_hl_hg_highr_est_thoery.pdf",width=10,height = 3.6)

ggplot(aes(x=bl_true,y=bl_formula,color=quartet_type,linetype), data=q[q$dataset=='hs_hl_hg_highr_rates',])+
  facet_grid(~branch)+
  scale_x_continuous(trans="log10",name="True length")+
  scale_y_continuous(trans="log10",name="Formula length")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-5.2,0.5),ylim=c(10^-5.2,0.5))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "none", panel.spacing.x = unit(5, "mm"))
ggsave("hs_hl_hg_highr_formulas_bls_rates.pdf",width=10,height = 2.2)

ggplot(aes(x=bl_true,y=bl_est,color=quartet_type,linetype), data=q[q$dataset=='hs_hl_hg_high_rates',])+
  facet_grid(~branch)+
  scale_x_continuous(trans="log10",name="True length")+
  scale_y_continuous(trans="log10",name="Estimated length")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-5.2,0.5),ylim=c(10^-5.2,0.5))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "bottom", panel.spacing.x = unit(5, "mm"))
ggsave("hs_hl_hg_highr_bls_est_rates_oct30.pdf",width=10,height = 2.8)

ggplot(aes(x=Condition,
           y=abserr,color=Branch.Type),
       data=dcast(data=q,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  scale_x_discrete(label=function(x) gsub(",","\n",x,fixed=T))+
  geom_boxplot(outlier.alpha = 0.3,width=0.86,outlier.size=1)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = c(.33,.88), legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(ylim=c(0,1),xlim=c(1,6),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("hs_hl_hg_highr_bls_est_rates-error-perrep.pdf",width=7*0.9,height = 4.5*0.9)

ggplot(aes(x=quartet_type,y=log10err,color=dataset),
       data=dcast(data=q,quartet_type+dataset+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  scale_x_discrete(label=function(x) gsub(",","\n",x,fixed=T))+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(ylim=c(0,4.5))
ggsave("hs_hl_hg_highr_bls_est_logabs.pdf",width=6.5,height = 5)




ggplot(aes(x=mu3*pop_size,y=l_empirical,color=quartet_type,linetype), data=q_hs_hl_hg[q_hs_hl_hg$gene_type=='non-matching'&q_hs_hl_hg$branch=='internal',])+
  scale_x_continuous(trans="log10",name="mu1")+
  scale_y_continuous(trans="log10",name="ln_i")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  #coord_cartesian(xlim=c(0.09,0.32),ylim=c(0.009,0.32))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "bottom", panel.spacing.x = unit(5, "mm"))
ggsave("mu1.pdf",width=5,height = 5)


ggplot(aes(x=t1), data=q_no_variation[q_no_variation$gene_type=='non-matching',])+
  scale_x_continuous(trans="log10",name="mu1")+
  #scale_y_continuous(trans="log10",name="ln_i")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  #coord_cartesian(xlim=c(0.009,0.32),ylim=c(0.009,0.32))+
  #geom_abline(color="grey30",linetype=2)+
  #geom_point(alpha=1,size=0.7)+
  geom_density()+
  theme_bw()+
  theme(legend.position = "bottom", panel.spacing.x = unit(5, "mm"))
ggsave("mu1.pdf",width=5,height = 5)

ggplot(aes(x=mu2*pop_size,y=l_empirical,color=quartet_type,linetype), data=q_no_variation[q_no_variation$gene_type=='non-matching'&q_no_variation$branch=='internal',])+
  scale_x_continuous(trans="log10",name="mu1")+
  scale_y_continuous(trans="log10",name="ln_i")+
  #stat_smooth(se=F,alpha=1,size=0.4,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  #coord_cartesian(xlim=c(0.009,0.32),ylim=c(0.009,0.32))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=1,size=0.7)+
  theme_bw()+
  theme(legend.position = "bottom", panel.spacing.x = unit(5, "mm"))
ggsave("mu1.pdf",width=5,height = 5)

#ggplot(, aes(x=d)) + geom_density()
x<-0:10
df<-data.frame(x)
ggplot(df,aes(x)) + stat_function(fun=function(x) exp(-3*x) - 3*exp(-x))


s=read.csv('s100_dating_all_branches.csv')
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

s3=read.csv('s100_dating_n3_all_branches.csv')
s3$Calibrations <- 3
s10=read.csv('s100_dating_n10_all_branches.csv')
s10$Calibrations <- 10
s <- rbind(s3,s10)
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



ggplot(data=dcast(data=s[!s$Condition=='true gene trees',],Condition+Method+replicate+Calibrations~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))), aes(x=Condition,y=log10err,fill=interaction(Method,Calibrations)))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  coord_cartesian(ylim=c(0,1))+
  guides(fill=guide_legend(title="Method, #calibrations"))
ggsave("S100-dating_calib_error-logabs-overall.pdf",width=7,height = 5)


ggplot(aes(x=l.true,y=l.est,color=Branch.Type,linetype),
       data=s[!s$Condition=='true gene trees',])+
  facet_grid(Method~Condition)+
  scale_x_continuous(trans="log10",name="True length")+
  scale_y_continuous(trans="log10",name="Estimated length")+
  scale_color_brewer(palette = "Dark2")+
  coord_cartesian(xlim=c(10^-4,0.9),ylim=c(10^-4,0.9))+
  geom_abline(color="grey30",linetype=2)+
  geom_point(alpha=0.1,size=0.5)+
  stat_smooth(se=F,alpha=1,size=0.7,method="glm",formula=y ~ poly(x, 2))+
  theme_bw()+
  theme(legend.position = "bottom")
ggsave("S100-dating-correlation.png",width=8,height =8)

ggplot(aes(x= Condition,y=l.est-l.true,color=interaction(Method,Calibrations), pattern=Calibrations), data=s[!s$Condition=='true gene trees',])+
  facet_wrap(~reorder(Branch.Type,-l.true),ncol=2)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank())+
  coord_cartesian(ylim=c(-2,2))
ggsave("S100-bias_dating_calib.pdf",width=9,height =  4)

ggplot(aes(x= Condition,y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank())
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.125))
ggsave("S100-bias_dating_n3_overall.pdf",width=3.5,height =  4)

ggplot(aes(x= Condition,y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression("Bias (est-true length)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()
  #theme(legend.position = "bottom", legend.direction = "horizontal",
  #      axis.title.x = element_blank())
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.125))
ggsave("S100-bias_dating_overall.pdf",width=7,height =  4)

ggplot(aes(x=Condition,y=abserr,fill=interaction(Method,Calibrations),pattern=Calibrations),
       data=dcast(data=s, Condition+Method+replicate+Branch.Type+Calibrations~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  guides(fill=guide_legend(title="Method, #calibrations"))
  #coord_cartesian(ylim=c(0,0.2),xlim=c(1,5),clip="off")
ggsave("S100-error-perrep_dating_calib.pdf",width=7,height = 5)


ggplot(aes(x=Condition,y=sqrt(se),fill=interaction(Method,Calibrations),pattern=Calibrations),
       data=dcast(data=s,Condition+Method+replicate+Branch.Type+Calibrations~'se' ,value.var = "se",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Root mean square error")+
  geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Paired")+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  guides(fill=guide_legend(title="Method, #calibrations"))
#coord_cartesian(ylim=c(0,0.2),xlim=c(1,5),clip="off")
ggsave("S100-rmse-perrep_dating_calib.pdf",width=7,height = 5)


bird=read.csv('TENT_CASTLES_BIRDS_2014_correlations.csv')


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


m = read.csv('mvroot_all_branches_estgt_dating.csv')
head(m)
nrow(m)
unique(m$Method)
m$se = (m$l.est - m$l.true)^2 
mvariants = m$Method %in% c("Naive")
names(m) =  c(names(s)[1:4],"AD", "GTEE", names(s)[5:7])
m$outgroup = factor(grepl("outgroup.0", m$Condition))
m$Method = factor(m$Method, levels=c('LSD+CASTLES', 'LSD+Concat+RAxML', 'wLogDate+CASTLES', 'wLogDate+Concat+RAxML'))
#m$ratevar =  unique(sub(".genes.*","",sub("outgroup.*.species.","",m$Condition)))

### Comment out to include negative branch lengths.
summary(with(m[m$Method =="CASTLES" ,],l.est < 0))
m$l.est = ifelse(m$l.est <=0, 1e-6, m$l.est)
m$log10err = log10(m$l.est / m$l.true )
m$abserr = abs(m$l.true - m$l.est)
m$se = (m$l.est - m$l.true)^2 

ggplot(aes(color=Method, y=log10err,x=cut(AD,4)),
       data=merge(
         dcast(data=m[!mvariants & m$outgroup ==TRUE,],
               outgroup+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==TRUE,], outgroup+replicate~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean log error")+
  #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
  geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary(position = position_dodge(width=0.8))+
  #geom_boxplot(outlier.size = 0)+
  scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Paired",name="")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  coord_cartesian(ylim=c(0.6,2.1))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("MV-logerr-perrep-ILS-bymethod_dating.pdf",width=6.2*0.95,height = 4.3*0.95)

ggplot(aes(color=Method, y=log10err,x=cut(AD,c(0,25,35,50,60,70,85)/100)),
       data=merge(
         dcast(data=m[!mvariants & m$outgroup ==TRUE & !grepl("Pat",m$Method),],
               outgroup+Method+replicate~'log10err' ,
               value.var = "log10err",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==TRUE,], outgroup+replicate~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_color_brewer(palette = "Paired",name="")+
  #geom_boxplot(outlier.size = 0)+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  theme_bw()+
  theme(legend.position =  c(.2,.8), 
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  coord_cartesian(ylim=c(0.8,2.1))
ggsave("MV-logerr-ILS-dating.pdf",width=6,height = 4)

ggplot(aes(x=Method, y=l.true-l.est,color=Method),
       data=m[!mvariants,])+
  scale_y_continuous(trans="identity",name=expression("True" - "Estimated length (bias)"))+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T))+
  stat_summary(position = position_dodge(width=0.9),size=0.8,fun.data = mean_sdl)+
  #geom_boxplot(outlier.size = 0)+
  #scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = 1,labels=c("High","Med","Low"),name="Clock deviation",direction = -1)+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  guides(color=guide_legend(nrow=1, byrow=TRUE),
         fill=guide_legend(nrow=1, byrow=TRUE))
ggsave("MV-bias_bymethod_pc3_dating.pdf",width=6.4,height = 5)

ggplot(aes(x=ratevar, y=l.true-l.est,color=Method,shape=outgroup),
       data=m[!mvariants,])+
  scale_y_continuous(trans="identity",name=expression("True" - "Estimated length (bias)"))+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T))+
  stat_summary(position = position_dodge(width=0.9),size=0.8,fun.data = mean_sdl)+
  #geom_boxplot(outlier.size = 0)+
  #scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  #scale_fill_manual(values=c("white","grey70"),name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_x_discrete(labels=c("High","Med","Low"),name="Clock deviation")+
  theme_bw()+
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  guides(color=guide_legend(nrow=3, byrow=TRUE),
         fill=guide_legend(nrow=3, byrow=TRUE))
ggsave("MV-bias_pc3.pdf",width=6.4,height = 5)
