require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern);library(stringr);library(dplyr)
library(tidyr);library(ggh4x)

# biological data analysis
s=read.csv('corrs.csv')
colnames(s) <- c('Bees', 'Birds', 'Mammals', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)', 'Bacterial\n(WoL)')
s = as.data.frame(sapply(s, as.numeric))
plot_s      <- pivot_longer(s, cols = 1:9)
nrow(melt(s))


#plot_s$value <- transform(plot_s, value = as.numeric(value))
ggplot(plot_s, aes(x=factor(name, level=c('Bees', 'Birds', 'Mammals', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)', 'Bacterial\n(WoL)')), log10(as.numeric(value)), fill = name)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_brewer(palette = "Spectral",name="",direction = -1)+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Log branch length ratio" )+
  geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  coord_cartesian(ylim=c(-2.3,2.3))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-violin.pdf",width=8,height=3)

ggplot(plot_s, aes(x=factor(name, level=c('Bees', 'Birds', 'Mammals', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)', 'Bacterial\n(WoL)')), as.numeric(value), fill = name)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_brewer(palette = "Spectral",name="",direction = -1)+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Branch length ratio" )+
  geom_hline(yintercept=1, linetype="dotted", color = "grey40")+
  geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  coord_cartesian(ylim=c(0,5))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-violin-bias.pdf",width=8,height=3)



#plot_s$value <- transform(plot_s, value = as.numeric(value))
ggplot(plot_s, aes(x=factor(name, level=c('Bees', 'Birds', 'Mammals', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)', 'Bacterial\n(WoL)')), log10(as.numeric(value)), fill = name)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_brewer(palette = "Spectral",name="",direction = -1)+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Log branch length ratio" )+
  geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(-4,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-violin-full.pdf",width=8,height=4)


ggplot(plot_s, aes(x=log10(as.numeric(value)), color =factor(name, level=c('Bees', 'Birds', 'Mammals', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)', 'Bacterial\n(WoL)')))) + 
  stat_ecdf()+
  scale_fill_brewer(palette = "Spectral",name="")+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Cumulative distribution" )+
  geom_vline(xintercept=0, linetype="dashed", color = "black")+
  #geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #geom_vline(xintercept=2.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=4.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = c(0.25, 0.55), legend.direction = "vertical",)+
  scale_x_continuous(name="Log branch length ratio")+
  coord_cartesian(xlim=c(-4,4))+
  guides(color=guide_legend(nrow=5, byrow=TRUE),
         linetype=guide_legend(nrow=5, byrow=TRUE))
ggsave("biological-ecdf.pdf",width=8,height=3)


s=read.csv('corrs_terminal_internal.csv')
s$Dataset = factor(s$Dataset)
levels(s$Dataset) = list('Birds'='Birds',
                           'Mammals'='Mammals',
                           'Bees'='Bees',
                           'Fungi'='Fungi',
                           'Plants(1kp)'='Plants(1kp)',
                           'Eudicots'='Eudicots',
                          'Bacterial\n(core)'='core',
                         'Bacterial\n(non-ribosomal)'='non-ribosomal',
                         'Bacterial\n(WoL)'='wol')


ggplot(data=subset(s, Dataset %in% c('Birds', 'Mammals', 'Bees', 'Fungi', 'Plants(1kp)', 'Eudicots', 'Bacterial\n(core)', 'Bacterial\n(non-ribosomal)')), aes(x=Dataset,y=l1.l2, fill = Branch.Type)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_brewer(palette = "Spectral",name="",direction = -1)+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Branch length ratio" )+
  geom_hline(yintercept=1, linetype="dotted", color = "grey40")+
  geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = c(0.15,0.8), legend.direction = "vertical",nrow=1)+
  scale_x_discrete(name="")+
  coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=1, byrow=TRUE),
         linetype=guide_legend(nrow=1, byrow=TRUE))
ggsave("biological-ratio.pdf",width=8,height=2.5)

s=read.csv('corrs_terminal_internal.csv')
s$Dataset = factor(s$Dataset)
levels(s$Dataset) = list('Birds'='Birds',
                         'Mammals'='Mammals',
                         'Bees'='Bees',
                         'Fungi'='Fungi',
                         'Plants(1kp)'='Plants(1kp)',
                         'Eudicots'='Eudicots',
                         'Bacterial\n(core)'='core',
                         'Bacterial\n(non-ribosomal)'='non-ribosomal',
                         'Bacterial\n(WoL)'='wol')

s = s %>% 
  mutate(discordance = case_when(Dataset == 'Birds' | Dataset == 'Mammals' | Dataset == 'Bees' ~ 'ILS', 
                          Dataset == 'Fungi' | Dataset == 'Plants(1kp)' | Dataset == 'Eudicots' ~ 'GDL', 
                          Dataset == 'Bacterial\n(core)' | Dataset == 'Bacterial\n(non-ribosomal)' | Dataset == 'Bacterial\n(WoL)' ~ 'HGT'))
s$discordance = factor(s$discordance, levels=c('ILS','GDL', 'HGT'))

ggplot(s, aes(x=Dataset,y=log10(as.numeric(l1.l2)), fill = Branch.Type)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_brewer(palette = "Spectral",name="",direction = -1)+
  scale_color_brewer(palette = "Spectral",name="")+
  scale_y_continuous(name="Log branch length ratio" )+
  facet_wrap(~discordance,scales="free",)+
  geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  #geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_classic()+
  theme(legend.position = c(0.8,0.77), legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-log-ratio.pdf",width=8,height=2.8)


s=read.csv('rtt_caml_castles_pro.csv')
s$Dataset = factor(s$Dataset)
levels(s$Dataset) = list('Birds'='Birds',
                         'Mammals'='Mammals',
                         'Bees'='Bees',
                         'Fungi'='Fungi',
                         'Plants(1kp)'='Plants(1kp)',
                         'Eudicots'='Eudicots',
                         'Bacterial\n(core)'='core',
                         'Bacterial\n(non-ribosomal)'='non-ribosomal',
                         'Bacterial\n(WoL)'='wol')
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "Concatenation"))
s = s %>% 
  mutate(discordance = case_when(Dataset == 'Birds' | Dataset == 'Mammals' | Dataset == 'Bees' ~ 'ILS', 
                                 Dataset == 'Fungi' | Dataset == 'Plants(1kp)' | Dataset == 'Eudicots' ~ 'GDL', 
                                 Dataset == 'Bacterial\n(core)' | Dataset == 'Bacterial\n(non-ribosomal)' | Dataset == 'Bacterial\n(WoL)' ~ 'HGT'))
s$discordance = factor(s$discordance, levels=c('ILS','GDL', 'HGT'))

ggplot(s, aes(x=Dataset,y=RTT, fill = Method)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_manual(values=c("#00AFBB", "#E7B800"), name="")+
  scale_color_manual(values=c("#00AFBB", "#E7B800"), name="")+
  facet_wrap(~discordance,scale='free')+
  scale_y_continuous(name="Root-to-tip distance" )+
  #geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  #geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_classic()+
  theme(legend.position = c(0.8,0.7), legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-rtt.pdf",width=8,height=2.8)

ggplot(s, aes(x=Dataset,y=RTT, fill = Method)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_fill_manual(values=c("#98b788","#d793dc"), name="")+
  scale_color_manual(values=c("#98b788","#d793dc"), name="")+
  facet_wrap(~discordance,scale='free')+
  scale_y_continuous(name="Root-to-tip distance" )+
  #geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  #geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_classic()+
  theme(legend.position = 'none', legend.direction = "vertical",)+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-rtt-ils.pdf",width=3,height=2.5)

ggplot(subset(s, Dataset %in% c('Fungi', 'Plants(1kp)', 'Eudicots')), aes(x=Dataset,y=RTT, fill = Method)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  #scale_fill_brewer(palette = "Set2",name="",direction = 1)+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#98b788","#d793dc"), name="")+
  scale_color_manual(values=c("#98b788","#d793dc"), name="")+
  scale_y_continuous(name="Root-to-tip distance" )+
  #geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  #geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "vertical",
        legend.box.margin = margin(0), legend.margin = margin(0),
        legend.text = element_text(angle=0,size=6))+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-rtt-gdl.pdf",width=3,height=2.5)

ggplot(subset(s, Dataset %in% c('Bacterial\n(core)', 'Bacterial\n(non-ribosomal)')), aes(x=Dataset,y=RTT, fill = Method)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  #scale_fill_brewer(palette = "Set2",name="",direction = 1)+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#98b788","#d793dc"), name="")+
  scale_color_manual(values=c("#98b788","#d793dc"), name="")+
  scale_y_continuous(name="Root-to-tip distance" )+
  #geom_hline(yintercept=0, linetype="dotted", color = "grey40")+
  #geom_vline(xintercept=3.5, linetype="dashed", color = "black")+
  #geom_vline(xintercept=6.5, linetype="dashed", color = "black")+
  theme_bw()+
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  scale_x_discrete(name="")+
  #coord_cartesian(ylim=c(0,4))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("biological-rtt-hgt.pdf",width=3.5,height=2.9)


s=read.csv('gdl_gtrees.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

ggplot(aes(x=as.factor(num_genes),y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+num_genes~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="Number of genes")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-gtrees.pdf",width=4,height = 3)

ggplot(aes(x=as.factor(num_genes),y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+num_genes~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Absolute error")+
  scale_x_discrete(name="Number of genes")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-abserr-gtrees.pdf",width=4,height = 3)

ggplot(aes(x=as.factor(num_genes),y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_x_discrete(name="Number of genes")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #scale_y_continuous(trans="identity",name="Bias (est-true length)")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))
ggsave("gdl-bias_overall_vary_gtrees.pdf",width=4,height =3)


ggplot(aes(x=as.factor(num_genes),y=l.est-l.true,color=Method), data=s[!s$Condition=='true gene trees',])+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  facet_wrap(~Branch.Type,ncol=3)+
  scale_x_discrete(name="Number of genes")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #scale_y_continuous(trans="identity",name="Bias (est-true length)")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))
ggsave("gdl-bias_overall_vary_gtrees_broken.pdf",width=7,height =3.5)


s=read.csv('gdl_dup_loss_ratio.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$Condition <- gsub('default', 'gdl_5e-10_1', s$Condition)
s$duprate = gsub(".*_(.+)_.*","\\1",s$Condition)
s$Method=factor(s$Method,levels=c("CASTLES-Pro", "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$duprate <- factor(s$duprate, levels=c("1e-10", "5e-10", "1e-9"))
#levels(s$duprate) = list("1e-10", "5e-10", "1e-9")
s$duploss = gsub(".*_(.+)","\\1",s$Condition)
s$duploss = gsub('05', '0.5', s$duploss)
s$ils = 'Low ILS, 100 taxa'

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
       data=dcast(data=s,duprate+duploss+Method+replicate+ils~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  scale_x_discrete(name="Loss/Dup ratio")+
  facet_nested(~ils+duprate)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_classic()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-overall-main.pdf",width=5,height = 2.5)

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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-overall.pdf",width=6,height = 2.5)


ggplot(aes(x=duploss,
           y=abserr,color=Method),
       data=dcast(data=s,duprate+duploss+Method+replicate+ils~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_nested(~ils+duprate)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_x_discrete(name="Loss/Dup ratio")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  # scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-loss-error-abs-main.pdf",width=5,height = 2.5) # width = 6

ggplot(aes(x=duploss,
           y=abserr,color=Method),
       data=dcast(data=s,duprate+duploss+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~duprate,ncol=3)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_x_discrete(name="Loss/Dup ratio")+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  # scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-loss-error-abs.pdf",width=6,height = 2.5) # width = 6


ggplot(aes(x=Branch.Type,
           y=l.est-l.true,color=Method),data=s)+facet_grid(duploss~duprate)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="")+
  #geom_boxplot(outlier.size = 0)+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
ggsave("gdl-dup-loss-bias.pdf",width=6.5,height =5.7)


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


# GDL time, memory

t=read.csv('gdl_species_1000_time.csv')
t$Method=factor(t$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO"))
mean(t[t$Method=='CASTLES-Pro',]$time_s/60)
mean(t[t$Method=='CASTLES-DISCO',]$time_s/60)
mean(t[t$Method=='CASTLES-Pro',]$mem_gb)
mean(t[t$Method=='CASTLES-DISCO',]$mem_gb)

t=read.csv('gdl_gtrees_time.csv')
t$genes_num =  factor(t$genes_num)
t$Method=factor(t$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))

# table for time
agg_t <- t[t$genes_num==10000,] %>% group_by(Method) %>% 
  summarise(mean_time=mean(time_s)/60,
            .groups = 'drop')
t2 <- agg_t %>% as.data.frame()
t2

# table for memory
agg_t <- t[t$genes_num==10000,] %>% group_by(Method) %>% 
  summarise(mean_mem=mean(mem_gb),
            .groups = 'drop')
t3 <- agg_t %>% as.data.frame()
t3

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
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  scale_x_discrete(name="Number of genes")+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-memory.pdf",width=4,height =3)

ggplot(aes(x=Method,y=mem_gb,fill=Method),
       data=t[t$genes_num==10000,])+
  #geom_bar(stat = "identify")+
  stat_summary(fun.y = "mean", geom = "bar")+
  #geom_text(nudge_y = 0.5) + 
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

t=read.csv('gdl_dup_loss_time.csv')
t$genes_num =  factor(t$genes_num)
t$Method=factor(t$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
t$condition <- gsub('default', 'gdl_5e-10_1', t$condition)
t$duprate = gsub(".*_(.+)_.*","\\1",t$condition)
t$duprate <- factor(t$duprate, levels=c("1e-10", "5e-10", "1e-9"))
t$duploss = gsub(".*_(.+)","\\1",t$condition)
t$duploss = gsub('05', '0.5', t$duploss)

mean(t[t$Method=='CASTLES-Pro' & t$duprate =='1e-9' & t$duploss=='0',]$time_s/60)

mean(t[t$Method=='CA-DISCO(RAxML)' & t$duprate =='1e-9' & t$duploss=='0',]$time_s/60)/60

ggplot(aes(x=as.factor(duprate),y=time_s/60,color=Method,group=Method), data=t)+
  stat_summary(geom="line")+
  stat_summary()+
  facet_wrap(~duploss)+
  scale_fill_brewer(palette = "Dark2",name="")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_y_continuous(trans="log10",name="Running time (minutes)" )+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",)+
  scale_x_discrete(name="Duplication rate")+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-time-duploss.pdf",width=5,height=2.5)

ggplot(aes(x=as.factor(duprate),y=mem_gb,color=Method,group=Method), data=t)+
  stat_summary(geom="line")+
  stat_summary()+
  facet_wrap(~duploss)+
  scale_fill_brewer(palette = "Dark2",name="")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_y_continuous(trans="log10",name="Peak memory usage (GB)" )+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",)+
  scale_x_discrete(name="Duplication rate")+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         linetype=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-mem-duploss.pdf",width=5,height=2.5)

i=read.csv('gdl_all_branches_ils.csv')
#i$Condition =  factor(i$Condition, levels=c('20% AD', '50% AD'))#, levels=c("50bp", "100bp", "500bp", "truegt")) 
i$Method=factor(i$Method,levels=c("CASTLES-Pro" , "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
i$l.est = ifelse(i$l.est <=0, 1e-6, i$l.est)
i$log10err = log10(i$l.est / i$l.true )
i$abserr = abs(i$l.true - i$l.est)
i$se = (i$l.est - i$l.true)^2 
i=na.omit(i)

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
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-logabs-ils.pdf",width=3,height = 3.5)

ggplot(aes(x=Condition,y=abserr,color=Method),
       data=dcast(data=i,Condition+Method+replicate+num_genes~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute errors")+
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
  theme(legend.position =  "bottom", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2,  byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-estgt-error-abserr-ils.pdf",width=3,height = 3.5)

ggplot(aes(x= Condition,y=l.est-l.true,color=Method), data=i)+
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
  theme_bw()
ggsave("gdl-bias_overall_vary_ils.pdf",width=6.5,height =3.5)


h = read.csv('hgt_estgt.csv')
head(h)
nrow(h)
unique(h$Method)
h$Method = factor(h$Method, levels=c("CASTLES-Pro" , "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)", "TCMM", "CASLTES-Pro+TCMM"))
h$Condition = factor(h$Condition, levels=c('0 (0)', '2e-09 (0.08)', '5e-09 (0.2)', '2e-08 (0.8)', '2e-07 (8)', '5e-07 (20)'))
levels(h$Condition) = list('0\n(0)' = '0 (0)',
                           '2e-09\n(0.08)' = '2e-09 (0.08)', 
                           '5e-09\n(0.2)' = '5e-09 (0.2)',
                           '2e-08\n(0.8)'= '2e-08 (0.8)',
                           '2e-07\n(8)' = '2e-07 (8)',
                           '5e-07\n(20)' = '5e-07 (20)')
h$branches = 'All branches'

#h$Condition = factor(h$Condition, levels=c('2e-08 (0.8)', '2e-07 (8)', '5e-07 (20)'))
#h %>% drop_na(Branch.Type)
#h %>% drop_na(Condition)
#h$Condition =  factor(h$Condition) 
#levels(h$Condition) = list('2e-08\n(0.8)'= '2e-08 (0.8)',
#                           '2e-07\n(8)' = '2e-07 (8)',
#                           '5e-07\n(20)' = '5e-07 (20)')


h$l.est = ifelse(h$l.est <=0, 1e-6, h$l.est)
h$log10err = log10(h$l.est / h$l.true )
h$abserr = abs(h$l.true - h$l.est)
h$se = (h$l.est - h$l.true)^2 
h$bias = h$l.est-h$l.true

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=h)+
  facet_wrap(~reorder(Branch.Type,-l.true))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("hgt-bias-broken.pdf",width=9,height =3.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=h)+
  facet_wrap(~reorder(Branch.Type,-l.true))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("hgt-bias-broken-line.pdf",width=9,height =3.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),data=h)+
  #facet_wrap(~reorder(Branch.Type,-l.true))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  facet_wrap(~branches)+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("hgt-bias.pdf",width=5,height =3.3)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),data=h)+
  #facet_wrap(~reorder(Branch.Type,-l.true))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("hgt-bias-mbe.pdf",width=3.5,height =3)



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
  theme(legend.position = 'none', legend.direction = "horizontal",
       # axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))
ggsave("hgt-error-perrep-line.pdf",width=4.5,height = 3.2)


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
  theme(legend.position = "bottom", legend.direction = "horizontal",
        # axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("legend-hgt.pdf",width=6,height = 3.2)


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
ggsave("hgt-logerror-perrep-line.pdf",width=4.5,height = 3.3)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=subset(h, !is.na(Condition)),Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_x_discrete(name="HGT rate")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = 'none', legend.direction = "horizontal",
        # axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=3, byrow=TRUE))
ggsave("hgt-error-perrep-line-mbe.pdf",width=3.5,height = 3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(h, !is.na(Condition)),Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
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
ggsave("hgt-logerror-perrep-line-main.pdf",width=2.5,height = 2.7)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(h, !is.na(Condition)),Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
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
ggsave("hgt-logerror-perrep-line-mbe.pdf",width=3.5,height = 3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(h, !is.na(Condition)),Condition+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~Branch.Type)+
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
ggsave("hgt-logerror-perrep-line-mbe-broken.pdf",width=5.5,height = 3)




m = read.csv('mvroot_estgt.csv')
head(m)
nrow(m)
unique(m$Method)
m$se = (m$l.est - m$l.true)^2 
mvariants = m$Method %in% c("Naive")
m$outgroup = factor(grepl("outgroup.1", m$Condition))
m$ratevar =  unique(sub(".genes.*","",sub("outgroup.*.species.","",m$Condition)))
m$Method = factor(m$Method, levels=c("CASTLES-Pro", "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)"))

summary(with(m[m$Method =="CASTLES" ,],l.est < 0))
m$l.est = ifelse(m$l.est <=0, 1e-6, m$l.est)
m$log10err = log10(m$l.est / m$l.true )
m$abserr = abs(m$l.true - m$l.est)
m$se = (m$l.est - m$l.true)^2

ggplot(aes(color=Method, y=log10err,x=cut(AD,c(0,25,50,75,100)/100)), 
       data=merge(
         dcast(data=m[!is.na(m$Method) & m$outgroup ==FALSE & !grepl("NA",m$Method),],
               outgroup+ratevar+Method+replicate+Branch.Type~'log10err' ,
               value.var = "log10err",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==FALSE,], outgroup+replicate+ratevar~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #facet_wrap(~outgroup,ncol=2,labeller = label_both)+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="ILS level")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.8,outlier.size = 0.8)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Dark2",name="")+#,labels=c("CASTLES","CASTLES-II","Concat (RAxML)"),values =c("#2E8B57" , "#6A5ACD" , "#D2691E"))+
  theme_bw()+
  theme(legend.position =  'none', 
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))
ggsave("MV-logerr-ILS_estgt.pdf",width=4.5,height = 3)

ggplot(aes(color=Method, y=abserr,x=cut(AD,c(0,25,50,75,100)/100)), #c(0,25,35,50,60,70,85)/100
       data=merge(
         dcast(data=m[!mvariants & m$outgroup ==FALSE,],
               outgroup+ratevar+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = function(x) mean(abs(x))),
         dcast(data=m[!mvariants  & m$outgroup ==FALSE,], outgroup+replicate+ratevar~'AD' ,value.var = "AD",fun.aggregate = mean)))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="ILS level")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_color_manual(values=c("black","grey50"),name="",labels=c("With outgroup","No outgroup"))+
  scale_shape(name="",labels=c("With outgroup","No outgroup"))+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position =  "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.text.x = element_text(angle=0,size=11))+
  guides(color=guide_legend(nrow=2, byrow=TRUE),
         fill=guide_legend(nrow=2, byrow=TRUE))
ggsave("MV-abserror-ILS-estgt.pdf",width=4.5,height = 3)
  
ggplot(aes(color=Method, y=l.est-l.true,x=cut(AD,4)), na.rm = TRUE,data=m[!mvariants & m$outgroup ==FALSE,])+
    scale_y_continuous(trans="identity",name="Mean log10 error")+
    geom_hline(color="grey50",linetype=1,yintercept = 0)+
    scale_x_discrete(label=function(x) gsub("+","\n",x,fixed=T),name="True gene tree discordance (ILS)")+
    stat_summary(position = position_dodge(width=0.9),size=0.8,fun.data = mean_sdl)+
    scale_shape(name="",labels=c("With outgroup","No outgroup"))+
    scale_color_brewer(palette = "Dark2",name="")+#,labels=c("CASTLES","CASTLES-II","Concat (RAxML)"),values =c("#2E8B57" , "#6A5ACD" , "#D2691E"))+
    theme_bw()+
    theme(legend.position =  'none', 
          legend.box.margin = margin(0), legend.margin = margin(0),
          axis.text.x = element_text(angle=0,size=11))
  ggsave("MV-bias-ILS-talk_estgt.pdf",width=5,height = 3.5)

# DISCO+QR datasets
s=read.csv('discoqr_duploss.csv')

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
  guides(color=guide_legend(nrow=1, byrow=TRUE))
ggsave("gdl-discoqr-dup-error-logerr.pdf",width=7,height = 3.1)

ggplot(aes(x=duprate,
           y=log10err,color=Method),
       data=dcast(data=subset(s, duprate %in% c("1e-10", "5e-10", "1e-9") & Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),duprate+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Duplication rate")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
ggsave("gdl-discoqr-dup-error-logerr-main.pdf",width=4,height = 2.5)

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

ggplot(aes(x=duprate,
           y=abserr,color=Method),
       data=dcast(data=subset(s, duprate %in% c("1e-10", "5e-10", "1e-9") & Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),duprate+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Duplication rate")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-error-perrep-line-main.pdf",width=4,height = 2.5)


ggplot(aes(x= duprate,
           y=l.est-l.true,color=Method),
       data=s)+
  facet_grid(Branch.Type~highILS)+
  #facet_wrap(~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_x_discrete(name="Duplication rate")+
  scale_y_continuous(name=expression("Est." - "True length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-dup-bias.pdf",width=8,height =5.5)

ggplot(aes(x= duprate,
           y=l.est-l.true,color=Method),
       data=subset(s, duprate %in% c("1e-10", "5e-10", "1e-9") & Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")))+
  facet_grid(~Branch.Type+highILS)+
  #facet_wrap(~highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_x_discrete(name="Duplication rate")+
  scale_y_continuous(name=expression("Est." - "True length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "right", legend.direction = "horizontal",
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=4, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-dup-bias-main.pdf",width=12,height =2.7)


# low ils, vary dup rate, num species
s=read.csv('gdl_dup_num_species.csv')

s$Condition =  factor(s$Condition)
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$duprate = gsub(".*_gdl.*_(.+)_.*","\\1",s$Condition)
s$duprate <- factor(s$duprate, levels=c("1e-10", "5e-10", "1e-9"))
s$species = factor(grepl("100_", s$Condition))
s$species = ifelse(s$species==FALSE,'20','100')


ggplot(aes(x=duprate,
           y=log10err,color=Method),
       data=dcast(data=s,duprate+Method+replicate+species~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~species,ncol=2,scales = "free_x")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-species-error-logerr.pdf",width=4,height = 2.5)

ggplot(aes(x=duprate,
           y=abserr,color=Method),
       data=dcast(data=s,duprate+Method+replicate+species~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~species,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_x_discrete(name="Number of species")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  # scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-dup-species-error-abs.pdf",width=4,height = 2.5)

ggplot(aes(x=duprate,
           y=l.est-l.true,color=Method),data=s)+facet_grid(~species+Branch.Type)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.size = 0)+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_classic()+
  theme(legend.position = "right", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=4, byrow=TRUE))
ggsave("gdl-dup-species-bias.pdf",width=8,height =2.5)


s=read.csv('gdl_ils_num_species.csv')

s$Condition =  factor(s$Condition)#, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

s$highILS = factor(grepl("hILS", s$Condition))
s$highILS = ifelse(s$highILS==FALSE,"Low ILS","High ILS")
levels(s$Condition) <- gsub("_hILS", "", levels(s$Condition), fixed=TRUE)
s$Condition =  factor(s$Condition, levels=c("20", "100"))

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2,scales = "free_x")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-ils-species-error-logerr.pdf",width=4,height = 2.5)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_x_discrete(name="Number of species")+
  scale_fill_brewer(palette = "Dark2")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-ils-species-error-abs.pdf",width=4,height = 2.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),data=s)+facet_grid(~highILS+Branch.Type)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "right", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=5, byrow=TRUE))
ggsave("gdl-ils-species-bias.pdf",width=8.5,height =2.5)


s=read.csv('discoqr_species_num.csv')

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
       data=dcast(data=subset(s, Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),Condition+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2,scales = "free_x")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of species")+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-species-error-logerr-main.pdf",width=4.5,height = 3)

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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-species-error-logerr.pdf",width=4,height = 2.5)

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
ggsave("gdl-species-error-perrep-line.pdf",width=4,height = 2.5)

pal_fill <- scales::brewer_pal(palette = "Dark2")(5)
names(pal_fill) <-  c(LETTERS[1],LETTERS[3:5])
pal_fill

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=subset(s, Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),Condition+Method+replicate+highILS~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Number of species")+
  #scale_fill_brewer(palette = "Dark2")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
 # scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-species-error-perrep-line-main.pdf",width=4,height = 2.5)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
       #data=subset(s, Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")))+
  facet_grid(~Branch.Type+highILS)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated - true length (bias)"))+
  stat_summary(fun.data = mean_sdl,position = position_dodge(width=0.75))+
  scale_x_discrete(name="Number of species")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "right", legend.direction = "horizontal",,
        legend.box.margin = margin(0), legend.margin = margin(0))+
  guides(color=guide_legend(nrow=5, byrow=TRUE))
#coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.06,0.2))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("gdl-discoqr-species-bias-main-1row.pdf",width=10,height =2.7)


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
ggsave("gdl-discoqr-species-bias.pdf",width=7,height =6)

s=read.csv('gdl_seqlen.csv')

s$Condition =  factor(s$Condition, levels=c("50bp", "100bp", "500bp"))
s$Method=factor(s$Method,levels=c("CASTLES-Pro" , "CASTLES-DISCO", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO"))
s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 
s$duprate = '5e-10, low ILS, 100 taxa'


ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(s, Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),Condition+Method+replicate+duprate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_grid(~duprate)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Sequence length")+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-seqlen-error-logerr.pdf",width=2.2,height = 2.5)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+duprate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_grid(~duprate)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Sequence length")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-seqlen-abserr.pdf",width=2.2,height = 2.5)

s=read.csv('discoqr_seqlen.csv')

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
       data=dcast(data=subset(s, Method %in% c("CASTLES-Pro", "CA-DISCO(RAxML)", "ERaBLE-DISCO", "FastME(AVG)-DISCO")),Condition+Method+replicate+highILS~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~highILS,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_x_discrete(name="Sequence length")+
  #scale_fill_brewer(palette = "Dark2")+
  #scale_color_brewer(palette = "Dark2",name="")+
  scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_classic()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-seqlen-error-logerr-main.pdf",width=4,height = 2.5)

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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-seqlen-error-logerr.pdf",width=4.5,height = 3)


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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-seqlen-error-perrep-line.pdf",width=4.5,height = 3)


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


s=read.csv('discoqr_genetrees.csv')

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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",,
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-discoqr-genetrees-error-logerr.pdf",width=4.5,height = 3)

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
  #scale_fill_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  #scale_color_manual(values=c("#1B9E77","#7570B3","#E7298A", "#66A61E"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("gdl-genetrees-error-perrep-line.pdf",width=4.5,height = 3)


# ILS S100 dataset

s = read.csv('s100.csv')
head(s)
unique(s$Method)
names(s)=c("X"             ,      "Condition"         ,  "Method"        ,      "replicate"    ,       "Branch.Type"  ,
           "l.true"      ,        "l.est", 'AD')
#s$Method=factor(s$Method,levels=c("CASTLES" , "CASTLES-Pro(taylor)", "CASTLES-Pro(lambert)", "CASTLES-Pro(lambert+1/s)", "CASTLES-Pro(lambert+1/2s)"))
s$Method = factor(s$Method, levels=c("CASTLES-Pro", "CASTLES", "Concat(RAxML)", "ERaBLE", "FastME(AVG)", "TCMM", "CASLTES-Pro+TCMM")) # 
s$Condition =  factor(s$Condition) 
levels(s$Condition) = list("200bp" = "fasttree_genetrees_200_non", 
                           "400bp" = "fasttree_genetrees_400_non", 
                           "800bp" = "fasttree_genetrees_800_non",
                           "1600bp" = "fasttree_genetrees_1600_non",
                           "true\ngene\ntrees" = "truegenetrees")
s$branches = 'All branches'
# CASTLES-pro: #1b9e77 - CASTLES-pro+TCMM: #a6761d

s$l.est = ifelse(s$l.est <=0, 1e-6, s$l.est)
s$log10err = log10(s$l.est / s$l.true )
s$abserr = abs(s$l.true - s$l.est)
s$se = (s$l.est - s$l.true)^2 

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')),Condition+Method+replicate+branches~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~branches,ncol=2)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep_castles_pro_tcmm.pdf",width=3.3,height = 2.7)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')),Condition+Method+replicate+branches~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~branches,ncol=2)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-logerror_castles_pro_tcmm.pdf",width=3.3,height = 2.7)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')),Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="")+
  facet_wrap(~Branch.Type,ncol=2)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep_castles_pro_tcmm_broken.pdf",width=5.2,height = 2.8)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')),Condition+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="")+
  facet_wrap(~Branch.Type,ncol=2)+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-logerror_castles_pro_tcmm_broken.pdf",width=5.2,height = 2.8)


ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')))+
  #facet_wrap(~reorder(Branch.Type,-l.true),ncol=2)+
  facet_wrap(~branches,ncol=2)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary(fun.data = mean_sd,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  #stat_summary()+
  #stat_summary(aes(group=Method),geom="line")+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.04))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point-castles_pro_tcmm.pdf",width=3.3,height =  2.7)


ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=subset(s, Method %in% c('CASTLES-Pro', 'CASLTES-Pro+TCMM')))+
  facet_wrap(~reorder(Branch.Type,-l.true),ncol=2)+
  #facet_wrap(~branches,ncol=2)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  #scale_x_continuous(trans="identity",name="True length")+
  scale_y_continuous(name=expression(""))+
  stat_summary(fun.data = mean_sd,position = position_dodge(width=0.75))+
  #geom_boxplot(outlier.size = 0)+
  #stat_summary()+
  #stat_summary(aes(group=Method),geom="line")+
  scale_fill_manual(values=c("#1B9E77","#a6761d"), name="")+
  scale_color_manual(values=c("#1B9E77","#a6761d"), name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.04))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point-castles_pro_tcmm_broken.pdf",width=5.2,height =  2.8)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.title = element_text(size = 12))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
#coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-legend-castles-pro-tcmm.pdf",width=10,height = 3)

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
ggsave("S100-error-perrep.pdf",width=6,height = 3.5)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.title = element_text(size = 14))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-small.pdf",width=2.8,height = 3)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+branches~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~branches,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-mbe.pdf",width=3.3,height = 2.7)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="")+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-mbe_broken.pdf",width=5.2,height = 2.8)

ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=subset(s),Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-mbe-broken.pdf",width=5,height = 4)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=abserr,color=Method),
       data=dcast(data=subset(s, Condition %in% c('1600bp')),Condition+Method+replicate+AD+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="")+
  stat_summary()+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  #coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-mbe-ILS-broken.pdf",width=5.2,height = 2.6)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=abserr,color=Method),
       data=dcast(data=subset(s, Condition %in% c('1600bp')),Condition+Method+replicate+AD+branches~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~branches,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  #coord_cartesian(xlim=c(1,5),clip="off")+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-error-perrep-mbe-ILS.pdf",width=3.3,height = 2.5)


ggplot(aes(x=Condition,
           y=abserr,color=Method),
       data=dcast(data=s,Condition+Method+replicate+Branch.Type~'abserr' ,value.var = "abserr",fun.aggregate = mean))+
  scale_y_continuous(trans="identity",name="Mean absolute error")+
  facet_wrap(~Branch.Type,ncol=2)+
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
ggsave("S100-error-perrep-broken.pdf",width=9,height = 3.5)

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
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep.pdf",width=6,height = 4.3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.title = element_text(size = 14))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
  #coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep-small.pdf",width=2.7,height = 3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+branches~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~branches,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep-mbe.pdf",width=3.3,height = 2.7)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="")+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep-mbe_broken.pdf",width=5.2,height = 2.8)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=log10err,color=Method),
       data=dcast(data=subset(s, Condition %in% c('1600bp')),Condition+Method+replicate+AD+branches~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  stat_summary()+
  facet_wrap(~branches,ncol=2)+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-logerror-perrep-mbe-ILS.pdf",width=3.3,height = 2.5)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=log10err,color=Method),
       data=dcast(data=subset(s, Condition %in% c('1600bp')),Condition+Method+replicate+AD+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="")+
  stat_summary()+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-logerror-perrep-mbe-ILS_broken.pdf",width=5.2,height = 2.6)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=log10err,color=Method),
       data=dcast(data=subset(s, Condition %in% c('1600bp')),Condition+Method+replicate+AD+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("S100-logerror-perrep-mbe-ILS_broken.pdf",width=4,height = 3)


ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.title = element_text(size = 12))+
  guides(color=guide_legend(nrow=1, byrow=TRUE))
  #coord_cartesian(ylim=c(0,1),clip="off")
  ggsave("S100-legend.pdf",width=10,height = 3)
  
  
  ggplot(aes(x=Condition,
             y=log10err,color=Method),
         data=dcast(data=s,Condition+Method+replicate+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
    scale_y_continuous(trans="identity",name="Mean log10 error")+
    facet_wrap(~Branch.Type,ncol=2)+
    #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
    #stat_summary(position = position_dodge(width=0.86))+
    #geom_boxplot(outlier.size = 0)+
    stat_summary()+
    stat_summary(aes(group=Method),geom="line")+
    #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
    #stat_summary(position = position_dodge(width=0.86))+
    scale_fill_brewer(palette = "Dark2")+
    scale_color_brewer(palette = "Dark2",name="")+
    theme_bw()+
    theme(legend.position = "none", legend.direction = "horizontal",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle=0))+
    guides(color=guide_legend(nrow=2, byrow=TRUE))
  #coord_cartesian(ylim=c(0,1),clip="off")
  ggsave("S100-logerror-perrep-mbe-broken.pdf",width=5,height = 4)
  
  ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+replicate~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  #geom_boxplot(outlier.size = 0)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  #geom_boxplot(outlier.alpha = 0.3,width=0.86)+
  #stat_summary(position = position_dodge(width=0.86))+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))
#coord_cartesian(ylim=c(0,1),clip="off")
ggsave("S100-logerror-perrep-mbe.pdf",width=3.5,height = 3)

ggplot(aes(x=Condition,
           y=log10err,color=Method),
       data=dcast(data=s,Condition+Method+Branch.Type~'log10err' ,value.var = "log10err",fun.aggregate = function(x) mean(abs(x))))+
  scale_y_continuous(trans="identity",name="Mean log10 error")+
  facet_wrap(~Branch.Type,ncol=2)+
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
ggsave("S100-logerror-perrep-broken.pdf",width=9,height = 4.3)

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
        legend.box.margin = margin(0), legend.margin = margin(0))
ggsave("S100-bias-point.pdf",width=6,height =  3.5)

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
        legend.box.margin = margin(0), legend.margin = margin(0),
        axis.title = element_text(size = 14))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.04))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point-small.pdf",width=2.8,height =  3)

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
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.04))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point-mbe.pdf",width=3.5,height =  3)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  facet_wrap(~branches,ncol=2)+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.003,0.018))
  #annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe.pdf",width=3.3,height =  2.7)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  facet_wrap(~Branch.Type,ncol=2)+
  scale_y_continuous(name=expression(""))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.004,0.03))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe_broken.pdf",width=5.2,height =  2.8)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=l.est-l.true,color=Method),
       data=subset(s, Condition %in% c('1600bp')))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  facet_wrap(~branches,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))
#coord_cartesian(ylim=c(-0.003,0.018))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe-ILS.pdf",width=3.3,height =  2.5)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=l.est-l.true,color=Method),
       data=subset(s, Condition %in% c('1600bp')))+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression(""))+
  facet_wrap(~Branch.Type,ncol=2)+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))
#coord_cartesian(ylim=c(-0.003,0.018))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe-ILS_broken.pdf",width=5.2,height =  2.6)

ggplot(aes(x=cut(AD,c(30,40,50,60)/100),
           y=l.est-l.true,color=Method),
       data=subset(s, Condition %in% c('1600bp')))+
  facet_wrap(~Branch.Type,ncol=2)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))
  #coord_cartesian(ylim=c(-0.003,0.018))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe-ILS-broken.pdf",width=4,height =  3)

ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  geom_hline(color="grey50",linetype=1,yintercept = 0)+
  facet_wrap(~Branch.Type,ncol=2)+
  scale_y_continuous(name=expression("Estimated" - "true length (bias)"))+
  stat_summary()+
  stat_summary(aes(group=Method),geom="line")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2",name="")+
  theme_bw()+
  theme(legend.position = "none", legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=0),
        legend.box.margin = margin(0), legend.margin = margin(0))+
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.003,0.03))
#annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-line-mbe-broken.pdf",width=5,height =  4)

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
  coord_cartesian(xlim=c(1,5),clip="off",ylim=c(-0.025,0.04))+
  annotate(geom="text",label="b)", x = -0.4, y = 0.145, size = 5)
ggsave("S100-bias-point-mbe.pdf",width=3.5,height =  3)


ggplot(aes(x= Condition,
           y=l.est-l.true,color=Method),
       data=s)+
  facet_wrap(~Branch.Type,ncol=2)+
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
ggsave("S100-bias-point-broken.pdf",width=9,height =  3.5)

