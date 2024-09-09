require(ggplot2);require(reshape2);require(scales);require(ggpubr);require(tidyr);require(ggpattern);library(stringr);library(dplyr)
library(tidyr)

# Root-to-Tip distance analysis
r2=read.csv('mrbayes_rtt.csv')
r1=read.csv('castles_pro_rtt.csv')
r <- rbind(r1, r2)

r$method =  factor(r$method) 
levels(r$method) = list("CASTLES-Pro" = "castles_pro", "MRBayes" = "mrbayes")

library(plyr)
mu <- ddply(r, "method", summarise, grp.mean=mean(rtt))
head(mu)

ggplot(r, aes(x=rtt, color=method, fill=method)) +
  geom_density(alpha=0.2)+
  theme_classic()+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2",name="")+
  scale_x_continuous(name="Root-to-tip distance" )+
  scale_y_continuous(name="Density" )+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=method),
             linetype="dashed")+
  geom_text(aes(color="CASTLES-Pro",y=2,x=1.116,label="avg.=1.116\nmax=1.381"),size=2.7)+
  geom_text(aes(color="MRBayes",y=3,x=1.014,label="avg.=1.014\nmax=1.176"),size=2.7)+
  theme(legend.position = c(.8,1), legend.direction = "vertical")
  #guides(color=guide_legend(nrow=2, byrow=TRUE))
ggsave("rtt_fungi.pdf",width=4,height=3.5)

corr=read.csv('fungi_corr.csv')


ggplot(aes(x=l1,y=l2,color=Branch.Type),data=corr)+
  geom_point(alpha=0.7)+
  scale_x_continuous(trans="log10",name="CASTLES-Pro length")+
  scale_y_continuous(trans="log10",name="CAML length")+
  stat_smooth(se=F,method="glm",formula=y ~ poly(x, 2))+
  scale_color_brewer(palette = "Dark2")+
  geom_abline(color="black",linetype=2)+
  coord_cartesian(xlim=c(10^-2,1.5),ylim=c(10^-2,1.5))+
  theme_classic()+
  theme(legend.position = c(.2,.8)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
ggsave("fungi_corr_r.pdf",width=4,height = 3.5)

