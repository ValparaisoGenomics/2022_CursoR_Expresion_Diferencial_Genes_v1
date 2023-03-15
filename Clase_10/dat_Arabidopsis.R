tidy <- Arabidopsis %>% dplyr::select(Sample, Concentration, Ct_TG, Ct_HKG) %>% gather("Gen","Ct",3:4) %>% mutate(Factores = paste(Sample, Gen))

# ggplot(Arabidopsis, aes(x=Concentration, y=ΔCt , Group=Sample))+
#   geom_jitter(color="#A52A2A")+
#   labs(title="Boxplot ΔCt Arabidopsis", x="Sample", y="ΔCt")

ggplot(tidy, aes(x=Factores, y=Ct , fill=Factores))+
  geom_boxplot()+
  geom_jitter(color="#A52A2A")+
  labs(title="Boxplot ΔCt Arabidopsis", x="Sample", y="ΔCt")

tidyplot <- ggplot(data = tidy, aes(x = log(Concentration), y = Ct, color = Factores, shape = Factores)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(log[10]~(Concentration~microparticles~ml^-1))) +
  stat_smooth(method='lm',formula=y~x, se=F) +
  scale_shape_manual(values=c(1,2,3,4)) +
  scale_color_brewer(palette="Set1") + 
  theme(legend.position= c(0.2, 0.2)) +
  theme(panel.border=element_blank(), axis.line=element_line())
tidyplot