library(gapminder)
library(tidyverse)

Pre3_fc_kegg_sigmet <- read_excel("Desktop/PROMISE/Pre3_fc.kegg.sigmet.xlsx")
Pre6_fc_kegg_sigmet <- read_excel("Desktop/PROMISE/Pre6_fc.kegg.sigmet.xlsx")
Pre9_fc_kegg_sigmet <- read_excel("Desktop/PROMISE/Pre9_fc.kegg.sigmet.xlsx")

df3 <- Pre3_fc_kegg_sigmet 
df6 <- Pre6_fc_kegg_sigmet 
df9 <- Pre9_fc_kegg_sigmet 

color <- ifelse(df3$stat.mean < 0, "#FC4E07", "#00AFBB")

A3 <- ggplot(data = df3,
       aes(x = reorder(HSA, stat.mean), y = stat.mean))+
       geom_bar(stat = "identity",
                show.legend = FALSE,
                aes(fill = color),  # Background color
                color = "black") + # Border color
       coord_flip()+
       labs(x = "Signature Enrichment", y = " ",
       title = " ",
       subtitles = " ")+
       geom_hline(yintercept = 0, color = 1, lwd = 1) +
       theme_minimal() +
       scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,2.5)) +
       theme(text=element_text(size=18))
A3
ggsave("Pre3_Pathway_Barplot.png", plot=A3, height=5, width=6, units=c("in"), dpi=300)


A6 <- ggplot(data = df6,
             aes(x = reorder(HSA, stat.mean), y = stat.mean))+
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = color),  # Background color
           color = "black") + # Border color
  coord_flip()+
  labs(x = "Signature Enrichment", y = " ",
       title = " ",
       subtitles = " ")+
  geom_hline(yintercept = 0, color = 1, lwd = 1) +
  theme_minimal() +
  scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,2.5)) +
  theme(text=element_text(size=18))
A6
ggsave("Pre6_Pathway_Barplot.png", plot=A6, height=5, width=6, units=c("in"), dpi=300)


A9 <- ggplot(data = df9,
             aes(x = reorder(HSA, stat.mean), y = stat.mean))+
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = color),  # Background color
           color = "black") + # Border color
  coord_flip()+
  labs(x = "Signature Enrichment", y = " ",
       title = " ",
       subtitles = " ")+
  geom_hline(yintercept = 0, color = 1, lwd = 1) +
  theme_minimal() +
  scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,2.5)) +
  theme(text=element_text(size=18))
A9
ggsave("Pre9_Pathway_Barplot.png", plot=A9, height=5, width=6, units=c("in"), dpi=300)

A3 <- ggplot(data = df3,
            aes(x = HSA, y = stat.mean))+
            geom_bar(stat = "identity")+
            coord_flip()
A3

df3 <- signature3
color <- ifelse(df3$stat.mean < 0, "#FC4E07", "#00AFBB")
sig3 <- ggplot(data = df3,
             aes(x = reorder(HSA, stat.mean), y = stat.mean))+
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = color),  # Background color
           color = "black") + # Border color
  coord_flip()+
  labs(x = "Signature Enrichment", y = " ",
       title = " ",
       subtitles = " ")+
  geom_hline(yintercept = 0, color = 1, lwd = 1) +
  theme_minimal() +
  scale_y_continuous(limits=c(-0.1, 0.1), breaks=seq(-0.1,0.1,0.1)) +
  theme(text=element_text(size=18))
sig3
ggsave("Sig3_Pathway_Barplot.png", plot=sig3, height=5, width=3.5, units=c("in"), dpi=300)

