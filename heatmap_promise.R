library(reshape2)
library(stats)
library(ggplot2)

df <- NPX_linear_integer0
dm_1 <- NPX_linear_dm0

fm_1 <- NPX_linear_integer3
dm_1 <- NPX_linear_dm3

fm_1 <- NPX_linear_integer6
dm_1 <- NPX_linear_dm6

NPX$id <- as.character(NPX$id)
df <- NPX
dat <- df[,2:2926]  # numerical columns
rownames(dat) <- as.data.frame( df[,1], drop=false)
row.order <- hclust(dist(dat))$order # clustering
col.order <- hclust(dist(t(dat)))$order
dat_new <- dat[row.order, col.order] # re-order matrix accoring to clustering

df_molten_dat <- melt(as.matrix(dat_new)) # reshape into dataframe
names(df_molten_dat)[c(1:2)] <- c("ID", "Protein")

A<- ggplot(data = df_molten_dat,
       aes(x = Protein, y = ID, fill = value)) +  geom_raster() +
       scale_fill_distiller(palette = "RdYlBu", trans = "log10") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
