library("DESeq2")
library("EnhancedVolcano")

df <- data.frame(NPX)
cols <- sapply(df, is.numeric)
df[cols] <- 2^df[cols]
head(df)
write_xlsx(df, "NPX_linear.xlsx")

NPX_linear_integer <- sapply(NPX_linear[,-1], as.integer)
head(NPX_linear_integer)
NPX_linear_integer<- as.data.frame(NPX_linear_integer)
write_xlsx(NPX_linear_integer, "NPX_linear_integer6.xlsx")


#Volcano Plot - Year 0
countData <- data.frame(NPX_linear_integer0)
metaData <- data.frame(NPX_linear_dm0)
dds <- DESeqDataSetFromMatrix(countData=countData, colData=metaData, design=~Site, tidy = TRUE)
dds <- DESeq(dds)
res <- results(dds)
res <- results(dds, contrast=c("Site","DM", "Control"))
head(results(dds, tidy=TRUE))
A_Y0 <- EnhancedVolcano(res,
                    title = 'Year 0',
                    subtitle = 'Converters vs Control',
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue',
                    xlab = bquote(~Log[2]~ 'fold change'),
                    pCutoff = 10e-3,
                    FCcutoff = 2,
                    pointSize = 2.5,
                    labSize = 4.0,
                    boxedLabels = TRUE,
                    colAlpha = .5,
                    legendPosition = 'bottom',
                    legendLabSize = 12,
                    legendIconSize = 5.0,
                    drawConnectors = TRUE,
                    widthConnectors = 0.5, xlim = c(-5, 5),ylim = c(0, -log10(10e-8)))
A_Y0

ggsave("Y0_Volcano_DEP.png", plot=A_Y0, height=8.25, width=7, units=c("in"), dpi=300)

#Volcano Plot - Year 3
countData <- data.frame(NPX_linear_integer3)
metaData <- data.frame(NPX_linear_dm3)
dds <- DESeqDataSetFromMatrix(countData=countData, colData=metaData, design=~Site, tidy = TRUE)
dds <- DESeq(dds)
res <- results(dds)
res <- results(dds, contrast=c("Site","DM", "Control"))
head(results(dds, tidy=TRUE))
A_Y3<- EnhancedVolcano(res,
                    title = 'Year 3',
                    subtitle = 'Converters vs Control',
                    lab = rownames(res),
                    x = 'log2FoldChange',
                    y = 'pvalue',
                    xlab = bquote(~Log[2]~ 'fold change'),
                    pCutoff = 10e-3,
                    FCcutoff = 2,
                    pointSize = 2.5,
                    labSize = 4.0,
                    boxedLabels = TRUE,
                    colAlpha = .5,
                    legendPosition = 'bottom',
                    legendLabSize = 12,
                    legendIconSize = 5.0,
                    drawConnectors = TRUE,
                    widthConnectors = 0.5, xlim = c(-5, 5),ylim = c(0, -log10(10e-8)))
A_Y3

ggsave("Y3_Volcano_DEP.png", plot=A_Y3, height=8.25, width=7, units=c("in"), dpi=300)

#Volcano Plot - Year 6
countData <- data.frame(NPX_linear_integer6)
metaData <- data.frame(NPX_linear_dm6)
dds <- DESeqDataSetFromMatrix(countData=countData, colData=metaData, design=~Site, tidy = TRUE)
dds <- DESeq(dds)
res <- results(dds)
res <- results(dds, contrast=c("Site","DM", "Control"))
head(results(dds, tidy=TRUE))
A_Y6<- EnhancedVolcano(res,
                       title = 'Year 6',
                       subtitle = 'Converters vs Control',
                       lab = rownames(res),
                       x = 'log2FoldChange',
                       y = 'pvalue',
                       xlab = bquote(~Log[2]~ 'fold change'),
                       pCutoff = 10e-2,
                       FCcutoff = 2,
                       pointSize = 2.5,
                       labSize = 4.0,
                       boxedLabels = TRUE,
                       colAlpha = .5,
                       legendPosition = 'bottom',
                       legendLabSize = 12,
                       legendIconSize = 5.0,
                       drawConnectors = TRUE,
                       widthConnectors = 0.5, xlim = c(-5, 5),ylim = c(0, -log10(10e-8)))
A_Y6

ggsave("Y6_Volcano_DEP.png", plot=A_Y6, height=8.25, width=7, units=c("in"), dpi=300)

