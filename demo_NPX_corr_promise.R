library(ggplot2)
library(ggpmisc)

PROMISE_sets <- read_excel("Desktop/PROMISE/PROMISE_sets.xlsx")

#Scatter Plot 1 (All)
my.formula <- y ~ x
p <- ggplot(data = PROMISE_sets, aes(x = NPL, y = HOMA, colour = Group , alpha=0.95)) +
  geom_smooth(method = "gam", se=TRUE, formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p <-p + labs(x = 'NPL (NPX)', y = 'HOMA')+ theme(text=element_text(size=20))
p<-p +theme_minimal()
p <- p + xlim(0, 20)+ylim(0, 100)
p
ggsave("my_graph_abc.png", plot=p, height=4, width=4.5, units=c("in"), dpi=300)

my.formula <- y ~ x
p <- ggplot(data = PROMISE_sets, aes(x = SCIN, y = HOMA, colour = Group, alpha=0.95)) +
  geom_smooth(method = "gam", se=TRUE, formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p <-p + labs(x = 'NPL (NPX)', y = 'HOMA')+ theme(text=element_text(size=20))
p<-p +theme_minimal()
p <- p + xlim(0, 20)+ylim(0, 100)
p


Heatmap1 <- read_excel("Desktop/PROMISE/Heatmap1.xlsx")
df <- Heatmap1
A_heat <- ggplot(df, aes(x =factor(Protien, 
                                   level=c('KCNIP4',	'NUBP1',	'RAB37',	'REEP4',	'CCNE1',	'CEND1',	'CXCL8',	'ENOPH1',	'IKZF2',	'LYPLA2',	'SMS', 'ACADM',	'ADRA2A',	'ARHGAP30',	'CACNB1',	'CLEC2L',	'DLG4',	'GAGE2A',	'GNGT1',	'NENF',	'PRKAG3',	'RBP1',	'SCIN',	'TXNL1',	'NPL',	'SUOX',	'TPM3',	'UGDH')), 
                         y = factor(Year, level=c('Pre9_Log2FC',	'Pre6_Log2FC', 'Pre3_Log2FC')), fill =Log2FC)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#00AFBB", mid = "#FFFFFF", high = "#F8766D", na.value = 'white')
A_heat <- A_heat + theme_minimal()
A_heat <- A_heat + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=0),text=element_text(family="Helvetica",size=20, color="black"))
A_heat
ggsave("Pre369_heat_L2FC.png", plot=A_heat , height=2.5, width=9.5, units=c("in"), dpi=300)



df <- as.data.frame(PROMISE_sets)
df <- df %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = T), Age))
df <- df %>% mutate(Height = ifelse(is.na(Height), median(Height, na.rm = T), Height))
df <- df %>% mutate(Weight = ifelse(is.na(Weight), median(Weight, na.rm = T), Weight))
df <- df %>% mutate(Waist = ifelse(is.na(Waist), median(Waist, na.rm = T), Waist))
df <- df %>% mutate(Butt = ifelse(is.na(Butt), median(Butt, na.rm = T), Butt))
df <- df %>% mutate(BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI))
df <- df %>% mutate(Systolic = ifelse(is.na(Systolic), median(Systolic, na.rm = T), Systolic))
df <- df %>% mutate(Diastolic = ifelse(is.na(Diastolic), median(Diastolic, na.rm = T), Diastolic))
df <- df %>% mutate(HOMA = ifelse(is.na(HOMA), median(HOMA, na.rm = T), HOMA))
df <- df %>% mutate(ISI = ifelse(is.na(ISI), median(ISI, na.rm = T), ISI))
df <- df %>% mutate(IGIIR = ifelse(is.na(IGIIR), median(IGIIR, na.rm = T), IGIIR))
df <- df %>% mutate(GlucoseAUC = ifelse(is.na(GlucoseAUC), median(GlucoseAUC, na.rm = T), GlucoseAUC))
df <- df %>% mutate(InsulinAUC = ifelse(is.na(InsulinAUC), median(InsulinAUC, na.rm = T), InsulinAUC))
df <- df %>% mutate(ISSI2 = ifelse(is.na(ISSI2), median(ISSI2, na.rm = T), ISSI2))
df <- df %>% mutate(Glucose0 = ifelse(is.na(Glucose0), median(Glucose0, na.rm = T), Glucose0))
df <- df %>% mutate(Glucose30 = ifelse(is.na(Glucose30), median(Glucose30, na.rm = T), Glucose30))
df <- df %>% mutate(Glucose120 = ifelse(is.na(Glucose120), median(Glucose120, na.rm = T), Glucose120))
df <- df %>% mutate(Insulin0 = ifelse(is.na(Insulin0), median(Insulin0, na.rm = T), Insulin0))
df <- df %>% mutate(Insulin30 = ifelse(is.na(Insulin30), median(Insulin30, na.rm = T), Insulin30))
df <- df %>% mutate(Insulin120 = ifelse(is.na(Insulin120), median(Insulin120, na.rm = T), Insulin120))
df <- df %>% mutate(HOMA2_S = ifelse(is.na(HOMA2_S), median(HOMA2_S, na.rm = T), HOMA2_S))
df <- df %>% mutate(Chol = ifelse(is.na(Chol), median(Chol, na.rm = T), Chol))
df <- df %>% mutate(TAG = ifelse(is.na(TAG), median(TAG, na.rm = T), TAG))
df <- df %>% mutate(HDL = ifelse(is.na(HDL), median(HDL, na.rm = T), HDL))
df <- df %>% mutate(LDL = ifelse(is.na(LDL), median(LDL, na.rm = T), LDL))
df <- df %>% mutate(ALT = ifelse(is.na(ALT), median(ALT, na.rm = T), ALT))
x <- df[, c('Age',	'Height',	'Weight',	'Waist',	'Butt',	'BMI', 'Chol',	'TAG',	'HDL',	'LDL',	'ALT',	'HOMA', 'HOMA2_S',	'ISI',	'IGIIR',	'ISSI2','GlucoseAUC',	'Glucose0',	'Glucose30', 'Glucose120','InsulinAUC',	'Insulin0',	'Insulin30',	'Insulin120',	'Systolic',	'Diastolic')]
y <- df[, c('KCNIP4',	'NUBP1',	'RAB37',	'REEP4',	'CCNE1',	'CEND1',	'CXCL8',	'ENOPH1',	'IKZF2',	'LYPLA2',	'SMS', 'ACADM',	'ADRA2A',	'ARHGAP30',	'CACNB1',	'CLEC2L',	'DLG4',	'GAGE2A',	'GNGT1',	'NENF',	'PRKAG3',	'RBP1',	'SCIN',	'TXNL1',	'NPL',	'SUOX',	'TPM3',	'UGDH')]
cor_mat <- cor(x, y)
corrplot(cor_mat, method="color", col= colorRampPalette(c("#00AFBB","#FFFFFF", "#F8766D"))(10), tl.col = "black", addgrid.col = "black", tl.srt=90)




df <- as.data.frame(PROMISE_sets)
df <- df %>% mutate(Age = ifelse(is.na(Age), median(Age, na.rm = T), Age))
df <- df %>% mutate(Height = ifelse(is.na(Height), median(Height, na.rm = T), Height))
df <- df %>% mutate(Weight = ifelse(is.na(Weight), median(Weight, na.rm = T), Weight))
df <- df %>% mutate(Waist = ifelse(is.na(Waist), median(Waist, na.rm = T), Waist))
df <- df %>% mutate(Butt = ifelse(is.na(Butt), median(Butt, na.rm = T), Butt))
df <- df %>% mutate(BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI))
df <- df %>% mutate(Systolic = ifelse(is.na(Systolic), median(Systolic, na.rm = T), Systolic))
df <- df %>% mutate(Diastolic = ifelse(is.na(Diastolic), median(Diastolic, na.rm = T), Diastolic))
df <- df %>% mutate(HOMA = ifelse(is.na(HOMA), median(HOMA, na.rm = T), HOMA))
df <- df %>% mutate(ISI = ifelse(is.na(ISI), median(ISI, na.rm = T), ISI))
df <- df %>% mutate(IGIIR = ifelse(is.na(IGIIR), median(IGIIR, na.rm = T), IGIIR))
df <- df %>% mutate(GlucoseAUC = ifelse(is.na(GlucoseAUC), median(GlucoseAUC, na.rm = T), GlucoseAUC))
df <- df %>% mutate(InsulinAUC = ifelse(is.na(InsulinAUC), median(InsulinAUC, na.rm = T), InsulinAUC))
df <- df %>% mutate(ISSI2 = ifelse(is.na(ISSI2), median(ISSI2, na.rm = T), ISSI2))
df <- df %>% mutate(Glucose0 = ifelse(is.na(Glucose0), median(Glucose0, na.rm = T), Glucose0))
df <- df %>% mutate(Glucose30 = ifelse(is.na(Glucose30), median(Glucose30, na.rm = T), Glucose30))
df <- df %>% mutate(Glucose120 = ifelse(is.na(Glucose120), median(Glucose120, na.rm = T), Glucose120))
df <- df %>% mutate(Insulin0 = ifelse(is.na(Insulin0), median(Insulin0, na.rm = T), Insulin0))
df <- df %>% mutate(Insulin30 = ifelse(is.na(Insulin30), median(Insulin30, na.rm = T), Insulin30))
df <- df %>% mutate(Insulin120 = ifelse(is.na(Insulin120), median(Insulin120, na.rm = T), Insulin120))
df <- df %>% mutate(HOMA2_S = ifelse(is.na(HOMA2_S), median(HOMA2_S, na.rm = T), HOMA2_S))
df <- df %>% mutate(Chol = ifelse(is.na(Chol), median(Chol, na.rm = T), Chol))
df <- df %>% mutate(TAG = ifelse(is.na(TAG), median(TAG, na.rm = T), TAG))
df <- df %>% mutate(HDL = ifelse(is.na(HDL), median(HDL, na.rm = T), HDL))
df <- df %>% mutate(LDL = ifelse(is.na(LDL), median(LDL, na.rm = T), LDL))
df <- df %>% mutate(ALT = ifelse(is.na(ALT), median(ALT, na.rm = T), ALT))
x <- df[, c('Age',	'Height',	'Weight',	'Waist',	'Butt',	'BMI', 'Chol',	'TAG',	'HDL',	'LDL',	'ALT',	'HOMA', 'HOMA2_S',	'ISI',	'IGIIR',	'ISSI2','GlucoseAUC',	'Glucose0',	'Glucose30', 'Glucose120','InsulinAUC',	'Insulin0',	'Insulin30',	'Insulin120',	'Systolic',	'Diastolic')]
y <- df[, c('LYPLA2', 'KCNIP4', 'CCNE1', 'DAND5', 'SNCG', 'CXCL8', 'UGDH', 'TSC22D1', 'RIPK4', 'CACNA1C', 'MMP10', 'DOCK9', 'ENSA', 'CST7',
            'PCNA', 'SOWAHA', 'OSM', 'TTF2', 'CRYM', 'SMS', 'NPL',  'ADAMTS4', 'IKZF2', 'REST', 'GLP1R', 'GHRH', 'SNX18','LBP', 'CEND1', 'REEP4', 'SUOX', 'TRIM26', 'MTHFD2', 'SAT2', 'RICTOR', 'BLOC1S2', 'MUC16', 'ENOPH1', 'INSL4', 'NAA10', 'RAB37', 'TPM3',  'PAGR1', 'BCL7B', 'NEK7', 'NUBP1', 'PPM1B')]
cor_mat <- cor(x, y)
corrplot(cor_mat, method="color", col= colorRampPalette(c("#00AFBB","#FFFFFF", "#F8766D"))(10), tl.col = "black", addgrid.col = "black", tl.srt=90)
