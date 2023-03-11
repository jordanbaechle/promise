library(tibble)
library(ggplot2)
library(ggpubr)

# Load Data: promise_meta (clinical & demographic), promise_npx (proteomics)
promise_meta <- read_excel("~/Desktop/promise_meta.xlsx")
promise_npx <- read_excel("~/Desktop/promise_npx.xlsx")

promise_meta$id_patient <- as.factor(promise_meta$id_patient)
promise_meta$DM <- as.factor(promise_meta$DM)
promise_meta$DM  <- relevel(promise_meta$DM , ref = "1")
promise_meta$id_patient <- factor(promise_meta$id_patient ,levels = c("1006",	"1131",	"1152",	"1177",	"1266",	"1300",	"1397",	"2042",	"2099",	"2127",	"2265",	"2321",
                                                                      "1075",	"1129",	"1150",	"1249",	"2070",	"2089",	"2186",	"2220",	"2249",	"2261",	"2280",	"2285"))

# Create timeline of clinical assessments of all patients
timeline_clinical_all <-
   ggplot(promise_meta, aes(pre_merged, id_patient)) + 
      geom_vline(xintercept = 0, size = 1, linetype = "solid", color="gray") +
      geom_line(aes(x=pre_merged, y=id_patient), linetype = "longdash") +
      geom_point(aes(x=pre_merged, y=id_patient, fill=DM), size=5, shape=21, stroke=1) +
      geom_point(aes(x=pre_npx, y=id_patient, fill=DM), size=2, shape=20) +
      geom_point(aes(x=pre_metab1, y=id_patient), size=7.4, shape=1) +
      geom_point(aes(x=pre_metab2, y=id_patient), size=9, shape=1) +
      geom_point(aes(x=pre_ogtt, y=id_patient), size=4, shape=4) +
      scale_x_continuous(breaks=seq(-9, 9, 3)) +
      ggtitle("Clinical Assessments of Healthy Controls") +
      xlab("Year of Study") + ylab("Patient ID") +
      theme_minimal() +
      theme(plot.title = element_text(face="bold"),
            axis.text.y = element_text(),
            axis.text = element_text(size = 12),
            legend.title = element_blank())
 timeline_clinical_all
 ggsave("timeline_clinical_all.png", plot=timeline_clinical_all, height=10, width=5, units=c("in"), dpi=300)
    
    
 
 # Create timeline of clinical assessments of all patients
 timeline_clinical_all_1 <-
   ggplot(promise_meta, aes(pre_merged_1, id_patient)) + 
   geom_line(aes(x=pre_merged1, y=id_patient), linetype = "longdash") +
   geom_point(aes(x=pre_merged1, y=id_patient, fill=DM), size=5, shape=21, stroke=1) +
   geom_point(aes(x=pre_npx1, y=id_patient, fill=DM), size=2, shape=20) +
   geom_point(aes(x=pre_metab1_1, y=id_patient), size=7.4, shape=1) +
   geom_point(aes(x=pre_metab2_1, y=id_patient), size=9, shape=1) +
   geom_point(aes(x=pre_ogtt1, y=id_patient), size=4, shape=4) +
   scale_x_continuous(breaks=seq(-9, 9, 3)) +
   ggtitle("Clinical Assessments of Healthy Controls") +
   xlab("Year of Study") + ylab("Patient ID") +
   theme_minimal() +
   theme(plot.title = element_text(face="bold"),
         axis.text.y = element_text(),
         axis.text = element_text(size = 12),
         legend.title = element_blank())
 timeline_clinical_all_1
 ggsave("timeline_clinical_all_1.png", plot=timeline_clinical_all_1, height=7.5, width=6.5, units=c("in"), dpi=300)
 
 

