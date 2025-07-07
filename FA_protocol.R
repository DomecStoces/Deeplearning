library(data.table)
library(readxl)
library(writexl)

# Read .xlsx dataset with header
dataset <- read_xlsx("data_picipennis.xlsx", col_names = TRUE)

# Convert to data.table
dataset_dt <- as.data.table(dataset)

### FA protocol ###
# Drop NAs
library(dplyr)
colSums(is.na(grubb_picipennis))
clean_data <- na.omit(grubb_picipennis)
# Skewness and kurtosis for grubb_XXX dataset
# High skewness is between +-1 and high kurtosis is above +-3: >3(leptocurtic); <3 (platykurtic)
library(moments)
library(outliers)
skewness(grubb_picipennis$a1)
kurtosis(grubb_picipennis$a1)
grubbs.test(grubb_picipennis$a1)

skewness(data_picipennis$a1)
kurtosis(clean_data$a1)
grubbs.test(clean_data$a1)

# Dependency on ME in raw dataset in grubb_XXX
aov_Ophonus <- aov(a1 ~ SIDE.a1 + Error(SIDE.a1:Group), data = grubb_picipennis)
summary(aov_Ophonus)

# Dependency on Directional Asymmetry
t.test(dataset_dt$DA.a1)
t.test(data_picipennis$DA.a1)

# Normality of raw dataset in grubb_XXX
shapiro.test(grubb_picipennis$a1)
# Normality of FA index
shapiro.test(data_picipennis$FA.a1)

# Homogenity of variance of FA index
library(car)
leveneTest(FA.a1~Treatment*Sex, data = dataset_dt)
leveneTest(FA.a1~Treatment*Sex, data = data_picipennis)

leveneTest(FA.a1~Treatment*Wing.m., data = dataset_dt)
leveneTest(FA.a1~Treatment*Wing.m., data = data_picipennis)
# Dependency |R-L| to Body size
data_picipennis$a1_abs <- abs(data_picipennis$`a1 R` - data_picipennis$`a1 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = data_picipennis)
summary(lm_abs)

# Dependency on Sex:Wing morphology
lm_sex <- lm(FA.a1 ~ Sex*Wing, data = data_picipennis)
summary(lm_sex)

library(lme4)
mod1<-lm(FA.a1~Body.size+Treatment*Wing+Sex+Month,data= data_picipennis)
summary(mod1)
aov1<-Anova(mod1)
aov1

library(ggplot2)
library(ggpubr)

d<-ggplot(data_picipennis, aes(x = Treatment, y = FA.a1, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Treatment), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Wing.m.) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "FA.a1 across Treatments by Wing morph",
    x = "Treatment",
    y = "Fluctuating Asymmetry (FA.a1)",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw(base_size = 12) +
  stat_compare_means(
    method = "wilcox.test",                            
    comparisons = list(c("Control", "Solar")),   
    label = "p.format",                          
    hide.ns = FALSE)
d
# Save the plot
tiff('Harpalus_picipennis.tiff',units="in",width=7,height=6,bg="white",res=600)
d
dev.off()
