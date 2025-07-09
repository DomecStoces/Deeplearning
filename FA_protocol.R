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
grubbs.test(grubb_picipennis1$a1)
# Outliers Rivera, G.; Neely, C.M.D. Patterns of fluctuating asymmetry in the limbs of freshwater turtles: Are more functionally important limbs more symmetrical? Evolution 2020, 74, 660–670.
Q1 <- quantile(grubb_picipennis1$a1, 0.25)
Q3 <- quantile(grubb_picipennis1$a1, 0.75)
IQR_value <- IQR(grubb_picipennis1$a1)

lower_bound <- Q1 - 3 * IQR_value
upper_bound <- Q3 + 3 * IQR_value

outliers_iqr <- grubb_picipennis1$a1[grubb_picipennis1$a1 < lower_bound | grubb_picipennis1$a1 > upper_bound]
outliers_iqr

skewness(grubb_picipennis1$a1)
kurtosis(grubb_picipennis1$a1)

# Dependency on ME in raw dataset in grubb_XXX; extract variance and correlation components
library(nlme)
lme_model <- lme(a1 ~ SIDE.a1, random = ~1|Group/SIDE.a1, data = grubb_picipennis1)
summary(lme_model)
VarCorr(lme_model)

aov(a1 ~ SIDE.a1+Error(Group/SIDE.a1), data = grubb_picipennis1)

# A linear mixed-effects model (REML) was used to test for directional asymmetry (DA), fluctuating asymmetry (FA), and measurement error (ME). The fixed effect of side was not significant (p = 0.33), indicating no DA. The variance attributable to individual × side interaction (FA) was 0.000175, while residual variance (ME) was 0.00000052, yielding a %ME of 0.30%.
# Palmer, A. R., & Strobeck, C. (2003). Fluctuating asymmetry analyses revisited. In Polak, M. (Ed.), Developmental Instability: Causes and Consequences. Oxford University Press, pp. 279–319.

# Dependency on Directional Asymmetry
t.test(dataset_dt$DA.a1)
t.test(data_picipennis1$DA.a1)

# Normality of raw dataset in grubb_XXX
shapiro.test(grubb_picipennis1$a1)
# Normality of FA index
shapiro.test(data_picipennis1$FA.a1)

# Homogenity of variance of FA index
library(car)
leveneTest(FA.a1~Treatment*Sex, data = dataset_dt)
leveneTest(FA.a1~Treatment*Sex, data = data_picipennis1)

leveneTest(FA.a1~Treatment*Wing.m., data = dataset_dt)
leveneTest(FA.a1~Treatment*Wing.m., data = data_picipennis1)
# Dependency |R-L| to Body size
data_picipennis1$a1_abs <- abs(data_picipennis1$`a1 R` - data_picipennis1$`a1 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = data_picipennis1)
summary(lm_abs)

# Dependency on Sex:Wing morphology
lm_sex <- lm(FA.a1 ~ Sex*Wing, data = data_picipennis1)
summary(lm_sex)

library(lme4)
mod1<-lm(FA.a1~Body.size+Treatment*Wing+Sex+Month+(1|ID),data= data_picipennis)
summary(mod1)
aov1<-Anova(mod1)
aov1

library(ggplot2)
library(ggpubr)

# Treatment with Wing morphology
d<-ggplot(data_picipennis1, aes(x = Treatment, y = FA.a1, fill = Treatment)) +
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

# Treatment with Sex
de<-ggplot(data_picipennis1, aes(x = Treatment, y = FA.a1, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Treatment), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Sex) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "FA.a1 across Treatments by Sex",
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
de
# Save the plot
tiff('Harpalus_picipennis1.tiff',units="in",width=7,height=6,bg="white",res=600)
de
dev.off()

# Wing morphology across Sex
df<-ggplot(data_picipennis1, aes(x = Sex, y = FA.a1, fill = Sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Sex), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Wing.m.) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "FA.a1 across Wing morphology by Sex",
    x = "Sex",
    y = "Fluctuating Asymmetry (FA.a1)",
    fill = "Sex") +
  theme_bw(base_size = 12) +
  stat_compare_means(
    method = "t.test",                            
    comparisons = list(c("M", "F")),   
    label = "p.format",                          
    hide.ns = FALSE)
df
# Save the plot
tiff('Harpalus_picipennis2.tiff',units="in",width=7,height=6,bg="white",res=600)
de
dev.off()



