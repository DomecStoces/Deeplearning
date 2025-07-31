library(data.table)
library(readxl)
library(writexl)

# Read .xlsx dataset with header
dataset <- read_xlsx("data_picipennis1.xlsx", col_names = TRUE)
dataset1 <- read_xlsx("grubb_picipennis1.xlsx", col_names = TRUE)
# Convert to data.table
dataset_dt <- as.data.table(dataset)
grubb_picipennis1 <- as.data.table(dataset1)
### FA protocol ###
# Drop NAs
library(dplyr)
colSums(is.na(grubb_picipennis))
clean_data <- na.omit(grubb_picipennis)
# Skewness and kurtosis for grubb_XXX dataset
# High skewness is between +-1 and high kurtosis is above +-3: >3(leptocurtic); <3 (platykurtic)
library(moments)
library(outliers)
grubbs.test(grubb_picipennis16$a1)
# Outliers Rivera, G.; Neely, C.M.D. Patterns of fluctuating asymmetry in the limbs of freshwater turtles: Are more functionally important limbs more symmetrical? Evolution 2020, 74, 660–670.
# points between 1.5×IQR and 3×IQR are considered as natural variation in developmental instability.
Q1 <- quantile(grubb_ophonus_a2_c$a2, 0.25)
Q3 <- quantile(grubb_ophonus_a2_c$a2, 0.75)
IQR_value <- IQR(grubb_ophonus_a2_c$a2)

lower_extreme <- Q1 - 3 * IQR_value
upper_extreme <- Q3 + 3 * IQR_value
lower_mild <- Q1 - 1.5 * IQR_value
upper_mild <- Q3 + 1.5 * IQR_value
grubb_ophonus_a2_c$outlier_type <- with(grubb_ophonus_a2_c, ifelse(
  a2 < lower_extreme | a2 > upper_extreme, "extreme",
  ifelse(a2 < lower_mild | a2 > upper_mild, "mild", "none")
))
table(grubb_ophonus_a2_c$outlier_type)

# Remove outliers
library(dplyr)
grubb_ophonus_a4 <- subset(grubb_ophonus_a4, outlier_type != "extreme")
IDs_mild_none <- unique(grubb_ophonus_a4$Group)
data_ophonus_a4_clean <- data_ophonus_a4 %>%
  filter(ID %in% IDs_mild_none)
str(data_ophonus_a4_clean)

skewness(grubb_ophonus_a2_c$a2)
kurtosis(grubb_ophonus_a2_c$a2)

# Dependency on ME in raw dataset in grubb_XXX; extract variance and correlation components
library(nlme)
lme_model <- lme(a2 ~ SIDE.a1, random = ~1|Group/SIDE.a1, data = grubb_ophonus_a2_c)
summary(lme_model)
VarCorr(lme_model)

aov1<-aov(a1 ~ SIDE.a1+Error(Group/SIDE.a1), data = grubb_picipennis1)
summary(aov1)

# A linear mixed-effects model (REML) was used to test for directional asymmetry (DA), fluctuating asymmetry (FA), and measurement error (ME). The fixed effect of side was not significant (p = 0.33), indicating no DA. The variance attributable to individual × side interaction (FA) was 0.000175, while residual variance (ME) was 0.00000052, yielding a %ME of 0.30%.
# Palmer, A. R., & Strobeck, C. (2003). Fluctuating asymmetry analyses revisited. In Polak, M. (Ed.), Developmental Instability: Causes and Consequences. Oxford University Press, pp. 279–319.

# Dependency |R-L| to Body size
data_ophonus_a2_c$a1_abs<- abs(data_ophonus_a2_c$`a2 R` - data_ophonus_a2_c$`a2 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = data_ophonus_a2_c)
summary(lm_abs)

# Normality of |R-L|; i) if significant -> error structure is non-additive; ii) test log transformation if significant -> non-multiplicative
shapiro.test(data_ophonus_a2_c$a1_abs)

data_picipennis19_clean$log_fa <- log(data_picipennis19_clean$a1_abs + 0.0001)
shapiro.test(data_picipennis19$log_fa)

# Homogenity of variance of FA index
library(car)
leveneTest(FA3~Treatment*Sex, data = data_ophonus_a2_c)

leveneTest(FA3~Treatment*Wing.m., data = data_ophonus_a2_c)

data_flav_a3_clean$Treatment <- factor(data_flav_a3_clean$Treatment)
data_flav_a3_clean$Sex <- factor(data_flav_a3_clean$Sex)
data_flav_a3_clean$Wing.m. <- factor(data_flav_a3_clean$Wing.m.)
# Dependency on Sex:Wing morphology
lm_sex <- lm(FA3 ~ Sex*Wing, data = data_ophonus_a2_c)
summary(lm_sex)
lm_treat <- lm(FA3 ~ Treatment*Wing, data = data_ophonus_a2_c)
summary(lm_treat)
lm_st <- lm(FA3 ~ Treatment*Sex, data = data_ophonus_a2_c)
summary(lm_st)
# When |R-L| are normal
library(lme4)
mod1<-lmer(FA3~Body.size+Treatment * Sex + Wing + (1 | ID)+(1|Trap),data= data_picipennis19)
summary(mod1)
library(lmerTest)
anova(mod1)
# When |R-L| is non-normal
library(glmmTMB)
mod_lognormal <- glmmTMB(FA3 ~ Body.size + Treatment*Wing +Sex + (1|ID)+(1|Locality.number),
                         data = data_picipennis_a2,
                         family = gaussian(link = "log"))
summary(mod_lognormal)
library(DHARMa)
simres <- simulateResiduals(mod_lognormal)
plot(simres)
library(car)
Anova(mod_lognormal, type = 3)

tiff('DHARMa_residual_HP_a2.tiff',units="in",width=7,height=6,bg="white",res=600)
plot(simres)
dev.off()

library(brms)
mod_3 <- brm(
  formula = FA3 ~ Body.size + Treatment * Wing + Sex + (1 | ID)+(1|Trap)+(1|Year),
  data = data_picipennis19,
  family = gaussian(link = "log"),
  chains = 4,
  cores = 4,
  iter = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 1234
)
summary(mod_3)

emm <- emmeans(mod_lognormal, ~ Treatment | Wing)
emm_df <- as.data.frame(emm)
ggplot(data_picipennis_a2, aes(x = Treatment, y = FA3,
                   color = Wing, group = Wing)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.4), width = 0.2) +
  geom_line(position = position_dodge(width = 0.4)) +
  labs(title = "Model-estimated FA3 across treatments and wing types",
       x = "Treatment", y = "Estimated FA3 (log scale)",
       color = "Wing morphology") +
  theme_bw()

library(ggplot2)
library(ggpubr)

# Treatment with Wing morphology
d<-ggplot(data_ophonus_a2_c, aes(x = Treatment, y = FA3, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Treatment), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Wing.m.) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    x = "Treatment",
    y = "Fluctuating asymmetry index",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw(base_size = 12) +
  stat_compare_means(
    method = "t.test",                            
    comparisons = list(c("Control", "Solar")),   
    label = "p.format",                          
    hide.ns = FALSE)
d
# Save the plot
tiff('Ophonus_cribricollis.tiff',units="in",width=7,height=6,bg="white",res=600)
d
dev.off()

# Treatment with Sex
de<-ggplot(data_flav_a3_clean, aes(x = Sex, y = FA3, fill = Sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Sex), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Treatment) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "Fluctuating asymmetry across treatments by Sex",
    x = "Treatment",
    y = "Fluctuating asymmetry",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw(base_size = 12) +
  stat_compare_means(
    method = "t.test",                            
    comparisons = list(c("F", "M")),   
    label = "p.format",                          
    hide.ns = FALSE)
de
# Save the plot
tiff('Harpalus_picipennis1.tiff',units="in",width=7,height=6,bg="white",res=600)
de
dev.off()

# Wing morphology across Sex
df<-ggplot(data_flav_a3_clean, aes(x = Sex, y = FA3, fill = Sex)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Sex), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Wing.m.) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "Fluctuating asymmetry across Wing morphology by Sex",
    x = "Sex",
    y = "Fluctuating asymmetry",
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



