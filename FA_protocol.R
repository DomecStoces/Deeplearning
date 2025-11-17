library(data.table)
library(readxl)
library(writexl)
library(dplyr)

# Read .xlsx dataset with header
data_picipennis_a2 <- read_xlsx("data_picipennis_a2.xlsx", col_names = TRUE)
data_ophonus_a2_c <- read_xlsx("data_ophonus_a2_c.xlsx", col_names = TRUE)
# Convert to data.table
dataset_dt <- as.data.table(dataset)
grubb_picipennis1 <- as.data.table(dataset1)
### FA protocol ###
# Drop NAs
colSums(is.na(grubb_picipennis))
clean_data <- na.omit(grubb_picipennis)
# Skewness and kurtosis for grubb_XXX dataset
# High skewness is between +-1 and high kurtosis is above +-3: >3(leptocurtic); <3 (platykurtic)
library(moments)
library(outliers)
grubbs.test(grubb_picipennis16$a1)
# Outliers Rivera, G.; Neely, C.M.D. Patterns of fluctuating asymmetry in the limbs of freshwater turtles: Are more functionally important limbs more symmetrical? Evolution 2020, 74, 660–670.
# points between 1.5×IQR and 3×IQR are considered as natural variation in developmental instability.
Q1 <- quantile(grubb_flav_a4$a4, 0.25)
Q3 <- quantile(grubb_flav_a4$a4, 0.75)
IQR_value <- IQR(grubb_flav_a4$a4)

lower_extreme <- Q1 - 3 * IQR_value
upper_extreme <- Q3 + 3 * IQR_value
lower_mild <- Q1 - 1.5 * IQR_value
upper_mild <- Q3 + 1.5 * IQR_value
grubb_flav_a4$outlier_type <- with(grubb_flav_a4, ifelse(
  a4 < lower_extreme | a4 > upper_extreme, "extreme",
  ifelse(a4 < lower_mild | a4 > upper_mild, "mild", "none")
))
table(grubb_flav_a4$outlier_type)

# Remove outliers
grubb_flav_a4 <- subset(grubb_flav_a4, outlier_type != "extreme")
IDs_mild_none <- unique(grubb_flav_a4$Group)
data_ophonus_a4_clean <- grubb_flav_a4 %>%
  filter(ID %in% IDs_mild_none)
str(data_ophonus_a4_clean)

skewness(grubb_flav_a4$a4)
kurtosis(grubb_flav_a4$a4)

# Dependency on ME/DA in raw dataset in grubb_XXX; extract variance and correlation components as per Van Dongen et al., 1999 (https://doi.org/10.1046/j.1420-9101.1999.00012.x)
grubb_ophonus_a2_c$Side_num <- ifelse(grubb_ophonus_a2_c$SIDE.a1 == "L", -1, 1)
library(nlme)
mod_fa <- lme(
  a2 ~ Side_num, 
  random = ~ Side_num | Group, 
  data = grubb_ophonus_a2_c,
  method = "REML")

summary(mod_fa)
VarCorr(mod_fa)

mod_fa_reduced <- lme(
  a2 ~ Side_num,
  random = ~ 1 | Group,
  data = grubb_ophonus_a2_c,
  method = "REML"
)
lrt <- anova(mod_fa, mod_fa_reduced)
print(lrt)
# Adjust p-value for variance component testing (50:50 mixture chi² distribution)
pval_adj <- lrt$"p-value"[2] / 2
cat("Adjusted p-value for FA variance component:", pval_adj, "\n")

# A linear mixed-effects model (REML) was used to test for directional asymmetry (DA), fluctuating asymmetry (FA), and measurement error (ME). The fixed effect of side was not significant (p = 0.33), indicating no DA. The variance attributable to individual × side interaction (FA) was 0.000175, while residual variance (ME) was 0.00000052, yielding a %ME of 0.30%.
# Palmer, A. R., & Strobeck, C. (2003). Fluctuating asymmetry analyses revisited. In Polak, M. (Ed.), Developmental Instability: Causes and Consequences. Oxford University Press, pp. 279–319.

# Dependency |R-L| to Body size
data_flav_a4$a1_abs<- abs(data_flav_a4$`a4 R` - data_flav_a4$`a4 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = data_flav_a4)
summary(lm_abs)

# Normality of |R-L|; i) if significant -> error structure is non-additive; ii) test log transformation if significant -> non-multiplicative
shapiro.test(data_flav_a4$a1_abs)

data_picipennis19_clean$log_fa <- log(data_picipennis19_clean$a1_abs + 0.0001)
shapiro.test(data_picipennis19$log_fa)

# Ordered factor of Wing
data_ophonus_a2_c$Wing <- factor(
  data_ophonus_a2_c$Wing,
  levels = c("B", "M"),
  labels = c("Brachypterous", "Macropterous"),
  ordered = TRUE
)
data_ophonus_a2_c$Dispersal.ability <- as.numeric(data_ophonus_a2_c$Wing)


data_picipennis_a2$Wing <- factor(
  data_picipennis_a2$Wing,
  levels = c("A","B","M"),
  labels = c("Apterous","Brachypterous", "Macropterous"),
  ordered = TRUE
)
data_picipennis_a2$Dispersal.ability <- as.numeric(data_picipennis_a2$Wing)

# Homogenity of variance of FA index
library(car)
leveneTest(FA3~Treatment*Sex, data = data_ophonus_a2_c)

leveneTest(FA3~Treatment*Wing, data = data_ophonus_a2_c)

data_ophonus_a2_c$Treatment <- factor(data_ophonus_a2_c$Treatment)
data_ophonus_a2_c$Sex <- factor(data_ophonus_a2_c$Sex)
data_flav_a3_clean$Wing <- factor(data_flav_a3_clean$Wing)
# Dependency on Sex:Wing morphology
lm_sex <- lm(FA3 ~ Sex*Wing, data = data_flav_a4)
summary(lm_sex)
lm_treat <- lm(FA3 ~ Treatment*Wing, data = data_flav_a4)
summary(lm_treat)
lm_st <- lm(FA3 ~ Treatment*Sex, data = data_flav_a4)
summary(lm_st)
# When |R-L| are normal
library(lme4)
mod1<-lmer(FA3~Body.size+Treatment * Sex + Wing + (1 | ID)+(1|Trap),data= data_flav_a4)
summary(mod1)
library(lmerTest)
anova(mod1)
# When |R-L| is non-normal
library(glmmTMB)
mod_lognormal <- glmmTMB(FA3 ~ Body.size + Treatment*Dispersal.ability + Sex +(1|Locality.number/ID),
                         data = data_picipennis_a2,
                         family = gaussian(link = "log"))
summary(mod_lognormal)
library(DHARMa)
simres <- simulateResiduals(mod_lognormal)
plot(simres)
library(car)
Anova(mod_lognormal, type = 3)

# Simple-slope estimates for treatment x dispersal ability
library(emmeans)
# On the link (log) scale:
tr_link <- emtrends(mod_lognormal, ~ Treatment, var = "Dispersal.ability")
summary(tr_link)       # slopes on log link
pairs(tr_link)         # Control vs. Solar park slope difference
# As multiplicative change per 1-step (ratio per step):
slopes <- as.data.frame(summary(tr_link))
slopes$ratio_per_step <- exp(slopes$Dispersal.ability.trend)
slopes
# On the response scale (absolute change in FA per step):
summary(emtrends(mod_lognormal, ~ Treatment, var = "Dispersal.ability", type = "response"))

tiff('DHARMa_residual_OC_a2.tiff',units="in",width=7,height=6,bg="white",res=600)
plot(simres)
dev.off()
# Plotting results of model estimations (predicted values) 
library(ggplot2)
library(ggpubr)
library(emmeans)

# Ophonus cribricollis: model-based predictions at Dispersal.ability = 1,2
x_grid <- seq(2, 3, length.out = 101)
emm_df <- as.data.frame(
  emmeans(mod_lognormal,
          ~ Treatment * Dispersal.ability,
          at = list(Dispersal.ability = x_grid),
          type = "response",
          weights = "proportional")
)

emm_df$Treatment <- factor(emm_df$Treatment,
                           levels = c("Control", "Solar park"),
                           labels = c("Extensive grassland", "Solar park"))

data_ophonus_a2_c$Treatment <- factor(data_ophonus_a2_c$Treatment,
                                      levels = c("Control", "Solar park"),
                                      labels = c("Extensive grassland", "Solar park"))

d<-ggplot(emm_df, aes(x = Dispersal.ability, y = response,
                      color = Treatment, group = Treatment)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = Treatment),
              alpha = 0.25) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = c(2, 3), labels = c("2 = Brachypterous", "3 = Macropterous")) +  
  labs(x = "Dispersal ability",
       y = "Fluctuating asymmetry index") +
  theme_classic(base_size = 15) +
  scale_color_manual(values = c("Extensive grassland" = "black", "Solar park" = "grey40")) +
  scale_fill_manual(values  = c("Extensive grassland" = "black", "Solar park" = "grey40")) +
  geom_jitter(data = data_ophonus_a2_c,
              aes(x = Dispersal.ability, y = FA3, color = Treatment),
              inherit.aes = FALSE, width = 0.1, alpha = 0.6, size = 2)
d
# Harpalus picipennis: model-based predictions at Dispersal.ability = 1,2,3
emm <- emmeans(
  mod_lognormal,
  ~ Treatment * Dispersal.ability, at = list(Dispersal.ability = 1:3),   
  type = "response"
)
emm_df <- as.data.frame(emm)

emm_df$Treatment <- factor(emm_df$Treatment,
                           levels = c("Control", "Solar park"),
                           labels = c("Extensive grassland", "Solar park"))

data_picipennis_a2$Treatment <- factor(data_picipennis_a2$Treatment,
                                      levels = c("Control", "Solar park"),
                                      labels = c("Extensive grassland", "Solar park"))

d<-ggplot(emm_df, aes(x = Dispersal.ability, y = response,
                   color = Treatment, group = Treatment)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = Treatment),
              alpha = 0.25, linewidth = 0.5) +
  geom_line(data = emm_df, 
            aes(x = Dispersal.ability, y = response, color = Treatment),linewidth = 1)+
  scale_x_continuous(breaks = c(1,2, 3), labels = c("1 = Apterous","2 = Brachypterous", "3 = Macropterous")) +
  labs(x = "Dispersal ability",
       y = "Fluctuating asymmetry index") +
  theme_bw(base_size = 15) + theme_classic(base_size = 15)+
  scale_color_manual(values = c("Extensive grassland" = "black", "Solar park" = "grey40")) +
  scale_fill_manual(values  = c("Extensive grassland" = "black", "Solar park" = "grey40")) +
  geom_jitter(data = data_picipennis_a2,
              aes(x = Dispersal.ability, y = FA3, color = Treatment), 
              inherit.aes = FALSE,
              width = 0.1, alpha = 0.6, size = 2.0)+coord_cartesian(ylim = c(0, 0.25))
d
# Save the plot
tiff('Harpalus_picipennis.tiff',units="in",width=8,height=6,bg="white",res=600)
d
dev.off()
# Boxplot options
#####
# Treatment with Wing morphology
d<-ggplot(emm_df, aes(x = Treatment, y = response, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = Treatment), width = 0.2, size = 1.5, alpha = 0.8) +
  facet_wrap(~ Wing) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    x = "Treatment",
    y = "Fluctuating asymmetry index",
    fill = "Treatment",
    color = "Treatment"
  ) +
  theme_bw(base_size = 15) +  stat_compare_means(
    method = "t.test",                            
    comparisons = list(c("Control", "Solar park")),   
    label = "p.format",                          
    hide.ns = FALSE)
d

# Save the plot
tiff('Harpalus_picipennis.tiff',units="in",width=7,height=6,bg="white",res=600)
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
    title = "Fluctuating asymmetry across treatments by sex",
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
  facet_wrap(~ Wing) +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_color_grey(start = 0.3, end = 0.8) +
  labs(
    title = "Fluctuating asymmetry across wing morphology by sex",
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



