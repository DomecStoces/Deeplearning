library(data.table)
library(readxl)
library(writexl)

# Read .xlsx dataset with header
dataset <- read_xlsx("data_picipennis.xlsx", col_names = TRUE)
grubb_picipennis3 <- read_xlsx("grubb_picipennis.xlsx", col_names = TRUE)
# Convert to data.table
dataset_dt <- as.data.table(dataset)
grubb_picipennis4 <- as.data.table(grubb_picipennis3)
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

skewness(clean_data$a1)
kurtosis(clean_data$a1)
grubbs.test(clean_data$a1)

# Dependency on ME in raw dataset in grubb_XXX
aov_Ophonus <- aov(a1 ~ SIDE.a1 + Error(SIDE.a1:Group), data = grubb_picipennis)
summary(aov_Ophonus)

# Dependency on Directional Asymmetry
t.test(dataset_dt$DA.a1)

# Normality of raw dataset in grubb_XXX
shapiro.test(grubb_picipennis$a1)

# Homogenity of variance of FA index
library(car)
leveneTest(FA.a1~Treatment*Sex, data = dataset_dt)

# Dependency |R-L| to Body size
dataset_dt$a1_abs <- abs(dataset_dt$`a1 R` - dataset_dt$`a1 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = dataset_dt)
summary(lm_abs)

# Dependency on Sex:Wing morphology
lm_sex <- lm(FA.a1 ~ Sex*Wing, data = dataset_dt)
summary(lm_sex)

library(lme4)
mod1<-lm(FA.a1~Treatment*Wing+Sex+Month,data= dataset_dt)
summary(mod1)
aov1<-Anova(mod1)
aov1
