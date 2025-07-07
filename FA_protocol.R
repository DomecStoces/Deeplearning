library(data.table)
library(readxl)
library(writexl)

# Read .xlsx dataset with header
dataset <- read_xlsx("data_Ophonus.xlsx", col_names = TRUE)

# Convert to data.table
dataset_dt <- as.data.table(dataset)

### FA protocol ###
# Skewness and kurtosis for grubb_XXX dataset
# High skewness is between +-1 and high kurtosis is above +-3: >3(leptocurtic); <3 (platykurtic)
library(moments)
skewness(grubb_Ophonus$a1)
kurtosis(grubb_Ophonus$a1)
library(outliers)
grubbs.test(grubb_Ophonus$a1)

# dependency |R-L| to elytra
dataset_dt$a1_abs <- abs(dataset_dt$`a1 R` - dataset_dt$`a1 L`) 
lm_abs <- lm(a1_abs ~ Body.size, data = dataset_dt)
summary(lm_abs)

aov_Ophonus <- aov(a1 ~ SIDE.a1 * Group + Error(Group:SIDE.a1), data = grubb_Ophonus)
summary(aov_Ophonus)

