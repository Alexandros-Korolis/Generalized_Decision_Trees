install.packages("VIM")
install.packages("naniar")
library(devtools)
install_github("cran/MissMech")
library(MissMech)
library(VIM)
library(naniar)

# Check Structure
str(data)
# Check Bare.Nuclei column
unique(data$Bare.Nuclei)
# Replace ? with NA
data$Bare.Nuclei = ifelse(data$Bare.Nuclei == "?", NA, data$Bare.Nuclei)
# Convert column to numeric type
data$Bare.Nuclei = as.numeric(data$Bare.Nuclei)
# Plot Missing Values
unobs = aggr(data, plot = FALSE)
plot(unobs, numbers = TRUE, prop = FALSE)
# Percent of missing values in Bare.Nuclei ( ~ 2% is missing data )
missing_percent = (nrow(subset(data, is.na(data$Bare.Nuclei), select = -c(Bare.Nuclei)))/nrow(data))*100
# Margin Plots 
# Let's observe if the distribution of a variable changes, whether Bare.Nuclei is missing or not
# Create new column (miss), 0 = not missing, 1 = missing
data$miss = ifelse(is.na(data$Bare.Nuclei),"1","0")
data$miss = as.factor(data$miss)
ggplot(data = data, mapping = aes(y = Clump.Thickness, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Uniformity.of.Cell.Size, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Uniformity.of.Cell.Shape, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Marginal.Adhesion, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Single.Epithelial.Cell.Size, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Bland.Chromatin, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Normal.Nucleoli, colour = miss))+geom_boxplot()
ggplot(data = data, mapping = aes(y = Mitoses, colour = miss))+geom_boxplot()
# Drop miss column
data = subset(data, select = -c(miss))
# Check assumption for Little's MCAR test
l_test = TestMCARNormality(subset(data, select = -c(Class)))
l_test
# Little's MCAR test 
# Ho: MCAR vs H1: Not MCAR 
mcar_test(data)






