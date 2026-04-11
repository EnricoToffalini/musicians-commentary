
#########################################

rm(list=ls())
library(readxl)
library(effsize)

df = data.frame(read_excel("Data/dat-clean.xlsx"))
names(df)

table(df$sex,df$group)

cohen.d(df$age ~ df$group)
tapply(df$age,df$group,mean)

cohen.d(df$years_of_education ~ df$group)
tapply(df$years_of_education,df$group,mean)

cohen.d(df$ses_nuclear_family ~ df$group)
tapply(df$ses_nuclear_family,df$group,mean)

vars = c("waisvocabulary", "raven", "nback", "digit_span", "spatial_span", "melody_span",
         "ses_nuclear_family")
for(v in vars) df[,v] = as.numeric(scale(df[,v]))
round(cor(df[,vars]),2)

fi = c("chisq","df","pvalue","rmsea","cfi")

save.image("Data/Prepared-data.RData")

#########################################

g1 = unique(df$group)[1]
g2 = unique(df$group)[2]

cor_g1 = cor(df[df$group == g1, vars], use = "pairwise.complete.obs")
cor_g2 = cor(df[df$group == g2, vars], use = "pairwise.complete.obs")

# empty character matrix
tab = matrix("", nrow = length(vars), ncol = length(vars))
rownames(tab) = vars
colnames(tab) = vars

# fill upper and lower triangles
for(i in seq_along(vars)) {
  for(j in seq_along(vars)) {
    if(i < j) tab[i, j] = sprintf("%.2f", cor_g1[i, j])   # upper = group 1
    if(i > j) tab[i, j] = sprintf("%.2f", cor_g2[i, j])   # lower = group 2
  }
}

tab = as.data.frame(tab, stringsAsFactors = FALSE)
tab

write.csv(tab,"Tables/TableCorr.csv")

#########################################
