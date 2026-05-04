
#########################################

library(lavaan)
load("Data/Prepared-data.RData")

#########################################

# INITIAL CHECK OF CORRELATED RESIDUALS

#########################################

model_alt_mv_checkres = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_checkres = sem(model_alt_mv_checkres, data = df, cluster="country")
fitMeasures(fit_mv_checkres, fi)
modificationindices(fit_mv_checkres, sort.=T)[1:4,]

model_alt_mv_checkres = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
digit_span ~~    melody_span

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_checkres = sem(model_alt_mv_checkres, data = df, cluster="country")
fitMeasures(fit_mv_checkres, fi)
modificationindices(fit_mv_checkres, sort.=T)[1:4,]

model_alt_mv_checkres = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
spatial_span ~~     digit_span
digit_span ~~    melody_span

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_checkres = sem(model_alt_mv_checkres, data = df, cluster="country")
fitMeasures(fit_mv_checkres, fi)
modificationindices(fit_mv_checkres, sort.=T)[1:4,]

model_alt_mv_checkres = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
waisvocabulary ~~ spatial_span
spatial_span ~~     digit_span
digit_span ~~    melody_span

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_checkres = sem(model_alt_mv_checkres, data = df, cluster="country")
fitMeasures(fit_mv_checkres, fi)
modificationindices(fit_mv_checkres, sort.=T)[1:4,]

#########################################

# FITTING MULTILEVEL MODELS

#########################################

model_alt_mv_0 = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
waisvocabulary ~~ spatial_span
digit_span ~~ spatial_span 
digit_span ~~ melody_span

g ~ group + ses_nuclear_family

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_0 = sem(model_alt_mv_0, data = df, cluster="country")
fitMeasures(fit_mv_0, fi)

model_alt_mv_1 = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
waisvocabulary ~~ spatial_span
digit_span ~~ spatial_span 
digit_span ~~ melody_span

waisvocabulary ~ group + ses_nuclear_family
raven ~ group + ses_nuclear_family
digit_span ~ group + ses_nuclear_family
spatial_span ~ group + ses_nuclear_family
nback ~ group + ses_nuclear_family
melody_span ~ group + ses_nuclear_family

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_1 = sem(model_alt_mv_1, data = df, cluster="country")
fitMeasures(fit_mv_1, fi)

summary(fit_mv_1)
BIC(fit_mv_0, fit_mv_1)

model_alt_mv_2 = "
level: 1
g ~~ 1*g
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
waisvocabulary ~~ spatial_span
digit_span ~~ spatial_span 
digit_span ~~ melody_span

g ~ group + ses_nuclear_family
waisvocabulary ~ group + ses_nuclear_family
raven ~ group + ses_nuclear_family
spatial_span ~ group + ses_nuclear_family
nback ~ group + ses_nuclear_family
melody_span ~ group + ses_nuclear_family
# digit_span (smallest detected effect of group) was not modeled with paths here or the model would be impossible to identify

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_2 = sem(model_alt_mv_2, data = df, cluster="country")
fitMeasures(fit_mv_2, fi)

summary(fit_mv_2)
BIC(fit_mv_0, fit_mv_1, fit_mv_2)

model_alt_mv_3 = "
level: 1
g =~ NA*raven + waisvocabulary + raven + nback + spatial_span + digit_span + melody_span
g ~~ 1*g
waisvocabulary ~~ spatial_span
digit_span ~~ spatial_span 
digit_span ~~ melody_span

g ~ group + ses_nuclear_family
waisvocabulary ~ group
melody_span ~ group

level: 2
waisvocabulary ~ 1
raven ~ 1 
nback ~ 1
digit_span ~ 1
spatial_span ~ 1 
melody_span ~ 1
"
fit_mv_3 = sem(model_alt_mv_3, data = df, cluster="country")
fitMeasures(fit_mv_3, fi)

BIC(fit_mv_0, fit_mv_1, fit_mv_2, fit_mv_3)

summary(fit_mv_3, standardized=T)

#########################################

# Create fit table 

model_list = list(
  MV0_g_only = fit_mv_0,
  MV1_observed_only = fit_mv_1,
  MV2_g_plus_observed = fit_mv_2,
  MV3_g_plus_targeted_residuals = fit_mv_3
)

model_label = c(
  MV0_g_only = "group differences and SES modeled only at the latent g level",
  MV1_observed_only = "group differences and SES modeled only at observed-variable level",
  MV2_g_plus_observed = "group differences and SES modeled at latent g plus observed-variable level (except digit span)",
  MV3_g_plus_targeted_residuals = "group differences and SES modeled at latent g plus group differences direct effects on vocabulary and melody"
)

fit_table_MV = do.call(
  rbind,
  lapply(names(model_list), function(m) {
    fm = fitMeasures(model_list[[m]], c("chisq", "df", "pvalue", "rmsea", "cfi", "bic"))
    data.frame(
      Model = m,
      Description = unname(model_label[m]),
      ChiSquare = round(unname(fm["chisq"]), 3),
      df = unname(fm["df"]),
      p = round(unname(fm["pvalue"]), 4),
      RMSEA = round(unname(fm["rmsea"]), 3),
      CFI = round(unname(fm["cfi"]), 3),
      BIC = round(unname(fm["bic"]), 3)
    )
  })
)

fit_table_MV$Delta_BIC = round(fit_table_MV$BIC - min(fit_table_MV$BIC), 3)
fit_table_MV

write.csv(fit_table_MV,"Tables/Table_FIT_MV.csv")

#########################################

writeLines(capture.output(sessionInfo()), "sessionInfo.txt")

