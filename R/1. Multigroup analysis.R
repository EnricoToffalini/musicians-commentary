
#########################################

library(lavaan)
load("Data/Prepared-data.RData")

#########################################

# INITIAL CHECK OF CORRELATED RESIDUALS

#########################################

modelcheckres = "
g =~ NA*raven + waisvocabulary + nback + spatial_span + digit_span + melody_span
g ~~ c(1,1)*g
g ~ ses_nuclear_family
"
fitcheckres = cfa(modelcheckres, df, group="group")
fitMeasures(fitcheckres, fi)
modificationindices(fitcheckres, sort.=T)[1:4,]

modelcheckres = paste0(modelcheckres,"\n digit_span ~~ melody_span")
fitcheckres = cfa(modelcheckres, df, group="group")
fitMeasures(fitcheckres, fi)
modificationindices(fitcheckres, sort.=T)[1:4,]

modelcheckres = paste0(modelcheckres,"\n waisvocabulary ~~ spatial_span")
fitcheckres = cfa(modelcheckres, df, group="group")
fitMeasures(fitcheckres, fi)
modificationindices(fitcheckres, sort.=T)[1:4,]

modelcheckres = paste0(modelcheckres,"\n spatial_span ~~ digit_span")
fitcheckres = cfa(modelcheckres, df, group="group")
fitMeasures(fitcheckres, fi)
modificationindices(fitcheckres, sort.=T)[1:4,]

#########################################

# FITTING MULTIGROUP INVARIANCE MODELS

#########################################

model = "
g =~ NA*raven + waisvocabulary + nback + spatial_span + digit_span + melody_span
g ~~ c(1,1)*g
g ~ ses_nuclear_family

waisvocabulary ~~ spatial_span
digit_span ~~ melody_span
spatial_span ~~ digit_span
"
fitConfigural = cfa(model, df, group="group")
fitMeasures(fitConfigural, fi)
modificationindices(fitConfigural, sort.=T)

fitMetric = cfa(model, df, group="group", 
           group.equal=c("loadings"))
fitMeasures(fitMetric, fi)
BIC(fitConfigural, fitMetric)

fitScalar = cfa(model, df, group="group", 
           group.equal=c("loadings","intercepts"))
fitMeasures(fitScalar, fi)
BIC(fitMetric, fitScalar)

summary(fitScalar, standardized=T)

fitScalarPartial = cfa(model, df, group="group", 
                group.equal=c("loadings","intercepts"),
                group.partial="melody_span~1")
fitMeasures(fitScalarPartial, fi)
BIC(fitMetric, fitScalarPartial)

fitScalarPartialReg1 = cfa(model, df, group="group", 
                       group.equal=c("loadings","intercepts","regressions"),
                       group.partial="melody_span~1")
fitMeasures(fitScalarPartialReg1, fi)
BIC(fitScalarPartial, fitScalarPartialReg1)

fitScalarPartialReg2 = cfa(model, df, group="group", 
                           group.equal=c("loadings","intercepts","regressions",
                                         "residual.covariances"),
                           group.partial="melody_span~1")
fitMeasures(fitScalarPartialReg2, fi)
BIC(fitScalarPartialReg1, fitScalarPartialReg2)

fitStrict = cfa(model, df, group="group", 
           group.equal=c("loadings","intercepts","regressions","residuals"),
           group.partial="melody_span~1")
fitMeasures(fitStrict, fi)
BIC(fitScalarPartialReg1, fitStrict)

fitLatentMean = cfa(model, df, group="group", 
           group.equal=c("loadings","intercepts","regressions","means"),
           group.partial="melody_span~1")
fitMeasures(fitLatentMean, fi)
BIC(fitScalarPartialReg1, fitLatentMean)

#########################################

# Best fitting model

summary(fitScalarPartialReg1, standardized=T)
fitMeasures(fitScalarPartialReg1, fi)

#########################################

# Create fit table 

model_list = list(
  Configural = fitConfigural,
  Metric = fitMetric,
  Scalar = fitScalar,
  ScalarPartial = fitScalarPartial,
  ScalarPartial_1 = fitScalarPartialReg1,
  ScalarPartial_2 = fitScalarPartialReg2,
  Strict = fitStrict,
  LatentMean = fitLatentMean
)

model_label = c(
  Configural = "Same factor structure across groups",
  Metric = "Equal loadings",
  Scalar = "Equal intercepts",
  ScalarPartial = "Equal intercepts except Melody span",
  ScalarPartial1 = "Equal SES regression added",
  ScalarPartial2 = "Equal residual correlations",
  Strict = "Equal residual variances",
  LatentMean = "Equal latent means"
)

fit_table_MG = do.call(
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

fit_table_MG$Delta_BIC = round(fit_table_MG$BIC - min(fit_table_MG$BIC), 3)
fit_table_MG

write.csv(fit_table_MG,"Tables/Table_FIT_MG.csv")

#########################################


