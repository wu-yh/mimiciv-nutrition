# import data
raw.data <- read.csv("raw data.csv", stringsAsFactors = FALSE)

# identify rows with missing values greater than 20%
row_missing_ratio <- apply(raw.data, 1, function(row) {
  missing_count <- sum(is.na(row))
  var_count <- length(row)
  missing_count / var_count
})
missing_rows <- which(row_missing_ratio >= 0.20)
if (length(missing_rows) > 0) {
  print(paste(
    "The following rows have missing values greater than 20%:",
    paste(missing_rows, collapse = ", ")
  ))
} else {
  print("There are no rows with missing values greater than 20% in the data frame")
}
# output of raw.data:
# "There are no rows with missing values greater than 20% in the data frame"

# convert variable types -> data.1
library(dplyr)
data.1 <- raw.data %>%
  mutate(
    stay_id = as.character(stay_id),
    gender = factor(
      gender,
      levels = c("M", "F"),
      labels = c("Male", "Female")
    ),
    race = factor(
      if_else(
        race %in% c(
          "WHITE",
          "WHITE - BRAZILIAN",
          "WHITE - EASTERN EUROPEAN",
          "WHITE - OTHER EUROPEAN",
          "WHITE - RUSSIAN"
        ),
        "White",
        "Other"
      ),
      levels = c("White", "Other")
    ),
    careunit = factor(careunit, levels = c("TSURG", "CSURG", "VSURG")),
    vaso_flag = factor(
      vaso_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    sepsis_flag = factor(
      sepsis_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    mechvent_flag = factor(
      mechvent_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    crrt_flag = factor(
      crrt_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    blood_flag = factor(
      blood_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    resp_flag = factor(
      resp_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    chest_flag = factor(
      chest_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    ut_flag = factor(
      ut_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    wound_flag = factor(
      wound_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    icustay_expire_flag = factor(
      icustay_expire_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    hospital_expire_flag = factor(
      hospital_expire_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    sur_flag_28d = factor(
      sur_flag_28d,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    # Yes/1 = Die
    sur_flag_1y = factor(
      sur_flag_1y,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    # Yes/1 = Die
    n_sur_flag_28d = as.numeric(sur_flag_28d),
    n_sur_flag_1y = as.numeric(sur_flag_1y)
  )

# replace missing values with means or medians -> data.2
data.1$sofa_24h[is.na(data.1$sofa_24h)] <-
  median(data.1$sofa_24h, na.rm = TRUE)
data.1$wbc_avg_d1[is.na(data.1$wbc_avg_d1)] <-
  mean(data.1$wbc_avg_d1, na.rm = TRUE)
data.1$vent_hour[is.na(data.1$vent_hour)] <-
  mean(data.1$vent_hour, na.rm = TRUE)
data.1$glu_d1[is.na(data.1$glu_d1)] <-
  mean(data.1$glu_d1, na.rm = TRUE)
data.1$glu_d2[is.na(data.1$glu_d2)] <-
  mean(data.1$glu_d2, na.rm = TRUE)
data.1$glu_d3[is.na(data.1$glu_d3)] <-
  mean(data.1$glu_d3, na.rm = TRUE)
data.1$glu_d4[is.na(data.1$glu_d4)] <-
  mean(data.1$glu_d4, na.rm = TRUE)
data.1$glu_d5[is.na(data.1$glu_d5)] <-
  mean(data.1$glu_d5, na.rm = TRUE)
data.1$glu_d6[is.na(data.1$glu_d6)] <-
  mean(data.1$glu_d6, na.rm = TRUE)
data.1$glu_d7[is.na(data.1$glu_d7)] <-
  mean(data.1$glu_d7, na.rm = TRUE)
data.2 <- data.1
# multiple imputation -> data.2
library(mice)
data.part1 <-
  data.2[, c(
    "age",
    "gender",
    "race",
    "bmi_admit",
    "charlson_comorbidity_index",
    "careunit",
    "mnutric",
    "sofa_24h",
    "apacheii",
    "vaso_flag",
    "sepsis_flag",
    "mechvent_flag",
    "crrt_flag",
    "wbc_avg_d1",
    "neutrophil_avg_d1",
    "lymphocyte_avg_d1",
    "albumin_avg_d1",
    "bun_avg_d1",
    "creatinine_avg_d1"
  )]
imputedData1 <- mice(data.part1)
completedData1 <- complete(imputedData1, 3)
data.2 <- data.2 %>%
  mutate(
    bmi_admit = completedData1$bmi_admit,
    neutrophil_avg_d1 = completedData1$neutrophil_avg_d1,
    lymphocyte_avg_d1 = completedData1$lymphocyte_avg_d1,
    albumin_avg_d1 = completedData1$albumin_avg_d1
  )
data.part2 <-
  data.2[, c(
    "los_hospital",
    "los_icu",
    "bmi_disch",
    "vent_hour",
    "blood_flag",
    "resp_flag",
    "ut_flag",
    "hospital_expire_flag",
    "icustay_expire_flag",
    "sur_flag_28d",
    "sur_flag_1y"
  )]
imputedData2 <- mice(data.part2)
completedData2 <- complete(imputedData2, 3)
data.2 <- data.2 %>%
  mutate(bmi_disch = completedData2$bmi_disch)
# replace outliers with means -> data.2
data.2$bmi_admit[data.2$bmi_admit > 50] <- NA
data.2$bmi_admit[is.na(data.2$bmi_admit)] <-
  mean(data.2$bmi_admit, na.rm = TRUE)
data.2$bmi_disch[data.2$bmi_disch > 50] <- NA
data.2$bmi_disch[is.na(data.2$bmi_disch)] <-
  mean(data.2$bmi_disch, na.rm = TRUE)
data.2$sur_day[data.2$sur_day < 4] <- NA
data.2$sur_day[is.na(data.2$sur_day)] <-
  mean(data.2$sur_day, na.rm = TRUE)

# add new variables -> ana.data
ana.data <- data.2 %>%
  mutate(
    en_tot = en_d1 + en_d2 + en_d3 + en_d4 + en_d5,
    pn_tot = pn_d1 + pn_d2 + pn_d3 + pn_d4 + pn_d5,
    cal_tot_tot = cal_tot_d1 + cal_tot_d2 + cal_tot_d3 + cal_tot_d4 + cal_tot_d5,
    cal_en_tot = cal_en_d1 + cal_en_d2 + cal_en_d3 + cal_en_d4 + cal_en_d5,
    cal_pn_tot = cal_pn_d1 + cal_pn_d2 + cal_pn_d3 + cal_pn_d4 + cal_pn_d5,
    pr_tot_tot = pr_tot_d1 + pr_tot_d2 + pr_tot_d3 + pr_tot_d4 + pr_tot_d5,
    pr_en_tot = pr_en_d1 + pr_en_d2 + pr_en_d3 + pr_en_d4 + pr_en_d5,
    pr_pn_tot = pr_pn_d1 + pr_pn_d2 + pr_pn_d3 + pr_pn_d4 + pr_pn_d5,
    grv_tot = grv_d1 + grv_d2 + grv_d3 + grv_d4 + grv_d5 + grv_d6 + grv_d7,
    ins_tot = ins_d1 + ins_d2 + ins_d3 + ins_d4 + ins_d5 + ins_d6 + ins_d7,
    bmi_change = (bmi_disch - bmi_admit) / bmi_admit,
    bmi_admit_c = cut(
      bmi_admit,
      breaks = c(-Inf, 25, 30, Inf),
      right = FALSE,
      labels = c("Normal", "Overweight", "Obese")
    ),
    cci_c = cut(
      charlson_comorbidity_index,
      breaks = c(-Inf, 2, 4, Inf),
      labels = c("Mild", "Moderate", "Severe")
    ),
    sofa_c = cut(
      sofa_24h,
      breaks = c(-Inf, 9, 14, Inf),
      labels = c("Mild", "Moderate", "Severe")
    ),
    mnutric_c = ifelse(mnutric < 6, "< 6", "≥ 6"),
    mnutric_c = factor(mnutric_c),
    group = ifelse(bfen_hour < 72, "Early", "Delayed"),
    group = factor(group, levels = c("Delayed", "Early")),
    sur_max28 = ifelse(sur_day <= 28, sur_day, 28),
    sur_max365 = ifelse(sur_day <= 365, sur_day, 365)
  ) %>%
  rowwise() %>%
  mutate(avg_glu = mean(c(
    glu_d1, glu_d2, glu_d3, glu_d4, glu_d5, glu_d6, glu_d7
  )))
# The data in ana.data is the data that will eventually be used for analysis.

# test for normality (Shapiro-Wilk (SW) test) of continuous variables
# of baseline and outcome
normtest_var <-
  c(
    "age",
    "bmi_admit",
    "charlson_comorbidity_index",
    "mnutric",
    "sofa_24h",
    "apacheii",
    "bfen_hour",
    "en_tot",
    "pn_tot",
    "cal_tot_tot",
    "cal_en_tot",
    "cal_pn_tot",
    "pr_tot_tot",
    "pr_en_tot",
    "pr_pn_tot",
    "wbc_avg_d1",
    "neutrophil_avg_d1",
    "lymphocyte_avg_d1",
    "albumin_avg_d1",
    "bun_avg_d1",
    "creatinine_avg_d1",
    "los_hospital",
    "los_icu",
    "bmi_change",
    "vent_hour",
    "grv_tot",
    "ins_tot",
    "avg_glu"
  )
normtest <-
  lapply(normtest_var, function(normtest_var)
    shapiro.test(ana.data[[normtest_var]]))
names(normtest) <- normtest_var
normtest

# analysis of baseline information (28 variables, 19 variables need to be balanced)
# table1 (overall)
library(tableone)
tab1_unclassified <-
  CreateTableOne(
    vars = c(
      "age",
      "gender",
      "race",
      "bmi_admit",
      "charlson_comorbidity_index",
      "careunit",
      "mnutric",
      "sofa_24h",
      "apacheii",
      "vaso_flag",
      "sepsis_flag",
      "mechvent_flag",
      "crrt_flag",
      "bfen_hour",
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    data = ana.data
  )
# table1 (before IPW)
tab1_unmatched <-
  CreateTableOne(
    vars = c(
      "age",
      "gender",
      "race",
      "bmi_admit",
      "charlson_comorbidity_index",
      "careunit",
      "mnutric",
      "sofa_24h",
      "apacheii",
      "vaso_flag",
      "sepsis_flag",
      "mechvent_flag",
      "crrt_flag",
      "bfen_hour",
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    strata = "group",
    data = ana.data
  )

# PS (propensity score) + IPW (inverse probability weighting)
psModel <-
  glm(
    group ~ age + gender + race + bmi_admit + charlson_comorbidity_index
    + careunit + mnutric + sofa_24h + apacheii + vaso_flag + sepsis_flag
    + mechvent_flag + crrt_flag + wbc_avg_d1 + neutrophil_avg_d1
    + lymphocyte_avg_d1 + albumin_avg_d1 + bun_avg_d1 + creatinine_avg_d1,
    family = binomial(link = "logit"),
    data = ana.data
  )
# PS
ana.data$ps <- predict(psModel, type = "response")
# IPW
ana.data$wt1 <- 1 / ana.data$ps
ana.data$wt2 <- 1 / (1 - ana.data$ps)
ana.data$wt <-
  ifelse(ana.data$group == "Early", ana.data$wt1, ana.data$wt2)
ana.data <- ana.data
# IPW-weighted data -> svydata
library(survey)
svydata <- svydesign(ids = ~ 1,
                     data = ana.data,
                     weights = ~ wt)
# table1 (after IPW)
tab1_ipw <-
  svyCreateTableOne(
    vars = c(
      "age",
      "gender",
      "race",
      "bmi_admit",
      "charlson_comorbidity_index",
      "careunit",
      "mnutric",
      "sofa_24h",
      "apacheii",
      "vaso_flag",
      "sepsis_flag",
      "mechvent_flag",
      "crrt_flag",
      "bfen_hour",
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    strata = "group",
    data = svydata
  )
# variable-SMD plot
library(reshape2)
library(ggplot2)
dataPlot <-
  data.frame(
    variable = rownames(ExtractSmd(tab1_unmatched)),
    Unmatched = as.numeric(ExtractSmd(tab1_unmatched)),
    IPW = as.numeric(ExtractSmd(tab1_ipw))
  )
dataPlotMelt <- melt(
  data = dataPlot,
  id.vars = c("variable"),
  variable.name = "Method",
  value.name = "SMD"
)
varNames <- as.character(dataPlot$variable)
varNames <-
  varNames[varNames %in% c(
    "age",
    "gender",
    "race",
    "bmi_admit",
    "charlson_comorbidity_index",
    "careunit",
    "mnutric",
    "sofa_24h",
    "apacheii",
    "vaso_flag",
    "sepsis_flag",
    "mechvent_flag",
    "crrt_flag",
    "wbc_avg_d1",
    "neutrophil_avg_d1",
    "lymphocyte_avg_d1",
    "albumin_avg_d1",
    "bun_avg_d1",
    "creatinine_avg_d1"
  )]
varNames <-
  varNames[order(dataPlot$Unmatched[dataPlot$variable %in% varNames])]
dataPlotMelt <- dataPlotMelt[dataPlotMelt$variable %in% varNames,]
dataPlotMelt$variable <-
  factor(dataPlotMelt$variable, levels = varNames)
smdplot <- ggplot(
  data = dataPlotMelt,
  mapping = aes(
    x = variable,
    y = SMD,
    group = Method,
    color = Method,
    shape = Method
  )
) +
  geom_line() +
  geom_point(size = 4) +
  geom_hline(
    yintercept = 0.1,
    color = "red",
    lty = 2,
    linewidth = 0.1
  ) +
  coord_flip() +
  theme_bw(base_size = 18)
ggsave(
  "SMD.tiff",
  plot = smdplot,
  width = 10,
  height = 8,
  dpi = 300
)

# table1: baseline characteristics
table1 <- cbind(
  print(
    tab1_unclassified,
    nonnormal = c(
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    printToggle = FALSE
  ),
  print(
    tab1_unmatched,
    nonnormal = c(
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    printToggle = FALSE
  ),
  print(
    tab1_ipw,
    nonnormal = c(
      "en_tot",
      "pn_tot",
      "cal_tot_tot",
      "cal_en_tot",
      "cal_pn_tot",
      "pr_tot_tot",
      "pr_en_tot",
      "pr_pn_tot",
      "wbc_avg_d1",
      "neutrophil_avg_d1",
      "lymphocyte_avg_d1",
      "albumin_avg_d1",
      "bun_avg_d1",
      "creatinine_avg_d1"
    ),
    printToggle = FALSE
  )
)
print(table1, quote = FALSE)
write.csv(table1, file = "Table1.csv")

# table1: outcome results (16 variables)
tab2_unclassified <-
  CreateTableOne(
    vars = c(
      "los_hospital",
      "los_icu",
      "bmi_change",
      "vent_hour",
      "blood_flag",
      "resp_flag",
      "chest_flag",
      "ut_flag",
      "wound_flag",
      "grv_tot",
      "ins_tot",
      "avg_glu",
      "hospital_expire_flag",
      "icustay_expire_flag",
      "sur_flag_28d",
      "sur_flag_1y"
    ),
    data = ana.data
  )
tab2_unmatched <-
  CreateTableOne(
    vars = c(
      "los_hospital",
      "los_icu",
      "bmi_change",
      "vent_hour",
      "blood_flag",
      "resp_flag",
      "chest_flag",
      "ut_flag",
      "wound_flag",
      "grv_tot",
      "ins_tot",
      "avg_glu",
      "hospital_expire_flag",
      "icustay_expire_flag",
      "sur_flag_28d",
      "sur_flag_1y"
    ),
    strata = "group",
    data = ana.data
  )
tab2_ipw <-
  svyCreateTableOne(
    vars = c(
      "los_hospital",
      "los_icu",
      "bmi_change",
      "vent_hour",
      "blood_flag",
      "resp_flag",
      "chest_flag",
      "ut_flag",
      "wound_flag",
      "grv_tot",
      "ins_tot",
      "avg_glu",
      "hospital_expire_flag",
      "icustay_expire_flag",
      "sur_flag_28d",
      "sur_flag_1y"
    ),
    strata = "group",
    data = svydata
  )
table2 <- cbind(
  print(
    tab2_unclassified,
    nonnormal = c("los_hospital", "los_icu", "vent_hour"),
    printToggle = FALSE
  ),
  print(
    tab2_unmatched,
    nonnormal = c("los_hospital", "los_icu", "vent_hour"),
    printToggle = FALSE
  ),
  print(
    tab2_ipw,
    nonnormal = c("los_hospital", "los_icu", "vent_hour"),
    printToggle = FALSE
  )
)
write.csv(table2, file = "Table2.csv")

# weighted binary logistic regression
# y = 28-day mortality
library(car)
glm28.1 <-
  svyglm(
    sur_flag_28d ~ I(age / 5) + gender + race + bmi_admit + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + sepsis_flag
    + mechvent_flag + crrt_flag + I(wbc_avg_d1 / 2) + I(neutrophil_avg_d1 / 2)
    + I(lymphocyte_avg_d1 / 0.2) + I(albumin_avg_d1 / 0.2) + I(bun_avg_d1 / 2)
    + I(creatinine_avg_d1 / 0.2) + group,
    design = svydata,
    family = quasibinomial()
  )
vif(glm28.1)
glm28.2 <- step(glm28.1, trace = FALSE)
# glm28.2 equivalent to svyglm(sur_flag_28d ~ I(age/5) + bmi_admit
#                               + charlson_comorbidity_index + careunit + vaso_flag
#                               + crrt_flag + albumin_avg_d1 + group,
#                                design = svydata, family = quasibinomial())
or28 <- exp(coef(glm28.2))
or.ci28 <- exp(confint(glm28.2))
write.csv(or28, file = "or28.csv")
write.csv(or.ci28, file = "or_ci28.csv")

# weighted Cox regression
# 28-day survival
cox28.1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + gender + race + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + sepsis_flag + mechvent_flag + crrt_flag + I(wbc_avg_d1 / 2)
    + I(neutrophil_avg_d1 / 2) + I(lymphocyte_avg_d1 / 0.2) + I(albumin_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    design = svydata
  )
vif(cox28.1)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5), design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ gender, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ race, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ bmi_admit, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ charlson_comorbidity_index,
         design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ careunit, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ mnutric_c, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ sofa_24h, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ apacheii, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ vaso_flag, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ sepsis_flag, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ mechvent_flag, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ crrt_flag, design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(wbc_avg_d1 / 2), design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(neutrophil_avg_d1 / 2),
         design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(lymphocyte_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(albumin_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(bun_avg_d1 / 2), design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ I(creatinine_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max28, n_sur_flag_28d) ~ group, design = svydata)
cox28.2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(albumin_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    design = svydata
  )
cox.zph(cox28.2)
# 1-year survival
cox365.1 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + gender + race + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + sepsis_flag + mechvent_flag + crrt_flag + I(wbc_avg_d1 / 2)
    + I(neutrophil_avg_d1 / 2) + I(lymphocyte_avg_d1 / 0.2) + I(albumin_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    design = svydata
  )
vif(cox365.1)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5), design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ gender, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ race, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ bmi_admit, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ charlson_comorbidity_index,
         design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ careunit, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ mnutric_c, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ sofa_24h, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ apacheii, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ vaso_flag, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ sepsis_flag, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ mechvent_flag, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ crrt_flag, design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(wbc_avg_d1 / 2), design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(neutrophil_avg_d1 / 2),
         design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(lymphocyte_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(albumin_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(bun_avg_d1 / 2), design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ I(creatinine_avg_d1 / 0.2),
         design = svydata)
svycoxph(Surv(sur_max365, n_sur_flag_1y) ~ group, design = svydata)
cox365.2 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(albumin_avg_d1 / 0.2) + I(bun_avg_d1 / 2)
    + I(creatinine_avg_d1 / 0.2) + group,
    design = svydata
  )
cox.zph(cox365.2)

# Kaplan-Meier Curve
# 28-day OS
library(jskm)
km28 <-
  svykm(Surv(sur_max28, n_sur_flag_28d) ~ group,
        design = svydata,
        se = TRUE)
tiff(
  "km28.tiff",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
km28.plot <- svyjskm(
  km28,
  xlabs = "Follow up time (d)",
  ylims = c(0.5, 1),
  ystrataname = "",
  ystratalabs = c("Delayed EN", "Early EN"),
  pval = TRUE,
  pval.size = 4,
  pval.coord = c(2, 0.55),
  pval.testname = TRUE,
  legendposition = c(0.90, 0.95),
  ci = TRUE,
  dashed = TRUE,
  table = TRUE,
  label.nrisk = "Number at risk"
)
dev.off()
# 365-day OS
km365 <-
  svykm(Surv(sur_max365, n_sur_flag_1y) ~ group,
        design = svydata,
        se = TRUE)
tiff(
  "km365.tiff",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
km365.plot <- svyjskm(
  km365,
  xlabs = "Follow up time (d)",
  ylims = c(0.5, 1),
  ystrataname = "",
  ystratalabs = c("Delayed EN", "Early EN"),
  pval = TRUE,
  pval.size = 4,
  pval.coord = c(26, 0.55),
  pval.testname = TRUE,
  legendposition = c(0.90, 0.90),
  ci = TRUE,
  dashed = TRUE,
  table = TRUE,
  label.nrisk = "Number at risk"
)
dev.off()

# subgroup analysis
# 28-day HR
age1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ bmi_admit + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (age < 75),
    design = svydata
  )
age2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ bmi_admit + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (age >= 75),
    design = svydata
  )
bmi1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit < 25),
    design = svydata
  )
bmi2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit >= 25 & bmi_admit < 30),
    design = svydata
  )
bmi3 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit >= 30),
    design = svydata
  )
cci1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit + careunit + mnutric_c
    + sofa_24h + apacheii + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 /
                                                        0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (charlson_comorbidity_index < 7),
    design = svydata
  )
cci2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit + careunit + mnutric_c
    + sofa_24h + apacheii + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 /
                                                        0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (charlson_comorbidity_index >= 7),
    design = svydata
  )
careunit1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "TSURG"),
    design = svydata
  )
careunit2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "CSURG"),
    design = svydata
  )
careunit3 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "VSURG"),
    design = svydata
  )
mnutric1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (mnutric < 6),
    design = svydata
  )
mnutric2 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (mnutric >= 6),
    design = svydata
  )
vaso1 <- svycoxph(
  Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
  + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
  + crrt_flag + I(albumin_avg_d1 / 0.2)
  + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
  subset = vaso_flag == "No",
  design = svydata
)
vaso2 <- svycoxph(
  Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
  + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
  + crrt_flag + I(albumin_avg_d1 / 0.2)
  + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
  subset = vaso_flag == "Yes",
  design = svydata
)
sofa1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (sofa_24h < 13),
    design = svydata
  )
# cannot calculate HR when sofa_24h >= 13 due to small number of patients
# 1-year HR
age3 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ bmi_admit + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (age < 75),
    design = svydata
  )
age4 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ bmi_admit + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (age >= 75),
    design = svydata
  )
bmi4 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit < 25),
    design = svydata
  )
bmi5 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit >= 25 & bmi_admit < 30),
    design = svydata
  )
bmi6 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + charlson_comorbidity_index
    + careunit + mnutric_c + sofa_24h + apacheii + vaso_flag + crrt_flag
    + I(lymphocyte_avg_d1 / 0.2) + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2)
    + group,
    subset = (bmi_admit >= 30),
    design = svydata
  )
cci3 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit + careunit + mnutric_c
    + sofa_24h + apacheii + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 /
                                                        0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (charlson_comorbidity_index < 7),
    design = svydata
  )
cci4 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit + careunit + mnutric_c
    + sofa_24h + apacheii + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 /
                                                        0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (charlson_comorbidity_index >= 7),
    design = svydata
  )
careunit4 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "TSURG"),
    design = svydata
  )
careunit5 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "CSURG"),
    design = svydata
  )
careunit6 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + mnutric_c + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (careunit == "VSURG"),
    design = svydata
  )
mnutric3 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (mnutric < 6),
    design = svydata
  )
mnutric4 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + sofa_24h + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (mnutric >= 6),
    design = svydata
  )
vaso3 <- svycoxph(
  Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
  + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
  + crrt_flag + I(albumin_avg_d1 / 0.2)
  + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
  subset = vaso_flag == "No",
  design = svydata
)
vaso4 <- svycoxph(
  Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
  + charlson_comorbidity_index + careunit + mnutric_c + sofa_24h + apacheii
  + crrt_flag + I(albumin_avg_d1 / 0.2)
  + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
  subset = vaso_flag == "Yes",
  design = svydata
)
sofa2 <-
  svycoxph(
    Surv(sur_max365, n_sur_flag_1y) ~ I(age / 5) + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric_c + apacheii
    + vaso_flag + crrt_flag + I(lymphocyte_avg_d1 / 0.2)
    + I(bun_avg_d1 / 2) + I(creatinine_avg_d1 / 0.2) + group,
    subset = (sofa_24h < 13),
    design = svydata
  )
# cannot calculate HR when sofa_24h >= 13 due to small number of patients

# sensitivity analysis
# 28-day Kaplan-Meier Curve with ana.data
library(survminer)
km28.raw <-
  survfit(Surv(sur_max28, n_sur_flag_28d) ~ group, data = ana.data)
tiff(
  "km28_raw.tiff",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
ggsurvplot(
  km28.raw,
  pval = TRUE,
  pval.method = TRUE,
  pval.coord = c(0, 0.55),
  pval.size = 4,
  pval.method.size = 4,
  pval.method.coord = c(2.5, 0.55),
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = "strata",
  surv.median.line = "hv",
  xlab = "Follow up time (d)",
  xlim = c(0, 28),
  break.x.by = 5,
  ylim = c(0.5, 1),
  legend.title = "",
  legend.labs = c("Delayed EN", "Early EN"),
  ggtheme = theme_bw(),
  palette = c("#E7B800", "#2E9FDF")
)
dev.off()
# 1-year Kaplan-Meier Curve with ana.data
km365.raw <-
  survfit(Surv(sur_max365, n_sur_flag_1y) ~ group, data = ana.data)
tiff(
  "km365_raw.tiff",
  width = 10,
  height = 8,
  units = "in",
  res = 300
)
ggsurvplot(
  km365.raw,
  pval = TRUE,
  pval.method = TRUE,
  pval.coord = c(0, 0.55),
  pval.size = 4,
  pval.method.size = 4,
  pval.method.coord = c(30, 0.55),
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = "strata",
  surv.median.line = "hv",
  xlab = "Follow up time (d)",
  xlim = c(0, 365),
  break.x.by = 50,
  ylim = c(0.5, 1),
  legend.title = "",
  legend.labs = c("Delayed EN", "Early EN"),
  ggtheme = theme_bw(),
  palette = c("#E7B800", "#2E9FDF")
)
dev.off()

# some plots and analysis of longitudinal data (9 variables)
library(tidyr)
library(ggplot2)
library(visreg)
# en
long_en.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("en_d"),
    names_to = "Day",
    values_to = "en_amount",
    names_prefix = "en_d",
    names_transform = list(Day = as.factor)
  )
long_en.svydata <- svydesign(ids = ~ 1,
                             data = long_en.data,
                             weights = ~ wt)
en <- ggplot(data = long_en.data, aes(
  x = Day,
  y = en_amount,
  fill = group,
  weight = wt
)) +
  geom_boxplot(outlier.shape = NA, coef = 0) +
  coord_cartesian(ylim =  c(0, 1150)) +
  theme(legend.position = "top") +
  labs (y = "Enteral nutrition (mL)") +
  scale_fill_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("en.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# cal_en
long_cal_en.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("cal_en_d"),
    names_to = "Day",
    values_to = "cal_en_amount",
    names_prefix = "cal_en_d",
    names_transform = list(Day = as.factor)
  )
cal_en <- ggplot(data = long_cal_en.data, aes(
  x = Day,
  y = cal_en_amount,
  fill = group,
  weight = wt
)) +
  geom_boxplot(outlier.shape = NA, coef = 0) +
  coord_cartesian(ylim =  c(0, 1450)) +
  theme(legend.position = "top") +
  labs (y = "Calories of enteral nutrition (kcal)") +
  scale_fill_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("cal_en.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# pr_en
long_pr_en.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("pr_en_d"),
    names_to = "Day",
    values_to = "pr_en_amount",
    names_prefix = "pr_en_d",
    names_transform = list(Day = as.factor)
  )
pr_en <- ggplot(data = long_pr_en.data, aes(
  x = Day,
  y = pr_en_amount,
  fill = group,
  weight = wt
)) +
  geom_boxplot(outlier.shape = NA, coef = 0) +
  coord_cartesian(ylim =  c(0, 80)) +
  theme(legend.position = "top") +
  labs (y = "Protein of enteral nutrition (g)") +
  scale_fill_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("pr_en.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# grv
long_grv.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("grv_d"),
    names_to = "Day",
    values_to = "grv_amount",
    names_prefix = "grv_d",
    names_transform = list(Day = as.factor)
  )
long_grv.svydata <- svydesign(ids = ~ 1,
                              data = long_grv.data,
                              weights = ~ wt)
grv_mean <- svyby(
  ~ grv_amount,
  ~ Day + group,
  design = long_grv.svydata,
  FUN = svymean,
  keep.names = FALSE
)
grv <-
  ggplot(data = grv_mean, aes(x = Day, y = grv_amount, fill = group)) +
  geom_col(position = "dodge") +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "GRV (mL)") +
  scale_fill_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("grv.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_grv <-
  svyglm(
    grv_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
    design = long_grv.svydata,
    family = gaussian(link = "identity")
  )
visfit_grv <-
  visreg(
    fit_grv,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "GRV (mL)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave("fit_grv.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# ins
long_ins.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("ins_d"),
    names_to = "Day",
    values_to = "ins_amount",
    names_prefix = "ins_d",
    names_transform = list(Day = as.factor)
  )
long_ins.svydata <- svydesign(ids = ~ 1,
                              data = long_ins.data,
                              weights = ~ wt)
ins_mean <- svyby(
  ~ ins_amount,
  ~ Day + group,
  design = long_ins.svydata,
  FUN = svymean,
  keep.names = FALSE
)
ins <-
  ggplot(data = ins_mean, aes(x = Day, y = ins_amount, fill = group)) +
  geom_col(position = "dodge") +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "Insulin (unit)") +
  scale_fill_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("ins.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_ins <-
  svyglm(
    ins_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
    design = long_ins.svydata,
    family = gaussian(link = "identity")
  )
visfit_ins <-
  visreg(
    fit_ins,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "Insulin (unit)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave("fit_ins.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# glu
long_glu.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("glu_d"),
    names_to = "Day",
    values_to = "glu_amount",
    names_prefix = "glu_d",
    names_transform = list(Day = as.factor)
  )
long_glu.svydata <- svydesign(ids = ~ 1,
                              data = long_glu.data,
                              weights = ~ wt)
glu_mean <- svyby(
  ~ glu_amount,
  ~ Day + group,
  design = long_glu.svydata,
  FUN = svymean,
  keep.names = FALSE
)
glu <- ggplot(data = glu_mean, aes(
  x = Day,
  y = glu_amount,
  group = group,
  color = group
)) +
  geom_point(size = 4) +
  geom_line(cex = 1.3) +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "Glucose (mg/dL)") +
  scale_color_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("glu.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_glu <-
  svyglm(
    glu_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
    design = long_glu.svydata,
    family = gaussian(link = "identity")
  )
visfit_glu <-
  visreg(
    fit_glu,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "Glucose (mg/dL)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave("fit_glu.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# wbc
long_wbc_avg.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("wbc_avg_d"),
    names_to = "Day",
    values_to = "wbc_avg_amount",
    values_drop_na = TRUE,
    names_prefix = "wbc_avg_d",
    names_transform = list(Day = as.factor)
  )
long_wbc_avg.svydata <- svydesign(ids = ~ 1,
                                  data = long_wbc_avg.data,
                                  weights = ~ wt)
wbc_avg_median <- svyby(
  ~ wbc_avg_amount,
  ~ Day + group,
  design = long_wbc_avg.svydata,
  FUN = svyquantile,
  quantiles = 0.5,
  keep.names = FALSE
)
wbc <- ggplot(data = wbc_avg_median, aes(
  x = Day,
  y = wbc_avg_amount,
  group = group,
  color = group
)) +
  geom_point(size = 4) +
  geom_line(cex = 1.3) +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "WBC (×10⁹/L)") +
  scale_color_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("wbc.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_wbc <-
  svyglm(
    wbc_avg_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
    design = long_wbc_avg.svydata,
    family = gaussian(link = "identity")
  )
visfit_wbc <-
  visreg(
    fit_wbc,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "WBC (×10⁹/L)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave("fit_wbc.tiff",
       width = 10,
       height = 8,
       dpi = 300)
# lymphocyte
long_lymphocyte_avg.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("lymphocyte_avg_d"),
    names_to = "Day",
    values_to = "lymphocyte_avg_amount",
    values_drop_na = TRUE,
    names_prefix = "lymphocyte_avg_d",
    names_transform = list(Day = as.factor)
  )
long_lymphocyte_avg.svydata <- svydesign(ids = ~ 1,
                                         data = long_lymphocyte_avg.data,
                                         weights = ~ wt)
lymphocyte_avg_median <-
  svyby(
    ~ lymphocyte_avg_amount,
    ~ Day + group,
    design = long_lymphocyte_avg.svydata,
    FUN = svyquantile,
    quantiles = 0.5,
    keep.names = FALSE
  )
lymphocyte <- ggplot(data = lymphocyte_avg_median,
                     aes(
                       x = Day,
                       y = lymphocyte_avg_amount,
                       group = group,
                       color = group
                     )) +
  geom_point(size = 4) +
  geom_line(cex = 1.3) +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "Lymphocyte (×10⁹/L)") +
  scale_color_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("lymphocyte.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_lymphocyte <-
  svyglm(
    lymphocyte_avg_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
    design = long_lymphocyte_avg.svydata,
    family = gaussian(link = "identity")
  )
visfit_lymphocyte <-
  visreg(
    fit_lymphocyte,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "Lymphocyte (×10⁹/L)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave(
  "fit_lymphocyte.tiff",
  width = 10,
  height = 8,
  dpi = 300
)
# albumin
long_albumin_avg.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("albumin_avg_d"),
    names_to = "Day",
    values_to = "albumin_avg_amount",
    values_drop_na = TRUE,
    names_prefix = "albumin_avg_d",
    names_transform = list(Day = as.factor)
  )
long_albumin_avg.svydata <- svydesign(ids = ~ 1,
                                      data = long_albumin_avg.data,
                                      weights = ~ wt)
albumin_avg_median <- svyby(
  ~ albumin_avg_amount,
  ~ Day + group,
  design = long_albumin_avg.svydata,
  FUN = svyquantile,
  quantiles = 0.5,
  keep.names = FALSE
)
albumin <- ggplot(data = albumin_avg_median,
                  aes(
                    x = Day,
                    y = albumin_avg_amount,
                    group = group,
                    color = group
                  )) +
  geom_point(size = 4) +
  geom_line(cex = 1.3) +
  theme(legend.position = "top", text = element_text(size = 25)) +
  labs (y = "Albumin (g/dL)") +
  scale_color_discrete(
    name = "",
    breaks = c("Delayed", "Early"),
    labels = c("Delayed EN", "Early EN")
  )
ggsave("albumin.tiff",
       width = 10,
       height = 8,
       dpi = 300)
fit_albumin <- svyglm(
  albumin_avg_amount ~ group * as.numeric(Day) + I(as.numeric(Day) ^ 2),
  design = long_albumin_avg.svydata,
  family = gaussian(link = "identity")
)
visfit_albumin <-
  visreg(
    fit_albumin,
    "Day",
    by = "group",
    gg = TRUE,
    type = "conditional",
    partial = FALSE,
    rug = FALSE,
    overlay = TRUE,
    ylab = "Albumin (g/dL)"
  ) +
  theme(legend.position = "top", text = element_text(size = 25))
ggsave("fit_albumin.tiff",
       width = 10,
       height = 8,
       dpi = 300)

# combine several plots into one
library(cowplot)
# figure2
plot_grid(
  en,
  cal_en,
  pr_en,
  nrow = 3,
  ncol = 1,
  align = "v",
  axis = "lr",
  labels = "auto"
)
ggsave("figure2.tiff",
       width = 7,
       height = 10,
       dpi = 300)
# figure3
figure3.1 <- plot_grid(
  grv,
  ins,
  glu,
  wbc,
  lymphocyte,
  albumin,
  nrow = 2,
  ncol = 3,
  align = "hv",
  axis = "tblr",
  labels = "auto",
  label_x = 0.1,
  label_size = 20
)
figure3.2 <-
  plot_grid(
    visfit_grv,
    visfit_ins,
    visfit_glu,
    visfit_wbc,
    visfit_lymphocyte,
    visfit_albumin,
    nrow = 2,
    ncol = 3,
    align = "hv",
    axis = "tblr",
    labels = "auto",
    label_x = 0.1,
    label_size = 20
  )
plot_grid(
  figure3.1,
  figure3.2,
  nrow = 2,
  ncol = 1,
  align = "v",
  axis = "lr",
  labels = "AUTO",
  label_size = 25
)
ggsave("figure3.tiff",
       width = 25,
       height = 25,
       dpi = 300)
# figure4
tiff(
  "figure4.tiff",
  width = 20,
  height = 10,
  units = "in",
  res = 300
)
plot_grid(
  km28.plot,
  km365.plot,
  nrow = 1,
  ncol = 2,
  align = "h",
  axis = "tb",
  labels = "AUTO"
)
dev.off()
# figure5
library(forestploter)
sub28 <- read.csv("sub28.csv", stringsAsFactors = FALSE)
sub28$` ` <- paste(rep(" ", 20), collapse = " ")
sub28$p <- ifelse(is.na(sub28$p), "", sub28$p)
tiff(
  "28forest.tiff",
  width = 6,
  height = 7,
  units = "in",
  res = 300
)
forest(
  sub28[, c(1, 5:7)],
  est = sub28$hr,
  lower = sub28$lower,
  upper = sub28$upper,
  sizes = (log(sub28$upper) - log(sub28$hr)) / 1.96,
  ci_column = 4,
  ref_line = 1,
  arrow_lab = c("Early EN better", "Delayed EN better"),
  xlim = c(0, 4),
  ticks_at = c(0.5, 1, 2, 3)
)
dev.off()
sub365 <- read.csv("sub365.csv", stringsAsFactors = FALSE)
sub365$` ` <- paste(rep(" ", 20), collapse = " ")
sub365$p <- ifelse(is.na(sub365$p), "", sub365$p)
tiff(
  "365forest.tiff",
  width = 6,
  height = 7,
  units = "in",
  res = 300
)
forest(
  sub365[, c(1, 5:7)],
  est = sub365$hr,
  lower = sub365$lower,
  upper = sub365$upper,
  sizes = (log(sub365$upper) - log(sub365$hr)) / 1.96,
  ci_column = 4,
  ref_line = 1,
  arrow_lab = c("Early EN better", "Delayed EN better"),
  xlim = c(0, 4),
  ticks_at = c(0.5, 1, 2, 3)
)
dev.off()

# weighted Cox regression of different total calories groups in the first 3 days
library(tidyverse)
ana.data <- ana.data %>%
  left_join(raw.data %>% select(stay_id, ht_admit, wt_admit), by = "stay_id") %>%
  mutate(
    ht_admit = ht_admit / 100,
    ht_admit = if_else(ht_admit > 2.30, median(ht_admit, na.rm = TRUE), ht_admit),
    ht_admit = if_else(ht_admit < 1.00, median(ht_admit, na.rm = TRUE), ht_admit)
  )
ana.data$ht_admit = if_else(is.na(ana.data$ht_admit),
                            mean(ana.data$ht_admit, na.rm = TRUE),
                            ana.data$ht_admit)
ana.data$wt_admit = if_else(is.na(ana.data$wt_admit),
                            mean(ana.data$wt_admit, na.rm = TRUE),
                            ana.data$wt_admit)
ana.data <- ana.data %>%
  mutate(
    ideal_wt = 25 * ht_admit * ht_admit,
    adj_wt = if_else(bmi_admit < 30, wt_admit, (wt_admit - ideal_wt) * 0.25 + ideal_wt)
  )
long_cal_tot.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("cal_tot_d"),
    names_to = "Day",
    values_to = "cal_tot_amount",
    names_prefix = "cal_tot_d"
  )
day3 <- long_cal_tot.data %>%
  filter(Day >= 1 & Day <= 3) %>%
  group_by(stay_id) %>%
  summarise(mean_cal_tot = mean(cal_tot_amount) / adj_wt,
            # kcal/kg/d
            .groups = "drop")
mean(day3$mean_cal_tot)
day3 <- distinct(day3, stay_id, .keep_all = TRUE)
long_cal_tot.data <-
  left_join(long_cal_tot.data, day3, by = "stay_id")
long_cal_tot.data <- filter(long_cal_tot.data, Day == 1)
long_cal_tot.data$mean_cal_tot_c <-
  cut(
    long_cal_tot.data$mean_cal_tot,
    breaks = c(-Inf, 3, 6, 9, Inf),
    labels = c("<3", "3-6", "6-9", ">9")
  )
median <- long_cal_tot.data %>%
  group_by(mean_cal_tot_c) %>%
  summarise(median = median(mean_cal_tot))
long_cal_tot.data <- long_cal_tot.data %>%
  mutate(
    mean_cal_tot_m = case_when(
      mean_cal_tot < 3 ~ 2.1,
      mean_cal_tot >= 3 & mean_cal_tot < 6 ~ 4.3,
      mean_cal_tot >= 6 & mean_cal_tot < 9 ~ 7.4,
      mean_cal_tot >= 9 ~ 12.6
    )
  )
summary(long_cal_tot.data$mean_cal_tot)

library(survey)
svydata2 <- svydesign(ids = ~ 1,
                      data = long_cal_tot.data,
                      weights = ~ wt)
cox28s <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ age + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric + sofa_24h + apacheii
    + vaso_flag + crrt_flag + albumin_avg_d1 + bun_avg_d1 + creatinine_avg_d1
    + mean_cal_tot_c,
    design = svydata2
  )
summary(cox28s)
cox28sm <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ age + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric + sofa_24h + apacheii
    + vaso_flag + crrt_flag + albumin_avg_d1 + bun_avg_d1 + creatinine_avg_d1
    + mean_cal_tot_m,
    design = svydata2
  )
summary(cox28sm)

# weighted Cox regression of different total protein groups in the first 3 days
long_pr_tot.data <- ana.data %>%
  pivot_longer(
    cols = starts_with("pr_tot_d"),
    names_to = "Day",
    values_to = "pr_tot_amount",
    names_prefix = "pr_tot_d"
  )
day3.1 <- long_pr_tot.data %>%
  filter(Day >= 1 & Day <= 3) %>%
  group_by(stay_id) %>%
  summarise(mean_pr_tot = mean(pr_tot_amount) / adj_wt,
            # g/kg/d
            .groups = "drop")
mean(day3.1$mean_pr_tot)
day3.1 <- distinct(day3.1, stay_id, .keep_all = TRUE)
long_pr_tot.data <-
  left_join(long_pr_tot.data, day3.1, by = "stay_id")
long_pr_tot.data <- filter(long_pr_tot.data, Day == 1)
long_pr_tot.data$mean_pr_tot_c <- cut(
  long_pr_tot.data$mean_pr_tot,
  breaks = c(-Inf, 0.2, 0.4, 0.6, Inf),
  labels = c("<0.2", "0.2-0.4", "0.4-0.6", ">0.6")
)
median2 <- long_pr_tot.data %>%
  group_by(mean_pr_tot_c) %>%
  summarise(median = median(mean_pr_tot))
long_pr_tot.data <- long_pr_tot.data %>%
  mutate(
    mean_pr_tot_m = case_when(
      mean_pr_tot < 0.2 ~ 0.00,
      mean_pr_tot >= 0.2 & mean_pr_tot < 0.4 ~ 0.29,
      mean_pr_tot >= 0.4 & mean_pr_tot < 0.6 ~ 0.47,
      mean_pr_tot >= 0.6 ~ 0.77
    )
  )
summary(long_pr_tot.data$mean_pr_tot)

library(survey)
svydata3 <- svydesign(ids = ~ 1,
                      data = long_pr_tot.data,
                      weights = ~ wt)
cox28s1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ age + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric + sofa_24h + apacheii
    + vaso_flag + crrt_flag + albumin_avg_d1 + bun_avg_d1 + creatinine_avg_d1
    + mean_pr_tot_c,
    design = svydata3
  )
summary(cox28s1)
cox28sm1 <-
  svycoxph(
    Surv(sur_max28, n_sur_flag_28d) ~ age + bmi_admit
    + charlson_comorbidity_index + careunit + mnutric + sofa_24h + apacheii
    + vaso_flag + crrt_flag + albumin_avg_d1 + bun_avg_d1 + creatinine_avg_d1
    + mean_pr_tot_m,
    design = svydata3
  )
summary(cox28sm1)

# save workspace
save.image("MyFile.Rdata")