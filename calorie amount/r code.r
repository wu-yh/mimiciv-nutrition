library(tidyverse)
# import raw data
rawdata <- read_csv("rawdata.csv")

# convert variable types
# handle NA and outliers
# add new variables
anadata <- rawdata %>%
  mutate(
    id = as.character(id),
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
    adm_type = factor(adm_type,
                      levels = c("TSURG", "SURG")),
    vaso = factor(
      vaso,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    sepsis = factor(
      sepsis,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    imv_flag = factor(
      imv_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    nimv_flag = factor(
      nimv_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    rrt = factor(
      rrt,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    dis_loc = factor(
      case_when(
        dis_loc %in% c("HOME", "HOME HEALTH CARE") ~ "Home",
        dis_loc %in% c("DIED") ~ "Die",
        TRUE ~ "Nursing facility"
      ),
      levels = c("Home", "Nursing facility", "Die")
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
    pleural_flag = factor(
      pleural_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    urine_flag = factor(
      urine_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    incision_flag = factor(
      incision_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    icu_expire_flag = factor(
      icu_expire_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    hosp_expire_flag = factor(
      hosp_expire_flag,
      levels = c("0", "1"),
      labels = c("No", "Yes")
    ),
    cal_tot = cal_tot0 + cal_supp_pn,
    cal_pn = cal_pn0 + cal_supp_pn,
    ht = if_else(ht > 2.30, median(ht, na.rm = TRUE), ht),
    ht = if_else(ht < 1.00, median(ht, na.rm = TRUE), ht),
    ht = if_else(is.na(ht), mean(ht, na.rm = TRUE), ht),
    wt = if_else(is.na(wt), mean(wt, na.rm = TRUE), wt),
    bmi = wt / (ht * ht),
    bmi_range = factor(
      case_when(
        bmi < 18.5 ~ "<18.5",
        bmi >= 18.5 & bmi < 25 ~ "18.5-25",
        bmi >= 25 & bmi < 30 ~ "25-30",
        bmi >= 30 ~ "≥30"
      ),
      levels = c("<18.5", "18.5-25", "25-30", "≥30")
    ),
    ideal_wt = 2.2 * bmi + 3.5 * bmi * (ht - 1.5),
    adj_wt = if_else(bmi < 30, wt, (wt - ideal_wt) * 0.33 + ideal_wt),
    los_hosp = if_else(los_hosp < los_icu, los_icu, los_hosp),
    surv_time_28d = if_else(surv_time_28d < 4, los_icu, surv_time_28d),
    surv_time_90d = if_else(surv_time_90d < 4, los_icu, surv_time_90d),
    ntr_type = factor(
      case_when(
        cal_tot == 0 ~ "na",
        # no nutrition
        cal_tot > 0 &
          cal_en == 0 & cal_pn == 0 ~ "nn",
        # non-nutritional calories only
        cal_en > 0 & cal_pn > 0 ~ "ep",
        # en and pn
        cal_en == 0 & cal_pn > 0 ~ "pn",
        # pn
        cal_en > 0 & cal_pn == 0 ~ "en" # en
      ),
      levels = c("na", "nn", "ep", "pn", "en"),
      labels = c("None", "non-NC only", "EN+PN", "PN", "EN")
    )
  )
# grouping
grouping <- anadata %>%
  filter(day >= 1 & day <= 5) %>%
  group_by(id) %>%
  summarise(
    mean_cal_tot = mean(cal_tot) / adj_wt,
    # kcal/kg/d
    mean_cal_en = mean(cal_en) / adj_wt,
    # kcal/kg/d
    mean_cal_pn = mean(cal_pn) / adj_wt,
    # kcal/kg/d
    mean_pr_tot = mean(pr_tot) / adj_wt,
    # g/kg/d
    en.tot = sum(cal_en) / sum(cal_tot),
    # % of total calories given via the enteral route
    .groups = "drop"
  )
grouping <- distinct(grouping, id, .keep_all = TRUE)
anadata <- left_join(anadata, grouping, by = "id")
# 10-day nutritional intake
day_10 <- anadata %>%
  group_by(id) %>%
  summarise(
    mean_cal_tot_10 = mean(cal_tot) / adj_wt,
    # kcal/kg/d
    mean_cal_en_10 = mean(cal_en) / adj_wt,
    # kcal/kg/d
    mean_cal_pn_10 = mean(cal_pn) / adj_wt,
    # kcal/kg/d
    mean_pr_tot_10 = mean(pr_tot) / adj_wt,
    # g/kg/d
    en.tot_10 = sum(cal_en) / sum(cal_tot),
    # % of total calories given via the enteral route
    .groups = "drop"
  )
day_10 <- distinct(day_10, id, .keep_all = TRUE)
anadata <- left_join(anadata, day_10, by = "id")
anadata <- anadata %>%
  mutate(
    pct = mean_cal_tot / 25,
    pct_10 = mean_cal_tot_10 / 25,
    group = factor(
      case_when(pct < 0.2 ~ "Low",
                pct >= 0.2 & pct <= 0.4 ~ "Moderate",
                pct > 0.4  ~ "High"),
      levels = c("Moderate", "Low", "High")
    ),
    group1 = factor(
      expire_flag_90d,
      levels = c(0, 1),
      labels = c("Survivors", "Non-survivors")
    )
  )
# total/max calories administered within 10 days
tot_cal <- anadata %>%
  group_by(id) %>%
  summarise(
    tot_cal = sum(cal_tot),
    max_cal = max(cal_tot) / max(adj_wt),
    .groups = "drop"
  )
data1 <- filter(anadata, day == 1)

# correlation between calories and protein intake
cor.test(anadata$cal_tot, anadata$pr_tot, method = "spearman")

# Table 1
library(epiDisplay)
tab1 <-
  tableStack(
    vars = c(
      age,
      gender,
      race,
      ht,
      adj_wt,
      bmi,
      cci,
      mnutric,
      adm_type,
      sapsii,
      sofa,
      apacheii,
      vaso,
      sepsis,
      imv_flag,
      nimv_flag,
      rrt,
      first_en,
      first_pn,
      pct,
      mean_cal_tot,
      mean_cal_en,
      mean_cal_pn,
      en.tot,
      mean_pr_tot,
      pct_10,
      mean_cal_tot_10,
      mean_cal_en_10,
      mean_cal_pn_10,
      en.tot_10,
      mean_pr_tot_10
    ),
    dataFrame = data1,
    by = group1,
    total.column = TRUE,
    assumption.p.value = .05
  )
write.csv(tab1, file = "Table1.csv")

# Table S1
tabs1 <-
  tableStack(
    vars = c(
      age,
      gender,
      race,
      ht,
      adj_wt,
      bmi,
      cci,
      mnutric,
      adm_type,
      sapsii,
      sofa,
      apacheii,
      vaso,
      sepsis,
      imv_flag,
      nimv_flag,
      rrt,
      first_en,
      first_pn,
      pct,
      mean_cal_tot,
      mean_cal_en,
      mean_cal_pn,
      en.tot,
      mean_pr_tot,
      pct_10,
      mean_cal_tot_10,
      mean_cal_en_10,
      mean_cal_pn_10,
      en.tot_10,
      mean_pr_tot_10
    ),
    dataFrame = data1,
    by = group,
    total.column = TRUE,
    assumption.p.value = .05
  )
write.csv(tabs1, file = "TableS1.csv")

# Table S2
tabs2 <-
  tableStack(
    vars = c(
      los_hosp,
      los_icu,
      imv_dur,
      blood_flag,
      resp_flag,
      urine_flag,
      icu_expire_flag,
      hosp_expire_flag,
      expire_flag_28d,
      expire_flag_90d
    ),
    vars.to.factor = c(expire_flag_28d, expire_flag_90d),
    dataFrame = data1,
    by = group,
    total.column = TRUE,
    assumption.p.value = .05
  )
write.csv(tabs2, file = "TableS2.csv")

# Figure 2
library(ggplot2)
library(cowplot)
library(car)
median_stats <- anadata %>%
  group_by(day) %>%
  summarise(median_cal = median(cal_tot / adj_wt),
            median_pr = median(pr_tot / adj_wt))
# boxplot of daily calorie intake
cal_plot <-
  ggplot(anadata, aes(x = as.factor(day), y = cal_tot / adj_wt)) +
  stat_boxplot(geom = 'errorbar',
               linetype = 1,
               width = 0.2) +
  geom_boxplot(fill = "lightblue", width = 0.4) +
  stat_summary(fun = median,
               geom = "text",
               aes(label = round(..y.., 1)),
               vjust = -0.5) +
  geom_line(
    data = median_stats,
    aes(x = day, y = median_cal),
    size = 1,
    group = 1,
    colour = "#377eb8"
  ) +
  labs(x = "Day", y = "Total Calorie Intake (kcal/kg)") +
  theme_bw()
# boxplot of daily protein intake
anadata$hjust_values <-
  ifelse(anadata$day %in% c('1', '2'), -0.9, 0.5)
pr_plot <-
  ggplot(anadata, aes(x = as.factor(day), y = pr_tot / adj_wt)) +
  stat_boxplot(geom = 'errorbar',
               linetype = 1,
               width = 0.2) +
  geom_boxplot(fill = "lightblue", width = 0.4) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(..y.., 1), hjust = hjust_values),
    vjust = -0.5
  ) +
  geom_line(
    data = median_stats,
    aes(x = day, y = median_pr),
    size = 1,
    group = 1,
    colour = "#377eb8"
  ) +
  labs(x = "Day", y = "Total Protein Intake (g/kg)") +
  theme_bw()
# percentage of each nutrition type per day (percent stacked area chart)
# including EN/PN/EN+PN/no nutrition/non-nutritional calories only
ntdata <- anadata %>%
  count(day, ntr_type) %>%
  group_by(day) %>%
  mutate(total = sum(n),
         percentage = n / total) %>%
  ungroup()
cal_pct_plot <-
  ggplot(ntdata, aes(x = day, y = percentage, fill = ntr_type)) +
  geom_area() +
  scale_x_continuous(breaks = seq(0, max(ntdata$day), by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_fill_manual(
    values = c(
      "EN" = "#FFDAB9",
      "PN" = "#FFEBCD",
      "EN+PN" = "#FFFAF0",
      "None" = "#DCDCDC",
      "non-NC only" = "#F5F5F5"
    )
  ) +
  labs(x = "Day", y = "Percentage", fill = "Nutrition type") +
  theme_classic()
plot_grid(
  cal_plot,
  cal_pct_plot,
  pr_plot,
  nrow = 3,
  ncol = 1,
  align = "v",
  axis = "lr",
  labels = "auto"
)
ggsave("figure2.tiff",
       width = 12,
       height = 12,
       dpi = 300)

# multinomial logistic regression
library(nnet)
library(broom)
library(epiDisplay)
mlog_all <- multinom(
  group ~ age + gender + race + bmi_range +
    cci + mnutric + adm_type + sapsii + sofa + apacheii +
    vaso + sepsis + imv_flag + rrt,
  data = data1
)
mlog_sw <- step(mlog_all, trace = FALSE, direction = "both")
summary(mlog_sw)
# multinom(formula = group ~ age + gender + bmi_range + mnutric +
# sapsii + sofa + vaso + imv_flag + rrt, data = data1)
mlog <-
  multinom(group ~ age + gender + bmi_range + cci + mnutric++sofa + vaso + sepsis + imv_flag + rrt,
           data = data1)
p_log <- tidy(mlog)
mlogit.display(mlog)
write.csv(p_log, file = "mlog.csv")

# Figure 3
library(lmerTest)
library(ggplot2)
library(cowplot)
# glucose
lme_glu <- lmer(glu ~ day + group + day:group + (1 + day | id),
                data = anadata)
summary(lme_glu)
glu_pred <- predict(lme_glu, anadata, re.form = NA)
glu_plot <-
  ggplot(anadata, aes(x = day, y = glu_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "Glucose (mg/dL)",
       colour = "Group") +
  theme(legend.position = "none")
# insulin
lme_ins <- lmer(ins ~ day + group + day:group + (1 + day | id),
                data = anadata)
summary(lme_ins)
ins_pred <- predict(lme_ins, anadata, re.form = NA)
ins_plot <-
  ggplot(anadata, aes(x = day, y = ins_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "Insulin (unit)",
       colour = "Group") +
  theme(legend.position = "none")
# tg
lme_tg <- lmer(tg ~ day + group + day:group + (1 | id),
               data = anadata)
summary(lme_tg)
tg_pred <- predict(lme_tg, anadata, re.form = NA)
tg_plot <-
  ggplot(anadata, aes(x = day, y = tg_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "Triglyceride (mg/dL)",
       colour = "Group") +
  theme(legend.position = "none")
# bun
lme_bun <- lmer(bun ~ day + group + day:group + rrt + (1 + day | id),
                data = anadata)
summary(lme_bun)
bun_pred <- predict(lme_bun, anadata, re.form = NA)
bun_plot <-
  ggplot(anadata, aes(x = day, y = bun_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "BUN (mg/dL)",
       colour = "Group")
# po4
lme_po4 <- lmer(po4 ~ day + group + day:group + rrt + (1 + day | id),
                data = anadata)
summary(lme_po4)
po4_pred <- predict(lme_po4, anadata, re.form = NA)
po4_plot <-
  ggplot(anadata, aes(x = day, y = po4_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "Phosphate (mg/dL)",
       colour = "Group") +
  theme(legend.position = "none")
# ph
lme_ph <-
  lmer(ph ~ day + group + day:group + imv_flag + rrt + (1 + day | id),
       data = anadata)
summary(lme_ph)
ph_pred <- predict(lme_ph, anadata, re.form = NA)
ph_plot <-
  ggplot(anadata, aes(x = day, y = ph_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "pH",
       colour = "Group") +
  theme(legend.position = "none")
# hco3
lme_hco3 <- lmer(hco3 ~ day + group + day:group + rrt + (1 + day | id),
                 data = anadata)
summary(lme_hco3)
hco3_pred <- predict(lme_hco3, anadata, re.form = NA)
hco3_plot <-
  ggplot(anadata, aes(x = day, y = hco3_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "Bicarbonate (mmol/L)",
       colour = "Group") +
  theme(legend.position = "none")
# paco2
lme_paco2 <-
  lmer(paco2 ~ day + group + day:group + imv_flag + (1 + day | id),
       data = anadata)
summary(lme_paco2)
paco2_pred <- predict(lme_paco2, anadata, re.form = NA)
paco2_plot <-
  ggplot(anadata, aes(x = day, y = paco2_pred, color = group)) +
  stat_summary(aes(group = group),
               fun = mean,
               geom = "line",
               size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  labs(x = "Day",
       y = "PaCO2 (mmHg)",
       colour = "Group") +
  theme(legend.position = "none")
plot_grid(
  glu_plot,
  ins_plot,
  tg_plot,
  bun_plot,
  ph_plot,
  hco3_plot,
  paco2_plot,
  po4_plot,
  nrow = 2,
  ncol = 4,
  align = "hv",
  axis = "lr",
  labels = "auto"
)
ggsave("figure3.tiff",
       width = 17,
       height = 6,
       dpi = 300)

# Cox regression
library(survival)
cox_all <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ group + age + gender + race + bmi_range +
      cci + mnutric + adm_type + sapsii + sofa + apacheii + vaso + sepsis +
      imv_flag + rrt + en.tot,
    data = anadata
  )
cox_sw <- step(cox_all, trace = FALSE, direction = "both")
summary(cox_sw)
# coxph(formula = Surv(surv_time_90d, expire_flag_90d) ~ group +
# age + gender + race + bmi_range + cci + mnutric + adm_type +
# sofa + apacheii + sepsis + imv_flag + en.tot, data = anadata)
car::vif(cox_sw)
cox_90 <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ group + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = anadata
  )
summary(cox_90)
cox.zph(cox_90)
# select patients with pct <= 0.283
data28.3l <- filter(anadata, pct <= 0.283)
cox_28.3l <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ pct + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data28.3l
  )
summary(cox_28.3l)
# select patients with pct > 0.283
data28.3r <- filter(anadata, pct > 0.283)
cox_28.3r <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ pct + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data28.3r
  )
summary(cox_28.3r)

# Restricted cubic spline (RCS)
library(rms)
library(ggplot2)
library(cowplot)
ddist <- datadist(anadata)
options(datadist = "ddist")
# RCS not stratified
rcs <-
  cph(
    Surv(surv_time_90d, expire_flag_90d) ~ rcs(pct, 4) + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = anadata
  )
anova(rcs)
pred <- Predict(rcs, pct, fun = exp, ref.zero = TRUE)
min_index <- which.min(pred$lower)
min_pct <- pred$pct[min_index] # x value when y is minimum
indices_hr_1 <- which(abs(pred$yhat - 1) < 0.01)
pct_hr_1 <- pred$pct[indices_hr_1] # x value when y (HR) = 1.0
# density plot
pct_density <- density(anadata$pct)
density_df <-
  data.frame(pct = pct_density$x, density = pct_density$y)
# RCS plot
rcs_plot <- ggplot(pred) +
  geom_line(aes(x = pct, y = yhat), color = "#1f77b4", size = 1) +
  geom_ribbon(aes(x = pct, ymin = lower, ymax = upper),
              fill = "#1f77b4",
              alpha = 0.3) +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = 2) +
  geom_vline(
    xintercept = c(min_pct, 0.20, 0.39),
    color = "#D62728",
    linetype = "dashed",
    size = 0.5
  ) +
  annotate(
    "text",
    x = c(min_pct, 0.20, 0.39),
    y = 1.1,
    label = paste("x =", c(round(min_pct, 3), "0.20", "0.39")),
    vjust = -1,
    hjust = 0.5,
    size = 4,
    color = "#D62728"
  ) +
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  theme_classic() +
  theme(
    text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.title.x = element_text(
      vjust = -0.2,
      face = "bold",
      size = 12
    ),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(x = "AdCal/REE Percentage",
       y = "Hazard Ratio of 90-day All-Cause Mortality") +
  geom_line(
    data = density_df,
    aes(x = pct, y = density * (2 / 1.5)),
    color = "#FFA500",
    linetype = "dashed",
    size = 1
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . / (2 / 1.5), name = "Probability Density"))
ggsave(
  "rcs.tiff",
  plot = rcs_plot,
  width = 10,
  height = 8,
  dpi = 300
)
# RCS stratified by gender
pred_gender <- Predict(rcs, pct, gender, fun = exp, ref.zero = TRUE)
rcs_gender_plot <- ggplot(pred_gender) +
  geom_line(aes(x = pct, y = yhat, color = gender), size = 1) +
  geom_ribbon(aes(
    x = pct,
    ymin = lower,
    ymax = upper,
    fill = gender
  ), alpha = 0.3) +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = 2) +
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), name = "Gender") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), guide = FALSE) +
  theme_classic() +
  theme(
    text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.title.x = element_text(
      vjust = -0.2,
      face = "bold",
      size = 12
    ),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  labs(x = "AdCal/REE Percentage",
       y = "Hazard Ratio of 90-day All-Cause Mortality")
# RCS stratified by BMI
pred_bmi <- Predict(rcs, pct, bmi_range, fun = exp, ref.zero = TRUE)
rcs_bmi_plot <- ggplot(pred_bmi) +
  geom_line(aes(x = pct, y = yhat, color = bmi_range), size = 1) +
  geom_ribbon(aes(
    x = pct,
    ymin = lower,
    ymax = upper,
    fill = bmi_range
  ), alpha = 0.3) +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = 2) +
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
                     name = "BMI range") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
                    guide = FALSE) +
  theme_classic() +
  theme(
    text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.title.x = element_text(
      vjust = -0.2,
      face = "bold",
      size = 12
    ),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(face = "bold")
  ) +
  labs(x = "AdCal/REE Percentage",
       y = "Hazard Ratio of 90-day All-Cause Mortality")
plot_grid(
  rcs_bmi_plot,
  rcs_gender_plot,
  nrow = 1,
  ncol = 2,
  align = "h",
  labels = "auto"
)
ggsave("rcs_class.tiff",
       width = 12,
       height = 5,
       dpi = 300)

# sensitivity analysis
# include patients with mean_cal_en != 0 (n = 986)
data2 <- filter(anadata, mean_cal_en != 0)
ddist_2 <- datadist(data2)
options(datadist = "ddist_2")
# RCS
rcs_2 <-
  cph(
    Surv(surv_time_90d, expire_flag_90d) ~ rcs(pct, 4) + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data2
  )
anova(rcs_2)
pred_2 <- Predict(rcs_2, pct, fun = exp, ref.zero = TRUE)
min_index_2 <- which.min(pred_2$lower)
min_pct_2 <- pred_2$pct[min_index_2] # x value when y is minimum
indices_hr_1_2 <- which(abs(pred_2$yhat - 1) < 0.01)
pct_hr_1_2 <- pred_2$pct[indices_hr_1_2] # x value when y (HR) = 1.0
# density plot
pct_density_2 <- density(data2$pct)
density_df_2 <-
  data.frame(pct = pct_density_2$x, density = pct_density_2$y)
# RCS plot
rcs_plot_2 <- ggplot(pred_2) +
  geom_line(aes(x = pct, y = yhat), color = "#1f77b4", size = 1) +
  geom_ribbon(aes(x = pct, ymin = lower, ymax = upper),
              fill = "#1f77b4",
              alpha = 0.3) +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = 2) +
  geom_vline(
    xintercept = c(min_pct_2, 0.18, 0.37),
    color = "#D62728",
    linetype = "dashed",
    size = 0.5
  ) +
  annotate(
    "text",
    x = c(min_pct_2, 0.18, 0.37),
    y = 1.1,
    label = paste("x =", c(round(min_pct_2, 3), "0.18", "0.37")),
    vjust = -1,
    hjust = 0.5,
    size = 4,
    color = "#D62728"
  ) +
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  theme_classic() +
  theme(
    text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.title.x = element_text(
      vjust = -0.2,
      face = "bold",
      size = 12
    ),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(x = "AdCal/REE Percentage",
       y = "Hazard Ratio of 90-day All-Cause Mortality") +
  geom_line(
    data = density_df_2,
    aes(x = pct, y = density * (2 / 1.5)),
    color = "#FFA500",
    linetype = "dashed",
    size = 1
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . / (2 / 1.5), name = "Probability Density"))
ggsave(
  "rcs_2.tiff",
  plot = rcs_plot_2,
  width = 10,
  height = 8,
  dpi = 300
)
# include patients with mean_cal_en_10 != 0 and survival >= 10 days (n = 987)
data3 <-
  filter(anadata, mean_cal_en_10 != 0 &
           (surv_time >= 10 | is.na(surv_time)))
ddist_3 <- datadist(data3)
options(datadist = "ddist_3")
# RCS
rcs_3 <-
  cph(
    Surv(surv_time_90d, expire_flag_90d) ~ rcs(pct, 4) + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data3
  )
anova(rcs_3)
pred_3 <- Predict(rcs_3, pct, fun = exp, ref.zero = TRUE)
min_index_3 <- which.min(pred_3$lower)
min_pct_3 <- pred_3$pct[min_index_3] # x value when y is minimum
indices_hr_1_3 <- which(abs(pred_3$yhat - 1) < 0.01)
pct_hr_1_3 <- pred_3$pct[indices_hr_1_3] # x value when y (HR) = 1.0
# density plot
pct_density_3 <- density(data3$pct)
density_df_3 <-
  data.frame(pct = pct_density_3$x, density = pct_density_3$y)
# RCS plot
rcs_plot_3 <- ggplot(pred_3) +
  geom_line(aes(x = pct, y = yhat), color = "#1f77b4", size = 1) +
  geom_ribbon(aes(x = pct, ymin = lower, ymax = upper),
              fill = "#1f77b4",
              alpha = 0.3) +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = 2) +
  geom_vline(
    xintercept = c(min_pct_3, 0.19, 0.38),
    color = "#D62728",
    linetype = "dashed",
    size = 0.5
  ) +
  annotate(
    "text",
    x = c(min_pct_3, 0.19, 0.38),
    y = 1.1,
    label = paste("x =", c(round(min_pct_3, 3), "0.19", "0.38")),
    vjust = -1,
    hjust = 0.5,
    size = 4,
    color = "#D62728"
  ) +
  coord_cartesian(xlim = c(0, 1.0),
                  ylim = c(0, 2.5),
                  expand = FALSE) +
  theme_classic() +
  theme(
    text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.title.x = element_text(
      vjust = -0.2,
      face = "bold",
      size = 12
    ),
    axis.title.y = element_text(vjust = 1, face = "bold", size = 12),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  labs(x = "AdCal/REE Percentage",
       y = "Hazard Ratio of 90-day All-Cause Mortality") +
  geom_line(
    data = density_df_3,
    aes(x = pct, y = density * (2 / 1.5)),
    color = "#FFA500",
    linetype = "dashed",
    size = 1
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . / (2 / 1.5), name = "Probability Density"))
ggsave(
  "rcs_3.tiff",
  plot = rcs_plot_3,
  width = 10,
  height = 8,
  dpi = 300
)

# sensitivity analysis of Cox regression
library(survival)
cox_90_2 <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ group + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data2
  )
summary(cox_90_2)
cox_90_3 <-
  coxph(
    Surv(surv_time_90d, expire_flag_90d) ~ group + age + gender + race
    + bmi_range + cci + mnutric + sofa + vaso + sepsis + imv_flag + rrt + en.tot,
    data = data3
  )
summary(cox_90_3)