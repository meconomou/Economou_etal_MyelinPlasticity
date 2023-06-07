# Load data ---------------------------------------------------------------

source(here::here("code","03_small_data.R"))

# Formatting --------------------------------------------------------------

# Colors
myColorsGroups<- brewer.pal(3,"Dark2")
myShapesGroups<- c(15,17,19)
names(myColorsGroups)<-c("GG_NE","ActiveControl","CONTROL")
names(myShapesGroups)<-c("GG_NE","ActiveControl","CONTROL")
groupcolor<-scale_colour_manual(name="Group",
                                values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                labels=c("GG-FL","Active control","Typical control"))
groupfill<-scale_fill_manual(name="Group",
                             values=myColorsGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                             labels=c("GG-FL","Active control","Typical control"))
groupshape <- scale_shape_manual(name="Group",
                                 values=myShapesGroups, limits=c("GG_NE", "ActiveControl", "CONTROL"),
                                 labels=c("GG-FL","Active control","Typical control"))

greyGroups <- grey.colors(3, start = 0.2, end = 0.7)
names(greyGroups) <- c("GG_NE","ActiveControl","CONTROL")

myColorsRisk <- c("#1F78B4", "#33A02C")
names(myColorsRisk)<-c("no","yes")
riskcolor<-scale_colour_manual(name="Group",
                               values=myColorsRisk, limits=c("no", "yes"),
                               labels=c("Typical control","At-risk"))
riskfill<-scale_fill_manual(name="Group",
                            values=myColorsRisk, limits=c("no", "yes"),
                            labels=c("Typical control","At-risk"))


# Axes
time.labs <- c("pre-test", "post-test")
names(time.labs) <- c("ses-01", "ses-02")
group.labs <- c("GG-FL", "Active control", "Typical control")
names(group.labs) <- c("GG_NE", "ActiveControl", "CONTROL")
risk.labs <- c("Typical control", "At-risk")
names(risk.labs) <- c("no", "yes")
tract.labs <- c("L AFdirect", "L AFanterior", "L IFOF","L ILF",
                "R AFdirect", "R AFanterior", "R IFOF","R ILF")

names(tract.labs) <- c("Left_Arcuate", "Left_SLF", "Left_IFOF", "Left_ILF",
                       "Right_Arcuate", "Right_SLF", "Right_IFOF","Right_ILF")

mytheme <- theme_bw() +
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

jitter <- position_jitter(width=0.05, seed=123)
dodge <- position_dodge(width = 0.4)

# Table 1 -----------------------------------------------------------------
# code to reproduce the participants table

analyzed_ids <- data_rois_voxels %>%
  group_by(id, time, intervention) %>%
  summarize(mean_RMSt=mean(mean_RMSt, na.rm=T)) %>%
  ungroup() %>%
  mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
  pivot_wider(names_from=c(time), values_from=mean_RMSt)

data_rois_voxels %>% group_by(intervention) %>%
  summarize(n=length(unique(id)))

table_dat <- merge(analyzed_ids, subject_data, by=c("id","intervention"), all.x=T)

table_participants <- table_dat %>%
  mutate(expectedHours=(15*6*12)/60) %>%
  mutate(proportionPlayed=(HoursPlayed/expectedHours)*100) %>%
  mutate(gamePeriod_weeks=gamePeriod_days/7) %>%
  select(intervention, sex, FR, iq, LQ, pre_ageM, post_ageM, M_SES, HoursPlayed, gamePeriod_weeks, proportionPlayed) %>%
  mutate(sex=recode_factor(sex, "Jongen"="M","Meisje"="F")) %>%
  mutate(LQ=recode_factor(LQ, "LH"="Left-handed","RH"="Right-handed","AD"="Ambidextrous")) %>%
  mutate(M_SES=recode_factor(M_SES, "1"="Low","2"="Middle","3"="High")) %>%
  rename(group=intervention) %>%
  mutate(group=ifelse(group=="GG_NE","GG-FL",
                      ifelse(group=="ActiveControl","Active Control","Typical Control"))) %>%
  mutate_if(is.character, ~factor(.)) %>%
  mutate(group=forcats::fct_relevel(group, "GG-FL", "Active Control")) %>%
  tbl_summary(by=group,
              type = list(c(FR, sex) ~ "dichotomous"),
              value = list(sex ~ "F"),
              statistic = list(all_continuous() ~ c("{median} ({min} - {max})"),
                               all_categorical() ~ "{n}"),
              digits = list(c(pre_ageM, post_ageM) ~ 0, c(iq, HoursPlayed, gamePeriod_weeks, proportionPlayed) ~ 0),
              label=list(pre_ageM ~ "Age at pre-test MRI (months)", post_ageM ~ "Age at post-test MRI (months)",
                         iq ~ "Non-verbal intelligence", FR ~ "Familial risk",
                         LQ ~ "Handedness", sex ~ "Sex (female/male)", M_SES ~ "Socioeconomic status",
                         HoursPlayed ~ "Training exposure (hours)",
                         proportionPlayed ~ "Proportion of completed intervention (%)",
                         gamePeriod_weeks ~ "Training period (weeks)")) %>%
  add_p(pvalue_fun = ~papaja::printp(.x))

# reformat table for word output (publication)
table_participants_tbl <- table_participants %>%
  gtsummary::as_tibble() %>%
  rename(Variable=1) %>% rename(GG=2) %>% rename(AC=3) %>% rename(TC=4) %>%
  mutate(GG = ifelse(Variable == "Sex (female/male)", paste0(GG,"/",29-as.numeric(GG)),GG)) %>%
  mutate(AC = ifelse(Variable == "Sex (female/male)", paste0(AC,"/",23-as.numeric(AC)),AC)) %>%
  mutate(TC = ifelse(Variable == "Sex (female/male)", paste0(TC,"/",27-as.numeric(TC)),TC)) %>%
  add_row(Variable = "Left/right/ambidextrous", GG = "2/26/1", AC = "2/19/2", TC = "2/22/3") %>%
  add_row(Variable = "Low/middle/high/unknown", GG = "7/11/11/0", AC = "7/11/4/1", TC = "3/10/14/0") %>%
  filter(!Variable %in% c("Left-handed","Right-handed","Ambidextrous",
                          "Low","Middle","High","Unknown")) %>%
  mutate(TC = ifelse(Variable == "Training exposure (hours)",NA,TC)) %>%
  mutate(TC = ifelse(Variable == "Training period (weeks)",NA,TC)) %>%
  mutate(TC = ifelse(Variable == "Proportion of completed intervention (%)",NA,TC)) %>%
  mutate(Variable = factor(Variable, levels=c("Sex (female/male)","Familial risk",
                                              "Non-verbal intelligence","Handedness",
                                              "Left/right/ambidextrous","Age at pre-test MRI (months)",
                                              "Age at post-test MRI (months)", "Socioeconomic status",
                                              "Low/middle/high/unknown","Training exposure (hours)",
                                              "Training period (weeks)","Proportion of completed intervention (%)"))) %>%
  arrange(Variable) %>%
  rename(`GraphoGame-Flemish (n = 29)`=2) %>%
  rename(`Active control (n = 23)`=3) %>%
  rename(`Typical control (n = 27)` = 4) %>%
  rename(`p-value`=5)

table_participants_flex <- flextable::flextable(table_participants_tbl) %>%
  flextable::autofit(add_w = 0, add_h = 0) %>%
  flextable::theme_booktabs(bold_header = T) %>%
  flextable::footnote(., part="header",
                      i=1,j=2:4,
                      value=as_paragraph("Counts (n) or Median (range)"),
                      ref_symbols = "a") %>%
  flextable::footnote(., part="body",inline=T,
                      i=4,j=1,
                      value=as_paragraph("Based on a subset of questions adapted from the Edinburgh Handedness Inventory (Oldfield, 1971)"),
                      ref_symbols = "b") %>%
  flextable::footnote(., part="body",inline=T,
                      i=8,j=1,
                      value=as_paragraph("Based on maternal educational level (low = no extra degree after secondary school; middle = professional bachelor/academic bachelor; high = Master or PhD)."),
                      ref_symbols = "c") %>%
  flextable::footnote(., part="body",inline=T,
                      i=12,j=1,
                      value=as_paragraph("The proportion of completed intervention is calculated as the number of hours spent on-task divided by the total amount of hours children were instructed to play during 12 weeks (estimated at 18 h)."),
                      ref_symbols = "d") %>%
  flextable::add_header_row(top=T, values=c("","Group",""), colwidths=c(1,3,1)) %>%
  set_table_properties(layout = "autofit")



# Table 2 -----------------------------------------------------------------

# code to reproduce table 2: number of analyzed datasets per group, timepoint and tract.
n_datasets_table <- data_rois_voxels %>%
  group_by(intervention, time, tract) %>%
  summarize(n=length(unique(id))) %>%
  mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  arrange(intervention, time) %>%
  pivot_wider(names_from=c(intervention, time), values_from=n) %>%
  mutate(tract=recode_factor(tract, "Left_Arcuate"="L AFdirect","Left_SLF"="L AFanterior","Left_IFOF"="L IFOF","Left_ILF"="L ILF",
                             "Right_Arcuate"="R AFdirect","Right_SLF"="R AFanterior","Right_IFOF"="R IFOF","Right_ILF"="R ILF")) %>%
  arrange(tract) %>%
  flextable() %>%
  autofit() %>%
  flextable::theme_booktabs(bold_header = T) %>%
  set_header_labels(values = list(tract = "Tract",
                                  GG_NE_pre = "pre", GG_NE_post = "post",
                                  ActiveControl_pre = "pre", ActiveControl_post = "post",
                                  CONTROL_pre = "pre", CONTROL_post = "post")) %>%
  flextable::add_header_row(top=T, values=c("","GG-FL","Active Control","Typical Control"), colwidths=c(1,2,2,2)) %>%
  align(align = "center", part="header") %>%
  align(align = "center", i=1:8, j=2:7) %>%
  set_table_properties(layout = "autofit")


# Table 3 -----------------------------------------------------------------
# code to reproduce Table 3: MWF baseline values and effect sizes for risk-related comparisons

# add raw MWF values to eff size table
pre_rawvals <- data_pre %>%
  group_by(tract, risk) %>%
  summarise(mean=mean(MWF, na.rm=T),
            sd=sd(MWF, na.rm=T)) %>%
  pivot_wider(names_from=risk, values_from=c(mean, sd)) %>%
  mutate_at(vars(mean_no:sd_yes), .funs = funs(. * 100))

pre_effsizes_table <- pre_effsizes_hedges %>%
  select(tract, n1, n2, effsize, conf.low, conf.high) %>%
  left_join(x =., y= {pre_rawvals}) %>%
  mutate(effsize = round(effsize, digits=2)) %>%
  mutate(across(c(mean_yes, mean_no, sd_yes, sd_no), round, 2)) %>%
  mutate(ci = paste0(conf.low, ", ", conf.high)) %>%
  select(-c(conf.low, conf.high)) %>%
  unite(c(mean_yes, sd_yes), col="MWF_yes", sep=" \U00B1 ") %>%
  unite(c(mean_no, sd_no), col="MWF_no", sep=" \U00B1 ") %>%
  mutate(tract=factor(tract, levels=c("Left_Arcuate","Left_SLF","Left_IFOF","Left_ILF",
                                      "Right_Arcuate","Right_SLF","Right_IFOF","Right_ILF"))) %>%
  # mutate(tract=recode_factor(tract, "Left_Arcuate"="L AFdirect","Left_SLF"="L AFanterior","Left_IFOF"="L IFOF","Left_ILF"="L ILF",
  #                            "Right_Arcuate"="R AFdirect","Right_SLF"="R AFanterior","Right_IFOF"="R IFOF","Right_ILF"="R ILF")) %>%
  arrange(tract) %>%
  select(tract, n2, MWF_yes, n1, MWF_no, effsize, ci) %>%
  flextable() %>%
  flextable::theme_booktabs(bold_header = T) %>%
  flextable::add_header_row(top=T, values=c("","At-risk","Typical Control",""), colwidths=c(1,2,2,2)) %>%
  set_header_labels(values = list(tract = "Tract",
                                  n2 = "N", n1 = "N", ni_tc = "N", ci="95% CI",
                                  MWF_no = "MWF (%)", MWF_yes = "MWF (%)", effsize="Hedge's g")) %>%
  # set_header_labels(values = list(tract = "Tract",
  #                                 n1 = "Typical Control", n2 = "At-risk",
  #                                 effsize = "Hedge's g", ci = "95% CI")) %>%
  compose(part="body", i=1, j=1, value=as_paragraph("L AF",as_sub("direct"))) %>%
  compose(part="body", i=2, j=1, value=as_paragraph("L AF",as_sub("anterior"))) %>%
  compose(part="body", i=3, j=1, value=as_paragraph("L IFOF")) %>%
  compose(part="body", i=4, j=1, value=as_paragraph("L ILF")) %>%
  compose(part="body", i=5, j=1, value=as_paragraph("R AF",as_sub("direct"))) %>%
  compose(part="body", i=6, j=1, value=as_paragraph("R AF",as_sub("anterior"))) %>%
  compose(part="body", i=7, j=1, value=as_paragraph("R IFOF")) %>%
  compose(part="body", i=8, j=1, value=as_paragraph("R ILF")) %>%
  # flextable::add_header_row(top=T, values=c("","GG-FL","Active Control","Typical Control"), colwidths=c(1,2,2,2)) %>%
  align(align = "center", part="header") %>%
  align(align = "center", i=1:8, j=2:7) %>%
  autofit() %>%
  set_table_properties(layout = "autofit")


# Table 4 -----------------------------------------------------------------
# code to reproduce Tabe 4: effect sizes for within-group changes in MWF across sessions

prepost_effsizes_table <- prepost_es %>%
  select(tract, ni_gg, hedges_Gav_gg, CLES_gg,
         ni_ac, hedges_Gav_ac, CLES_ac,
         ni_tc, hedges_Gav_tc, CLES_tc) %>%
  mutate(tract=factor(tract, levels=c("Left_Arcuate","Left_SLF","Left_IFOF","Left_ILF",
                                      "Right_Arcuate","Right_SLF","Right_IFOF","Right_ILF"))) %>%
  arrange(tract) %>%
  flextable() %>%
  flextable::theme_booktabs(bold_header = T) %>%
  flextable::autofit() %>%
  flextable::add_header_row(top=T, values=c("","GG-Flemish","Active Control","Typical Control"), colwidths=c(1,3,3,3)) %>%
  set_header_labels(values = list(tract = "Tract",
                                  ni_gg = "N", ni_ac = "N", ni_tc = "N",
                                  CLES_gg = "CLES", CLES_ac = "CLES", CLES_tc = "CLES")) %>%
  compose(part="header", j=c("hedges_Gav_gg","hedges_Gav_ac","hedges_Gav_tc"), value=as_paragraph("g",as_sub("av"))) %>%
  compose(part="body", i=1, j=1, value=as_paragraph("L AF",as_sub("direct"))) %>%
  compose(part="body", i=2, j=1, value=as_paragraph("L AF",as_sub("anterior"))) %>%
  compose(part="body", i=3, j=1, value=as_paragraph("L IFOF")) %>%
  compose(part="body", i=4, j=1, value=as_paragraph("L ILF")) %>%
  compose(part="body", i=5, j=1, value=as_paragraph("R AF",as_sub("direct"))) %>%
  compose(part="body", i=6, j=1, value=as_paragraph("R AF",as_sub("anterior"))) %>%
  compose(part="body", i=7, j=1, value=as_paragraph("R IFOF")) %>%
  compose(part="body", i=8, j=1, value=as_paragraph("R ILF")) %>%
  colformat_double(digits=2) %>%
  set_table_properties(layout = "autofit") %>%
  ftExtra::colformat_md(md_extensions = "+hard_line_breaks") %>%
  ftExtra::colformat_md() %>%
  align(align = "center", i=1:8, j=2:10) %>%
  align(align = "center", part="header")


# Figure 1 ----------------------------------------------------------------

# Gathering data for figure
rlmer_int_lh_emms <- as.data.frame(ggemmeans(rlmer_int_s_lh, terms=c("time","intervention"))) %>%
  rename(intervention=group) %>%
  rename(time=x) %>%
  rename(MWF=predicted)

rlmer_int_lh_emms_all <- as.data.frame(ggemmeans(rlmer_int_s_lh, terms=c("time","intervention","tract"))) %>%
  rename(intervention=group) %>%
  rename(time=x) %>%
  rename(MWF=predicted) %>%
  rename(tract=facet) %>%
  mutate(tract = factor(tract, levels=c("Left_Arcuate","Left_SLF", "Left_IFOF","Left_ILF")))

# Code for plotting
plot_indivchange_lh <- data_rois_lh %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  ggplot(aes(x=time, y=MWF)) +
  geom_point(aes(group=id, colour=intervention), alpha=.08) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(inherit.aes=F, data=rlmer_int_lh_emms_all,
            aes(x=time, y=MWF, colour=intervention, group=intervention), size=1) +
  geom_errorbar(inherit.aes=F, data=rlmer_int_lh_emms_all,
                aes(x=time, y=MWF, ymin=conf.low, ymax=conf.high, colour=intervention, group=intervention),
                size=0.6, width=0.2) +
  facet_grid(intervention~tract, labeller=labeller(intervention=group.labs, tract=tract.labs)) +
  labs(x="Session", y="MWF") +
  scale_x_discrete(labels=c("pre","post")) +
  mytheme +
  groupcolor +
  theme(legend.position="none")

# Write figure to file. Dimensions defined to fit the manuscript/journal guidelines.

tiff(here("figs","Figure1.tiff"),
     units="mm", width=100, height=170, res=300)
plot_indivchange_lh
dev.off()

ggsave(here("figs","Figure1.png"), plot=plot_indivchange_lh,
       units="mm", width=100, height=170, dpi=300)


# Figure 2 ----------------------------------------------------------------
# Gathering data for figure
rlmer_int_rh_emms <- as.data.frame(ggemmeans(rlmer_int_s_rh, terms=c("time","intervention"))) %>%
  rename(intervention=group) %>%
  rename(time=x) %>%
  rename(MWF=predicted)

rlmer_int_rh_emms_all <- as.data.frame(ggemmeans(rlmer_int_s_rh, terms=c("time","intervention","tract"))) %>%
  rename(intervention=group) %>%
  rename(time=x) %>%
  rename(MWF=predicted) %>%
  rename(tract=facet)

# Code for plotting
plot_indivchange_rh <- data_rois_rh %>%
  mutate(intervention=factor(intervention, levels=c("GG_NE","ActiveControl","CONTROL"))) %>%
  ggplot(aes(x=time, y=MWF)) +
  geom_point(aes(group=id, colour=intervention), alpha=.08) +
  geom_line(aes(group=id, colour=intervention), alpha=.2) +
  geom_line(inherit.aes=F, data=rlmer_int_rh_emms_all,
            aes(x=time, y=MWF, colour=intervention, group=intervention), size=1) +
  geom_errorbar(inherit.aes=F, data=rlmer_int_rh_emms_all,
                aes(x=time, y=MWF, ymin=conf.low, ymax=conf.high, colour=intervention, group=intervention),
                size=0.6, width=0.2) +
  facet_grid(intervention~tract, labeller=labeller(intervention=group.labs, tract=tract.labs)) +
  scale_y_continuous(limits=c(0.02, 0.12)) +
  labs(x="Session", y="MWF") +
  scale_x_discrete(labels=c("pre","post")) +
  groupcolor +
  mytheme +
  theme(legend.position="none")

# Write figure to file. Dimensions defined to fit the manuscript/journal guidelines.
tiff(here("figs","Figure2.tiff"),
     units="mm", width=100, height=170, res=300)
plot_indivchange_rh
dev.off()

ggsave(here("figs","Figure2.png"), plot=plot_indivchange_rh,
       units="mm", width=100, height=170, dpi=300)
