# Load data ---------------------------------------------------------------

source(here::here("code","01_load_data.R"))
pre_effsizes_hedges <- fread(here("data","models","pre_effsizes_hedges.csv"), dec=".", sep=",", na="NA")
load(here::here("data","models","models.RData"))

# Baseline risk effects ---------------------------------------------------

lmer_risk_pre_lh_anova <- joint_tests(lmer_risk_pre_lh_Id, data=data_pre_lh) %>% mutate(chi = F.ratio * df1)
lmer_risk_pre_rh_anova <- joint_tests(lmer_risk_pre_rh_Id, data=data_pre_rh) %>% mutate(chi = F.ratio * df1)
rlmer_risk_pre_rh_anova <- joint_tests(rlmer_risk_pre_rh_Id, data=data_pre_rh) %>%  mutate(chi = F.ratio * df1)

lmer_risk_pre_rh_emms <- emmeans(lmer_risk_pre_rh_Id, specs=pairwise~risk|tract)$contrasts %>%
  summary(infer=T) %>% as.data.frame()

# Intervention effects ----------------------------------------------------

rlmer_int_lh_anova <- joint_tests(rlmer_int_s_lh, data=data_rois_lh) %>% mutate(chi = F.ratio * df1)
rlmer_int_rh_anova <- joint_tests(rlmer_int_s_rh, data=data_rois_rh) %>% mutate(chi = F.ratio * df1)

rlmer_int_lh_emms <- emmeans(rlmer_int_s_lh, specs=pairwise~time|intervention)$contrasts %>%
  summary(infer=T) %>% as.data.frame()
rlmer_int_rh_emms <- emmeans(rlmer_int_s_rh, specs=pairwise~time|intervention|tract)$contrasts %>%
  summary(infer=T) %>% as.data.frame()

es_raf_tc <- (prepost_es %>% filter(tract %in% "Right_Arcuate") %>% select(hedges_Gav_tc))$hedges_Gav_tc
es_rslf_tc <- (prepost_es %>% filter(tract %in% "Right_SLF") %>% select(hedges_Gav_tc))$hedges_Gav_tc
es_rifof_gg <- (prepost_es %>% filter(tract %in% "Right_IFOF") %>% select(hedges_Gav_gg))$hedges_Gav_gg
es_rilf_gg <- (prepost_es %>% filter(tract %in% "Right_ILF") %>% select(hedges_Gav_gg))$hedges_Gav_gg

