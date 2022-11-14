# Load data ---------------------------------------------------------------

source(here::here("code","01_load_data.R"))

# Baseline risk models -----------------------------------------------------

lmer_risk_pre_lh_Id <- lmer(MWF ~ risk*tract + (1|id), data=data_pre_lh)

lmer_risk_pre_rh_Id <- lmer(MWF ~ risk*tract + (1|id), data=data_pre_rh)

rlmer_risk_pre_rh_Id <- robustlmm::rlmer(MWF ~ risk*tract + (1|id), data=data_pre_rh) # fit robust lmer for rh


# Baseline risk effect sizes ----------------------------------------------

pre_effsizes_hedges <- data_pre %>%
  group_by(tract) %>%
  rstatix::cohens_d(MWF ~ risk, ci=T, hedges.correction = T)

fwrite(pre_effsizes_hedges, file=here("data","models","pre_effsizes_hedges.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")


# Intervention effects ----------------------------------------------------

# data_rois_lh <- data_rois_voxels %>% filter(tract %in% c("Left_Arcuate","Left_SLF","Left_IFOF","Left_ILF"))
# data_rois_rh <- data_rois_voxels %>% filter(tract %in% c("Right_Arcuate","Right_SLF","Right_IFOF","Right_ILF"))
# data_rois_rh_noRAF <- data_rois_voxels %>% filter(tract %in% c("Right_SLF","Right_IFOF","Right_ILF"))

lmer_int_s_lh <- lmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_lh)
lmer_int_s_rh <- lmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_rh)

# fit robust models

rlmer_int_s_lh <- robustlmm::rlmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_lh)
rlmer_int_s_rh <- robustlmm::rlmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_rh)
# rlmer_int_s_rh_noAF <- robustlmm::rlmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_rh_noRAF)

# Intervention effect sizes -----------------------------------------------

gg_dat <- data_rois_voxels %>%
  filter(intervention %in% "GG_NE") %>%
  select(id, time, intervention, tract, MWF) %>%
  mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
  pivot_wider(id_cols=c(id, tract), values_from=MWF, names_from=c(time)) %>%
  group_by(tract) %>%
  mutate(ri = cor.test(pre, post)$estimate) %>%
  group_by(tract, ri) %>%
  summarise(m_pre = mean(pre, na.rm=T),
            sd_pre = sd(pre, na.rm=T),
            m_post = mean(post, na.rm=T),
            sd_post = sd(post, na.rm=T),
            ni = length(unique(id)))

ac_dat <- data_rois_voxels %>%
  filter(intervention %in% "ActiveControl") %>%
  select(id, time, intervention, tract, MWF) %>%
  mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
  pivot_wider(id_cols=c(id, tract), values_from=MWF, names_from=c(time)) %>%
  group_by(tract) %>%
  mutate(ri = cor.test(pre, post)$estimate) %>%
  group_by(tract, ri) %>%
  summarise(m_pre = mean(pre, na.rm=T),
            sd_pre = sd(pre, na.rm=T),
            m_post = mean(post, na.rm=T),
            sd_post = sd(post, na.rm=T),
            ni = length(unique(id)))

tc_dat <- data_rois_voxels %>%
  filter(intervention %in% "CONTROL") %>%
  select(id, time, intervention, tract, MWF) %>%
  mutate(time=recode_factor(time, "ses-01"="pre","ses-02"="post")) %>%
  pivot_wider(id_cols=c(id, tract), values_from=MWF, names_from=c(time)) %>%
  group_by(tract) %>%
  mutate(ri = cor.test(pre, post)$estimate) %>%
  group_by(tract, ri) %>%
  summarise(m_pre = mean(pre, na.rm=T),
            sd_pre = sd(pre, na.rm=T),
            m_post = mean(post, na.rm=T),
            sd_post = sd(post, na.rm=T),
            ni = length(unique(id)))


datT <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=gg_dat)
datC <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=ac_dat)
datCONTR <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=tc_dat)

datT$group <- "gg"; datC$group <- "ac"; datCONTR$group <- "tc"

dat_es <- rbind(datT, datC, datCONTR)
d_ppc <- dat_es %>%
  pivot_wider(id_cols=c(tract),
              values_from=c(ri, m_pre, m_post, sd_pre, sd_post, ni, yi, vi),
              names_from=group) %>%
  mutate(yi_ggac = yi_gg - yi_ac) %>% # gg and active control
  mutate(vi_ggac = vi_gg - vi_ac) %>%
  mutate(sd_pool_ggac = sqrt(((ni_gg - 1)*sd_pre_gg^2 + (ni_ac - 1)*sd_pre_ac^2) / (ni_gg + ni_ac - 2))) %>%
  mutate(yi2_ggac = metafor:::.cmicalc(ni_gg + ni_ac - 2) * ((m_post_gg - m_pre_gg) - (m_post_ac - m_pre_ac)) / sd_pool_ggac) %>%
  mutate(vi2_ggac = 2*(1-ri_gg) * (1/ni_gg + 1/ni_ac) + yi2_ggac^2 / (2*(ni_gg + ni_ac))) %>%
  mutate(dppc2_ggac_ci_low = yi2_ggac - 1.96*vi2_ggac) %>%
  mutate(dppc2_ggac_ci_high = yi2_ggac + 1.96*vi2_ggac) %>%
  mutate(dppc1_ggac_ci_low = yi_ggac - 1.96*vi_ggac) %>%
  mutate(dppc1_ggac_ci_high = yi_ggac + 1.96*vi_ggac)

d_ppc_table <- d_ppc %>%
  select(tract, yi_gg, yi_ac, vi_gg, vi_ac, yi_ggac, vi_ggac, yi2_ggac, vi2_ggac, ri_gg, ri_ac, ni_gg, ni_ac)

# prepost_corrs <- data_rois %>%
#   select(id, time, intervention, tract, MWF) %>%
#   # group_by(time, intervention, tract) %>%
#   # summarize(mean=mean(MWF, na.rm=T),
#   #           sd=sd(MWF, na.rm=T)) %>%
#   mutate(time=recode_factor(time, "ses-01"="pre", "ses-02"="post")) %>%
#   pivot_wider(values_from=c(MWF), names_from=time) %>%
#   ggplot(aes(x=pre, y=post, color=intervention)) +
#   geom_point() +
#   facet_wrap(~tract) +
#   stat_cor() +
#   geom_smooth(method="lm")

# file:///C:/Users/u0119203/Downloads/fcac008_supplementary_data.pdf

# formula for Hedge's g from the paper by Lakens (2013) - Gav and just Hedge's g as standardized mean difference

prepost_es_comparison <- d_ppc %>%
  mutate(mdiff_gg = abs(m_post_gg - m_pre_gg)) %>%
  mutate(sdiff_gg = sqrt(sd_post_gg^2 + sd_pre_gg^2-2*ri_gg*sd_post_gg*sd_pre_gg)) %>%
  mutate(cohens_Dz_gg = mdiff_gg/sdiff_gg) %>%
  mutate(cohens_Dav_gg = mdiff_gg/sqrt((sd_post_gg^2 + sd_pre_gg^2)/2)) %>%
  mutate(hedges_Gav_gg = cohens_Dav_gg*(1-(3/(4*(ni_gg-1)-1)))) %>%
  mutate(g_gg =
           (m_post_gg - m_pre_gg)/
           sqrt((sd_pre_gg^2 + sd_post_gg^2)/2)) %>%
  mutate(g_gg = g_gg*(1-(3/(8*ni_gg-9)))) %>%
  select(tract, cohens_Dz_gg, cohens_Dav_gg, hedges_Gav_gg, yi_gg, g_gg)

prepost_es <- d_ppc %>%
  mutate(mdiff_gg = abs(m_post_gg - m_pre_gg)) %>% # gg
  mutate(sdiff_gg = sqrt(sd_post_gg^2 + sd_pre_gg^2-2*ri_gg*sd_post_gg*sd_pre_gg)) %>%
  # mutate(SEdiff_gg = sqrt(sd_post_gg^2 + sd_pre_gg^2-2*ri_gg*sd_post_gg*sd_pre_gg)) %>%
  mutate(cohens_Dz_gg = mdiff_gg/sdiff_gg) %>%
  mutate(cohens_Dav_gg = mdiff_gg/sqrt((sd_post_gg^2 + sd_pre_gg^2)/2)) %>%
  mutate(hedges_Gav_gg = cohens_Dav_gg*(1-(3/(4*(ni_gg-1)-1)))) %>%
  mutate(CLES_gg = pnorm(mdiff_gg/sdiff_gg)) %>%
  mutate(mdiff_ac = abs(m_post_ac - m_pre_ac)) %>% # active control
  mutate(sdiff_ac = sqrt(sd_post_ac^2 + sd_pre_ac^2-2*ri_ac*sd_post_ac*sd_pre_ac)) %>%
  mutate(cohens_Dz_ac = mdiff_ac/sdiff_ac) %>%
  mutate(cohens_Dav_ac = mdiff_ac/sqrt((sd_post_ac^2 + sd_pre_ac^2)/2)) %>%
  mutate(hedges_Gav_ac = cohens_Dav_ac*(1-(3/(4*(ni_ac-1)-1)))) %>%
  mutate(CLES_ac = pnorm(mdiff_ac/sdiff_ac)) %>%
  mutate(mdiff_tc = abs(m_post_tc - m_pre_tc)) %>% # typical control
  mutate(sdiff_tc = sqrt(sd_post_tc^2 + sd_pre_tc^2-2*ri_tc*sd_post_tc*sd_pre_tc)) %>%
  mutate(cohens_Dz_tc = mdiff_tc/sdiff_tc) %>%
  mutate(cohens_Dav_tc = mdiff_tc/sqrt((sd_post_tc^2 + sd_pre_tc^2)/2)) %>%
  mutate(hedges_Gav_tc = cohens_Dav_tc*(1-(3/(4*(ni_tc-1)-1)))) %>%
  mutate(CLES_tc = pnorm(mdiff_tc/sdiff_tc))


# Write model outputs -----------------------------------------------------

save(lmer_risk_pre_lh_Id, lmer_risk_pre_rh_Id, rlmer_risk_pre_rh_Id,
     lmer_int_s_lh, rlmer_int_s_lh, lmer_int_s_rh, rlmer_int_s_rh,
     d_ppc, prepost_es,
     file = here("data","models","models.RData"))


