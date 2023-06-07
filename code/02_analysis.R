# Load data ---------------------------------------------------------------

source(here::here("code","01_load_data.R"))

# Baseline risk models -----------------------------------------------------

# fitting LMEs to assess the (interaction) effect of risk and tract on MWF. One model per hemisphere.

lmer_risk_pre_lh_Id <- lmer(MWF ~ risk*tract + (1|id), data=data_pre_lh)

lmer_risk_pre_rh_Id <- lmer(MWF ~ risk*tract + (1|id), data=data_pre_rh)

# Baseline risk effect sizes ----------------------------------------------

pre_effsizes_hedges <- data_pre %>%
  group_by(tract) %>%
  rstatix::cohens_d(MWF ~ risk, ci=T, hedges.correction = T)

fwrite(pre_effsizes_hedges, file=here("data","models","pre_effsizes_hedges.csv"),
       col.names=TRUE, row.names=FALSE, dec=".", sep=",", na="NA")


# Intervention effects ----------------------------------------------------

# fitting robust LMEs to assess the (interaction) effect of intervention group, session and tract on MWF.
# One model per hemisphere.

rlmer_int_s_lh <- robustlmm::rlmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_lh)
rlmer_int_s_rh <- robustlmm::rlmer(MWF ~ time*intervention*tract + (1|id), data=data_rois_rh)

# Intervention effect sizes -----------------------------------------------

# In this section we calculate effect sizes for pre-to-post MWF changes in individual tracts.
# The measure reported is Hedge’s gav, which is an appropriate metric for standardized mean change in dependent samples.
# The formula for Hedge's gav is defined in the paper by Lakens (2013). https://doi.org/10.3389/fpsyg.2013.00863

# First some data preparation (e.g. calculating pre-post correlation)
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

#
datT <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=gg_dat)
datC <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=ac_dat)
datCONTR <- metafor::escalc(measure="SMCR", m1i=m_post, m2i=m_pre, sd1i=sd_pre, ni=ni, ri=ri, data=tc_dat)
datT$group <- "gg"; datC$group <- "ac"; datCONTR$group <- "tc"

# Putting everything together to create the dataframe that will be used for calculation of the effect sizes.
dat_es <- rbind(datT, datC, datCONTR) %>%
  pivot_wider(id_cols=c(tract),
              values_from=c(ri, m_pre, m_post, sd_pre, sd_post, ni, yi, vi),
              names_from=group)

# This is the dataframe that will be used for the manuscript table
prepost_es <- dat_es %>%
  mutate(mdiff_gg = abs(m_post_gg - m_pre_gg)) %>% # gg
  mutate(sdiff_gg = sqrt(sd_post_gg^2 + sd_pre_gg^2-2*ri_gg*sd_post_gg*sd_pre_gg)) %>%
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

# CLES = The probability that an individual has a higher value on one measurement than the other (See Lakens, 2013).


# Write model outputs -----------------------------------------------------

save(lmer_risk_pre_lh_Id, lmer_risk_pre_rh_Id, # baseline models
     rlmer_int_s_lh, rlmer_int_s_rh, # pre-post models
     dat_es, prepost_es, # effect sizes
     file = here("data","models","models.RData"))

