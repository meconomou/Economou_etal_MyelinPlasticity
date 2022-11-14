# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggeffects)
library(gtsummary)
library(performance)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(papaja)
library(effects)
library(RColorBrewer)
library(lm.beta)
library(flextable)

# Helper functions --------------------------------------------------------

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

lm2pred <- function(m){
  dat <- do.call(rbind, (lapply(m, ggeffects::ggemmeans(terms=c("MWF_percent_change")))))
  dat <- do.call(rbind, dat)
  dat<-as.data.frame(dat)
  dat <- dat %>%
    # filter(!group %in% c("age_years")) %>%
    mutate_if(is.character,~factor(.))
}

# Load data ---------------------------------------------------------------

motion <- fread(here("data","tidy","motion.csv"), header=T)
subject_data <-fread(here::here("data","tidy","subject_data.csv"))
data_rois_voxels <-fread(here::here("data","tidy","data_rois_voxels.csv"))
data_wide <-fread(here::here("data","tidy","data_wide.csv"))
age_long <-fread(here::here("data","tidy","age_long.csv"))

data_pre <- data_rois_voxels %>%
  # filter(!mean_RMSt > 2.2) %>%  # try with motion exclusion
  filter(time %in% "ses-01") %>%
  mutate_at("risk", ~as.factor(.)) %>%
  mutate_at("tract", ~as.factor(.)) %>%
  droplevels() %>%
  mutate_if(is.character, ~factor(.))

data_pre_lh <- data_pre %>% filter(tract %in% c("Left_Arcuate","Left_SLF", "Left_IFOF","Left_ILF"))
data_pre_rh <- data_pre %>% filter(tract %in% c("Right_Arcuate","Right_SLF", "Right_IFOF","Right_ILF"))

data_rois_lh <- data_rois_voxels %>% filter(tract %in% c("Left_Arcuate","Left_SLF","Left_IFOF","Left_ILF"))
data_rois_rh <- data_rois_voxels %>% filter(tract %in% c("Right_Arcuate","Right_SLF","Right_IFOF","Right_ILF"))
