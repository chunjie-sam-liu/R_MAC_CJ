
# library -----------------------------------------------------------------

library(magrittr)
library(survival)
library(survminer)

# path --------------------------------------------------------------------

path <- '/Users/liuchunjie/Downloads'


# load data ---------------------------------------------------------------

status <- readxl::read_xlsx(path = file.path(path, 'tj-prognosis.xlsx'))
plt_cnts <- readxl::read_xlsx(path = file.path(path, 'plt-survival.xlsx'), sheet = 4)
plt_size <- readxl::read_xlsx(path = file.path(path, 'plt-survival.xlsx'), sheet = 3)
plt_pct <- readxl::read_xlsx(path = file.path(path, 'plt-survival.xlsx'), sheet = 5)


# analysis ----------------------------------------------------------------

status %>% 
  dplyr::inner_join(plt_cnts, by = 'VisitId') ->
  sts_plt_cnts

status %>% 
  dplyr::inner_join(plt_size, by = 'VisitId') ->
  sts_plt_size

status %>% 
  dplyr::inner_join(plt_pct, by = 'VisitId') ->
  sts_plt_pct


sts_plt_cnts %>% 
  dplyr::filter(!is.na(is_dead)) %>% 
  dplyr::mutate(OS = as.numeric(OS), PFS = as.numeric(PFS), is_dead = ifelse(is_dead == 'yes', 1, 0)) %>% 
  dplyr::filter(!is.na(PFS), !is.na(OS)) %>% 
  dplyr::mutate(time = PFS, status = is_dead) %>% 
  dplyr::select(VisitId, time, status, Result, AbnormalIndicator) %>% 
  dplyr::filter(AbnormalIndicator != 'l') %>%
  dplyr::mutate(group = as.factor(AbnormalIndicator)) %>% 
  dplyr::filter(time > 9) %>% 
  dplyr::mutate(time  = time - 9) ->
  ana_ready

fit <- survfit(Surv(time, status) ~ group, data = ana_ready)

ggsurvplot(
  fit, 
  data = ana_ready, 
  xlab = 'Months',
  ylab = 'Proportion Alive (%)',
  title = 'Progression-free survival with platelet count',
  size = 1,                 # change line size
  palette = 
    c("red", "blue"),# custom color palettes
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Thrombocytosis", "Normal"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
) -> plot_surv_count


sts_plt_size %>% 
  dplyr::filter(!is.na(is_dead)) %>% 
  dplyr::mutate(OS = as.numeric(OS), PFS = as.numeric(PFS), is_dead = ifelse(is_dead == 'yes', 1, 0)) %>% 
  dplyr::filter(!is.na(PFS), !is.na(OS)) %>% 
  dplyr::mutate(time = PFS, status = is_dead) %>%
  dplyr::select(VisitId, time, status, Result, AbnormalIndicator) %>% 
  dplyr::filter(AbnormalIndicator != 'h') %>%
  dplyr::mutate(group = as.factor(AbnormalIndicator)) ->
  ana_ready_size


fit_size <- survfit(Surv(time, status) ~ group, data = ana_ready_size)

ggsurvplot(
  fit_size, 
  data = ana_ready_size, 
  xlab = 'Months',
  ylab = 'Proportion Alive (%)',
  title = 'Progression-free survival with platelet MPV',
  size = 1,                 # change line size
  palette = 
    c("red", "blue"),# custom color palettes
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Low size", "Normal"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
) -> plot_surv_size



sts_plt_pct %>% 
  dplyr::filter(!is.na(is_dead)) %>% 
  dplyr::mutate(OS = as.numeric(OS), PFS = as.numeric(PFS), is_dead = ifelse(is_dead == 'yes', 1, 0)) %>% 
  dplyr::filter(!is.na(PFS), !is.na(OS)) %>% 
  dplyr::mutate(time = PFS, status = is_dead) %>% 
  dplyr::select(VisitId, time, status, Result, AbnormalIndicator) %>% 
  dplyr::filter(AbnormalIndicator != 'l') %>%
  dplyr::mutate(group = as.factor(AbnormalIndicator)) %>% 
  dplyr::filter(time > 9) %>% 
  dplyr::mutate(time  = time - 9) ->
  ana_ready_pct

fit_pct <- survfit(Surv(time, status) ~ group, data = ana_ready_pct)

ggsurvplot(
  fit_pct, 
  data = ana_ready_pct, 
  xlab = 'Months',
  ylab = 'Proportion Alive (%)',
  title = 'Progression-free survival with platelet pct',
  size = 1,                 # change line size
  palette = 
    c("red", "blue"),# custom color palettes
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Large PCT", "Normal"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()      # Change ggplot2 theme
) -> plot_surv_pct





