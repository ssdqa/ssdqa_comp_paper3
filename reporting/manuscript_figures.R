## figure 2
asis_att <- postgres_session$results_tbl('attrition_counts') %>%
  group_by(step_number, attrition_step) %>%
  summarise(num_pts = sum(num_pts)) %>%
  mutate(output_function = 'ca_ss_exp_cs',
         site = 'combined') %>% collect() %>% ungroup() %>%
  arrange(step_number) %>%
  mutate(cohort = 'as-is')

dfe_att <- postgres_session$results_tbl('attrition_counts_r7') %>%
  group_by(step_number, attrition_step) %>%
  summarise(num_pts = sum(num_pts)) %>%
  mutate(output_function = 'ca_ss_exp_cs',
         site = 'combined') %>% collect() %>% ungroup() %>%
  arrange(step_number) %>%
  mutate(cohort = 'dfe')

fig2 <- ggplot(dfe_att, aes(y = num_pts, x = step_number)) + 
  geom_line(color = 'gray') +
  geom_line(data = dfe_att %>% filter(step_number > 2),
            color = "navy") + 
  geom_line(data = asis_att %>% filter(step_number > 2), 
            color = 'yellowgreen') +
  geom_point(aes(color = as.character(step_number)), 
                 show.legend = FALSE) + 
  geom_point(data = asis_att, aes(color = as.character(step_number)), 
             show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(1, 7, 1)) + 
  labs(x = "Step", y = 'Number of Patients') + 
  theme_minimal() + 
  squba.gen::scale_color_squba()

ggsave('results/jamia/figure2.png',
       fig2,
       width = 7,
       height = 6,
       dpi = 300)


## figure 3
fig3_int <- ssc_output(process_output = postgres_session$results_tbl('ssc_ms_exp_cs_r7') %>% 
             collect() %>%
             filter(cohort_characteristic %in% c('median_procedures_ppy',
                                                 'median_ed visits_ppy',
                                                 'median_inpatient visits_ppy',
                                                 'median_specialty_visits_ppy',
                                                 'prop_black_race',
                                                 'prop_ANC labs',
                                                 'prop_MCV labs',
                                                 'prop_Hydroxyurea')) %>%
               mutate(cohort_characteristic = ifelse(cohort_characteristic == 'median_specialty_visits_ppy',
                                                     'median_hematology_visits_ppy', cohort_characteristic)) %>%
               inner_join(readr::read_csv('specs/site_anon_map.csv')) %>%
               mutate(site = site_anon),
           alt_cohort_filter = '1+ SCA Dx')


fig3_int[[1]]$data <- fig3_int[[1]]$data %>% mutate(cohort_id = ifelse(cohort_id == 'Base Cohort',
                                                                  '1+ SCA \nDiagnosis', '2+ SCA \nDiagnoses'))
fig3_int[[2]]$data <- fig3_int[[2]]$data %>% mutate(cohort_id = ifelse(cohort_id == 'Base Cohort',
                                                                       '1+ SCA \nDiagnosis', '2+ SCA \nDiagnoses'))

fig3 <- (fig3_int[[1]] + theme(legend.position = 'none', plot.title = element_blank())) + 
  (fig3_int[[2]] + guides(shape = 'none') + theme(plot.title = element_blank())) + 
  plot_layout(axis_titles = 'collect')

ggsave('results/jamia/figure3.png',
       fig3,
       width = 8,
       height = 6,
       dpi = 300)

## figure 4
fig4 <- evp_output(process_output = postgres_session$results_tbl('evp_ms_exp_cs_r8') %>%
                    collect() %>%
                     inner_join(readr::read_csv('specs/site_anon_map.csv')) %>%
                     mutate(site = site_anon),
                  output_level = 'row')

ggsave('results/jamia/figure4.png',
       fig4 + theme(plot.title = element_blank()),
       width = 7,
       height = 6,
       dpi = 300)

## figure 5

library(figpatch)

fig5a_path <- 'results/figure2a_choa_ed.png'
fig5b_path <- 'results/figure2b_squba_ed.png'
fig5c_path <- 'results/figure2c_asis_ed.png'

fig5a <- fig(fig5a_path) + labs(tag = 'A') + theme(plot.tag.position = c(0.03, 0.8),
                                                   plot.tag = element_text(size = 22))
fig5b <- fig(fig5b_path) + labs(tag = 'B') + theme(plot.tag.position = c(0.1, 0.97),
                                                   plot.tag = element_text(size = 22))
fig5c <- fig(fig5c_path) + labs(tag = 'C') + theme(plot.tag.position = c(0.1, 1),
                                                   plot.tag = element_text(size = 22))

fig5 <- wrap_plots(fig5a, fig5b, fig5c,
                   design = "AB\nAC",
                   widths = c(5, 7)) #+
  # plot_annotation(tag_levels = 'A') &
  # theme(plot.tag.position = c(0.1, 0.9))

fig5

ggsave('results/jamia/figure5.png',
       fig5,
       width = 7,
       height = 6,
       dpi = 300)

# figure 6

fig6a_path <- 'results/figure3a_c_choa_hgb.png'
fig6b_path <- 'results/figure3b_squba_hgb.png'
fig6c_path <- 'results/figure3d_adherent_hgb.png'

fig6a <- fig(fig6a_path) + labs(tag = 'A') + theme(plot.tag = element_text(size = 22))
fig6b <- fig(fig6b_path) + labs(tag = 'B') + theme(plot.tag = element_text(size = 22))
fig6c <- fig(fig6c_path) + labs(tag = 'C') + theme(plot.tag = element_text(size = 22))

fig6 <- wrap_plots(fig6a, fig6b, fig6c,
                   design = "AB
                             AC",
                   widths = c(5, 5)) #+
# plot_annotation(tag_levels = 'A')
# theme(plot.tag.position = c(0.1, 0.9))

fig6

ggsave('results/jamia/figure6.png',
       fig6,
       width = 7, 
       height = 6,
       dpi = 300)

# supp figure 2
supp2a_path <- 'results/supp_figure2a_choa_ipvis.png'
supp2b_path <- 'results/supp_figure2b_squba_ipvis.png'
supp2c_path <- 'results/supp_figure2c_asis_ipvis.png'

supp2a <- fig(supp2a_path) + labs(tag = 'A') + theme(plot.tag.position = c(0.03, 0.8),
                                                   plot.tag = element_text(size = 22))
supp2b <- fig(supp2b_path) + labs(tag = 'B') + theme(plot.tag.position = c(0.08, 0.97),
                                                   plot.tag = element_text(size = 22))
supp2c <- fig(supp2c_path) + labs(tag = 'C') + theme(plot.tag.position = c(0.08, 1),
                                                   plot.tag = element_text(size = 22))

supp2 <- wrap_plots(supp2a, supp2b, supp2c,
                   design = "AB\nAC",
                   widths = c(6, 6)) #+
# plot_annotation(tag_levels = 'A') &
# theme(plot.tag.position = c(0.1, 0.9))

supp2

ggsave('results/supp_figure2_combined.png',
       supp2,
       width = 7,
       height = 6,
       dpi = 300)
