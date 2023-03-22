
###Old attempt using separate model per week:###

# df_subset_wks <- wc_qudo_df %>% 
#  filter(week %in% c(14, 15, 17, 21, 29))
# 
# for (i in unique(df_subset_wks$week)) {
# 
#    df <- df_subset_wks %>%
#     filter(week == i)
# 
#    m_wk <- lmer(water_potential ~ lwc + (1|tree), data = df)
#    m_wk
# 
#    df2 <- broom.mixed::tidy(m_wk) %>%
#      mutate(week = i) %>%
#      #select(estimate, week, term) %>% 
#      data.frame()
# 
#    assign(paste0("m_df_", i), data.frame(df2))
#    #print(paste0("m_df_", i))
# }
# 
# all_hyd_mods <- bind_rows(#m_df_9, 
#                           #m_df_10,
#                           #m_df_11,
#                           #m_df_12, 
#                          # m_df_13, 
#                           m_df_14, 
#                           m_df_15, 
#                           m_df_17, 
#                          # m_df_18, 
#                          # m_df_19, 
#                           m_df_21, 
#                           m_df_29)
# 
# all_hyd_mods %>% 
#  # filter(week %in% c(14, 15, 17, 21, 29)) %>% 
#   filter(term == "lwc") %>% 
#   mutate(upr = estimate + (std.error/2), 
#          lwr = estimate - (std.error/2)) %>% 
#   ggplot(aes(color= week)
#     ) + 
#   geom_point(aes(y = estimate,x = week), size =3) +
#     geom_point(aes(y = upr, x = week), size = 1) +
#   geom_point(aes(y = lwr, x = week), size = 1) +
#   #geom_line(aes(group = week, y = week, x = upr)) +
#   geom_segment(aes(y= lwr, 
#                    yend = upr, 
#                    x = week, 
#                    xend = week, 
#                    color = week))+
#   labs(x = "Week", 
#        color = "week", 
#        y = "Grand Slope") +
#    # facet_wrap(~y_var_name, scales = "free") + 
# theme(
#   strip.background = element_blank(),
#   strip.text.y = element_blank(), 
#   strip.text.x = element_text(size = 18), 
#  # axis.text.y = element_blank(), 
#   #axis.ticks.y = element_blank(), 
#   axis.title = element_text(size = 20), 
#   legend.title = element_text(size = 18)
#   ) +
#   geom_hline(yintercept = 0, linetype="dotted") + 
#   scale_x_continuous(breaks = seq(9, 30, by = 1))


##RWC####


# rwc_df_leaves_y1 <- rwc_df %>% 
#   filter(type == "leaf", 
#          year %in% c(1, "NA")) %>% 
#   group_by(tree, date) %>% 
# mutate(mean_swc_per_dry_g_y1 = mean(swc_per_dry_g)
#          , mean_swc_g_y1 = mean(swc_g), 
#        swc_ww_g_y1= swc_ww_g, 
#        swc_dw_g_y1= swc_ww_g) %>% 
#   ungroup() %>% 
#   mutate(date_rwc = date, 
#          week = as.numeric(week)) %>% 
#   select(-date) %>% 
#   distinct() %>% 
#   select(mean_swc_per_dry_g_y1, mean_swc_g_y1, tree, week, date_rwc, 
#          swc_dw_g_y1, swc_ww_g_y1) %>% 
#   as.data.frame()

#RWC all weeks: 
# rwc_df_leaves_all <- merge(rwc_df_leaves_y1 , rwc_df_leaves_y0 ,
#                     by = c("tree",
#                            "week",
#                            "date_rwc"
#                           # "rep"
#                            ),
#                     #all.x = T,
#                     all = T
#                     ) 
# #646 rows



###For loop for plots: 
# ```{r, include=F}
# wc_lm_hyd_df <- wk_all
# 
# for (i in unique(wc_lm_hyd_df$week)) {
#   
#   df <- wc_lm_hyd_df %>%
#     filter(week == i)
#   
#   p <- df %>%
#     ggplot(aes(y = water_potential,
#                x = lwc,
#                color = as.factor(tree)
#     )) +
#     geom_point(aes(shape = time)) +
#     geom_smooth(method = "lm",
#                 se = F) +
#     #theme(legend.position = "none") +
#     labs(title = i)
#   
#   if (!is.null(p)) plot(p)
#   
# }
# ```