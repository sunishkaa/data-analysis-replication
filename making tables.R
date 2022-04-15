# sham_l_more = as.data.frame(c(small_males, large_males)) %>% 
#   mutate(male_type = male_type, score = c(small_males, large_males))

novel_table1 = as.data.frame(novel_vars) %>% 
  mutate(novel_PC1_loadings = as.data.frame(novel_pca_loadings)$Dim.1) %>% 
  rbind(c("eigenvalue", (novel_pca_summary$importance[1, "PC1"])^2)) %>% 
  rbind(c("percent_variance", novel_pca_summary$importance[2, "PC1"]))

barrier_table1 = as.data.frame(barrier_vars) %>% 
  mutate(barrier_PC1_loadings = as.data.frame(barrier_pca_loadings)$Dim.1) %>% 
  rbind(c("eigenvalue", (barrier_pca_summary$importance[1, "PC1"])^2)) %>% 
  rbind(c("percent_variance", barrier_pca_summary$importance[2, "PC1"]))


#data[1, "V1"]  # Row first, quoted column name second

#result <- rbind(baskets.df, c(7, 4))


#for corelations
# p value = cortest$parameter$p.value
# r = cortest$estimate$cor

cor_test = c("cor_boldness_novel", "cor_boldness_barrier", "cor_novel_barrier")
r_paper = c(-0.357, 0.374, -0.263)
r_replicated = c(cor_boldness_novel$estimate, cor_boldness_barrier$estimate, cor_novel_barrier$estimate)
p_value_paper = c(0.0087, 0.0058, 0.057)
p_value_replicated = c(cor_boldness_novel$p.value, cor_boldness_barrier$p.value, cor_novel_barrier$p.value)
cor_test_summary = as.data.frame(cor_test) %>% 
  mutate(r_paper = r_paper,
         r_replicated = r_replicated,
         p_value_paper = p_value_paper,
         p_value_replicated = p_value_replicated)
view(cor_test_summary)



inital_w_behaviors = c("boldness", "novel object", "barrier task")
iwb_beta1_paper = c(0.159, 0.162, 0.196)
iwb_beta1_replicated = c(initial_w_boldness[["coefficients"]][["LN_Emerge_Average"]],
                     initial_w_novel[["coefficients"]][["novel_PC1_scores"]],
                     initial_w_barrier[["coefficients"]][["barrier_PC1_scores"]])
iwb_z_paper = c(-2.343, 3.37, -3.732)
iwb_z_replicated = c(coef(summary(initial_w_boldness))[, "z value"][["LN_Emerge_Average"]],
                 coef(summary(initial_w_novel))[, "z value"][["novel_PC1_scores"]],
                 coef(summary(initial_w_barrier))[, "z value"][["barrier_PC1_scores"]])
iwb_p_value_paper = c(0.0191, 0.0007, 0.0002)
iwb_p_value_replicated = c(coef(summary(initial_w_boldness))[, "Pr(>|z|)"][["LN_Emerge_Average"]],
                           coef(summary(initial_w_novel))[, "Pr(>|z|)"][["novel_PC1_scores"]],
                           coef(summary(initial_w_barrier))[, "Pr(>|z|)"][["barrier_PC1_scores"]])
initial_w_behaviors_summary = as.data.frame(inital_w_behaviors) %>% 
  mutate(iwb_beta1_paper = iwb_beta1_paper,
         iwb_beta1_replicated = iwb_beta1_replicated,
         iwb_z_paper = iwb_z_paper,
         iwb_z_replicated= iwb_z_replicated,
         iwb_p_value_paper = iwb_p_value_paper,
         iwb_p_value_replicated = iwb_p_value_replicated)
  

reversal_w_behaviors = c("boldness", "novel object")
rwb_beta1_paper = c(-0.268, 0.234)
rwb_beta1_replicated = c(reversal_w_boldness[["coefficients"]][["LN_Emerge_Average"]],
                         reversal_w_novel[["coefficients"]][["novel_PC1_scores"]])
rwb_z_paper = c(-2.852, 3.714)
rwb_z_replicated = c(coef(summary(reversal_w_boldness))[, "z value"][["LN_Emerge_Average"]],
                     coef(summary(reversal_w_novel))[, "z value"][["novel_PC1_scores"]])
rwb_p_value_paper = c(0.0044, 0.0002)
rwb_p_value_replicated = c(coef(summary(reversal_w_boldness))[, "Pr(>|z|)"][["LN_Emerge_Average"]],
                           coef(summary(reversal_w_novel))[, "Pr(>|z|)"][["novel_PC1_scores"]])
reversal_w_behaviors_summary = as.data.frame(reversal_w_behaviors) %>% 
  mutate(rwb_beta1_paper = rwb_beta1_paper,
         rwb_beta1_replicated = rwb_beta1_replicated,
         rwb_z_paper = rwb_z_paper,
         rwb_z_replicated= rwb_z_replicated,
         rwb_p_value_paper = rwb_p_value_paper,
         rwb_p_value_replicated = rwb_p_value_replicated)


wrong_w_behaviors = c("boldness", "barrier object")
wwb_beta1_paper = c(0.5882, 0.4849)
wwb_beta1_replicated = c(wrong_w_boldness[["coefficients"]][["LN_Emerge_Average"]],
                         wrong_w_barrier[["coefficients"]][["barrier_PC1_scores"]])
wwb_z_paper = c(2.417, 2.265)
wwb_z_replicated = c(coef(summary(wrong_w_boldness))[, "z value"][["LN_Emerge_Average"]],
                     coef(summary(wrong_w_barrier))[, "z value"][["barrier_PC1_scores"]])
wwb_p_value_paper = c(0.0157, 0.0235)
wwb_p_value_replicated = c(coef(summary(wrong_w_boldness))[, "Pr(>|z|)"][["LN_Emerge_Average"]],
                           coef(summary(wrong_w_barrier))[, "Pr(>|z|)"][["barrier_PC1_scores"]])
wrong_w_behaviors_summary = as.data.frame(wrong_w_behaviors) %>% 
  mutate(wwb_beta1_paper = wwb_beta1_paper,
         wwb_beta1_replicated = wwb_beta1_replicated,
         wwb_z_paper = wwb_z_paper,
         wwb_z_replicated= wwb_z_replicated,
         wwb_p_value_paper = wwb_p_value_paper,
         wwb_p_value_replicated = wwb_p_value_replicated)
