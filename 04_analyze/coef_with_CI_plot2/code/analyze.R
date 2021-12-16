library(tidyverse)
library(dotwhisker)

# COVID Infection final model
source("04_analyze/Covid_reg_postas/code/analyze.R")

# Economic effect final model
source("04_analyze/Postas_reg/code/analyze.R")


# Coef plot (COVID)-----


coefplot <- dwplot(list(covid_9.1, covid_9.3, covid_9.5),
                   ci = .95,
                   show_intercept = TRUE,
                   vline = geom_vline(xintercept = 0,
                                      colour = "grey60",
                                      linetype = 2))  +
  theme_bw(base_size = 4) +
  ggtitle("Coefficients of models (Infection Inhibition)") +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"))

coefplot

ggsave("04_analyze/coef_with_CI_plot2/output/coefs_infection.png", coefplot, width = 10, height = 8, dpi = 300)


# dwplot(list(m1, m2, m3),
#        vline = geom_vline(
#          xintercept = 0,
#          colour = "grey60",
#          linetype = 2
#        ),
#        vars_order = c("am", "cyl", "disp", "gear", "hp", "wt"),
#        model_order = c("Model 2", "Model 1", "Model 3")
# ) %>% # plot line at zero _behind_coefs
#   relabel_predictors(
#     c(
#       am = "Manual",
#       cyl = "Cylinders",
#       disp = "Displacement",
#       wt = "Weight",
#       gear = "Gears",
#       hp = "Horsepower"
#     )
#   ) +
#   theme_bw(base_size = 4) + 
#   # Setting `base_size` for fit the theme
#   # No need to set `base_size` in most usage
#   xlab("Coefficient Estimate") + ylab("") +
#   geom_vline(xintercept = 0,
#              colour = "grey60",
#              linetype = 2) +
#   ggtitle("Predicting Gas Mileage") +
#   theme(
#     plot.title = element_text(face = "bold"),
#     legend.position = c(0.007, 0.01),
#     legend.justification = c(0, 0),
#     legend.background = element_rect(colour = "grey80"),
#     legend.title = element_blank()
#   ) 


# Coef plot (Economic Effect)-----


coefplot_cus <- dwplot(list(dlmpostas.cus1, dlmpostas.cus2, dlmpostas.cus3),
                       ci = .95,
                       show_intercept = TRUE,
                       vline = geom_vline(xintercept = 0,
                                          colour = "grey60",
                                          linetype = 2))  +
  theme_bw(base_size = 4) +
  ggtitle("Coefficients of models (Economic effect on # of customers)") +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"))

coefplot_cus
ggsave("04_analyze/coef_with_CI_plot2/output/coefs_customers.png", coefplot_cus, width = 10, height = 8, dpi = 300)


coefplot_sale <- dwplot(list(dlmpostas1, dlmpostas2, dlmpostas3),
                       ci = .95,
                       show_intercept = TRUE,
                       vline = geom_vline(xintercept = 0,
                                          colour = "grey60",
                                          linetype = 2))  +
  theme_bw(base_size = 4) +
  ggtitle("Coefficients of models (Economic effect on sales)") +
  theme(title = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"))

coefplot_sale

ggsave("04_analyze/coef_with_CI_plot2/output/coefs_sales.png", coefplot_sale, width = 10, height = 8, dpi = 300)





