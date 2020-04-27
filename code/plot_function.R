library(patchwork)
library(SDMTools)
library(viridis)

# ------------------------- Plotting function -------------------------------- #

pd <- position_dodge(width = 0.5)
shapes <- c("median" = 15, "median (derived)" = 17, "mean" = 16)
ltypes <- c("Yes" = "solid", "No" = "dashed")
status <- c("#56B4E9", "#009E73", "#E69F00")
severity <- c("#0072B2","#F0E442",  "#CC79A7", "#D55E00")
palette1 <- c("#9d8EFF","#00A45E","#9E2228","#FF9B91")
palette2 <- c( "#4379f2",
               "#00973d",
               "#e2672c",
               "#34e9fa")
# pal <- RColorBrewer::brewer.pal(7, "Dark2")
# status <- pal[1:3]
# severity <- pal[4:7]

plot_los_outcome <- function(data, 
                             plot_outcome = NULL, 
                             order = "study_date", 
                             lty = "complete_fup", 
                             col = "covid_severity", 
                             group = "plot_oth_group", 
                             facet = "Setting",
                             pal = palette2){
  
  if (!is.null(plot_outcome)){
    data <- filter(data, outcome == plot_outcome)
  }
  
  # Reorder
  data <- 
    mutate(data, Study = fct_reorder(Study, !!sym(order)))
  
  # define centre lines for facets
  data <- 
    data %>%
    group_by(!!sym(facet)) %>%
    mutate(centre = wt.mean(LOS_avg, N)) %>%
    ungroup()
  centre_lines <- unique(pull(data, centre))
  
  data %>%
    ggplot(aes(y = Study, x = LOS_avg, xmin = LOS_q25, xmax = LOS_q75, shape = metric, lty = !!sym(lty), col = !!sym(col), group = !!sym(group))) +
    geom_point(aes(size = N), position=pd) +
    geom_errorbarh(height=0.3, position = pd) +
    geom_vline(aes(xintercept = centre), col = "darkgrey", lty = "longdash") +
    labs(x = "Length of stay (days)",
         title = "Length of stay in hospital: all outcomes (incl. survivors, non-survivors and mixed)",
         caption = paste0("Studies ordered by ", order, ". Weighted means at ", round(centre_lines[2],1),", ", round(centre_lines[1],1), " days."),
         size = "Study size",
         lty = "Follow-up complete?",
         shape = "Measure",
         col = "Disease severity") +
    facet_grid(rows = vars(!!sym(facet)), scales = "free_y", space = "free_y") +
    scale_color_manual(values = pal) +
    # scale_color_viridis(discrete = TRUE, option = "D")+
    scale_shape_manual(values = shapes, drop = F) +
    scale_linetype_manual(values = ltypes, drop = F) +
    theme_minimal()
  
}

plot_los_outcome_nofacet <- function(data, 
                                     plot_outcome = NULL, 
                                     order = "study_date", 
                                     lty = "complete_fup", 
                                     col = "covid_severity", 
                                     group = "plot_oth_group", 
                                     pal = palette2){
  
  if (!is.null(plot_outcome)){
    data <- filter(data, outcome == plot_outcome)
  }
  
  # Reorder and define centre
  data <- 
    mutate(data, Study = fct_reorder(Study, !!sym(order)),
           centre = wt.mean(LOS_avg, N))
  
  data %>%
    ggplot(aes(y = Study, x = LOS_avg, xmin = LOS_q25, xmax = LOS_q75, shape = metric, lty = !!sym(lty), col = !!sym(col), group = !!sym(group))) +
    geom_point(aes(size = N), position=pd) +
    geom_errorbarh(height=0.3, position = pd) +
    geom_vline(aes(xintercept = centre), col = "darkgrey", lty = "longdash") +
    labs(x = "Length of stay (days)",
         title = "Length of stay in hospital: all outcomes (incl. survivors, non-survivors and mixed)",
         caption = paste0("Studies ordered by ", order, ". Weighted mean at ", round(unique(data$centre),1), " days."),
         size = "Study size",
         lty = "Follow-up complete?",
         shape = "Measure",
         col = "Disease severity") +
    scale_color_manual(values = pal) +
    # scale_color_viridis(discrete = TRUE, option = "D")+
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = ltypes, drop = F) +
    theme_minimal()
  
}
