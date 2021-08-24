# quantile differences between shot length distributions of two films
qdiff <- function(x,y){
  
  # arrange data as a matrix and drop the first columns which contain names
  x <- data.matrix(x[,-1])
  y <- data.matrix(y[,-1])
  
  # calculate pairwise difference between groups for each quantile
  df <- data.frame()
  for (i in 1:19){
    
    diff <- as.vector(outer(x[,i], y[,i], '-'))
    s <- i * 0.05
    df_a <- cbind(rep(s, length(diff)), diff)
    df <- rbind(df, df_a)
    
  }
  
  # tidy column names
  colnames(df) <- c("quantile", "diff")
  return(df)
  
}

# normalise a numeric vector to a range [0, 1]
norm <- function(x){
  res <- (x - min(x))/(max(x) - min(x))
  return(res)
}

# summarise shot lengths and return five number summary
sl_sum <- function(x){
  
  x %>% group_by(Title) %>% 
    summarise(N = n(), `Running time` = sum(SL),
              Min = min(SL),
              Q1 = quantile(SL, probs = c(0.25)),
              Median = median(SL),
              Q3 = quantile(SL, probs = c(0.75)),
              Max = max(SL))
}

# summarise shot lengths and return five number summary - group version
group_sl_sum <- function(x){
  
  x %>% group_by(Group, Title) %>% 
    summarise(N = n(), `Running time` = sum(SL),
              Min = min(SL),
              Q1 = quantile(SL, probs = c(0.25)),
              Median = median(SL),
              Q3 = quantile(SL, probs = c(0.75)),
              Max = max(SL))
}

# return table of pairwise comparisons of dominance statistics: HLD and Cliff's d
dom_tab <- function(x){
  
  # nest data
  dataNested <- x %>%
    group_by(Title) %>%
    nest()
  
  # get number of pairwise comparisons to perform
  k <- as.data.frame(combinations(1:n_distinct(x$Title), 2))
  
  df_t <- data.frame()
  for (i in 1:length(k$V1)){
    name1 <- dataNested$Title[k$V1[i]]
    name2 <- dataNested$Title[k$V2[i]]
    hld <- HodgesLehmann(dataNested$data[[k$V1[i]]]$SL, dataNested$data[[k$V2[i]]]$SL, conf.level = 0.95)[1]
    d <- dmes(dataNested$data[[k$V2[i]]]$SL, dataNested$data[[k$V1[i]]]$SL)$dc
    df_a <- data.frame(F1 = name1, F2 = name2, 
                       HLD = format(round(hld, 2), nsmall = 2), 
                       cd = format(round(d, 2), nsmall = 2))
    df_t <- rbind.data.frame(df_t, df_a)
  }
  
  # tidy column names
  df_t <- df_t %>% rename(`Film 1` = F1, `Film 2` = F2, 
                          `Hodges-Lehmann difference (s)` = HLD, `Cliff's d` = cd)
  
  return(df_t)
  
}

# adjusted boxplot and kde for a single film
adjkde_plot <- function(x){
  
  # get adjusted boxplot stats using robustbase::adjboxStats
  adjbox_stats <- adjboxStats(x$SL)
  
  df_stats <- data.frame(
    x = unique(x$Title),
    ylf = adjbox_stats$stats[1],
    y25 = adjbox_stats$stats[2],
    y50 = adjbox_stats$stats[3],
    y75 = adjbox_stats$stats[4],
    yuf = adjbox_stats$stats[5]
  )
  
  # identify outliers
  df <- data.frame(x = rep(1, length(x$SL)), 
                   y = x$SL,
                   out = if_else(x$SL < adjbox_stats$stats[1] | x$SL > adjbox_stats$stats[5], "yes", "no"))
  
  # minor gridlines for log-scale y-axis
  min_breaks <- c(seq(0.02, 0.09, 0.01), seq(0.2, 0.9, 0.1), seq(2, 9, 1),
                  seq(20, 90, 10), seq(200, 900, 100))

  # draw boxplot with data overlaid as jittered points
  adj_plot <- ggplot() +
    geom_boxplot(data = df_stats, 
                 aes(x = x, 
                     ymin = ylf, lower = y25, middle = y50, upper = y75, ymax = yuf),
                 fill = "#440154", colour = "black",
                 stat = "identity") +
    geom_jitter(data = df, aes(x = x, y = y, colour = out, fill = out, shape = out), 
                position = position_jitter(width = 0.2, height = 0)) +
    coord_flip() + 
    scale_y_continuous(trans = "log10", minor_breaks = min_breaks) + 
    scale_colour_manual(values = c("black", "black")) +
    scale_fill_manual(values = c("grey40", "white")) +
    scale_shape_manual(values = c(21, 23)) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid = element_line(colour = "grey70"))
  
  # plot kernel density estimate
  kde_plot <- ggplot(data = x) +
      geom_density(aes(x = SL), fill = "#440154", bw = 0.08, trim = FALSE) +
      geom_rug(aes(x = SL)) +
      scale_x_continuous(trans = "log10", minor_breaks = min_breaks) +
      labs(x = "\nShot length (s)", y = "Density\n") +
      theme_minimal() +
      theme(panel.grid = element_line(colour = "grey70"))
  
  # arrange figure
  fig <- ggarrange(adj_plot, kde_plot, nrow = 2, align = "v", heights = c(0.5, 1.1))
  return(fig)
  
}

# adjusted boxplots for multiple films

adjbox_M_plot <- function(x){
  
  # get overall minimum and maximum shot length to set axis limits
  x_min <- min(x$SL); x_max <- max(x$SL)
  
  # set number of colours in palette
  c <- if(n_distinct(x$Title) > 2){viridis(n_distinct(x$Title), begin = 0, end = 1)} else {viridis(2, begin = 0, end = 0.5)}

  # minor gridlines for log-scale y-axis
  min_breaks <- c(seq(0.02, 0.09, 0.01), seq(0.2, 0.9, 0.1), seq(2, 9, 1),
                  seq(20, 90, 10), seq(200, 900, 100))
  
  # split data by film
  df <- x %>% group_by(Title) %>% group_split()
  
  plot_list <- list()
  
  for(i in seq_along(df)){
    
    # get adjusted boxplot stats using robustbase::adjboxStats
    Title <- rep(df[[i]]$Title[1], 5)
    ajbs <- adjboxStats(df[[i]]$SL)$stats
    cols <- c("ylf", "y25", "y50", "y75", "yuf")
    
    df_film <- cbind.data.frame(Title, cols, ajbs)
    
    df_film <- df_film %>% 
      pivot_wider(names_from = "cols", values_from = "ajbs")
    
    # identify outliers
    Title <- rep(df[[i]]$Title[1], length(df[[i]]$SL))
    SL <- df[[i]]$SL
    out <- if_else(df[[i]]$SL < ajbs[1] | df[[i]]$SL > ajbs[5], "yes", "no")
    
    df_film2 <- cbind.data.frame(Title, SL, out)
    
    p <- ggplot() +
      geom_boxplot(data = df_film, 
                   aes(x = Title, 
                       ymin = ylf, lower = y25, middle = y50, upper = y75, ymax = yuf),
                   fill = c[i], colour = "black",
                   stat = "identity") +
      geom_jitter(data = df_film2, aes(x = Title, y = SL, colour = out, fill = out, shape = out), 
                  position = position_jitter(width = 0.2, height = 0)) +
      coord_flip() + 
      scale_y_continuous(trans = "log10", limits = c(x_min, x_max), minor_breaks = min_breaks) + 
      scale_colour_manual(values = c("black", "black")) +
      scale_fill_manual(values = c("grey40", "white")) +
      scale_shape_manual(values = c(21, 23)) +
      labs(title = unique(df_film2$Title),
           y = "Shot length (s)") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_blank(),
            panel.grid = element_line(colour = "grey70"))
    
    plot_list[[i]] <- p
    
  }
  
  # arrange and annotate figure
  fig <- ggarrange(plotlist = plot_list, align = "v", nrow = n_distinct(x$Title))
  fig <- annotate_figure(fig, bottom = text_grob("Shot length (s)", size = 10.5))
  return(fig)
  
}

# ecdf plot
ecdf_plot <- function(x){
  
  # set legend position and number of rows in legend
  leg_pos <- if_else(n_distinct(x$Title) > 1, "bottom", "none")
  leg_row <- if_else(n_distinct(x$Title) == 2, 1, 2)
  # set number of limits on colour palette
  pal_ul <- if_else(n_distinct(x$Title) < 3, 0.5, 1)
  
  # minor gridlines for log-scale y-axis
  min_breaks <- c(seq(0.02, 0.09, 0.01), seq(0.2, 0.9, 0.1), seq(2, 9, 1),
                  seq(20, 90, 10), seq(200, 900, 100))
  
  ggplot(data = x, aes(SL, colour = Title)) +
    geom_line(aes(y = ..y..), stat = "ecdf") +
    scale_x_continuous(trans = "log10", minor_breaks = min_breaks) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    labs(x = "\nShot length (s)", y = "ECDF\n") +
    theme_minimal() +
    theme(legend.position = leg_pos,
          panel.grid = element_line(colour = "grey70")) +
    guides(colour = guide_legend(ncol = 3, nrow = leg_row, byrow = TRUE, title = NULL)) +
    scale_colour_viridis_d(begin = 0, end = pal_ul)

}

# ecdf plot for groups
ecdf_group_plot <- function(x){
  
  # minor gridlines for log-scale y-axis
  min_breaks <- c(seq(0.02, 0.09, 0.01), seq(0.2, 0.9, 0.1), seq(2, 9, 1),
                  seq(20, 90, 10), seq(200, 900, 100))
  
  ggplot(data = x, aes(x = SL, group = Title)) +
    geom_line(aes(y = ..y.., colour = factor(Group)), stat = "ecdf") +
    scale_x_continuous(trans = "log10", minor_breaks = min_breaks) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    labs(x = "\nShot length (s)", y = "ECDF\n") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_line(colour = "grey70")) +
    scale_colour_viridis_d(name = "", begin = 0, end = 0.5)
  
}

# histogram
hist_plot <- function(x){
  
  # set bin width according to range of data
  if(max(x$SL) <= 50) {b_width = 0.5}
  else if(max(x$SL) > 50 && max(x$SL) <= 100) {b_width = 1}
  else{b_width = 2}
  
  ggplot(data = x) +
    geom_histogram(aes(x = SL, fill = Title), binwidth = b_width, center = b_width * 0.5,
                   fill = "#440154", colour = "black") +
    scale_x_continuous(limits = c(0, plyr::round_any(max(x$SL), 2, ceiling))) +
    labs(x = "\nShot length (s)", y = "Frequency\n") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid = element_line(colour = "grey70"))
  
}

# cut density plot for a single film - NB: scaled density
cutd_plot <- function(x){
  
  # cut timings
  df <- data.frame(v1 = cumsum(x$SL))
  
  # set breaks according to running time of film
  if(max(df$v1) <= 600) {pbreaks = seq(0, 600, 30)}
  else if(max(df$v1) > 600 && max(df$v1) <= 3600) {pbreaks = seq(0, 3600, 300)}
  else {pbreaks = seq(0, 12000, 600)}
  
  ggplot(data = df) +
    geom_density(aes(x = v1, y = ..scaled..), fill = "#440154", adjust = 1/11, trim = FALSE) +
    geom_rug(aes(x = v1)) +
    scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, max(df$v1)),
                       breaks = pbreaks) +
    labs(x = "\nRunning time (s)", y = "Scaled density\n") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_line(colour = "grey70"))
  
}

# multiple loess smoothers for a single film
loessggplot <- function(x){
  
  # get cut times
  n <- length(x$SL)
  t <- cumsum(x$SL); t <- 100 * t/max(t)
  
  # fit spans in range
  df <- data.frame()
  for(s in seq(0.1, 0.9, 0.01)) {
    fit <- loess(x$SL ~ t, span = s, degree = 2)$fitted
    sn <- as.numeric(rep(s, n))
    df_a <- data.frame(cbind(sn, t, fit))
    df <- rbind(df,df_a)
  }

  p <- ggplot(data = df, aes(x = t, y = fit, group = sn, colour = sn))+
    geom_line() +
    scale_x_continuous(name = "\nRunning time (%)", breaks = seq(0, 100, 10)) +
    labs(y  = "Fitted values (s)\n") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 10), 
          legend.text = element_text(size = 9),
          panel.grid = element_line(colour = "grey70")) +
    guides(colour = guide_colourbar(barwidth = 20, barheight = 1, title.position = "top")) +
    scale_colour_viridis_c(name = "Span", breaks = seq(0.1, 0.9, 0.1))

  return(p)
}

# simple point process plot
spp_plot <- function(x){
  
  # data frame containing cut times and cut number
  df <- data.frame(v1 = cumsum(x$SL),
                   y = 1:length(x$SL))
  
  # set breaks according to running time of film
  if(max(df$v1) <= 600) {pbreaks = seq(0, 600, 30)}
  else if(max(df$v1) > 600 && max(df$v1) <= 3600) {pbreaks = seq(0, 3600, 300)}
  else {pbreaks = seq(0, 12000, 600)}
  
  ggplot(data = df, aes(x = v1, y = y)) +
    geom_line(col = "#440154") +
    scale_x_continuous(expand = c(0.01, 0.01), name="\n Running time (s)", limits = c(0, max(df$v1)),
                       breaks = pbreaks) + 
    scale_y_continuous(name = "Count (*N*(*t*))<br>") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.title.y = ggtext::element_markdown(size = 10.5),
          panel.grid = element_line(colour = "grey70"))
  
}

# quantile plot for two films
quan_2_plot <- function(x){
  
  # calculate quantiles for each film
  df <- x %>% group_by(Title) %>%
    summarise(quantile = seq(0.05, 0.95, 0.05), length = quantile(SL, seq(0.05, 0.95, 0.05)))
  
  # plot quantile profiles
  qt_plot <- ggplot(data = df, aes(x = quantile, y = length, group = Title)) +
    geom_line(aes(colour = Title)) +
    geom_point(aes(colour = Title)) +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    labs(title = "Quantile plot", x = "\nQuantile", y = " Shot length (s)\n") +
    scale_colour_manual(values = c("#440154", "#21908C")) +
    guides(colour = guide_legend(title = NULL)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 10.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          panel.grid = element_line(colour = "grey70"))
  
  # calculate difference between quantiles
  df_a <- df %>% group_by(Title) %>% group_split()
  a <- df_a[[1]] %>% pivot_wider(names_from = quantile, values_from = length)
  b <- df_a[[2]] %>% pivot_wider(names_from = quantile, values_from = length)
  
  df_diff <- qdiff(a, b)
  
  # plot quantile differences
  q_diff_plot <- ggplot(data = df_diff, 
                        aes(x = quantile, y = diff, label = sprintf("%0.1f", round(diff, digits = 1)))) +
    geom_point(aes(colour = diff), size = 8, show.legend = FALSE) +
    geom_text(colour = "white", size = 2.5, fontface = "bold") +
    scale_colour_gradient2(low = "#21908C", mid = "#334970", high = "#440154", midpoint = 0) +
    labs(title = "Quantile differences", x = "\nQuantile", y = "Difference (s)\n") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    scale_y_continuous(limits = c(min(df_diff$diff) - 2, max(df_diff$diff) + 2)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 10.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          panel.grid = element_line(colour = "grey70"))
  
  # arrange figure
  fig <- ggarrange(qt_plot, q_diff_plot, nrow = 2, align = "v", labels = "AUTO", heights = c(1, 1))
  
  return(fig)
}

# simple point process for two to six films
countp_M_plot <- function(x){
  
  # set number of rows in legend
  leg_row <- if_else(n_distinct(x$Title) == 2, 1, 2)
  # set limits for colour palette
  pal_ul <- if_else(n_distinct(x$Title) < 3, 0.5, 1)
  
  # data frame containing cut times and cut number - normalise both for comparison
  df <- x %>% group_by(Title) %>% mutate(time = norm(cumsum(SL)),
                                         count = norm(1:length(SL)))
  
  ggplot(data = df, aes(x = time, y = count)) +
    geom_line(aes(colour = Title)) +
    scale_x_continuous(expand = c(0.01, 0.01), name="\nNormalised running time", 
                       breaks = seq(0, 1, 0.2), labels = function(x) paste0(x * 100, "%")) + 
    scale_y_continuous(name = "Normalised count\n",
                       breaks = seq(0, 1, 0.2), labels = function(x) paste0(x * 100, "%")) +
    scale_colour_viridis_d("", begin = 0, end = pal_ul) +
    theme_minimal() +
    guides(colour = guide_legend(ncol = 3, nrow = leg_row, byrow = TRUE, title = NULL)) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          panel.grid = element_line(colour = "grey70"))
  
}

# simple point process for two groups of films
countp_group_plot <- function(x){
  
  # group data, get cut times, and normalise running times
  df <- x %>% group_by(Group, Title) %>% mutate(time = norm(cumsum(SL)),
                                         count = norm(1:length(SL)))
  
  ggplot(data = df, aes(x = time, y = count, group = Title)) +
    geom_line(aes(colour = factor(Group))) +
    scale_x_continuous(expand = c(0.01, 0.01), name="\nNormalised running time", 
                       breaks = seq(0, 1, 0.2), labels = function(x) paste0(x * 100, "%")) + 
    scale_y_continuous(name = "Normalised count\n",
                       breaks = seq(0, 1, 0.2), labels = function(x) paste0(x * 100, "%")) +
    scale_colour_viridis_d("", begin = 0, end = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          panel.grid = element_line(colour = "grey70"))
  
}

# quantile plot for three to six films
quan_M_plot <- function(x){
  
  # set number of rows in legend
  leg_row <- if_else(n_distinct(x$Title) == 2, 1, 2)
  
  # get quantiles of each film
  df <- x %>% group_by(Title) %>%
    summarise(quantile = seq(0.05, 0.95, 0.05), length = quantile(SL, seq(0.05, 0.95, 0.05)))
  
  ggplot(data = df, aes(x = quantile, y = length, group = Title)) +
    geom_line(aes(colour = Title)) +
    geom_point(aes(colour = Title)) +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    labs(x = "\nQuantile", y = " Shot length (s)\n") +
    scale_colour_viridis_d() +
    guides(colour = guide_legend(ncol = 3, nrow = leg_row, byrow = TRUE, title = NULL)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 10.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          panel.grid = element_line(colour = "grey70"))

}

# quantile plot for two groups of films
quan_group_plot <- function(x){
  
  # get quantiles of each film
  df <- x %>% group_by(Group, Title) %>%
    summarise(quantile = seq(0.05, 0.95, 0.05), length = quantile(SL, seq(0.05, 0.95, 0.05)))
  
  ggplot(data = df, aes(x = quantile, y = length, group = Title)) +
    geom_line(aes(colour = factor(Group))) +
    geom_point(aes(colour = factor(Group))) +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    labs(x = "\nQuantile", y = " Shot length (s)\n") +
    scale_colour_viridis_d(name = "", begin = 0, end = 0.5) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 10.5),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          panel.grid = element_line(colour = "grey70"))
  
}

# plot difference distributions for quantiles of two groups of films
quan_group_diff_plot <- function(x, y){
  
  # get quantiles of each film in group x
  df_x <- x %>% group_by(Title) %>%
    summarise(quantile = seq(0.05, 0.95, 0.05), length = quantile(SL, seq(0.05, 0.95, 0.05)))
  
  # get quantiles of each film in group y
  df_y <- y %>% group_by(Title) %>%
    summarise(quantile = seq(0.05, 0.95, 0.05), length = quantile(SL, seq(0.05, 0.95, 0.05)))
  
  # re-arrange data to wide format expected by qdiff
  df_a <- df_x %>% pivot_wider(names_from = quantile, values_from = length)
  df_b <- df_y %>% pivot_wider(names_from = quantile, values_from = length)
  
  # get pairwise differences between groups
  df_diff <- qdiff(df_a, df_b)
  
  p <- ggplot(data = df_diff, aes(x = diff, group = as.factor(quantile), colour = as.factor(quantile))) +
    geom_density(size = 1) +
    labs(x = "\nDifference (s)", y = "Density\n") + 
    scale_colour_viridis(name = "Quantile", 
                         breaks = seq(0.05, 0.95, 0.05), 
                         labels = c("0.05","0.10","0.15","0.20","0.25","0.30",
                                    "0.35","0.40","0.45","0.50","0.55","0.60",
                                    "0.65","0.70","0.75","0.80","0.85","0.90", "0.95"), 
                         discrete = TRUE, direction = -1) +
    theme_minimal() +  
    theme(legend.key.width = unit(1, "cm"), 
          legend.key.height = unit(0.4, "cm"))
  
  return(p)
  
}

# plot kdes for three to six films
kde_M_plot <- function(x){
  
  # minor gridlines for log-scale y-axis
  min_breaks <- c(seq(0.02, 0.09, 0.01), seq(0.2, 0.9, 0.1), seq(2, 9, 1),
                  seq(20, 90, 10), seq(200, 900, 100))
  
  # set limits for colour palette
  pal_ul <- if_else(n_distinct(x$Title) < 3, 0.5, 1)
  
  ggplot(data = x, aes(x = SL, fill = Title)) +
    geom_density(bw = 0.08, alpha = 0.9, trim = FALSE) +
    geom_rug(aes(group = Title)) +
    scale_x_continuous(expand = c(0.01, 0), trans = "log10", minor_breaks = min_breaks) +
    scale_fill_viridis_d(begin = 0, end = pal_ul) +
    labs(x = "\nShot length (s)",
         y = "Density\n") +
    facet_wrap(~ Title, nrow = n_distinct(x$Title)) +
    theme_minimal() +
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold", size = 10.5, hjust = 0),
          panel.grid = element_line(colour = "grey70"))
  
}

# cut density for multiple films
kde_ts_plot <- function(x){
  
  # group by film and get cut timings over normalised interval
  df <- x %>% group_by(Title) %>% mutate(time = norm(cumsum(SL)))
  
  # set number of colours for palette
  c <- if(n_distinct(x$Title) > 2){viridis(n_distinct(x$Title), begin = 0, end = 1)} else {viridis(2, begin = 0, end = 0.5)}
  
  ggplot(data = df, aes(x = time)) + 
    geom_density(aes(y = ..scaled.., fill = Title, alpha = 0.9), adjust = 1/11, trim = FALSE) +
    geom_rug(aes(group = Title)) +
    scale_x_continuous(expand = c(0.01, 0), name="\nNormalised running time", limits = c(0, 1),
                       breaks = seq(0, 1, 0.2), labels = function(x) paste0(x * 100, "%")) +
    scale_y_continuous(name = "Scaled density\n", breaks = c(0,1, 0.5), 
                                  sec.axis = dup_axis(name = " ", labels = NULL)) +
    facet_wrap(~ Title, nrow = n_distinct(x$Title)) +
    scale_fill_manual(values = c) + 
    theme_minimal() +  
    theme(legend.position = "none",
          strip.background = element_blank(), 
          strip.text = element_text(face = "bold", size = 10.5, hjust = 0),
          panel.grid = element_line(colour = "grey70"))

}

# heat map of pairwise Cliff's d statistics of two groups of films
cd_heatmap <- function(x, y, label_x = "", label_y = ""){
  
  # nest data
  dataNestedx <- x %>% group_by(Title) %>% nest()
  dataNestedy <- y %>% group_by(Title) %>% nest()
  
  # pairwise comparisons
  df_t <- data.frame()
  for (i in 1:length(dataNestedx$Title)){
    for (j in 1:length(dataNestedy$Title)){
    name1 <- dataNestedx$Title[i]
    name2 <- dataNestedy$Title[j]
    d <- dmes(dataNestedy$data[[j]]$SL, dataNestedx$data[[i]]$SL)$dc
    df_a <- data.frame(F1 = name1, F2 = name2, 
                       cd = d)
    df_t <- rbind.data.frame(df_t, df_a)
  }
  }
  
 p <- ggplot(data = df_t) +
   geom_tile(aes(x = F1, y = reorder(F2, desc(F2)), fill = as.numeric(cd))) +
   labs(x = label_x, y = label_y) +
   theme_minimal() +
   theme(legend.position = "right",
         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
   guides(fill = guide_colourbar(barwidth = 1, barheight = 20, title.position = "top")) +  
   scale_fill_viridis_c(name = "Cliff's d")
 
 return(p)
 
}

# heat map of pairwise Hodges-Lehmann median difference statistics of two groups of films
hld_heatmap <- function(x, y, label_x = "", label_y = ""){
  
  # nest data
  dataNestedx <- x %>% group_by(Title) %>% nest()
  dataNestedy <- y %>% group_by(Title) %>% nest()
  
  # pairwise comparisons
  df_t <- data.frame()
  for (i in 1:length(dataNestedx$Title)){
    for (j in 1:length(dataNestedy$Title)){
      name1 <- dataNestedx$Title[i]
      name2 <- dataNestedy$Title[j]
      hld <- HodgesLehmann(dataNestedx$data[[i]]$SL, dataNestedy$data[[j]]$SL, conf.level = 0.95)[1]
      df_a <- data.frame(F1 = name1, F2 = name2, 
                         HLD = hld)
      df_t <- rbind.data.frame(df_t, df_a)
    }
  }
  
  p <- ggplot(data = df_t) +
    geom_tile(aes(x = F1, y = reorder(F2, desc(F2)), fill = as.numeric(HLD))) +
    labs(x = label_x, y = label_y) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    guides(fill = guide_colourbar(barwidth = 1, barheight = 20, title.position = "top")) +  
    scale_fill_viridis_c(name = "HLD (s)")
  
  return(p)
  
}
