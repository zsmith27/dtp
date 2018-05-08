percentage <- function(long.df, site.col, dichotomous.col, taxa.col, count.col) {
  site.col <- rlang::enquo(site.col)
  dichotomous.col <- rlang::enquo(dichotomous.col)
  taxa.col <- rlang::enquo(taxa.col)
  count.col <- rlang::enquo(count.col)
  
  final.df <- long.df %>% 
    group_by(!!site.col, !!dichotomous.col, !!taxa.col) %>% 
    summarize(!!rlang::quo_name(count.col) := sum(!!count.col)) %>% 
    group_by(!!site.col, !!dichotomous.col) %>% 
    mutate(total = sum(!!count.col)) %>% 
    ungroup() %>% 
    mutate(percent = (!!count.col) / total * 100) %>% 
    rename(site = !!site.col,
           dicho = !!dichotomous.col) %>% 
    tidyr::complete(!!taxa.col,  nesting(site, dicho)) %>% # NESTING NEEDS WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mutate(percent := if_else(is.na(percent), 0, percent)) %>% 
    select(-(!!count.col), -total) %>% 
    rename(!!rlang::quo_name(site.col) := "site",
           !!rlang::quo_name(dichotomous.col) := "dicho")
}
#------------------------------------------------------------------------------
calc_stat <- function(long.df, dichotomous.col, dichotomous.vec, taxa.col, value.col, stat.funs) {
  dichotomous.col <- rlang::enquo(dichotomous.col)
  taxa.col <- rlang::enquo(taxa.col)
  value.col <- rlang::enquo(value.col)
  
  final.df <- long.df %>% 
    group_by(!!dichotomous.col, !!taxa.col) %>% 
    summarize(value = stat.funs(!!value.col)) %>% 
    spread(!!dichotomous.col, value) %>% 
    mutate(diff = !!rlang::sym(dichotomous.vec[[2]]) - !!rlang::sym(dichotomous.vec[[1]]), 
           rank = rank(diff, ties.method = "first")) %>% 
    #filter(abs(diff) >= diff.thresh) %>% 
    gather(!!dichotomous.col, value, dichotomous.vec) %>% 
    arrange(rank) %>% 
    mutate(!!rlang::quo_name(taxa.col) := factor(!!taxa.col, levels = unique(!!taxa.col)))
  
}
#------------------------------------------------------------------------------
dichotomous_taxa_plot <- function(long.df, dichotomous.col, dichotomous.vec, taxa.col, value.col,
                                  coord.flip) {
  dichotomous.col <- rlang::sym(dichotomous.col)
  taxa.col <- rlang::sym(taxa.col)
  value.col <- rlang::sym(value.col)
  
  final.plot <- long.df %>% 
    arrange(!!dichotomous.col) %>% 
    ggplot(aes_string(x = dplyr::quo_name(taxa.col),
                      y = dplyr::quo_name(value.col),
                      group = dplyr::quo_name(dichotomous.col),
                      fill = dplyr::quo_name(dichotomous.col),
                      color = dplyr::quo_name(dichotomous.col))) +
    geom_ribbon(aes(ymin = 0,
                    ymax = value),
                alpha= 0.3) +
    #geom_area(position = "stack", stat = "identity", alpha = 0.5) +
    #geom_point() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 20, face = "bold"),
          legend.text = element_text(size = 15, face = NULL),
          legend.title = element_text(size = 20)) +
    xlab("") +
    ylab("Relative Abundance")
  
  if (coord.flip == TRUE) {
    final.plot <- final.plot + coord_flip()
  } else {
    final.plot <- final.plot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
    return(final.plot)
}
#------------------------------------------------------------------------------
dichotomous_summary <- function(long.df, 
                                dichotomous.col, dichotomous.vec, 
                                site.col, taxa.col, count.col,
                                diff.thresh) {
  site.col <- rlang::sym(site.col)
  dichotomous.col <- rlang::sym(dichotomous.col)
  taxa.col <- rlang::sym(taxa.col)
  count.col <- rlang::sym(count.col)
  #coord.flip <- if_else(orientation == "Vertical", TRUE, FALSE)
  
  long.df %>% 
    filter((!!dichotomous.col) %in% dichotomous.vec) %>% 
    percentage(!!site.col, !!dichotomous.col, !!taxa.col, !!count.col) %>% 
    calc_stat(!!dichotomous.col, dichotomous.vec, !!taxa.col, percent, median) #%>% 
     # dichotomous_taxa_plot(!!dichotomous.col, dichotomous.vec, !!taxa.col, value,
     #                       coord.flip)
}
