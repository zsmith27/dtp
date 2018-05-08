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
calc_stat <- function(long.df, dichotomous.col, dichotomous.vec, taxa.col, value.col) {
  dichotomous.col <- rlang::enquo(dichotomous.col)
  taxa.col <- rlang::enquo(taxa.col)
  value.col <- rlang::enquo(value.col)
  
  final.df <- long.df %>% 
    group_by(!!dichotomous.col, !!taxa.col) %>% 
    summarize(median = median(!!value.col),
              quant25 = quantile(!!value.col, 0.25),
              quant75 = quantile(!!value.col, 0.75)) 
  
}
#------------------------------------------------------------------------------
organize_stat <- function(long.df, dichotomous.col, dichotomous.vec, taxa.col) {
  dichotomous.col <- rlang::enquo(dichotomous.col)
  taxa.col <- rlang::enquo(taxa.col)
  
  final.df <- long.df %>% 
    tidyr::unite("value", c("median", "quant25", "quant75")) %>% 
    spread(!!dichotomous.col, value) %>% 
    separate(!!rlang::sym(dichotomous.vec[[2]]),
             c(paste(dichotomous.vec[[2]], "median", sep = "_"),
               paste(dichotomous.vec[[2]], "quant25", sep = "_"),
               paste(dichotomous.vec[[2]], "quant75", sep = "_")
             ), "_", convert = TRUE) %>% 
    separate(!!rlang::sym(dichotomous.vec[[1]]),
             c(paste(dichotomous.vec[[1]], "median", sep = "_"),
               paste(dichotomous.vec[[1]], "quant25", sep = "_"),
               paste(dichotomous.vec[[1]], "quant75", sep = "_")
             ), "_", convert = TRUE) %>% 
    mutate(median_diff = !!rlang::sym(paste(dichotomous.vec[[2]], "median", sep = "_")) - 
             !!rlang::sym(paste(dichotomous.vec[[1]], "median", sep = "_")),
           diff = case_when(
             median_diff < 0 ~ !!rlang::sym(paste(dichotomous.vec[[1]], "quant25", sep = "_")) - 
               !!rlang::sym(paste(dichotomous.vec[[2]], "quant75", sep = "_")),
             median_diff > 0 ~ !!rlang::sym(paste(dichotomous.vec[[2]], "quant25", sep = "_")) - 
               !!rlang::sym(paste(dichotomous.vec[[1]], "quant75", sep = "_")),
             median_diff == 0 ~ 0,
             TRUE ~ 100000000
           ),
           diff = median_diff,
           rank = rank(diff, ties.method = "first")) %>% 
    unite(!!rlang::sym(dichotomous.vec[[1]]), 
          c(paste(dichotomous.vec[[1]], "median", sep = "_"),
            paste(dichotomous.vec[[1]], "quant25", sep = "_"),
            paste(dichotomous.vec[[1]], "quant75", sep = "_"))) %>% 
    unite(!!rlang::sym(dichotomous.vec[[2]]), 
          c(paste(dichotomous.vec[[2]], "median", sep = "_"),
            paste(dichotomous.vec[[2]], "quant25", sep = "_"),
            paste(dichotomous.vec[[2]], "quant75", sep = "_"))) %>% 
    gather(!!dichotomous.col, value, dichotomous.vec) %>% 
    arrange(rank) %>% 
    mutate(!!rlang::quo_name(taxa.col) := factor(!!taxa.col, levels = unique(!!taxa.col))) %>% 
    separate("value", c("median", "quant25", "quant75"), "_", convert = TRUE)
}
#------------------------------------------------------------------------------
dichotomous_summary <- function(long.df, 
                                dichotomous.col, dichotomous.vec, 
                                site.col, taxa.col, count.col) {
  site.col <- rlang::sym(site.col)
  dichotomous.col <- rlang::sym(dichotomous.col)
  taxa.col <- rlang::sym(taxa.col)
  count.col <- rlang::sym(count.col)
  
  final.df <- long.df %>% 
    filter((!!dichotomous.col) %in% dichotomous.vec) %>% 
    percentage(!!site.col, !!dichotomous.col, !!taxa.col, !!count.col) %>% 
    calc_stat(!!dichotomous.col, dichotomous.vec, !!taxa.col, percent) %>% 
    organize_stat(!!dichotomous.col, dichotomous.vec, !!taxa.col)
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
    geom_ribbon(aes(ymin = quant25,
                    ymax = quant75),
                alpha = 0.2,
                linetype = 0) +
    geom_line(size = 1) +
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

