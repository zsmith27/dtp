original.df <- data.table::fread("data/ny_fish.csv")

long.df<- original.df
dichotomous.col <- "basin"
dichotomous.vec <- unique(original.df$basin)[1:2]
site.col <- "site"
taxa.col <- "family"
count.col <- "count"

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
    tidyr::complete(!!taxa.col,  nesting(site, basin)) %>% # NESTING NEEDS WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mutate(percent := if_else(is.na(percent), 0, percent)) %>% 
    select(-(!!count.col), -total)
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

test <- dichotomous_summary(original.df, "basin", unique(original.df$basin)[c(1,4)],
                            "site", "vernacular_name", "count")
dichotomous.col <- quo(basin)
taxa.col <- quo(vernacular_name)
value.col <- quo(median)
test %>% 
  arrange(!!dichotomous.col) %>% 
  ggplot(aes_string(x = dplyr::quo_name(taxa.col),
                    y = dplyr::quo_name(value.col),
                    group = dplyr::quo_name(dichotomous.col),
                    fill = dplyr::quo_name(dichotomous.col),
                    color = dplyr::quo_name(dichotomous.col))) +
  geom_ribbon(aes(ymin = quant25,
                  ymax = quant75),
              alpha= 0.3, linetype = 0) +
  geom_line(size = 1) +
  #geom_area(position = "stack", stat = "identity", alpha = 0.5) +
  #geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  coord_flip()

dichotomous_taxa_plot(test, "basin", c("chesapeake", "Mississippi"), "vernacular_name", )
dichotomous.col <- quo(basin)
taxa.col <- quo(family)
value.col <- quo(value)
dichotomous_taxa_plot <- function(long.df, dichotomous.col, dichotomous.vec, taxa.col, value.col) {
  dichotomous.col <- rlang::enquo(dichotomous.col)
  taxa.col <- rlang::enquo(taxa.col)
  value.col <- rlang::enquo(value.col)
  
  long.df %>% 
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_flip()
}

taxa.col <- rlang::quo(vernacular_name)
dichotomous.col <- rlang::quo(gradient)
dichotomous.vec <- c("reference", "degraded")
ditaxa_plot <- function(fish.long, dichotomous.col, dichotomous.vec, taxa.col) {
  taxa.col <- rlang::enquo(taxa.col)
  dichotomous.col <- rlang::enquo(dichotomous.col)
  
  fish.long2 <- fish.long %>% 
    filter((!!dichotomous.col) %in% dichotomous.vec) %>% 
    filter(!is.na(!!dichotomous.col)) %>% 
    select(!!dichotomous.col, !!taxa.col, count) %>% 
    group_by(!!dichotomous.col, !!taxa.col) %>% 
    summarize(count = sum(count)) %>% 
    ungroup() %>% 
    mutate(!!rlang::quo_name(dichotomous.col) := tolower(!!dichotomous.col),
           !!rlang::quo_name(dichotomous.col) := str_replace_all(!!dichotomous.col, " |/", "_"),
           !!rlang::quo_name(dichotomous.col) := factor(!!dichotomous.col, levels = dichotomous.vec)) %>% 
    group_by(!!dichotomous.col, !!taxa.col) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    mutate(count = rank(count, ties.method = "first")) 
  
  
  final.df <- fish.long2 %>% 
    group_by(!!dichotomous.col) %>% 
    mutate(total = sum(count)) %>% 
    group_by(!!dichotomous.col, !!taxa.col) %>% 
    summarize(pct = count / total * 100,
              value = median(pct)) %>% 
    select(-pct) %>% 
    spread(!!dichotomous.col, value, fill = 0) %>% 
    mutate(diff = !!rlang::sym(dichotomous.vec[[2]]) - !!rlang::sym(dichotomous.vec[[1]]), 
           rank = rank(diff, ties.method = "first")) %>% 
    gather(!!dichotomous.col, percent, dichotomous.vec) %>% 
    arrange(rank) %>% 
    mutate(!!rlang::quo_name(taxa.col) := factor(!!taxa.col, levels = unique(!!taxa.col)))
  
  final.df %>% 
    arrange(!!dichotomous.col) %>% 
    ggplot(aes_string(x = dplyr::quo_name(taxa.col), y = "percent",
                      group = dplyr::quo_name(dichotomous.col),
                      fill = dplyr::quo_name(dichotomous.col),
                      color = dplyr::quo_name(dichotomous.col))) +
    geom_ribbon(aes(ymin = 0, ymax = percent), alpha= 0.3) +
    #geom_area(position = "stack", stat = "identity", alpha = 0.5) +
    #geom_point() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_flip()
  
}
