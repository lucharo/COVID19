suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(gganimate))

args = commandArgs(trailingOnly = T)
if (purrr::is_empty(args)){
  gif = F
}else{
  gif = as.logical(args[1])
}

# Race plots (GIFs) -------------------------------------------------------
if (gif){
  all.time = readRDS('ProcessedData/cleanTime.rds')
  print("Making GIFS...")
  
  all.time = all.time %>% distinct()
  
  all.timeLONG = all.time %>%
    pivot_wider(names_from = Metric, values_from = Amount) %>%
    filter(Date == max(Date)) %>% arrange(-Confirmed)
  
  ## inspired by"
  # https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other/53163549
  # https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim
  anim_table =  all.time %>%
    filter(Country %in% all.timeLONG$Country[1:20]) %>%
    mutate(Date=as.numeric(Date-min(Date))) %>%
    arrange(Metric)
  
  anim_table_summary.conf = anim_table %>% filter(Metric == "Confirmed") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  anim_table_summary.dead = anim_table %>% filter(Metric == "Deaths") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  anim_table_summary.reco = anim_table %>% filter(Metric == "Recovered") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  
  # anim_table_smooth = anim_table %>%
  #   filter(Metric == "Confirmed") %>%
  #   group_by(Country) %>%
  #   # Do somewhat rough interpolation for ranking
  #   # (Otherwise the ranking shifts unpleasantly fast.)
  #   complete(Date= full_seq(Date, 0.5)) %>%
  #   mutate(Amount = spline(x = Date, y = Amount, xout = Date)$y) %>%
  #   group_by(Date) %>%
  #   mutate(rank = min_rank(-Amount) * 1) %>%
  #   ungroup() %>%
  #   # Then interpolate further to quarter years for fast number ticking.
  #   # Interpolate the ranks calculated earlier.
  #   group_by(Country) %>%
  #   complete(Date = full_seq(Date, .25)) %>%
  #   mutate(Amount = spline(x = Date, y = Amount, xout = Date)$y) %>%
  #   # "approx" below for linear interpolation. "spline" has a bouncy effect.
  #   mutate(rank = approx(x = Date, y = rank, xout = Date)$y) %>%
  #   ungroup() %>%
  #   arrange(Country,Date)
  
  # anim_table_smooth$Amount = ifelse(anim_table_smooth$Amount <0,
  #                                   0, round(anim_table_smooth$Amount))
  # anim_table_smooth$Country = as.factor(anim_table_smooth$Country)
  
  StartDate = min(all.time$Date)
  
  toAnimate = function(anim_table){
    p <- ggplot(anim_table, aes(rank, group = as.factor(Country),
                                fill = as.factor(Continent),
                                color = as.factor(Continent))) +
      geom_tile(aes(y = Amount/2,
                    height = Amount,
                    width = 0.9), alpha = 0.8) +
      
      # text in x-axis (requires clip = "off" in coord_*)
      # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
      #   leads to weird artifacts in text spacing.
      geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y = Amount,
                    label = scales::comma(Amount)), hjust = 0, nudge_y = 300)  +
      
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous() +
      scale_x_reverse() +
      guides(color = FALSE) +
      labs(title='Date: {format(StartDate+closest_state %>% as.numeric %>% floor,"%B %dnd, %Y")}',
           x = "", y = "Confirmed cases", fill = "Continent", color ="") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0, size = 22),
            axis.ticks.y = element_blank(),  # These relate to the axes post-flip
            axis.text.y  = element_blank(),  # These relate to the axes post-flip
            plot.margin = margin(t=1,r=2,b=1,l=3, "cm")) +
      transition_states(Date, transition_length = 1, state_length = 3) +
      enter_grow() +
      exit_shrink() +
      ease_aes('linear')
    
    return(p)
  }
  
  anim_save("Plots/confirmed.gif",
            animate(toAnimate(anim_table_summary.conf), fps = 15,duration = 30, end_pause = 10))
  print("GIF 1 done.")
  anim_save("Plots/deaths.gif",
            animate(toAnimate(anim_table_summary.dead), fps = 15,duration = 30, end_pause = 10))
  print("GIF 2 done.")
  anim_save("Plots/recovered.gif",
            animate(toAnimate(anim_table_summary.reco), fps = 15,duration = 30, end_pause = 10))
  print("GIF 3 done.")
}