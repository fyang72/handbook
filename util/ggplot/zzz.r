
.onAttach<- function(libname, pkgname){
  packageStartupMessage("My package for handbook Tables/Figures/Reports produced by Feng Yang\n")
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 12))
  ggplot2::theme_update(legend.position = "bottom",
               panel.margin = ggplot2::unit(0, "inches"),
               strip.background = ggplot2::element_rect(fill = "white", colour = "grey50", size = 0.2, linetype = NULL))
}


