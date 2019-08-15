
# https://davidgohel.github.io/officer/reference/index.html


#Automated Generation of PowerPoint Presentations from CDISC/ADaM Datasets
# https://www.pharmasug.org/proceedings/tokyo2018/presentations/PharmaSUG-Tokyo-2018-05.pdf

# http://lenkiefer.com/2017/09/23/crafting-a-powerpoint-presentation-with-r/


# install.packages(c("dplyr", "ggplot2", "magrittr",
#                    + "xtable", "flextable", "officer"), dep=T)
#  library(dplyr)
#  library(ggplot2)
#  library(magrittr)
#  library(knitr)
#  library(xtable)
#  library(flextable)
#  library(officer)



print2pptx <- function(mypptx=NULL, FIGURE=NULL, TABLE=NULL ) { 
  
  library(magrittr)
  library(officer)
  library(ggplot2)
  #library(extrafont)
  library(flextable)
  library(knitr)
  
  default_pptx_width=8;     # 8
  default_pptx_height=5.2;    # 6
  
  default_fontsize = 16; 
  default_title = "Type in title"
  
  # loadfonts(device = "win")
  # windowsFonts(Times = windowsFont("TT Times New Roman"))
  # 
  # theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
  #             theme(panel.grid.major = element_blank(), 
  #                   panel.grid.minor = element_blank()))
  
  if (is.null(mypptx)) {mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate.pptx"))}  
  mylab <- layout_summary(mypptx)[[1]] # Slide Layout Name
  mytmp <- layout_summary(mypptx)[[2]][1] # Show Slide Master Name
  
  # mypptx %>%
  #    layout_properties(layout="Title and Content", master=mytmp)%>%
  #   knitr::kable(digits=2) 
  # 
  # for (i in 1:length(mylab)) {
  #   layout_properties(mypptx, mylab[i], master=mytmp) %>% kable(digits=2) %>% print()
  # }
  
  ############################
  # Title, subtitle, date
  ############################  
  # mypptx <- mypptx %>%
  #    add_slide(layout="Title Slide", master=mytmp) %>%
  #    ph_with_text(type="ctrTitle", str="Key Result Memo") #%>%
     #ph_with_text(type="subTitle", str="Table & Figure") %>%
     #ph_with_text(type="dt", str=format(Sys.Date()))  
    
  
  
  #https://stackoverflow.com/questions/50726187/how-can-i-change-in-r-the-font-family-of-a-title-with-officer
  format_main_title <- fp_text(font.family='Rage Italic', font.size=72)
  format_page_title <- fp_text(font.family='Calibri Light', font.size=32)
  
  
  #print(mypptx, target="./sample.pptx")
  ########################################################################################
  ########################################################################################
  # to word and ppt
  ########################################################################################
  ########################################################################################
   
  #--------------------------------------
  # figure
  #--------------------------------------  
  figure_name_lst = names(FIGURE)
  
  if (length(figure_name_lst)>0) {
  for(i in 1:length(FIGURE)) {
    if (length(FIGURE)==0) {break}
    
    figure_name = figure_name_lst[i]
    print(figure_name)
    
    new_figure <- FIGURE[[figure_name]]
    title = attr(new_figure, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_figure, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    
    pptx_width =  attr(new_figure, "pptx_width"); pptx_width = ifelse(is.null(pptx_width), default_pptx_width, pptx_width)
    pptx_height =  attr(new_figure, "pptx_height"); pptx_height = ifelse(is.null(pptx_height), default_pptx_height, pptx_height)
    
    fontsize =  attr(new_figure, "fontsize"); fontsize = ifelse(is.null(fontsize), default_fontsize, fontsize)
     
    #filename <- tempfile()
    #ggsave("fig.pdf", new_figure,  width=12, height=6, scale=0.5, dpi="retina", device="pdf")
    #ggsave(file, new_figure,  width=12, height=6, scale=0.5, dpi="retina", device="png")
    
    # grDevices::png(filename = "fig1.png",
    #                 width = width, height = height,
    #                 units = 'in', pointsize = fontsize, bg = "white",
    #                 res = 300
    # )
    # new_figure
    # fig1
    # dev.off()
    # 
    # unlink(filename)
    
    # add figure 
    # -----------------------
    # style_list %>% filter(style_type %in% "figure")   
    
    
     
    mypptx <- mypptx %>% 
      # Add Figures (image file / ggplot object)
      # ---------------------------------------------
       add_slide(layout="Title and Content", master=mytmp) %>%
      
       ph_empty(type='title') %>%
       ph_add_fpar(fpar(ftext(title, prop=format_page_title)))  %>%
       #ph_with_text(type="title", str=title) %>%
       
       #ph_with_text(type="ftr", str="A footnote") %>%
       #ph_with_text(type="dt", str=format(Sys.Date())) %>%
       
       ph_with_gg_at(value=new_figure, width = pptx_width, height = pptx_height, left=2.5, top=2)   
      #ph_with_img(src = "fig1.png", type = "body", width = width, height = height)
      #ph_with_img_at(src="C:/temp/myplot.png", left=2, top=2, width=5, height=5)
    
      #  add unordered lists to a slide with some format.
      # -----------------------------------------------------
       # add_slide(layout="Title and Content", master=mytmp) %>%
       # ph_with_text(type="title", str="A title") %>%
       # ph_with_ul(type="body", level_list=c(1,2), str_list=c("aaa","bbb"),
       #            style=fp_text(font.size=0, color="red")) #%>%
       #ph_add_par(type="body", level=3) %>%
       #ph_add_text(type="body", str="ccc", style=fp_text(color="blue"))
    
    
  }}
  
  #--------------------------------------
  # table
  #--------------------------------------
  table_name_lst = names(TABLE)
  
  if (length(table_name_lst)>0) {
  for(i in 1:length(TABLE)) {
    if (length(TABLE)==0) {break}
    
    table_name = table_name_lst[i]
    print(table_name)
    
    new_table <- TABLE[[table_name]]
    title = attr(new_table, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_table, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    fontsize =  attr(new_table, "fontsize"); fontsize = ifelse(is.null(fontsize), default_fontsize, fontsize)
    
    # add table 
    # https://davidgohel.github.io/officer/articles/offcran/tables.html
    # -----------------------
    # style_list %>% filter(style_type %in% "table")  
    library(flextable)
    # myft <- regulartable(new_table) # Create "flextable" object
    #  myft <- theme_booktabs(myft) # Change "flextable" theme
    #   myft <- autofit(myft) # Adjust Cell Width and Height
    
    #Both flextable() and regulartable() functions produce a     flextable. The first one is resource consuming.
    ft_new_table <- regulartable(data = new_table) %>%  #Create "flextable" object
      theme_zebra() %>% # Change "flextable" theme
      
      # theme_booktabs() %>% # Change "flextable" theme
      # theme_box() %>% # Change "flextable" theme
      # theme_tron()%>% # Change "flextable" theme
      # theme_tron_legacy()%>% # Change "flextable" theme
      # theme_vanilla()%>% # Change "flextable" theme
      # theme_zebra()%>% # Change "flextable" theme
      # empty_blanks() %>% # Change "flextable" theme
    
      #set_header_labels( n = "#", Mean = "\u03D1", SD = "\u03C3") %>% 
      #color(i = ~ n < 4, color = "wheat") %>% 
      fontsize(size = fontsize, part = "all") %>%    #'all', 'body', 'header', 'footer')
      autofit()  # Adjust Cell Width and Height
    
    mypptx <- mypptx %>% 
      add_slide(layout="Title and Content", master=mytmp) %>%
      #ph_with_text(type="title", str=title) %>%
      
      ph_empty(type='title') %>%
      ph_add_fpar(fpar(ftext(title, prop=format_page_title)))  %>%
      
      #ph_with_table(value=new_table)   %>% 
      #ph_with_table_at(value=new_table, left=1, top=2, width=8, height=5)
    
      ph_with_flextable_at(ft_new_table, left=3, top=3) #%>% 
      
      #add_slide(layout = "Title Only", master = "Office Theme") %>% 
      #ph_with_flextable(ft_new_table, location = ph_location(left = 3, top = 3)) 
    
      # ph_with_table_at(value=iris[1:5,], left=1, top=2, width=8, height=5)
      # ph_with_flextable(myft, type="body")
    
      # add_slide(layout="Two Content", master=mytmp) %>%
      #   + ph_with_text(type="title", str=title) %>%
      #   + ph_with_table(type="body", value=iris[1:5,], index=1) %>%
      #   + ph_with_text(type="body", str="Iris data", index=2)
  
  }}
  
  return(mypptx)
}



idebug = 0
if (idebug == 1) { 
  library(magrittr)
  library(officer)
  library(ggplot2)
  library(knitr)
  
  fig1 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
    geom_point()
  fig2 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) + 
    geom_point()
  
  
  tab1 <- head(iris)
  tab2 <- head(mtcars)[, 1:4]
  
  FIGURE <- NULL
  
  attr(fig1, "title") = "sfsfssssssssssssssssssssssssssssssssssssssssssssssgsdf"
  FIGURE[["fig1"]] = fig1
  FIGURE[["fig2"]] = fig2
  FIGURE[["fig3"]] = fig2
  
  TABLE <- NULL
  attr(tab1, "title") = "sfsfssssssssssssssssssssssssssssssssssssssssssssssgsdf"
  
  TABLE[["tab1"]] = tab1
  TABLE[["tab2"]] = tab2
  
  mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate_long_format.pptx"))
  mypptx <- mypptx %>% print2pptx(FIGURE, TABLE)
  print(mypptx, target = "./aoutput.pptx")
   

}
