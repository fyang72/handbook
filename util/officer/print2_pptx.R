
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



print2_pptx <- function(mypptx=NULL, 
                        FIGURE=NULL, TABLE=NULL,  
                        width_default=6.4,     # 8
                        height_default=4.8,    # 6
                        fontsize_default=12, 
                        title_default = "Type in title"
) { 
  
  library(magrittr)
  library(officer)
  library(ggplot2)
  library(extrafont)
  library(flextable)
  library(knitr)
  
  
  # loadfonts(device = "win")
  # windowsFonts(Times = windowsFont("TT Times New Roman"))
  # 
  # theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
  #             theme(panel.grid.major = element_blank(), 
  #                   panel.grid.minor = element_blank()))
  
  if (is.null(mypptx)) {mypptx <- read_pptx(paste0(HOME, "/lib/pptTemplate_long_format.pptx"))}  
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
  mypptx <- mypptx %>%
     add_slide(layout="Title Slide", master=mytmp) %>%
     ph_with_text(type="ctrTitle", str="Iris Data") %>%
     ph_with_text(type="subTitle", str="Table & Figure") %>%
     ph_with_text(type="dt", str=format(Sys.Date()))  
    
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
  
  for(i in 1:length(FIGURE)) {
    if (length(FIGURE)==0) {break}
    
    figure_name = figure_name_lst[i]
    print(figure_name)
    
    new_figure <- FIGURE[[figure_name]]
    title = attr(new_figure, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_figure, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    width =  attr(new_figure, "width"); width = ifelse(is.null(width), width_default, width)
    height =  attr(new_figure, "height"); height = ifelse(is.null(height), height_default, height)
    fontsize =  attr(new_figure, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
     
    filename <- tempfile()
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
       ph_with_text(type="title", str=title) %>%
       
       ph_with_text(type="ftr", str="A footnote") %>%
       ph_with_text(type="dt", str=format(Sys.Date())) %>%
       
       ph_with_gg(value=new_figure)  %>% 
      #ph_with_img(src = "fig1.png", type = "body", width = width, height = height)
      #ph_with_img_at(src="C:/temp/myplot.png", left=2, top=2, width=5, height=5)
    
      #  add unordered lists to a slide with some format.
      # -----------------------------------------------------
       add_slide(layout="Title and Content", master=mytmp) %>%
       ph_with_text(type="title", str="A title") %>%
       ph_with_ul(type="body", level_list=c(1,2), str_list=c("aaa","bbb"),
                  style=fp_text(font.size=0, color="red")) #%>%
       #ph_add_par(type="body", level=3) %>%
       #ph_add_text(type="body", str="ccc", style=fp_text(color="blue"))
    
    
  }
  
  #--------------------------------------
  # table
  #--------------------------------------
  table_name_lst = names(TABLE)
  
  for(i in 1:length(TABLE)) {
    if (length(TABLE)==0) {break}
    
    table_name = table_name_lst[i]
    print(table_name)
    
    new_table <- TABLE[[table_name]]
    title = attr(new_table, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_table, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    width =  attr(new_table, "width"); width = ifelse(is.null(width), width_default, width)
    height =  attr(new_table, "height"); height = ifelse(is.null(height), height_default, height)
    fontsize =  attr(new_table, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
    
    # add table 
    # -----------------------
    # style_list %>% filter(style_type %in% "table")  
    library(flextable)
    # myft <- regulartable(new_table) # Create "flextable" object
    #  myft <- theme_booktabs(myft) # Change "flextable" theme
    #   myft <- autofit(myft) # Adjust Cell Width and Height
    
    mypptx <- mypptx %>% 
      add_slide(layout="Title and Content", master=mytmp) %>%
      ph_with_text(type="title", str=title) %>%
      ph_with_table(value=new_table) 
      # ph_with_table_at(value=iris[1:5,], left=1, top=2, width=8, height=5)
      # ph_with_flextable(myft, type="body")
    
      # add_slide(layout="Two Content", master=mytmp) %>%
      #   + ph_with_text(type="title", str=title) %>%
      #   + ph_with_table(type="body", value=iris[1:5,], index=1) %>%
      #   + ph_with_text(type="body", str="Iris data", index=2)
  
  }  
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
  FIGURE[["fig1"]] = fig1
  FIGURE[["fig2"]] = fig2
  FIGURE[["fig3"]] = fig2
  
  TABLE <- NULL
  TABLE[["tab1"]] = tab1
  TABLE[["tab2"]] = tab2
  
  mypptx <- read_pptx("./lib/pptTemplate_long_format.pptx")
  mypptx <- mypptx %>% print2_pptx(FIGURE, TABLE)
  print(myppt, target = "./toc_and_captions.pptx")
   

}
