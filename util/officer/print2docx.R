 
# https://davidgohel.github.io/officer/reference/index.html

# https://www.pharmasug.org/proceedings/tokyo2018/presentations/PharmaSUG-Tokyo-2018-05.pdf

# http://lenkiefer.com/2017/09/23/crafting-a-powerpoint-presentation-with-r/


print2docx <- function(mydocx=NULL, FIGURE=NULL, TABLE=NULL) { 
  
  library(magrittr)
  library(officer)
  library(ggplot2)
  #library(extrafont)
  library(flextable)
  
  default_docx_width=6.4;     # 8
  default_docx_height=4.8;    # 6
  
  fontsize_default=12; 
  title_default = "Type in title"
  
  # loadfonts(device = "win")
  # windowsFonts(Times = windowsFont("TT Times New Roman"))
  # 
  # theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
  #             theme(panel.grid.major = element_blank(), 
  #                   panel.grid.minor = element_blank()))
  
  if (is.null(mydocx)) {
    mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx"))
  }  
  
  ########################################################################################
  ########################################################################################
  # to word and ppt
  ########################################################################################
  ########################################################################################
  
  style_list <- styles_info(mydocx)  
  bookmark_list <- docx_bookmarks(mydocx)
   
  #--------------------------------------
  # figure
  #--------------------------------------  
  figure_name_lst = names(FIGURE)

  if (length(figure_name_lst)>0) {
  for(i in 1:length(FIGURE)) {
    
    figure_name = figure_name_lst[i]
    print(figure_name)

    new_figure <- FIGURE[[figure_name]]
    title = attr(new_figure, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_figure, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    
    docx_width =  attr(new_figure, "docx_width"); docx_width = ifelse(is.null(docx_width), default_docx_width, docx_width)
    docx_height =  attr(new_figure, "docx_height"); docx_height = ifelse(is.null(docx_height), default_docx_height, docx_height)
 
    fontsize =  attr(new_figure, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
    
    # filename <- tempfile()
    # #ggsave("fig.pdf", new_figure,  width=12, height=6, scale=0.5, dpi="retina", device="pdf")
    # #ggsave(file, new_figure,  width=12, height=6, scale=0.5, dpi="retina", device="png")
    #  
    # grDevices::png (filename = filename,
    #                 width = docx_width, height = docx_height,
    #                 units = 'in', pointsize = fontsize, bg = "white",
    #                 res = 300
    # )
    # new_figure
    # dev.off()
    # 
    # unlink(filename)
    
    # add figure 
    # -----------------------
    # style_list %>% filter(style_type %in% "figure")   
    if (figure_name %in% bookmark_list) {
      mydocx <- mydocx %>% 
        cursor_begin() %>%  
        cursor_bookmark(figure_name) %>%
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_gg(value = new_figure, style = "Figure", width = docx_width, height = docx_height)
        #body_add_img(src = "fig.png", width = 5, height = 6, style = "Figure")
        
    }else{
      mydocx <- mydocx %>% 
        cursor_end() %>%   
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_gg(value = new_figure, style = "Figure", width = docx_width, height = docx_height)
        #body_add_img(src = "fig.png", width = 5, height = 6, style = "Figure")
    }
    
  
  }}

  #--------------------------------------
  # table
  #--------------------------------------
  table_name_lst = names(TABLE)
   
  if (length(table_name_lst)>0) {
  for(i in 1:length(TABLE)) {
    
    table_name = table_name_lst[i]
    print(table_name)
    
    new_table <- TABLE[[table_name]]
    title = attr(new_table, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_table, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    fontsize =  attr(new_table, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
    
    # add table 
    # -----------------------
    # style_list %>% filter(style_type %in% "table")  
    if (table_name %in% bookmark_list) {    
      mydocx <- mydocx %>% 
        cursor_begin() %>%  
        cursor_bookmark(table_name) %>%
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        #body_add_table(value = new_table, style = "Body Table") 
        body_add_flextable(
          flextable(data = new_table) %>% 
            theme_booktabs() %>% 
            #set_header_labels( n = "#", Mean = "\u03D1", SD = "\u03C3") %>% 
            #color(i = ~ n < 4, color = "wheat") %>% 
            autofit() 
        )
    }else{
      mydocx <- mydocx %>% 
        cursor_end() %>%   
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        #body_add_table(value = new_table, style = "Body Table")
        body_add_flextable(
          flextable(data = new_table) %>% 
            theme_booktabs() %>% 
            #set_header_labels( n = "#", Mean = "\u03D1", SD = "\u03C3") %>% 
            #color(i = ~ n < 4, color = "wheat") %>% 
            autofit() 
        )
    }
    
  }}  
  return(mydocx)
}
  
   
idebug = 0
if (idebug == 1) { 
  
  library(magrittr)
  library(officer)
  library(ggplot2)

  fig1 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
    geom_point()
  fig2 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) + 
    geom_point()
  
  
  tab1 <- head(iris)
  tab2 <- head(mtcars)[, 1:4]
  
  attr(fig1, "docx_width") = 12   
  attr(fig1, "docx_height") = 6   
  attr(fig1, "title") = "just test fig1"
  
  FIGURE <- NULL
  FIGURE[["fig1"]] = fig1
  FIGURE[["fig2"]] = fig2
  FIGURE[["fig3"]] = fig2

  
  attr(tab1, "title") = "just test tab1"
  
  TABLE <- NULL
  TABLE[["tab1"]] = tab1
  TABLE[["tab2"]] = tab2

  
  mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx"))
  mydocx <- mydocx %>% print2docx(FIGURE, TABLE)
  print(mydocx, target = paste0(HOME, "/toc_and_captions.docx"))
  
    # add paragraph,  
    # -----------------------  
    # Let add text at the beginning of the
    # paragraph containing text "paragraph 4"
    # slip_in_img
    # slip_in_seqfield
    # slip_in_text
    #cursor_reach(keyword = "paragraph 4") %>%
    #slip_in_text("This is ", pos = "before", style = "Default Paragraph Font") %>%
    
    
    # body_add_par("R logo: ", style = "Normal") %>%
    # slip_in_img(src = img.file, style = 'Header Char', 
    #             width = .3, height = .3, pos = "after") %>% 
    # slip_in_text(" - This is ", style = 'Header Char', pos = "before") %>% 
    # slip_in_seqfield(str = "SEQ Figure \u005C* ARABIC",
    #                  style = 'Header Char', pos = "before")
   
} 
