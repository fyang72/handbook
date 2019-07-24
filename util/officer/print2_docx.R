 
# https://davidgohel.github.io/officer/reference/index.html

# https://www.pharmasug.org/proceedings/tokyo2018/presentations/PharmaSUG-Tokyo-2018-05.pdf

# http://lenkiefer.com/2017/09/23/crafting-a-powerpoint-presentation-with-r/


print2_docx <- function(mydoc=NULL, 
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
  
  loadfonts(device = "win")
  windowsFonts(Times = windowsFont("TT Times New Roman"))
  
  theme_set(theme_bw(base_size=12,base_family='Times New Roman')+
              theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank()))
  
  if (is.null(mydoc)) {
    mydoc <- read_docx("./lib/docTemplate.docx")
  }  
  
  ########################################################################################
  ########################################################################################
  # to word and ppt
  ########################################################################################
  ########################################################################################
  
  style_list <- styles_info(mydoc)  
  bookmark_list <- docx_bookmarks(mydoc)
   
  #--------------------------------------
  # figure
  #--------------------------------------  
  figure_name_lst = names(FIGURE)

  for(i in 1:length(FIGURE)) {
    
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
     
    grDevices::png (filename = filename,
                    width = width, height = height,
                    units = 'in', pointsize = fontsize, bg = "white",
                    res = 300
    )
    new_figure
    dev.off()
    
    unlink(filename)
    
    # add figure 
    # -----------------------
    # style_list %>% filter(style_type %in% "figure")   
    if (figure_name %in% bookmark_list) {
      mydoc <- mydoc %>% 
        cursor_begin() %>%  
        cursor_bookmark(figure_name) %>%
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_gg(value = new_figure, style = "Figure")
        #body_add_img(src = "fig.png", width = 5, height = 6, style = "Figure")
        
    }else{
      mydoc <- mydoc %>% 
        cursor_end() %>%   
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_gg(value = new_figure, style = "Figure")
        #body_add_img(src = "fig.png", width = 5, height = 6, style = "Figure")
    }
    
  
  }

  #--------------------------------------
  # table
  #--------------------------------------
  table_name_lst = names(TABLE)
   
  for(i in 1:length(TABLE)) {
    
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
    if (table_name %in% bookmark_list) {    
      mydoc <- mydoc %>% 
        cursor_begin() %>%  
        cursor_bookmark(table_name) %>%
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_table(value = new_table, style = "Body Table")  
    }else{
      mydoc <- mydoc %>% 
        cursor_end() %>%   
        body_add_par(value = title, style = "Caption1") %>% 
        #shortcuts$slip_in_plotref(depth = 1) %>%
        body_add_table(value = new_table, style = "Body Table")
    }
    
  }  
  return(mydoc)
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
  
  FIGURE <- NULL
  FIGURE[["fig1"]] = fig1
  FIGURE[["fig2"]] = fig2
  FIGURE[["fig3"]] = fig2
  
  TABLE <- NULL
  TABLE[["tab1"]] = tab1
  TABLE[["tab2"]] = tab2
  
  mydoc <- read_docx("./lib/docTemplate.docx")
  mydoc <- mydoc %>% print2_word(FIGURE, TABLE)
  print(mydoc, target = "./toc_and_captions.docx")
  
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
