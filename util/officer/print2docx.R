
# https://www.brodrigues.co/blog/2018-10-05-ggplot2_purrr_officer/
  
# devtools::install_github("davidgohel/officer")
# devtools::install_github("davidgohel/rvg")

# https://github.com/tomwenseleers/export


# https://davidgohel.github.io/officer/reference/index.html

# https://www.pharmasug.org/proceedings/tokyo2018/presentations/PharmaSUG-Tokyo-2018-05.pdf

# http://lenkiefer.com/2017/09/23/crafting-a-powerpoint-presentation-with-r/

#body_bookmark("bookmark_time_profile_typical_subj_linear") %>%
# cursor_bookmark("bookmark_time_profile_typical_subj_linear") %>%
#body_replace_text_at_bkm("bookmark_time_profile_typical_subj_linear", "not left aligned")

# https://github.com/davidgohel/officer/issues/75
#   The easiest way to fix this problem is to select the entire line of text in question, copy-and-paste it into a text editor (such as Notepad or TextEdit), and then copy-and-paste from the text editor back into Word, replacing the previous content.
# 
# If you then re-load the doc, navigate the cursor to the line in question, and run docx_show_chunk(), you should get output like this:


print2docx <- function(
  mydocx=NULL, FIGURE=NULL, TABLE=NULL, 
  BODY_REPLACE_TEXT_AT_BKM=NULL, BODY_REPLACE_ALL_TEXT=NULL) { 
  
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
    
  
  style_list <- styles_info(mydocx)  
  bookmark_list <- docx_bookmarks(mydocx)
   
  # bookmark -----
  if (!is.null(BODY_REPLACE_TEXT_AT_BKM)) {
  mydocx <- mydocx %>% 
    batch_body_replace_text_at_bkm(
      bookmark=BODY_REPLACE_TEXT_AT_BKM$BOOKMARK, 
      value=BODY_REPLACE_TEXT_AT_BKM$VALUE
    ) 
  }
  
  
  # global substitution ----- 
  if (!is.null(BODY_REPLACE_ALL_TEXT)) {
  mydocx <- mydocx %>% 
    batch_body_replace_all_text(
      old_value=BODY_REPLACE_ALL_TEXT$OLD_VALUE, 
      new_value=BODY_REPLACE_ALL_TEXT$NEW_VALUE
    ) 
  }
    
    
  # figure -------- 
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
    founded <- tryCatch({
      mydocx %>% cursor_reach(keyword = figure_name)
      TRUE
    },
    error=function(cond="No such figure name found!") {
      message(paste0("Figure not found in docx: ", figure_name))
      # Choose a return value in case of error
      return(FALSE)
    }
    )
    
    if (founded) {
      mydocx <- mydocx %>% 
        cursor_reach(keyword = figure_name) %>% 
        body_replace_all_text(figure_name, 
                              attr(new_figure, "title"), 
                              only_at_cursor = TRUE, 
                              ignore.case=TRUE) %>% 
        body_add_gg(value = new_figure, style = "Figure", width = docx_width, height = docx_height) %>% 
        #body_add_vg(code = new_figure,  width = docx_width, height = docx_height) %>% 
        
        body_add_par(value = attr(new_figure, "footnote"), style = "C-Footnote")   %>% 
        body_add_break(pos = "after") 
      
    }else {
      mydocx <- mydocx %>% cursor_end() %>%
        #body_add_break(pos = "after")  %>% 
        body_add_par(value=attr(new_figure, "title"), style = "Caption1") %>% 
        
        body_add_gg(value = new_figure, style = "Figure", width = docx_width, height = docx_height) %>% 
        #body_add_vg(code = new_figure,   width = docx_width, height = docx_height) %>% 
        
        body_add_par(value = attr(new_figure, "footnote"), style = "C-Footnote")   %>% 
        body_add_break(pos = "after") 
    }
    #body_add_par("centered text", style = "centered") %>%
    #slip_in_text(". How are you", style = "Strong1") %>%
    #body_bookmark("text_to_replace") %>%
    #body_bookmark("bookmark_time_profile_typical_subj_linear") %>%
    # cursor_bookmark("bookmark_time_profile_typical_subj_linear") %>%
    #body_replace_text_at_bkm("bookmark_time_profile_typical_subj_linear", "not left aligned")
    
  }}
 
  # table ---- 
  table_name_lst = names(TABLE)
   
  if (length(table_name_lst)>0) {
  for(i in 1:length(TABLE)) {
    
    table_name = table_name_lst[i]
    print(table_name)
    
    new_table <- TABLE[[table_name]]
    title = attr(new_table, "title"); title = ifelse(is.null(title), "No title yet", title) 
    footnote = attr(new_table, "footnote") ; footnote = ifelse(is.null(footnote), "", footnote)
    fontsize =  attr(new_table, "fontsize"); fontsize = ifelse(is.null(fontsize), fontsize_default, fontsize)
    
    # add table  ----
    founded <- tryCatch({
      mydocx %>% cursor_reach(keyword = table_name)
      TRUE
    },
    error=function(cond="No such table name found!") {
      message(paste0("Table not found in docx: ", table_name))
      # Choose a return value in case of error
      return(FALSE)
    }
    )
    
    if (founded) { 
      mydocx <- mydocx %>% 
        cursor_begin() %>%  
        cursor_reach(keyword = table_name) %>% 
        body_replace_all_text(table_name, 
                              attr(new_table, "title"), 
                              only_at_cursor = TRUE, 
                              ignore.case=TRUE) %>% 
         
        #cursor_bookmark(table_name) %>%
        #shortcuts$slip_in_plotref(depth = 1) %>%
        #body_add_table(value = new_table, style = "Body Table") 
        body_add_flextable(
          flextable(data = new_table) %>% 
            theme_booktabs() %>% 
            #set_header_labels( n = "#", Mean = "\u03D1", SD = "\u03C3") %>% 
            #color(i = ~ n < 4, color = "wheat") %>% 
            autofit() 
        ) %>% 
        body_add_break(pos = "after")
      
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
        )%>% 
        body_add_break(pos = "after")
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
  FIGURE[["fig_time_profile_1000_subj_95CI_log_panel"]] = fig1
  

  
  
  
  TABLE <- NULL
  
  attr(tab1, "title") = "just test tab1"
  attr(tab2, "title") = "just test tab2"
  TABLE[["tab_exposure_metrics_a_typical_subj"]] = tab1
  TABLE[["tab_exposure_metrics_1000_subj_95CI"]] = tab2
  
  
  
  bookmark_lst <- c("study_name", "report_author")
  value_lst <- c("R2222-HV-1888", "fENG YANG 2019-09-13")
  
  old_value <- c("REGN1193", "R1193")
  new_value <- c("REGN2222", "R2222")
  
  read_docx(paste0(WORKING_HOME, "/lib/sim-memo-template.docx"))  %>% 
    
    batch_body_replace_text_at_bkm(bookmark_lst, value_lst) %>% 
    
    #R1193-HV-1219 and R1193-DM-1402
    # only replace the last instance of R1193, a bug.
    batch_body_replace_all_text(old_value, new_value,
              only_at_cursor = FALSE,
              ignore.case=TRUE, 
              warn = TRUE   #FALSE
    ) %>%  
    
    #docx_show_chunk()  %>% 
  print(target = paste0(WORKING_HOME, '/docs/docx_result.docx'))
  
  
  
  
  
  #mydocx <- read_docx(paste0(HOME, "/lib/docTemplate.docx"))
  mydocx <- read_docx(paste0(WORKING_HOME, "/lib/sim-memo-template.docx"))  %>% 
    print2docx(FIGURE, TABLE) %>%
    print(target = paste0(WORKING_HOME, "/docs/toc_and_captions.docx"))
  
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















