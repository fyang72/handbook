#https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
#https://yutani.rbind.io/post/2018-06-16-re-intro-to-gghighlight/



library(tidyverse)

set.seed(2)
d <- map_dfr(
  letters,
  ~ data.frame(
    idx = 1:400,
    value = cumsum(runif(400, -1, 1)),
    type = .,
    flag = sample(c(TRUE, FALSE), size = 400, replace = TRUE),
    stringsAsFactors = FALSE
  )
)

ggplot(d) +
  geom_line(aes(idx, value, colour = type))


library(dplyr, warn.conflicts = FALSE)

d_filtered <- d %>%
  group_by(type) %>% 
  filter(max(value) > 20) %>%
  ungroup()

ggplot(d_filtered) +
  geom_line(aes(idx, value, colour = type))

ggplot(d_filtered) +
  geom_line(aes(idx, value, group = type), data = d, colour = alpha("grey", 0.7)) +
  geom_line(aes(idx, value, colour = type))


library(gghighlight)
ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  theme_minimal() +
  facet_wrap(~ flag) + 
  gghighlight(max(value) > 19) 



set.seed(10)
d2 <- dplyr::sample_n(d, 20)

ggplot(d2, aes(idx, value)) +
  geom_point() +
  gghighlight(value > 0, label_key = type)




####################



file.name = paste0(HOME, "/data/nmdat_PKPD_1024_2018.csv")
nmdat = read_csv(file.name, 
                 col_type=cols(.default=col_character()))   # read as character as defualt


nmdat <- nmdat %>% 
  mutate(ARMA = gsub("_", " ", ARMA, fix=TRUE), 
         ARMA = ordered(ARMA, levels=unique(as.character(ARMA))), # make ARMA as a ordered factor
         ARMAN = as.integer(ARMA), 
         TIME=as.numeric(TIME), 
         DV   = as.numeric(DV),
         DVOR = as.numeric(DVOR), 
         ID = as.integer(ID), 
         TIME = as.numeric(TIME), 
         AMT = as.numeric(AMT),  ################
         RATE = as.numeric(RATE), 
         EVID = as.integer(EVID), 
         CMT = as.integer(CMT), 
         MDV = as.integer(MDV), 
         NTIM = as.numeric(NTIM),
         LLOQ = as.numeric(LLOQ)
         
         
         #WGTBL = as.numeric(WGTBL),
         #CH50HBL = as.numeric(CH50HBL), 
         #C5BL = as.numeric(C5BL) 
  )

fig = ggplot(nmdat%>%filter(TEST%in%c("REGN3918", "CH50H")) %>% group_by(TEST), 
             aes(x=TIME, y=DVOR, group=ID, col=ARMA)) + geom_point() + geom_line()  + 
  facet_wrap(~TEST,ncol=1) #+ 
  #gghighlight(USUBJID=="R3918-HV-1659-826-001-055", label_key = TEST)






