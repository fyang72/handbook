 

\@ref(fig:nice-fig1).  
\@ref(fig:figurelabel)


, fig.height=8, fig.width=6, out.width='80%', fig.asp=.75}

```{r plot-mkdata, fig.cap='Here is a nice figure!', fig.align='center', out.width='80%', fig.asp=.75}
 
plot(pressure, type = 'b', pch = 19, main="placeholder")

```


```{r summary-demog-cscc, tidy=FALSE, message=FALSE, warning=FALSE}

caption = "Summary of Baseline Demographic Characteristics "
knitr::kable(tabl, booktabs = TRUE, caption = caption)

```

<br> 
 
 

6. Calculate the Mean, 5th, and 95th percentile concentration at each time point for each formulation and dose level. hint: you can use `?quantile` to calculate various quantiles

```{r, echo=FALSE}
adpx %>% mutate(DVOR=as_numeric(DVOR)) %>%
  group_by(NTIM) %>% 
  s_quantiles(DVOR, probs = c(0.05, 0.5, 0.95)) %>% head() %>% kable()

```


\begin{equation}
\label{eq-abc}
a + b = c
\end{equation}
[\code]
 
 

`r if (knitr:::is_html_output()) '# References {-}'`

# Literature

cemiplimab 

Section \@ref(population).
\@ref(tab:summary-studies). 
see Figure \@ref(fig:nice-fig1).

```{r summary-studies-cscc-test, tidy=FALSE, message=FALSE, warning=FALSE}

tdata = adpx 
TABLE_ALL[["summary_studies_by_CSCC"]] = tabl

caption = 'Summary of Subjects and PK Samples by Study and Treatment Group in the PopPK Analysis (Index Data Set in Study 1423 and 1540)'
knitr::kable(tabl, booktabs = TRUE, caption=caption, padding = 2)

```

[@Elassaiss-Schaap-2016]

[@xie2015].

(Elassaiss-Schaap, 2016)

