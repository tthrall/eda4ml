# eda4ml-pkg-per-chpt

## book chapters

| idx_1 | file               | title                                      |
| ----- | ------------------ | ------------------------------------------ |
| 1     | eda.qmd            | Exploratory Data Analysis                  |
| 2     | conditioning.qmd   | Conditional Distributions                  |
| 3     | clustering.qmd     | Clustering: EDA in Higher Dimensions       |
| 4     | simulation.qmd     | Statistical Simulation                     |
| 5     | study-design.qmd   | Sampling and Study Design                  |
| 6     | info-theory.qmd    | Information Theory                         |
| 7     | lin-reg.qmd        | Linear Regression                          |
| 8     | pca.qmd            | Principal Component Analysis               |
| 9     | lin-discr.qmd      | Linear Discriminant Analysis               |
| 10    | text-as-data.qmd   | Text as Data                               |
| 11    | topic-models.qmd   | Topic Models                               |
| 12    | ts-data.qmd        | Time Series Data                           |
| 13    | ts-time-domain.qmd | Time Domain Methods                        |
| 14    | ts-freq-domain.qmd | Frequency Domain Methods                   |
| 15    | graph-theory.qmd   | Graph Theory for Machine Learning          |
| 16    | mcmc-appendix.qmd  | MCMC: Markov Chain Monte Carlo             |
| 17    | em-algorithm.qmd   | EM: the Expectation-Maximization Algorithm |

## packages

01	library(here)
01	library(knitr)
01	library(tidyverse)
01	library(UsingR)

02	library(here)
02	library(knitr)
02	library(tidyverse)
02	library(UsingR)

03	library(dbscan)
03	library(GGally)
03	library(here)
03	library(ISLR2)
03	library(knitr)
03	library(tidyverse)

04	library(here)
04	library(knitr)
04	library(tidyverse)

05	library(here)
05	library(knitr)
05	library(tidyverse)

06	library(here)
06	library(knitr)
06	library(tidyverse)

07	library(knitr)
07	library(scatterplot3d)
07	library(tidyverse)

08	library(beans)
08	library(Cairo)
08	library(conflicted)
08	library(corrplot)
08	library(GGally)
08	library(HistData)
08	library(here)
08	library(ISLR2)
08	library(knitr)
08	library(MASS)
08	library(mclust)
08	library(patchwork)
08	library(plot3D)
08	library(pracma)
08	library(tidymodels)
08	library(tidyverse)

09	library(Cairo)
09	library(conflicted)
09	library(corrplot)
09	library(discrim)
09	library(GGally)
09	library(here)
09	library(knitr)
09	library(MASS)
09	library(mclust)
09	library(patchwork)
09	library(plotly)
09	library(pracma)
09	library(rlang)
09	library(tidymodels)
09	library(tidyverse)

10	library(here)
10	library(janeaustenr)
10	library(knitr)
10	library(quanteda)
10	library(tidytext)
10	library(tidyverse)
10	library(tm)

11	library(here)
11	library(knitr)
11	library(tidytext)
11	library(tidyverse)
11	library(tm)
11	library(topicmodels)

12	library(astsa)
12	library(here)
12	library(knitr)
12	library(tidyverse)

13	library(astsa)
13	library(here)
13	library(knitr)
13	library(tidyverse)

14	library(astsa)
14	library(here)
14	library(knitr)
14	library(tidyverse)

15	library(here)
15	library(igraph)
15	library(knitr)
15	library(tidyverse)

16	library(astsa)
16	library(tidyverse)

17	library(here)
17	library(knitr)
17	library(mixtools)
17	library(tidyverse)

## R scripts

### pkg_per_chpt_tbl

```{r}
pkg_per_chpt_tbl <- tibble::tribble(
  ~ch, ~call, 
  01,	"library(here)", 
  01,	"library(knitr)", 
  01,	"library(tidyverse)", 
  01,	"library(UsingR)", 
  
  02,	"library(here)", 
  02,	"library(knitr)", 
  02,	"library(tidyverse)", 
  02,	"library(UsingR)", 
  
  03,	"library(dbscan)", 
  03,	"library(GGally)", 
  03,	"library(here)", 
  03,	"library(ISLR2)", 
  03,	"library(knitr)", 
  03,	"library(tidyverse)", 
  
  04,	"library(here)", 
  04,	"library(knitr)", 
  04,	"library(tidyverse)", 
  
  05,	"library(here)", 
  05,	"library(knitr)", 
  05,	"library(tidyverse)", 
  
  06,	"library(here)", 
  06,	"library(knitr)", 
  06,	"library(tidyverse)", 
  
  07,	"library(knitr)", 
  07,	"library(scatterplot3d)", 
  07,	"library(tidyverse)", 
  
  08,	"library(beans)", 
  08,	"library(Cairo)", 
  08,	"library(conflicted)", 
  08,	"library(corrplot)", 
  08,	"library(GGally)", 
  08,	"library(HistData)", 
  08,	"library(here)", 
  08,	"library(ISLR2)", 
  08,	"library(knitr)", 
  08,	"library(MASS)", 
  08,	"library(mclust)", 
  08,	"library(patchwork)", 
  08,	"library(plot3D)", 
  08,	"library(pracma)", 
  08,	"library(tidymodels)", 
  08,	"library(tidyverse)", 
  
  09,	"library(Cairo)", 
  09,	"library(conflicted)", 
  09,	"library(corrplot)", 
  09,	"library(discrim)", 
  09,	"library(GGally)", 
  09,	"library(here)", 
  09,	"library(knitr)", 
  09,	"library(MASS)", 
  09,	"library(mclust)", 
  09,	"library(patchwork)", 
  09,	"library(plotly)", 
  09,	"library(pracma)", 
  09,	"library(rlang)", 
  09,	"library(tidymodels)", 
  09,	"library(tidyverse)", 
  
  10,	"library(here)", 
  10,	"library(janeaustenr)", 
  10,	"library(knitr)", 
  10,	"library(quanteda)", 
  10,	"library(tidytext)", 
  10,	"library(tidyverse)", 
  10,	"library(tm)", 
  
  11,	"library(here)", 
  11,	"library(knitr)", 
  11,	"library(tidytext)", 
  11,	"library(tidyverse)", 
  11,	"library(tm)", 
  11,	"library(topicmodels)", 
  
  12,	"library(astsa)", 
  12,	"library(here)", 
  12,	"library(knitr)", 
  12,	"library(tidyverse)", 
  
  13,	"library(astsa)", 
  13,	"library(here)", 
  13,	"library(knitr)", 
  13,	"library(tidyverse)", 
  
  14,	"library(astsa)", 
  14,	"library(here)", 
  14,	"library(knitr)", 
  14,	"library(tidyverse)", 
  
  15,	"library(here)", 
  15,	"library(igraph)", 
  15,	"library(knitr)", 
  15,	"library(tidyverse)", 
  
  16,	"library(astsa)", 
  16,	"library(tidyverse)", 
  
  17,	"library(here)", 
  17,	"library(knitr)", 
  17,	"library(mixtools)", 
  17,	"library(tidyverse)"
)
```



```{r}
ppc_tbl <- pkg_per_chpt_tbl |> 
  dplyr::mutate(
    ch  = as.integer(ch), 
    pkg = call |> stringr::str_remove_all("library\\("), 
    pkg = pkg  |> stringr::str_remove_all("\\)")
  ) |> 
  dplyr::select(- call)
```



```{r}
ppc_tbl |> readr::write_tsv(here::here(
  "eda4ml", "data", "retain", "ppc_tbl.txt"
))
```

### eda4ml_pkg_tbl

```{r}
eda4ml_pkg_vec <-(ppc_tbl$ pkg) |> 
  unique() |> sort()
```



```{r}
eda4ml_pkg_tbl <- tibble::tibble(pkg = eda4ml_pkg_vec)
```



```{r}
eda4ml_pkg_tbl |> readr::write_tsv(here::here(
  "eda4ml", "data", "retain", "eda4ml_pkg_tbl.txt"
))
```



```{r}
ch_tbl <- readr::read_tsv(here::here(
    "eda4ml", "data", "retain", "chapters_eda4ml.txt"
))

# > ch_tbl |> print(n = 4)
# # A tibble: 21 × 5
#   idx_0 idx_1 title                     file               n_l
#   <dbl> <dbl> <chr>                     <chr>            <dbl>
# 1     0    NA NA                        index.qmd            7
# 2     0    NA NA                        preface.qmd        266
# 3     1     1 Exploratory Data Analysis eda.qmd            491
# 4     1     2 Conditional Distributions conditioning.qmd  1144
# # ℹ 17 more rows
```

### packages_per_chapter_tbl

```{r}
packages_per_chapter_tbl <- ppc_tbl |> 
  left_join(
      y  = ch_tbl, 
      by = join_by(ch == idx_1)
  ) |> 
  dplyr::mutate(
      part = as.integer(idx_0), 
      chpt = as.integer(ch), 
      n_lines = as.integer(n_l)
  ) |> 
  dplyr::select(part, chpt, title, file, n_lines, pkg)

# > packages_per_chapter_tbl |> print(n = 4)
# # A tibble: 92 × 6
#    part  chpt title                     file    n_lines pkg      
#   <int> <int> <chr>                     <chr>     <int> <chr>    
# 1     1     1 Exploratory Data Analysis eda.qmd     491 here     
# 2     1     1 Exploratory Data Analysis eda.qmd     491 knitr    
# 3     1     1 Exploratory Data Analysis eda.qmd     491 tidyverse
# 4     1     1 Exploratory Data Analysis eda.qmd     491 UsingR   
# # ℹ 88 more rows
```

```{r}
packages_per_chapter_tbl |> readr::write_tsv(here::here(
  "eda4ml", "data", "retain", "packages_per_chapter_tbl.txt"
))
```



