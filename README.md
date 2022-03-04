# mi-wantem-luk
A graphing package for visualising data, designed specifically to work with FLR4MFCL and Hierophant.

This package uses ggplot2 and several other graphing packages with dependancy on the tidyverse. Most of those packages can be installed from CRAN, however the chorddiag package is maintained by mattflor and can be installed from github

```{r}
install.github("mattflor/chorddiag")
```

It may also be necessary to explicitly install the igraph and networkD3 packages prior to installing mi-wantem-luk.

```{r}
install.packages("igraph")
install.packages("networkD3")
```


