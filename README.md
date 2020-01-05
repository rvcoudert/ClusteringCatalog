# ClusteringCatalog

## Run from R environment.
> shiny::runGitHub(
  repo = "ClusteringCatalog",
  username = "rvcoudert",
  subdir = "Shiny")
  
## Require packages
list of packages required
>list.of.packages <- c(
  "magrittr",
  "shiny",
  "shinyWidgets",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "reshape2",
  "cluster")

checking missing packages from list
>new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[
    ,"Package"])]

install missing ones
>if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

##Screen shots

###3 data generation methods

*blobs

![image of shiny blobs](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData.blobs.jpg)

*moons

![image of shiny moons](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData.moons.jpg)

*circles

![image of shiny circles](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData.circles.jpg)

###kMeans overview

![image of shiny circles](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/kMeans.jpg)
