# ClusteringCatalog

Still in development, this Shiny Application presents several clustering methods through examples with data set in the plan.

## Run from R environment.
> shiny::runGitHub(
  repo = "ClusteringCatalog",
  username = "rvcoudert",
  subdir = "Shiny")
  
## Require packages
>list.of.packages <- c(
  "magrittr",
  "shiny",
  "shinyWidgets",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "reshape2",
  "cluster")
>new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[
    ,"Package"])]
>if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

## Screen shots

### 3 data generation methods

* blobs

![image of shiny blobs](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData_blobs.jpg)

* moons

![image of shiny moons](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData_moons.jpg)

* circles

![image of shiny circles](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/genData_circles.jpg)

### kMeans overview

![image of shiny circles](https://raw.githubusercontent.com/rvcoudert/ClusteringCatalog/master/Screenshots/kMeans.jpg)
