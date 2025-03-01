**Loading and preprocessing the data**

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

1.  Unzip the file

```{r}

knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7, warning = FALSE)

unzip("activity.zip")
```

2.  Read data

```{r}
Data <- read.csv("activity.csv", header=TRUE)
Data$date <- as.Date(Data$date, "%Y-%m-%d")

```

**What is the mean total number of steps taken per day?**

1.  Calculate the total number of steps taken per day

```{r}
library(dplyr)
Tsteps <- Data %>% group_by(date) %>% summarise(total = sum(steps))
```

2.  Make a histogram of the total number of steps taken each day

```{r scatterplot, fig.height=4}
print(hist(Tsteps$total))

```
