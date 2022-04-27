
packagesTBI <- c("twitteR", "ROAuth", "hms", "tidytext", "tm", "wordcloud", "igraph", "glue", "networkD3",
              "rtweet", "stringr", "ggeasy", "janeaustinr", "widyr")
install.packages(packagesTBI)

librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes, pracma,
                 twitteR, ROAuth, hms, lubridate, tidytext, tm, wordcloud, igraph, glue, networkD3,
                 rtweet, stringr, ggeasy, plotly, janeaustenr, widyr)



tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")
