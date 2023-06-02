setwd("~/Desktop/TemporalEnsamble")
library(readr)
WebAddress <- read_csv("WebAddress.csv")
View(WebAddress)
datos <- WebAddress
for (i in 2:5){  url <- datos$URLS[i];browseURL(url)}
for (i in 6:10){ url <- datos$URLS[i];browseURL(url)}
for (i in 11:15){url <- datos$URLS[i];browseURL(url)}
for (i in 16:20){url <- datos$URLS[i];browseURL(url)}
for (i in 21:25){url <- datos$URLS[i];browseURL(url)}


waitingtime <- 45*60
for (i in 2:5) {url <- datos$URLS[i]; browseURL(url); Sys.sleep(waitingtime)}
for (i in 6:10) {url <- datos$URLS[i]; browseURL(url); Sys.sleep(waitingtime)}
for (i in 11:15) {url <- datos$URLS[i]; browseURL(url); Sys.sleep(waitingtime)}
for (i in 16:20) {url <- datos$URLS[i]; browseURL(url); Sys.sleep(waitingtime)}
for (i in 21:25) {url <- datos$URLS[i]; browseURL(url); Sys.sleep(waitingtime)}