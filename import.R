#  ------------------------------------------------------------------------
#
# Title : Import medgachis
#    By :
#  Date : 2025-09-04
#
#  ------------------------------------------------------------------------


library("tidyverse")
library("readODS")
library("janitor")
library("labelled")
library("baseph")
#
nax <- c("", " ", "NA","D")

patients <- read_ods("./datas/patients.ods", na = nax) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/patients.ods", sheet = "bnom", na = nax)
var_label(patients) <- bn$titres
patients <- remove_constant(patients, na.rm = TRUE)
#
bloc <- read_ods("./datas/bloc.ods", na = nax) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/bloc.ods", sheet = "bnom", na =  nax)
var_label(bloc) <- bn$titres
bloc <- remove_constant(bloc, na.rm = TRUE)
#
tt <- read_ods("./datas/traitement.ods", na = nax) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/traitement.ods", sheet = "bnom", na = nax)
var_label(tt) <- bn$titres
tt <- remove_constant(tt, na.rm = TRUE)


rm(bn)
save(patients, bloc, tt, file = "datas/medgachis.RData")
load("datas/medgachis.RData")
