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
patients <- read_ods("./datas/patients.ods", na = c("", " ", "NA")) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/patients.ods", sheet = "bnom", na = c("", " ", "NA"))
var_label(patients) <- bn$titres
patients <- remove_constant(patients)
#
bloc <- read_ods("./datas/bloc.ods", na = c("", " ", "NA")) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/bloc.ods", sheet = "bnom", na = c("", " ", "NA"))
var_label(bloc) <- bn$titres
bloc <- remove_constant(bloc)
#
tt <- read_ods("./datas/traitement.ods", na = c("", " ", "NA")) |>
  clean_names() |>
  mutate(across(where(is.character), as.factor))
bn <- read_ods("./datas/traitement.ods", sheet = "bnom", na = c("", " ", "NA"))
var_label(tt) <- bn$titres
tt <- remove_constant(tt)


rm(bn)
save(patients, bloc, tt, file = "datas/medgachis.RData")
load("datas/medgachis.RData")
