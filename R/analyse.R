library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)

soudci_ksc <- read_excel("../data/soudci2011.xlsx", trim_ws = T)

soudci_ksc$lastName <- soudci_ksc$fullname

#remove titles
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "JUDr.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "Mgr.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "prom. práv.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "Prof.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "Doc.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, ", CSc.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, ", Ph.D.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "prom. práv.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "PhDr.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, ",", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "Ph.D.", "")
soudci_ksc$lastName <- str_replace_all(soudci_ksc$lastName, "Ing.", "")


#split and remove whitespace
soudci_ksc$lastName <- str_trim(soudci_ksc$lastName)
soudci_ksc$firstName <- unlist(str_split(soudci_ksc$lastName, " "))[rep(c(FALSE, TRUE), 597)]
soudci_ksc$lastName <- unlist(str_split(soudci_ksc$lastName, " "))[rep(c(TRUE, FALSE), 597)]
soudci_ksc$lastName <- str_to_title(soudci_ksc$lastName)
soudci_ksc$lastName <- str_trim(soudci_ksc$lastName)
soudci_ksc$firstName <- str_to_title(soudci_ksc$firstName)
soudci_ksc$firstName <- str_trim(soudci_ksc$firstName)

#soudci teď
soudci <- read_excel("../data/soudci-sjednoceni.xlsx")

soudci <- soudci %>%
  filter(active==TRUE)

soudci <- soudci[!soudci$concatenatedWorkingPositionOrganizations=="Ústavní soud", ]

inner <- soudci %>%
  filter(active==TRUE) %>%
  inner_join(soudci_ksc) %>%
  distinct()

look <- soudci %>%
  left_join(inner) %>%
  distinct() %>%
  select(soud = concatenatedWorkingPositionOrganizations, fullname, fullName) %>%
  group_by(soud) %>%
  summarise(pocet=n(), komunistu=sum(!is.na(fullname))) %>%
  mutate(pct=komunistu/pocet) %>%
  arrange(desc(pct))

toJSON(look$soud)
  


export <- data.frame(y=look$pct*100, p=look$pocet, k=look$komunistu)

toJSON(export)

