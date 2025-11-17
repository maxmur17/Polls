# library(tidyverse)
# library(shiny)
# library(plotly)
# library(shinyWidgets)
# 
# library(rvest)
# library(lubridate)
# library(readxl)
# library(httr)
# library(curl)
# library(polite)

library(tidyverse)
library(rvest)
library(httr)
library(readxl)

links <- list("https://338canada.com/polls.htm","https://338canada.com/polls-atl.htm","https://338canada.com/polls-qc.htm","https://338canada.com/polls-on.htm",
              "https://338canada.com/polls-pr.htm", "https://338canada.com/polls-ab.htm", "https://338canada.com/polls-bc.htm")

C338_pages <- list()

for (i in seq_along(links)) {
  C338_pages[[i]] <- read_html(links[[i]], user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0")
  
}

dat_343_list <- list()

for (i in seq_along(C338_pages)) {
  dat_343_list[[i]] <- C338_pages[[i]] %>%
    html_elements(css = "#myTable") %>%
    html_table()
}

dat_343 <- list()

for (i in 1:7) {
  dat_343[[i]] <- dat_343_list[[i]][[1]]
}


# capture digits and commas after "n =" (case-insensitive)
pattern <- "(?i)n\\s*=\\s*([0-9,]+)"

for (i in seq_along(C338_pages)) {
  sample_nodes <- C338_pages[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[2]") %>%  # adjust XPath if needed
    html_text(trim = TRUE)
  
  # extract capture group 1 (digits + commas)
  sample_n_raw <- str_match(sample_nodes, pattern)[,2]
  
  # remove commas and convert to integer (NA if not found)
  sample_n <- as.integer(gsub(",", "", sample_n_raw))
  
  # attach to corresponding dataframe (assumes same order)
  dat_343[[i]]$sample <- sample_n
}

for (i in seq_along(C338_pages)) {
  # extract the first text node (dates)
  date_nodes <- C338_pages[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[2]/text()[1]") %>%
    html_text(trim = TRUE)
  
  # often looks like "2025-09-20 to 2025-09-22" or "Sep 15–17, 2025"
  poll_dates <- str_squish(date_nodes)  # clean up whitespace
  
  dat_343[[i]]$date <- poll_dates
}

for (i in seq_along(C338_pages)) {
  # polling firm is in div/div[1]
  firm_nodes <- C338_pages[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[1]") %>%
    html_text(trim = TRUE)
  
  dat_343[[i]]$Firm <- firm_nodes
}

party_names <- c("LPC", "CPC", "NDP", "GPC")
party_names_QC <- c("LPC", "CPC", "NDP", "BQ")
party_names_all <- c("LPC", "CPC", "NDP", "GPC", "BQ")

for (i in c(2,4:7)) {
  blank_idx <- which(colnames(dat_343[[i]]) == "")
  colnames(dat_343[[i]])[blank_idx] <- party_names[seq_along(blank_idx)]
}

blank_idx <- which(colnames(dat_343[[3]]) == "")
colnames(dat_343[[3]])[blank_idx] <- party_names_QC[seq_along(blank_idx)]

colnames(dat_343[[1]])[3:7] <- party_names_all

for (i in 1:7) {
  dat_343[[i]][1] <- NULL
  dat_343[[i]]$Rating <- NULL
}

dat_343[[1]]$region <- "National"

for (i in c(2,4:7)) {
  dat_343[[i]]$BQ <- NA
  
}

dat_343[[3]]$GPC <- NA

region <- c("ATL","QC","ON","MB/SK","AB","BC" )

for (i in 2:7) {
  dat_343[[i]]$region <- region[i-1]
}

dat_343 <- do.call(rbind, dat_343)

dat_343 <- dat_343 %>%
  rename(firm = Firm)

dat_343$date <- as.Date(dat_343$date)

dat_343_long <- dat_343 %>%
  pivot_longer(cols=LPC:BQ,
               names_to = "party",
               values_to = "pop_sup") %>%
  mutate(party = fct_relevel(party, c("LPC", "CPC", "NDP", "BQ", "GPC")),
         firm = as.factor(firm))


num_data_points <- 30
alpha <- num_data_points / 150


# Provincial all elections


links_prov <- c("https://338canada.com/alberta/polls.htm","https://338canada.com/bc/polls.htm","https://338canada.com/manitoba/polls.htm",
                "https://338canada.com/nb/polls.htm", "https://338canada.com/nl/", "https://338canada.com/ns/polls.htm",
                "https://338canada.com/ontario/polls.htm", "https://338canada.com/quebec/polls.htm", "https://338canada.com/saskatchewan/polls.htm")

C338_pages_prov <- list()

# for (i in seq_along(links_prov)) {
#   C338_pages_prov[[i]] <- fetch_html(links_prov[[i]])
# }

for (i in seq_along(links_prov)) {
  C338_pages_prov[[i]] <- read_html(links_prov[[i]], user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:73.0) Gecko/20100101 Firefox/73.0")
  
}

dat_343_list_prov <- list()

for (i in seq_along(C338_pages_prov)) {
  dat_343_list_prov[[i]] <- C338_pages_prov[[i]] %>%
    html_elements(css = "#myTable") %>%
    html_table()
}

dat_343_prov <- list()

for (i in 1:9) {
  dat_343_prov[[i]] <- dat_343_list_prov[[i]][[1]]
}

dat_343_prov[[2]] <- dat_343_prov[[2]][-10,]

pattern <- "(?i)n\\s*=\\s*([0-9,]+)"

for (i in c(1:3,6:8)) {
  sample_nodes <- C338_pages_prov[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[2]") %>%  # adjust XPath if needed
    html_text(trim = TRUE)
  
  # extract capture group 1 (digits + commas)
  sample_n_raw <- str_match(sample_nodes, pattern)[,2]
  
  # remove commas and convert to integer (NA if not found)
  sample_n <- as.integer(gsub(",", "", sample_n_raw))
  
  # attach to corresponding dataframe (assumes same order)
  dat_343_prov[[i]]$sample <- sample_n
}



for (i in c(1:3,6:8)) {
  # extract the first text node (dates)
  date_nodes <- C338_pages_prov[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[2]/text()[1]") %>%
    html_text(trim = TRUE)
  
  # often looks like "2025-09-20 to 2025-09-22" or "Sep 15–17, 2025"
  poll_dates <- str_squish(date_nodes)  # clean up whitespace
  
  dat_343_prov[[i]]$date <- poll_dates
}

for (i in c(1:3,6:8)) {
  # polling firm is in div/div[1]
  firm_nodes <- C338_pages_prov[[i]] %>%
    html_elements(xpath = "//table//tr/td[1]/div/div[1]") %>%
    html_text(trim = TRUE)
  
  dat_343_prov[[i]]$firm <- firm_nodes
}

ON_parties <- c("PCPO","ONDP","OLP","GPO")
QC_parties <- c("CAQ","PLQ","QS","PQ","PCQ")
BC_parties <- c("NDP(BC)", "CPBC", "BCG")
AB_parties <- c("UCP", "NDP(AB)","LIB(AB)","GPA","REP","ABP")
NS_parties <- c("LIB(NS)","PC(NS)","NDP(NS)","NSG")
MB_parties <- c("LIB(MB)", "MBG", "NDP(MB)", "PC(MB)")


blank_idx <- which(colnames(dat_343_prov[[2]]) == "")
colnames(dat_343_prov[[2]])[blank_idx] <- BC_parties[seq_along(blank_idx)]

blank_idx <- which(colnames(dat_343_prov[[7]]) == "")
colnames(dat_343_prov[[7]])[blank_idx] <- ON_parties[seq_along(blank_idx)]

blank_idx <- which(colnames(dat_343_prov[[8]]) == "")
colnames(dat_343_prov[[8]])[blank_idx] <- QC_parties[seq_along(blank_idx)]

blank_idx <- which(colnames(dat_343_prov[[1]]) == "")
colnames(dat_343_prov[[1]])[blank_idx] <- AB_parties[seq_along(blank_idx)]

blank_idx <- which(colnames(dat_343_prov[[6]]) == "")
colnames(dat_343_prov[[6]])[blank_idx] <- NS_parties[seq_along(blank_idx)]

blank_idx <- which(colnames(dat_343_prov[[3]]) == "")
colnames(dat_343_prov[[3]])[blank_idx] <- MB_parties[seq_along(blank_idx)]

for (i in 1:9) {
  dat_343_prov[[i]] <- dat_343_prov[[i]] %>%
    mutate(row = row_number())
}



province <- c("AB","BC","MB","NB","NL","NS","ON","QC","SK","PEI")
election_dates_vec <- c(as.Date("2023-05-29"), as.Date("2024-10-19"),as.Date("2023-10-03"), as.Date("2024-10-21"),as.Date("2021-03-25"),
                        as.Date("2024-11-26"),as.Date("2022-06-02"),as.Date("2022-09-03"),as.Date("2024-10-28"),as.Date("2023-04-03"))

for (i in c(4:5,9)) {
  dat_343_prov[[i]] <- dat_343_prov[[i]] %>%
    rename(firm = Firm, date = `Date(middle)`, sample = Sample)
}

dat_343_prov[[2]] <- dat_343_prov[[2]] %>%
  select(c(-`British Columbia polls`, -CEN, -ONE))

dat_343_prov[[7]] <- dat_343_prov[[7]] %>%
  select(-`Ontario polls`)

dat_343_prov[[8]] <- dat_343_prov[[8]] %>%
  select(-`Quebec polls`)

dat_343_prov[[1]] <- dat_343_prov[[1]] %>%
  select(-`Alberta polls`)

dat_343_prov[[6]] <- dat_343_prov[[6]] %>%
  select(-`Nova Scotia polls`)

dat_343_prov[[3]] <- dat_343_prov[[3]] %>%
  select(-`Manitoba polls`)


for (i in 1:9) {
  dat_343_prov[[i]] <- dat_343_prov[[i]] %>%
    pivot_longer(cols = -any_of(c("firm","Rating","date","sample","Leader","row")),
                 names_to = "party",
                 values_to = "pop_sup")
  dat_343_prov[[i]]$region <- province[i]
  dat_343_prov[[i]]$election_dates <- election_dates_vec[i]
  
}

dat_343_prov <- do.call(rbind, dat_343_prov)

dat_343_prov <- dat_343_prov %>%
  mutate(party = case_when(region %in% "AB" & party %in% "NDP" ~ "NDP(AB)",
                           region %in% "AB" & party %in% "LIB" ~ "LIB(AB)",
                           region %in% "BC" & party %in% "NDP" ~ "NDP(BC)",
                           region %in% "BC" & party %in% "GRN" ~ "BCG",
                           region %in% "MB" & party %in% "LIB" ~ "LIB(MB)",
                           region %in% "MB" & party %in% "PC" ~ "PC(MB)",
                           region %in% "MB" & party %in% "NDP" ~ "NDP(MB)",
                           region %in% "MB" & party %in% "GRN" ~ "MBG",
                           region %in% "NB" & party %in% "LIB" ~ "LIB(NB)",
                           region %in% "NB" & party %in% "PC" ~ "PC(NB)",
                           region %in% "NB" & party %in% "NDP" ~ "NDP(NB)",
                           region %in% "NB" & party %in% "GRN" ~ "NBG",
                           region %in% "NL" & party %in% "LIB" ~ "LIB(NL)",
                           region %in% "NL" & party %in% "PC" ~ "PC(NL)",
                           region %in% "NL" & party %in% "NDP" ~ "NDP(NL)",
                           region %in% "NL" & party %in% "GRN" ~ "NLG",
                           region %in% "NS" & party %in% "LIB" ~ "LIB(NS)",
                           region %in% "NS" & party %in% "PC" ~ "PC(NS)",
                           region %in% "NS" & party %in% "NDP" ~ "NDP(NS)",
                           region %in% "NS" & party %in% "GRN" ~ "NSG",
                           region %in% "SK" & party %in% "NDP" ~ "NDP(SK)",
                           region %in% "SK" & party %in% "GRN" ~ "SKG",
                           region %in% "SK" & party %in% "PC" ~ "PC(SK)",
                           region %in% "ON" & party %in% "OLP" ~ "OLP",
                           region %in% "ON" & party %in% "NDP" ~ "ONDP",
                           region %in% "ON" & party %in% "PC" ~ "PCPO",
                           .default = party))



dat_343_prov <- dat_343_prov %>%
  rename(leader = Leader) %>%
  select(-Rating) %>%
  mutate(party = as.factor(party),
         firm = as.factor(firm))

dat_343_prov$date <- as.Date(dat_343_prov$date)

levels(dat_343_prov$party)

colors_pal_app_fed <- c("LPC" = "red", "CPC" = "blue","NDP" = "orange", "BQ" = "turquoise4", "GPC" = "green3", "PPC" = "blueviolet")

colors_pal_app <- c("ABP" = "turquoise1", "CPBC" = "steelblue4", "BCG" ="green3", "BCU" ="brown1", "BUF" ="blueviolet", "CAQ" ="aquamarine", "GPA" ="green3", "GPO" ="darkgreen",
                    "LIB(AB)" ="red", "LIB(MB)" ="red","LIB(NB)" ="red","LIB(NL)" ="red","LIB(NS)" ="red","MBG" = "green3", "NBG" ="green3","NDP(AB)" = "orange","NDP(BC)" = "orange","NDP(MB)" ="orange","NDP(NB)" ="orange",
                    "NDP(NL)" ="orange","NDP(NS)" ="orange","NDP(SK)" ="orange", "NLA" ="darkorchid4","NLG" = "green3", "NSG" ="green3", "OLP" ="red", "ONDP" ="orange", "PA" ="darkorchid4",
                    "PC(MB)" ="blue","PC(NB)" ="blue","PC(NL)" ="blue","PC(NS)" ="blue","PC(SK)" ="blue","PCPO" ="blue", "PCQ" ="steelblue4", "PLQ" ="red", "PQ" ="blue", "QS" ="orange", "SKG" ="green3", "SKP" ="darkgreen",
                    "SPP" = "red","SUP" = "turquoise1", "UCP" ="steelblue4", "PC(PEI)" = "blue", "GPPEI" = "green3", "LIB(PEI)" = "red", "NDP(PEI)" = "orange", "ISL" = "red4", "Other" = "grey")


range <-  c(as.Date("2017-01-05"), as.Date("2025-01-01"))

election_name <- c("Election Date")
election_name <- replicate(10,election_name)

for (i in 1:9) {
  election_name[i] <- paste(province[i],election_name[i], collapse = " ")
}

line_positions <- setNames(election_dates_vec, province)

dat_343_prov_wide <- dat_343_prov %>%
  pivot_wider(names_from = party, values_from = pop_sup)

# Prediction model data

el_results <- read.csv("table_tableau12.csv")
el_results_cols <- c("province","district","district_number","candidate","candidate_residence","candidate_occupation","votes","votes_pct","majority","majority_pct")
colnames(el_results) <- el_results_cols


el_results$party_lib <- str_extract(el_results$candidate, "Liberal")
el_results$party_con <- str_extract(el_results$candidate, "Conservative")
el_results$party_ndp <- str_extract(el_results$candidate, "New Democratic Party")
el_results$party_ppc <- str_extract(el_results$candidate, "PPC")
el_results$party_gpc <- str_extract(el_results$candidate, "Green Party")
el_results$party_bq <- str_extract(el_results$candidate, "Bloc Québécois")

el_results <- el_results %>%
  mutate(party = coalesce(party_lib,party_con,party_ndp,party_ppc,party_gpc,party_bq))

gen_elec <- dat_343_long %>%
  filter(firm == "General election")

el_results <- el_results %>%
  mutate(language = case_when(province == "Newfoundland and Labrador/Terre-Neuve-et-Labrador" ~ "English",
                              province == "Prince Edward Island/Île-du-Prince-Édouard" ~ "English",
                              province == "Nova Scotia/Nouvelle-Écosse" ~ "English",
                              province == "New Brunswick/Nouveau-Brunswick" ~ "English",
                              province == "Quebec/Québec" ~ "French",
                              province == "Ontario" ~ "English",
                              province == "Manitoba" ~ "English",
                              province == "Saskatchewan" ~ "English",
                              province == "Alberta" ~ "English",
                              province == "British Columbia/Colombie-Britannique" ~ "English"))

el_results <- el_results %>%
  mutate(province = case_when(province == "Newfoundland and Labrador/Terre-Neuve-et-Labrador" ~ "Newfoundland and Labrador",
                              province == "Prince Edward Island/Île-du-Prince-Édouard" ~ "Prince Edward Island",
                              province == "Nova Scotia/Nouvelle-Écosse" ~ "Nova Scotia",
                              province == "New Brunswick/Nouveau-Brunswick" ~ "New Brunswick",
                              province == "Quebec/Québec" ~ "Quebec",
                              province == "British Columbia/Colombie-Britannique" ~ "British Columbia",
                              province == "Northwest Territories/Territoires du Nord-Ouest" ~ "Northwest Territories",
                              .default = as.character(province)))

el_results_wide <- el_results %>%
  pivot_wider(names_from = party, values_from = votes_pct) %>%
  rename(lib = "Liberal", con = "Conservative", ndp = "New Democratic Party", ppc = "PPC", gpc = "Green Party", bq = "Bloc Québécois") %>%
  group_by(district,province,language) %>%
  summarise(lib = sum(lib, na.rm = T),
            con = sum(con, na.rm = T),
            ndp = sum(ndp, na.rm = T),
            ppc = sum(ppc, na.rm = T),
            gpc = sum(gpc, na.rm = T),
            bq = sum(bq, na.rm = T))


# link_plus_old <- c("https://338canada.com/polls.htm", "https://338canada.com/polls2.htm")
# C338_page <- read_html(link)
# 
# 
# dat_343_nat <-  C338_page %>%
#   html_elements(css = "#myTable") %>%
#   html_table()
# 
# dat_343_nat <- dat_343_nat[[1]]


dat_343_nat <- dat_343 %>%
  filter(region == "National") %>%
  select(-region)




dat_343_nat$date <- as.Date(dat_343_nat$date)

dat_343_nat_long <- dat_343_nat %>%
  pivot_longer(cols=LPC:BQ,
               names_to = "party",
               values_to = "pop_sup") %>%
  mutate(party = fct_relevel(party, c("CPC", "LPC", "NDP", "BQ", "GPC")),
         firm = as.factor(firm))

election_date <- as.Date("2025-10-20")

today_date <- Sys.Date()

dat_343_nat <- dat_343_nat %>%
  mutate(sample = str_replace_all(sample, " \\s*\\([^\\)]+\\)", ""),
         sample = str_replace_all(sample, ",",""),
         sample = as.numeric(sample),
         elec_time = election_date - date,
         recency = today_date - date,
         elec_time_num = as.numeric(elec_time),
         recency_num = as.numeric(recency))

dat_343_prov[dat_343_prov == ""] <- NA

dat_343_prov <- dat_343_prov %>%
  mutate(sample = str_replace_all(sample, ",", ""),
         sample = str_replace_all(sample, " \\s*\\([^\\)]+\\)", ""),
         sample = as.numeric(sample),
         recency = today_date - date,
         recency_num = as.numeric(recency))


dat_343_nat <- dat_343_nat %>%
  filter(firm != "General election") %>%
  mutate(poll_weight = sqrt(sample)/recency_num)

prov_gen_elec <- dat_343_prov %>%
  filter(firm == "General election")





dat_343_prov <- dat_343_prov %>%
  filter(firm != "General election") %>%
  mutate(poll_weight = sqrt(sample)/recency_num)


## PEI

# PEI

link_pei <- "https://en.wikipedia.org/wiki/68th_Prince_Edward_Island_general_election"
pei_wiki_page <- read_html(link_pei)


pei_wiki_page <-  pei_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]") %>%
  html_table()

pei_polls <- pei_wiki_page[[1]]
pei_polls <- pei_polls[-1,]
pei_general <- pei_polls %>%
  filter(`Polling firm` == "General election results")

pei_cols <- c("firm","date","PC(PEI)","GPPEI","LIB(PEI)","NDP(PEI)","ISL","Other","sample","leader","row")

pei_polls <- pei_polls %>%
  select(-Link) %>%
  mutate(row = row_number())

colnames(pei_polls) <- pei_cols

pei_polls <- pei_polls %>%
  filter(firm != "General election results") %>%
  mutate(date = str_replace_all(date, "\\d+–", ""),
         date = mdy(date))


pei_num_cols <- colnames(pei_polls[, 3:8])

pei_polls[, pei_num_cols] <- lapply(pei_num_cols, function(x) as.numeric(pei_polls[[x]]))

pei_polls <- pei_polls %>%
  pivot_longer(cols=3:8,
               names_to = "party",
               values_to = "pop_sup")
pei_polls <- pei_polls %>%
  mutate(region = "PEI",
         election_dates = "2023-04-03",
         recency = today_date - date,
         recency_num = as.numeric(recency),
         sample = as.numeric(sample),
         poll_weight = sqrt(sample)/recency_num)

dat_343_prov <- rbind(dat_343_prov, pei_polls)

dat_343_prov_agg <- dat_343_prov %>%
  filter(date >= "2023-05-29") %>%
  group_by(party,region) %>%
  summarize(poll_agg = weighted.mean(pop_sup,poll_weight, na.rm = T)) %>%
  mutate(poll_agg = ifelse(party == "BCU", NA, poll_agg))

dat_343_prov <- dat_343_prov %>%
  group_by(region) %>% 
  mutate(pop_sup = ifelse(party == "BCU" & date > "2024-09-03", 0, pop_sup))





lib_poll_agg <- weighted.mean(dat_343_nat$LPC,dat_343_nat$poll_weight)
con_poll_agg <- weighted.mean(dat_343_nat$CPC,dat_343_nat$poll_weight)
ndp_poll_agg <- weighted.mean(dat_343_nat$NDP,dat_343_nat$poll_weight)
gpc_poll_agg <- weighted.mean(dat_343_nat$GPC,dat_343_nat$poll_weight, na.rm = T)
bq_poll_agg <- weighted.mean(dat_343_nat$BQ,dat_343_nat$poll_weight)
#ppc_poll_agg <- weighted.mean(dat_343_nat$PPC,dat_343_nat$poll_weight, na.rm = T)

# lib_diff_pct <- abs((32.6 - lib_poll_agg)/32.6*100)  
# con_diff_pct <- abs((33.7 - con_poll_agg)/33.7*100)
# ndp_diff_pct <- abs((17.8 - ndp_poll_agg)/17.8*100)
# gpc_diff_pct <- abs((2.3 - gpc_poll_agg)/2.3*100)
# bq_diff_pct <- abs((7.6 - bq_poll_agg)/7.6*100)
# ppc_diff_pct <- abs((4.9 - ppc_poll_agg)/4.9*100)

lib_diff_pct <- (lib_poll_agg - 32.6 )/32.6*100
con_diff_pct <- (con_poll_agg - 33.7)/33.7*100
ndp_diff_pct <- (ndp_poll_agg - 17.8)/17.8*100
gpc_diff_pct <- (gpc_poll_agg - 2.3)/2.3*100
bq_diff_pct <- (bq_poll_agg - 7.6)/7.6*100
#ppc_diff_pct <- (ppc_poll_agg - 4.9)/4.9*100


#poll_diff_pct = ((poll_agg - pop_sup)/pop_sup)*100
#poll_result = votes_pct + votes_pct * poll_diff_pct / 100


el_results_wide <- el_results_wide %>%
  mutate(lib_poll = lib + lib * lib_diff_pct / 100,
         con_poll = con + con * con_diff_pct / 100,
         ndp_poll = ndp + ndp * ndp_diff_pct / 100,
         gpc_poll = gpc + gpc * gpc_diff_pct / 100,
         bq_poll = bq + bq * bq_diff_pct / 100)
#ppc_poll = ppc + ppc * ppc_diff_pct /100)

party_avgs <- c(lib_poll_agg,con_poll_agg,ndp_poll_agg,gpc_poll_agg,bq_poll_agg)#ppc_poll_agg)

party_names <- colnames(dat_343_nat)[1:5]

results_chart <- data.frame(party_names, party_avgs)


results_chart <- results_chart %>%
  mutate(party_names = fct_relevel(party_names, c("CPC", "LPC", "NDP", "BQ", "GPC")))


el_results_wide$winner <- apply(el_results_wide, 1, function(row) {
  party_names <- colnames(dat_343_nat)[1:5]
  party_names[which.max(row[10:15])]
})

seats_proj <- as.data.frame(table(el_results_wide$winner))

seats_proj <- seats_proj %>%
  rename(party = "Var1", seats = "Freq") %>%
  mutate(party = fct_relevel(party, c("CPC", "LPC", "NDP", "BQ", "GPC")))


# Provincial election results

## BC

link_bc <- "https://en.wikipedia.org/wiki/2024_British_Columbia_general_election"
bc_wiki_page <- read_html(link_bc)


bc_wiki_page <-  bc_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[24]") %>%
  html_table()

bc_results <- bc_wiki_page[[1]]

colnames(bc_results) <- bc_results[1,]

bc_results <- bc_results[-1:-3,]
bc_results <- bc_results %>%
  select(1, 15:19)

bc_results[bc_results == "–"] <- NA

bc_results <- bc_results %>%
  pivot_longer(cols = NDP:Other,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = str_replace_all(votes, "\\[a 3\\]", ""),
         votes = as.numeric(votes)) %>%
  rename(district = Riding)


bc_results <- bc_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "BC")

#bc_results <- read.csv("provincial_voting_results_BC.csv")
#bc_results <- bc_results %>%
#filter(EVENT_NAME == "2020 General Election" & VOTE_CATEGORY == "Valid") %>%
#select(EVENT_YEAR, ED_NAME, VOTING_OPPORTUNITY, ELECTED, AFFILIATION, VOTES_CONSIDERED, VOTE_CATEGORY)

#bc_results <- bc_results %>%
#group_by(ED_NAME, AFFILIATION) %>%
#summarise(votes = sum(VOTES_CONSIDERED))

#bc_results <- bc_results %>%
#group_by(ED_NAME) %>%
#mutate(votes_pct = votes/sum(votes)*100,
#province = "BC") %>%
#rename(district = ED_NAME,
#      party = AFFILIATION) %>%
#mutate(party = case_when(party == "BC Green Party" ~ "BCG",
#                        party == "BC Liberal Party" ~ "BCU",
#                       party == "BC NDP" ~ "NDP(BC)",
#                      party == "Conservative" ~ "CPBC"))


# bcucpbc_votes <- bc_results %>%
#   filter(party %in% c("BCU", "CPBC")) %>%
#   group_by(district, party) %>%
#   summarise(votes = sum(votes, na.rm = TRUE)) %>%
#   spread(key = party, value = votes, fill = 0)

# bcucpbc_votes <- bcucpbc_votes %>%
#   mutate(CPBC = CPBC + BCU)
# 
# bc_results <- bc_results %>%
#   left_join(bcucpbc_votes %>% select(district, CPBC), by = "district") %>%
#   mutate(votes = ifelse(party == "CPBC", CPBC, votes))



# no_cpbc_districts <- bc_results %>%
#   filter(party == "BCU" & !district %in% unique(bc_results$district[bc_results$party == "CPBC"])) %>%
#   select(district)
# 
# bc_results <- bc_results %>%
#   mutate(party = ifelse(district %in% no_cpbc_districts$district & party == "BCU", "CPBC", party))
# 
#   
# bc_results <- bc_results %>%
#   filter(party != "BCU") %>%
#   select(-CPBC) 
# 
# bc_results <- bc_results %>%
#   group_by(district) %>%
#   mutate(votes_pct = votes/sum(votes)*100)
# 
# 
# 
# write.csv(bc_results, "provincial_voting_results_BC_clean.csv")

### Git BC results
#bc_results <- read.csv("provincial_voting_results_BC_clean.csv")

## AB

ab_results <- read_excel("provincial_voting_results_AB.xlsx", skip = 1)
ab_results <- ab_results[-1,-1]
ab_results <- ab_results %>%
  select(-`Voting Locations`, -`Voter Turnout (%)`)
ab_results <- ab_results[-88,]
ab_results[61,16] <- "625"
ab_results[80,16] <- "3393"  

i <- c(2:16)  
ab_results[, i] <- apply(ab_results[, i], 2, function(x) as.numeric(as.character(x)))

ab_results <- ab_results %>%
  pivot_longer(cols = APA:IND,
               names_to = "party",
               values_to = "votes") %>%
  rename(district = `ED Name`)

ab_results <- ab_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "AB")

## MB

mb_results <- read_excel("provincial_voting_results_MB.xlsx", skip = 1)
mb_results <- mb_results[c(-58,-59),]

mb_cols <- c("district", "mp", "party", "reg_voters", "votes", "turnout", "CPC_M", "GPM", "Ind", "KP", "MLP", "MP", "NDP", "PC", "rejected", "declined", "plurality")
colnames(mb_results) <- mb_cols
mb_results <- mb_results %>%
  select(district, CPC_M:PC)

mb_results <- mb_results %>%
  pivot_longer(cols = CPC_M:PC,
               names_to = "party",
               values_to = "votes")

mb_results <- mb_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "MB")

## NB

link_nb <- "https://en.wikipedia.org/wiki/2024_New_Brunswick_general_election"
nb_wiki_page <- read_html(link_nb)


nb_wiki_page <-  nb_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[16]") %>%
  html_table()

nb_results <- nb_wiki_page[[1]]

colnames(nb_results) <- nb_results[1,]
nb_results <- nb_results[-1:-3,]
nb_results <- nb_results %>%
  select(1, 13:19)
nb_results[nb_results == "–"] <- NA

nb_results <- nb_results %>%
  pivot_longer(cols = Lib:Other,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = str_replace_all(votes, "\\[a 3\\]", ""),
         votes = as.numeric(votes)) %>%
  rename(district = Riding)


nb_results <- nb_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "NB")

## NL

link_nl <- "https://en.wikipedia.org/wiki/2021_Newfoundland_and_Labrador_general_election"
nl_wiki_page <- read_html(link_nl)

nl_wiki_page <-  nl_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[3]") %>%
  html_table()

nl_results <- nl_wiki_page[[1]]
colnames(nl_results) <- nl_results[1,]
nl_results <- nl_results[-1:-3,]
nl_results <- nl_results %>%
  select(1, 11:15)
nl_results[nl_results == "–"] <- NA
nl_results <- nl_results %>%
  pivot_longer(cols = Lib:Ind,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = as.numeric(votes)) %>%
  rename(district = `Riding[a 2]`)




nl_results <- nl_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "NL")

## NS

link_ns <- "https://en.wikipedia.org/wiki/2024_Nova_Scotia_general_election"
ns_wiki_page <- read_html(link_ns)
ns_wiki_page <- ns_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[8]") %>%
  html_table()

ns_results <- ns_wiki_page[[1]]
ns_results <- ns_results[-1:-3,]
ns_cols <- c(1:22)
colnames(ns_results) <- ns_cols
ns_results <- ns_results %>%
  select(1, 15:19)
ns_cols <- c("district","PC","LIB","NDP","GRN","Ind")
colnames(ns_results) <- ns_cols
ns_results[ns_results == "–"] <- NA

ns_results <- ns_results %>%
  pivot_longer(cols = PC:Ind,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = str_replace_all(votes, "\\[a 6\\]", ""),
         votes = as.numeric(votes)) 


ns_results <- ns_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         district = str_replace_all(district, " \\[[^\\)]+\\]", ""),
         province = "NS")

## ON

on_results <- read.csv("provincial_voting_results_ON.csv")
on_results <- on_results %>%
  filter(PollingDate == "2022-Jun-02") %>%
  select(5,12,13,14)
on_cols <- c("district","party","votes","votes_pct")
colnames(on_results) <- on_cols
on_results <- on_results %>%
  mutate(votes_pct = votes_pct*100,
         province = "ON") %>%
  arrange(district)

## QC

link_qc <- "https://en.wikipedia.org/wiki/2022_Quebec_general_election"
qc_wiki_page <- read_html(link_qc)
qc_wiki_page <- qc_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[15]") %>%
  html_table()

qc_results <- qc_wiki_page[[1]]
colnames(qc_results) <- qc_results[1,]
qc_results <- qc_results[-1:-3,]
qc_results <- qc_results %>%
  select(1,15:23)
qc_results[qc_results == "–"] <- NA
qc_results <- qc_results %>%
  pivot_longer(cols = CAQ:Other,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = str_replace_all(votes, "\\[a 5\\]", ""),
         votes = as.numeric(votes)) %>%
  rename(district = Name)

qc_results <- qc_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "QC")

## SK

link_sk <- "https://en.wikipedia.org/wiki/2024_Saskatchewan_general_election"

sk_wiki_page <- read_html(link_sk)
sk_wiki_page <- sk_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[20]") %>%
  html_table()
sk_results <- sk_wiki_page[[1]]
colnames(sk_results) <- sk_results[1,]
sk_results <- sk_results[-1:-3,]
sk_results <- sk_results %>%
  select(1, 15:22)
sk_results[sk_results == "–"] <- NA
sk_results <- sk_results %>%
  pivot_longer(cols = Sask:Ind,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = as.numeric(votes)) %>%
  rename(district = `Riding[a 1]`)
sk_results <- sk_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "SK")


## PEI Election

link_pei <- "https://en.wikipedia.org/wiki/2023_Prince_Edward_Island_general_election"
pei_wiki_page <- read_html(link_pei)


pei_wiki_page <-  pei_wiki_page %>%
  html_elements(xpath = "/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[4]") %>%
  html_table()

pei_results <- pei_wiki_page[[1]]
colnames(pei_results) <- pei_results[1,]
pei_results <- pei_results[c(-1:-3,-12,-19,-26),]
pei_results <- pei_results %>%
  select(1,11:16)
pei_results[pei_results == "–"] <- NA

pei_cols2 <- c("district","PC(PEI)","GPPEI","LIB(PEI)","NDP(PEI)","ISL","Ind")
colnames(pei_results) <- pei_cols2
pei_results <- pei_results %>%
  pivot_longer(cols = 2:7,
               names_to = "party",
               values_to = "votes") %>%
  mutate(votes = str_replace_all(votes, ",",""),
         votes = as.numeric(votes)) 

pei_results <- pei_results %>%
  group_by(district) %>%
  mutate(votes_pct = votes/sum(votes, na.rm = T)*100,
         province = "PEI")

# Province results

prov_results <- rbind(ab_results,bc_results,mb_results,nb_results,nl_results,
                      ns_results,on_results,qc_results,sk_results, pei_results)


prov_results <- prov_results %>%
  mutate(party = case_when(province %in% "AB" & party %in% "NDP" ~ "NDP(AB)",
                           province %in% "AB" & party %in% "LIB" ~ "LIB(AB)",
                           province %in% "AB" & party %in% "AP" ~ "ABP",
                           province %in% "BC" & party %in% "NDP" ~ "NDP(BC)",
                           province %in% "BC" & party %in% "Green" ~ "BCG",
                           province %in% "BC" & party %in% "Con." ~ "CPBC",
                           province %in% "MB" & party %in% "MP" ~ "LIB(MB)",
                           province %in% "MB" & party %in% "PC" ~ "PC(MB)",
                           province %in% "MB" & party %in% "NDP" ~ "NDP(MB)",
                           province %in% "MB" & party %in% "GPM" ~ "MBG",
                           province %in% "NB" & party %in% "Lib" ~ "LIB(NB)",
                           province %in% "NB" & party %in% "PC" ~ "PC(NB)",
                           province %in% "NB" & party %in% "NDP" ~ "NDP(NB)",
                           province %in% "NB" & party %in% "Green" ~ "NBG",
                           province %in% "NL" & party %in% "Lib" ~ "LIB(NL)",
                           province %in% "NL" & party %in% "PC" ~ "PC(NL)",
                           province %in% "NL" & party %in% "NDP" ~ "NDP(NL)",
                           province %in% "NL" & party %in% "GRN" ~ "NLG",
                           province %in% "NS" & party %in% "LIB" ~ "LIB(NS)",
                           province %in% "NS" & party %in% "PC" ~ "PC(NS)",
                           province %in% "NS" & party %in% "NDP" ~ "NDP(NS)",
                           province %in% "NS" & party %in% "GRN" ~ "NSG",
                           province %in% "SK" & party %in% "NDP" ~ "NDP(SK)",
                           province %in% "SK" & party %in% "Grn" ~ "SKG",
                           province %in% "SK" & party %in% "PC" ~ "PC(SK)",
                           province %in% "SK" & party %in% "Buff" ~ "BUF",
                           province %in% "SK" & party %in% "Lib" ~ "SPP",
                           province %in% "SK" & party %in% "Sask" ~ "SKP",
                           province %in% "ON" & party %in% "LIB" ~ "OLP",
                           province %in% "ON" & party %in% "NDP" ~ "ONDP",
                           province %in% "ON" & party %in% "PCP" ~ "PCPO",
                           province %in% "QC" & party %in% "NDP" ~ "ONDP",
                           .default = party))

prov_results_on <- prov_results %>%
  filter(province == "ON")

prov_results <- prov_results %>%
  rename(region = province) %>%
  left_join(dat_343_prov_agg, join_by(region,party))

prov_gen_elec$date[prov_gen_elec$date == "2019-09-10"] <- "2023-10-03"

prov_gen_elec <- prov_gen_elec %>%
  mutate(pop_sup = case_when(date %in% "2023-10-03" & party %in% "PC(MB)" ~ 41.86,
                             date %in% "2023-10-03" & party %in% "NDP(MB)" ~ 45.63,
                             date %in% "2023-10-03" & party %in% "LIB(MB)" ~ 10.63,
                             date %in% "2023-10-03" & party %in% "MBG(MB)" ~ 0.74,
                             .default = pop_sup))

prov_gen_elec <- prov_gen_elec %>%
  filter(date >= "2019-09-10") %>%
  filter(date != "2020-10-24" & date != "2020-09-14" & date != "2020-10-26" & date != "2021-08-17") %>%
  select(party,pop_sup,region)

party <- c("PC(PEI)","GPPEI","LIB(PEI)","NDP(PEI)","ISL","Other")
pop_sup <- c(55.9,21.6,17.2,4.5,0.6,0.3)



pei_gen_elec <- data.frame(party,pop_sup)
pei_gen_elec <- pei_gen_elec %>%
  mutate(region = "PEI")

prov_gen_elec <- rbind(prov_gen_elec,pei_gen_elec)

#prov_gen_elec$pop_sup[prov_gen_elec$party == "CPBC"] <- 35.3 

prov_gen_elec <- prov_gen_elec %>%
  filter(party != "BCU")

#prov_gen_elec <- prov_gen_elec[-10,]



prov_results <- prov_results %>%
  left_join(prov_gen_elec, join_by(region,party))

prov_results <- prov_results %>%
  mutate(pop_sup = case_when(region == "NB" & party == "PC(NB)" ~ 39.34,
                             region == "NB" & party == "LIB(NB)" ~ 34.35,
                             region == "NB" & party == "NBG" ~ 15.24,
                             region == "NB" & party == "PA" ~ 9.19,
                             region == "NS" & party == "PC(NS)" ~ 38.44,
                             region == "NS" & party == "LIB(NS)" ~ 36.82,
                             region == "NS" & party == "NDP(NS)" ~ 20.93,
                             region == "NS" & party == "NSG" ~ 2.15,
                             .default = pop_sup))

prov_results <- prov_results %>%
  group_by(region) %>%
  mutate(poll_diff_pct = ((poll_agg - pop_sup)/pop_sup)*100,
         poll_result = votes_pct + votes_pct * poll_diff_pct / 100)

table(prov_results$party)

prov_results_bc <- prov_results %>%
  filter(region == "BC")


prov_results2 <- prov_results %>%
  group_by(region) %>%
  mutate(poll_diff_pct = ((poll_agg - pop_sup)/pop_sup)*100,
         poll_result = votes_pct + votes_pct * poll_diff_pct / 100,
         poll_result = case_when(poll_result > 100 ~ 51,
                                 poll_diff_pct > 1000 & votes_pct <= 5 ~ 7,
                                 poll_diff_pct > 100 & votes_pct <= 5 ~ 10,
                                 .default = poll_result))



winners <- prov_results %>%
  group_by(region,district) %>%
  slice(which.max(poll_result)) %>%
  ungroup()


winners2 <- winners %>%
  group_by(region) %>%
  count(party)

# List of variable names
vars <- c("dat_343", "results_chart", "seats_proj", 
          "dat_343_prov_agg", "prov_results", "dat_343_prov",
          "winners2", "dat_343_long")


# Loop over the variables and write each to CSV
for (v in vars) {
  # Use get() to access the variable by name
  write.csv(get(v), file = paste0(v, ".csv"), row.names = FALSE)
}


# url <- "https://338canada.com/alberta/polls.htm"
# 
# fetch_html <- function(url) {
#   message("Fetching: ", url)
#   res <- try(
#     GET(url, user_agent("Mozilla/5.0 (compatible; R script; +https://github.com/maxmur17/Polls)")),
#     silent = TRUE
#   )
#   
#   if (inherits(res, "try-error")) {
#     stop("Request failed: ", conditionMessage(attr(res, "condition")))
#   }
#   
#   if (http_error(res)) {
#     stop("HTTP error: ", status_code(res))
#   }
#   
#   read_html(res)
# }
# 
# base_url <- "https://338canada.com/alberta/polls.htm"
# page <-  fetch_html(base_url)
# 
# ab_polls <- page %>% 
# html_elements(css = "#myTable") %>%
#   html_table()

