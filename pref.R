library(tidyverse)
library(tsibble)
library(gt)


# functions -----

read_folder <- function(x) { 
  files <- list.files(x)
  files <- files[grep("\\.csv$", files)]
  data <- read_csv(file.path(x, files), skip=1)
  return(data) }  

summ_pref_flow <- function(data) { 
  data <- data %>% 
    mutate(PartyAb = coalesce(PartyAb, Surname), 
           PartyAb = ifelse(PartyAb=="IND", Surname, PartyAb))
  data %>% 
    filter(CalculationType=="Preference Count") %>% 
    group_by(CountNum, CandidateId, Surname, PartyAb) %>% 
    summarise(Votes=sum(CalculationValue)) %>% 
    arrange(PartyAb, CountNum) %>% 
    group_by(PartyAb) %>% 
    mutate(PrefFlow = ifelse(CountNum==0, Votes, difference(Votes))) %>% 
    left_join(., filter(., PrefFlow<0) %>% select(CountNum, FromPartyAb = PartyAb), 
              by="CountNum", copy = TRUE) %>% 
    mutate(FromPartyAb=ifelse(is.na(FromPartyAb), "FirstPref", FromPartyAb)) %>% 
    bind_cols(distinct(data, StateAb, DivisionId)) %>% 
    inner_join(distinct(data, PartyAb, PartyNm), 
               by="PartyAb") } 

gt_stack_party <- function(gt, ab, nm) {
  data_in <- gt_index(gt, column = {{ nm }})
  gt <- text_transform(
    data=gt, 
    locations=cells_stub(), 
    fn = \(x) {
      glue::glue(
        "<div style='line-height:1;'><span style='font-weight:bold;color:black;'>{x}</span></div>
        <div style='line-height:0.8;'><span style='color:gray;font-size:10px;'>{data_in}</span></div>"
      )
    }) 
  gt <- cols_hide(gt, {{ nm}})
  return(gt) }


summ_twopp <- function(data, division=NULL) { 
  data %>% 
    ungroup() %>% 
    mutate(PrefFlow=PrefFlow/sum(data$PrefFlow), 
           PrefFlow = ifelse(PrefFlow==0, NA_real_, PrefFlow)) %>% 
    pivot_wider(id_cols = PartyAb, names_from=FromPartyAb, values_from=PrefFlow) %>% 
    mutate(TwoPP = sum(pick(where(is.numeric)), na.rm=TRUE), .by=PartyAb) %>% 
    gt(rowname_col = "PartyAb") %>% 
    fmt(
      columns= -PartyAb, 
      fns = \(x) ifelse(is.na(x), "-", scales::label_percent(accuracy=0.1)(x))) %>% 
    cols_label( FirstPref=md("First<br>Preferences"), 
                 TwoPP= md("Final Two Party<br>Preferred")) %>% 
    cols_width(FirstPref ~ px(100), 
               TwoPP ~ px(120)) %>% 
    tab_style(style = cell_fill(color = "lightblue"),
              locations = cells_body(columns=everything(), 
                                     rows = TwoPP>0.5)) %>%  
    tab_spanner(label = "Preferences from:",
                columns = -c(PartyAb, FirstPref, TwoPP))  
  }

calc_party_preference_flow <- function(data) { 
  indep <- data %>% 
    filter(Surname==PartyAb) %>% distinct(CandidateId, Surname) 
  temp <- data %>% 
    anti_join(indep, by="CandidateId") %>% 
    filter(!(FromPartyAb %in% indep$Surname)) %>% 
    filter(FromPartyAb!="FirstPref" & PrefFlow>0) %>% 
    inner_join(data %>% filter(FromPartyAb!="FirstPref" & PrefFlow<0) %>% 
                 select(DivisionNm, CountNum, PrefFlowFrom=PrefFlow), 
               by=c("DivisionNm", "CountNum")) %>% 
    mutate(PrefFlowPc = PrefFlow/abs(PrefFlowFrom)) %>% 
    group_by(PartyAb, FromPartyAb) %>% 
    summarise(PrefFlowPc=mean(PrefFlowPc), 
              VotesN = sum(PrefFlow)) 
  top10 <- temp %>% 
    group_by(PartyAb) %>% 
    summarise(s = sum(VotesN)) %>% 
    slice_max(order_by=s,  n=10)
  temp %>% 
    mutate(PartyAb = ifelse(PartyAb %in% top10$PartyAb, PartyAb, "Other"), 
           PartyAb = factor(PartyAb, levels=c(top10$PartyAb, "Other"))) %>% 
    group_by(PartyAb, FromPartyAb) %>% 
    summarise(VotesN=sum(VotesN)) %>% 
    group_by(FromPartyAb) %>% 
    mutate(PrefFlowPc = VotesN/sum(VotesN)) %>% 
    pivot_wider(id_cols=FromPartyAb, names_from=PartyAb, values_from=PrefFlowPc) } 

gt_party_pref <- function(data, year="2022") { 
  parties <- data %>% 
    group_by(PartyAb) %>% 
    summarise(PartyNm=head(PartyNm, 1)) %>% 
    mutate(PartyNm = str_remove_all(PartyNm, "\\(.+\\)"))
  calc_party_preference_flow(data) %>% 
    left_join(parties, by=c("FromPartyAb"="PartyAb")) %>% 
    ungroup() %>% 
    arrange(FromPartyAb) %>% 
    gt(rowname_col = "FromPartyAb") %>% 
    gt_stack_party(ab=FromPartyAb, nm=PartyNm) %>% 
    fmt(
      columns= -c(FromPartyAb, PartyNm),
      fns = \(x) ifelse(is.na(x), "-", scales::label_percent(accuracy=0.1)(x))) %>% 
    tab_stubhead(label="Preferences from:") %>% 
    tab_spanner(columns=-c(FromPartyAb, PartyNm), 
                label="Preference flow to:") %>% 
    tab_header(title=glue::glue("Australian {year} General Election Lower House Preference Flows"), 
               subtitle="Preference flows from political party candidates 
               to the top ten party preference recipients. Excludes preferences flowing 
               from and to all independent and non-aligned candidates.") %>% 
    tab_style(style=cell_fill(color = "#eb403433"), 
              locations = cells_body(columns= ALP)) %>% 
    tab_style(style=cell_fill(color = "#0000ff33"), 
              locations = cells_body(columns= c(LP, LNP, NP))) %>% 
    tab_style(style=cell_fill(color = "#00cc0033"), 
              locations = cells_body(columns= c(GRN, GVIC))) %>% 
    tab_style(style=cell_fill(color = "#ffff0033"), 
              locations = cells_body(columns= UAPP)) %>% 
    tab_style(style=cell_fill(color = "#ff993333"), 
              locations = cells_body(columns= c(ON, XEN)))  } 

# summ_pref_flow(data) %>% summ_twopp() ----

setwd("~/Documents/AECMAPS/2022-election/") 
folder <- paste("HouseDopByPPDownload-27966", c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"), sep="-")
data <- map_dfr(folder, read_folder) %>% 
  nest_by(DivisionNm) %>% 
  rowwise() %>% 
  mutate(data=list(summ_pref_flow(data))) %>% 
  unnest(data)  

gt_party_pref(data, "2022")

setwd("~/Documents/AECMAPS/2019-election/") 
folder <- paste("HouseDopByPPDownload-24310", c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"), sep="-")
data <- map_dfr(folder, read_folder) %>% 
  nest_by(DivisionNm) %>% 
  rowwise() %>% 
  mutate(data=list(summ_pref_flow(data))) %>% 
  unnest(data) 

t1 <- gt_party_pref(data, "2019")
gtsave(t1, "prefflows2019.html")
