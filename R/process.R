
library(here)
library(readxl)
library(splines)
source("R/pref.R")

folder <- paste(here("2022-election", "HouseDopByPPDownload-27966"), c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"), sep="-")
data <- map_dfr(folder, read_folder) %>% 
  nest_by(DivisionNm) %>% 
  rowwise() %>% 
  mutate(data=list(summ_pref_flow(data))) %>% 
  unnest(data)  
write_csv(data, here("data/prefflows2022.csv"))

gt_party_pref(data, "2022")


# 2019 preference flows 
folder <- paste(here("2019-election", "HouseDopByPPDownload-24310"), c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"), sep="-")
data <- map_dfr(folder, read_folder) %>% 
  nest_by(DivisionNm) %>% 
  rowwise() %>% 
  mutate(data=list(summ_pref_flow(data))) %>% 
  unnest(data) 
write_csv(data, here("data/prefflows2019.csv"))

t1 <- gt_party_pref(data, "2019")
gtsave(t1, "prefflows2019.html") 

# 2022 diversity -----
entropy <- function(x) { 
  x <- x/sum(x)
  return(-sum(x*log(x))) }
 
herfindahl <- function(x) { 
  sum((x/sum(x))^2) } 

maxfirstpp <- function(x) { 
  max(x/sum(x)) }

demo <- read_excel("demographic-classification-as-at-2-august-2021.xlsx", 
                   skip=3, col_names = c("DivisionNm", "DemographicClass", "StateAb"))

diversity <- data %>% 
  filter(CountNum==0 & CalculationType=="Preference Count" & CalculationValue>0) %>% 
  group_by(StateAb, DivisionNm, PPNm) %>% 
  summarise(
    Candidates=length(unique(CandidateId)),
    MaxFirstPref = maxfirstpp(CalculationValue),
    Entropy=entropy(CalculationValue), 
    Herfindahl=herfindahl(CalculationValue)) %>% 
  left_join(demo %>% select(-StateAb), 
            by=c("DivisionNm")) %>% 
  ungroup() %>% 
  mutate(ExpEntropy= predict(lm(Entropy ~ bs(Candidates) + DemographicClass, data = .)))
  


ggplot(diversity, mapping=aes(x=Candidates)) + 
  geom_point(aes(y=Entropy, col=DemographicClass)) + 
  geom_line(aes(y=ExpEntropy), lwd=1.3) + 
  facet_wrap(vars(DemographicClass), nrow=1) + 
  theme_bw() + 
  theme(legend.position = "none")

summary(lm(Entropy ~ bs(Candidates) + DemographicClass, diversity)) 



   