
#first preferences
data |> 
  filter(CountNum==0) |> 
  mutate(Right=rightness(PartyAb)) |> 
  filter(!is.na(Right)) |> 
  summarise(Right= sum(Right*Votes)/sum(Votes), 
            Votes = sum(Votes),
            .by=c("StateAb", "DivisionId", "DivisionNm")) |> 
  inner_join(data |> 
               filter(CountNum==max(CountNum), 
                      .by="DivisionNm") |> 
               filter(Votes==max(Votes), 
                      .by="DivisionNm") |> 
               select(DivisionId, PartyAb, PartyNm), 
             by="DivisionId")
  mutate(i = row_number()-1, 
         .by=c("StateAb", "Right")) |> 
  ggplot(aes(y=i, x=Right)) + 
  geom_point(size=0.4) + 
  facet_grid(rows=vars(StateAb), switch="y") + 
  theme_bw() + 
  theme(axis.text=element_blank(), 
        axis.ticks=element_blank()) +
  geom_text(aes(label=DivisionNm), size=2, check_overlap=TRUE, angle=45, vjust="bottom", nudge_y = 0.3) + 
  coord_cartesian(clip = 'off') + 
  labs(title="Electorates First Preferences", y=NULL)

#rightness of parties based on preference flows
data |> 
  filter(CountNum==1) |> 
  filter(PrefFlow > 0) |> 
  mutate(Right = rightness(PartyAb)) |> 
  filter(!is.na(Right)) |> 
  summarise(Right = sum(Right*PrefFlow)/sum(PrefFlow), 
            Votes = sum(PrefFlow),
             .by="FromPartyAb") |>  
  inner_join(
    data |> 
      distinct(PartyAb, PartyNm) |> 
      mutate(PartyNm = str_remove(PartyNm, "\\s\\(.+\\)")) |> 
      summarise(PartyNm = head(PartyNm,1), .by="PartyAb"), 
    by=c("FromPartyAb"="PartyAb")
  )
  readr::write_csv("parties2ndpref.csv") 
  inner_join
  mutate(i = row_number()-1, 
         .by=c("Right")) |> 
  ggplot(aes(y=i, x=Right)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.text=element_blank(), 
        axis.ticks=element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank()) +
  geom_text(aes(label=FromPartyAb), size=2, check_overlap=TRUE, angle=45, vjust="bottom", nudge_y = 0.1) + 
  coord_cartesian(clip = 'off') + 
  labs(title="Party Leaning Based on Preferences", y=NULL)

  
  
data |> 
  filter(CountNum==max(CountNum), 
         .by="DivisionNm") |> 
  filter(Votes==max(Votes), 
         .by="DivisionNm") |> 
  select(DivisionId, PartyAb, 