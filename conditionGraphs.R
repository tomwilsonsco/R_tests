conditioncalc <- NFICONDITION %>%
mutate(COUNTRY = case_when (REGION_CODE ==1 ~ "Wales",
                                           REGION_CODE >1 & REGION_CODE < 10 ~ "England",
                                           REGION_CODE > 9 ~ "Scotland")) %>%
mutate(NATIVE = case_when(WITHIN_NATIVE == 0 ~ "Non-Native",
                          WITHIN_NATIVE == 1 ~ "Near-Native",
                          WITHIN_NATIVE == 2 ~ "Native",
                          WITHIN_NATIVE >3 ~"Non-woodland"))  
  


cond_colours <- function(field){
  recode(field, '1' ="RED", '2' = "AMBER", '3' = "GREEN")
}

sum_cond <- function(data, ...) {
  data %>% group_by_(.dots = lazyeval::lazy_dots(...)) %>% summarise(TOTAL_AREA = sum(WEC_PATCH_AREA))
}



verticalstructure_res <- sum_cond(conditioncalc, VERTICALSTRUCTURE_S, COUNTRY, NATIVE)
verticalstructure_res<- mutate(verticalstructure_res, SCORES = cond_colours(VERTICALSTRUCTURE_S))

verticalstructure_res<-verticalstructure_res %>% 
filter(!is.na(SCORES)& !is.na(NATIVE))%>%
group_by(COUNTRY, NATIVE) %>%
mutate(GROUP_TOTAL = sum(TOTAL_AREA)) %>% 
ungroup() %>%
mutate(PERCENTAGE = (TOTAL_AREA/ GROUP_TOTAL) * 100)

#verticalstructure_res$SCORES <- factor(verticalstructure_res$SCORES, levels = c("GREEN","AMBER","RED"))
        
colour.values<-c("RED" = "red", "AMBER" = "orange", "GREEN" = "green")

    
p<-ggplot(data = verticalstructure_res) + 
  geom_bar(mapping = aes(x = factor(SCORES, levels =c("GREEN", "AMBER", "RED")), y = PERCENTAGE, fill = SCORES), stat="identity")+
  facet_wrap(~ COUNTRY ~NATIVE, labeller = label_wrap_gen(multi_line=FALSE)) +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      legend.position = "none")
p + scale_fill_manual("legend",values=colour.values)

ggplot(data = verticalstructure_res) +
  geom_bar(mapping = aes(x=SCORES, y = PERCENTAGE),stat="identity")
