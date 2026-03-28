############## Code for Real Chemistry Writing Assignment ################

# Written by Michelle Frankot
# March 28, 2026 

# Plot statistics in Table 3 (adverse events) for https://www.nejm.org/doi/full/10.1056/NEJMoa1409077




### Environment Set Up ###

library(dplyr) # for data manipulation
library(ggplot2) # for plotting



### Create dataframe from Table 3 ###

data <- data.frame(
  Event = rep(c("Symptomatic Hypotension", 
                "Symptomatic Hypotension (Systolic <90 mmHG)",
                "Serum Creatinine >=2.5 mg/dl", 
                "Serum Creatinine >=3.0 mg/dl",  
                "Serum Potassium >5.5 mmol/L",
                "Serum Potassium >6.0 mmol/L",
                "Cough",
                "Angioedema (No Tx or AH Only",
                "Angioedema (CA orGCs; No Hospitalization",
                "Angioedema (Hospitalization; No Airway Compromise",
                "Angioedema (Airway Compromise)"), 
              times = 1, each = 2),
  Group = rep(c("LCZ696", "Enalapril"), times = 11),
  n = c(588, 388, 112, 59, 139, 188, 63, 83, 674, 727, 
        181, 236, 474, 601, 10, 5, 6, 4, 3, 1, 0, 0),
  p.sig = rep(c("***", "***", "**", "", "", "**", "***", "", "", "", ""), 
              times = 1, each = 2)
)


# calculate percentages and cross-check with Table 3

to.plot <- data %>%
  mutate(Total_N = ifelse(Group == "LCZ696", 4187, 4212))%>%
  mutate(Percent = round(100*(n/Total_N), digits = 1))%>%
  filter(Event != "Angioedema (Airway Compromise)")





### Create barplot ###

ggplot(data = to.plot, 
       aes(x = Event, y = Percent, fill = Group))+
  geom_bar(stat = "identity", 
           color = "black",
           position = "dodge",
           )+
  coord_flip()+
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(width = 1),
            hjust = -0.3, 
            size = 4)+
  ggtitle("Adverse Events During Treatment")+
  ylab("Experienced Event (%)")+
  xlab("")+
  scale_fill_manual(values = c("#590E36", "#E0C1D2"))+
  theme_classic(base_size = 16)+
  theme(legend.position = "bottom")


