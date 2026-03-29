############## Code for Real Chemistry Writing Assignment ################

# Written by Michelle Frankot
# March 28, 2026 

# Plot statistics in Table 3 (adverse events) for https://www.nejm.org/doi/full/10.1056/NEJMoa1409077




### Environment Set Up ###

library(dplyr) # for data manipulation
library(ggplot2) # for plotting




### Constants and Labels ###

# AE names for dataframe
AEs <- c("Symptomatic Hypotension", 
            "Symptomatic Hypotension (Systolic <90 mmHG)",
            "Serum Creatinine >=2.5 mg/dl", 
            "Serum Creatinine >=3.0 mg/dl",  
            "Serum Potassium >5.5 mmol/L",
            "Serum Potassium >6.0 mmol/L",
            "Cough",
            "Angioedema (No Tx or AH Only)",
            "Angioedema (CA or GCs)",
            "Angioedema (Hospitalized; No Airway Compromise)",
            "Angioedema (Hospitalized; Airway Compromise)")

# AE names for axis labels
AE_Labels <- c("Symptomatic Hypotension", 
            "Systolic Blood Pressure < 90 mmHG",
            expression("Serum Creatinine ">="2.5 mg/dl"), 
            expression("Serum Creatinine ">="3.0 mg/dl"),   
            "Serum Potassium > 5.5 mmol/L",
            "Serum Potassium > 6.0 mmol/L",
            "Cough",
            "Angioedema: Antihistamines or No Tx",
            "Angioedema: Catecholamines or GCs",
            "Angioedema: Hospitalized")





### Create dataframe containing the statistics from Table 3 ###

data <- data.frame(
  Event = rep(AEs, times = 1, each = 2),
  Group = rep(c("LCZ696", "Enalapril"), times = 11),
  n = c(588, 388, 112, 59, 139, 188, 63, 83, 674, 727, 
        181, 236, 474, 601, 10, 5, 6, 4, 3, 1, 0, 0),
  p = rep(c("p < 0.001", "p < 0.001", "p = 0.007", "", "", "p = 0.007", "p < 0.001", "", "", "", ""), 
          times = 1, each = 2),
  p.sig = rep(c("***", "***", "**", "", "", "**", "***", "", "", "", ""), 
              times = 1, each = 2)
)



### Data Processing ###

# Calculate percentages to cross-check against Table 3
# Assign levels to factor variables
# Create variable to assign location to p-values

to.plot <- data %>%
  mutate(Total_N = ifelse(Group == "LCZ696", 4187, 4212))%>%
  mutate(Percent = round(100*(n/Total_N), digits = 1))%>% 
  mutate(Group = factor(Group, levels = c("LCZ696", "Enalapril")),
    Event = factor(Event, levels = rev(AEs)))%>%
  filter(Event != "Angioedema (Hospitalized; Airway Compromise)")%>%
  group_by(Event)%>%
  mutate(p.label = max(Percent) + 1)%>%
  ungroup()





### Create Barplot ###

p <- ggplot(data = to.plot, 
       aes(x = Event, y = Percent, fill = Group))+
  geom_bar(stat = "identity", 
           color = "black",
           position = "dodge")+
  coord_flip()+
  geom_text(aes(x = Event, y = p.label, label = p), 
            vjust = 0.4, 
            size = 4)+
  ggtitle("Adverse Events During Treatment")+
  ylab("Experienced Event (%)")+
  xlab("")+
  scale_fill_manual(name = "", values = c("#590E36", "#E0C1D2"))+
  scale_x_discrete(labels = rev(AE_Labels))+
  ylim(c(0, 20))+
  theme_classic(base_size = 14)+
  theme(legend.position = "bottom")
p

ggsave(plot = p, filename = "AE_Plot.png", width = 32, height = 12, unit = "cm")




