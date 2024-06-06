setwd("D:/")
library(tidyverse)
library(pROC)


dt_adj <- read_csv('timeroc_test.csv')

# ggplot
ggplot(dt_adj, aes(x = Time)) +
  # ROC
  geom_line(aes(y = DL, color = "DL"), size = 1.5) +
  geom_line(aes(y = DLCS, color = "DLCS"), size = 1.5) +
  geom_line(aes(y = Clinical, color = "Clinical model"), size = 1.5) +
  geom_line(aes(y = AJCC, color = "AJCC stage"), size = 1.5) +
  geom_line(aes(y = HPV, color = "HPV"), size = 1.5) +
  geom_line(aes(y = Tstage, color = "Tstage"), size = 1.5) +
  
  geom_point(aes(x = Time, y = DL, color = "DL"), size = 3) +
  geom_point(aes(x = Time, y = DLCS, color = "DLCS"), size = 3) +
  geom_point(aes(x = Time, y = Clinical, color = "Clinical model"), size = 3) +
  geom_point(aes(x = Time, y = AJCC, color = "AJCC stage"), size = 3) +
  geom_point(aes(x = Time, y = HPV, color = "HPV"), size = 3) +
  geom_point(aes(x = Time, y = Tstage, color = "Tstage"), size = 3) +
  # legend
  labs(x = "Time after surgery (months)", y = "AUC") +
  scale_x_continuous(breaks = seq(0, 50, 12)) +
  scale_color_manual(values = c("#1F78B4", "#B2DF8A", "#FB9A99","MediumPurple","red","green")) +
  theme_minimal() +
  theme(legend.position = "top", legend.box.margin = margin(-10, 0, 0, 0),
        legend.title = element_blank(), legend.text = element_text(size = 12),
        panel.grid = element_blank(),  
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text = element_text(size = 12),  
        axis.title = element_text(size = 14)) 

