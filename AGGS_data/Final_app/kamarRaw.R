library(ggplot2)
library(plotly)

kamarRaw <- read.csv("NCEA_Level3_ProgressionAlt13MQ.csv")
kamarRaw <- read.csv("NCEA_Level3_ProgressionAltModified.csv")
head(kamarRaw)
tail(kamarRaw)

colnames(kamarRaw)
# Select vars to keep
kamar <- kamarRaw[c(1:3, 7, 8, 
                    12, 15, 21, 24, 
                    32, 40, 41, 44, 
                    47, 50, 56, 59)]

# Rename vars
colnames(kamar) <- c("ID", "Last_name", "First_name", "Tutor", "Ethnicity", 
                     "Attd_percent", "Prev_Level", "Current_Level", "Credits_remain", 
                     "Int_remain", "Ext_remain", "Lit1_AS", "Lit1_US",
                     "Lit_Read", "Lit_Write", "Num_AS", "Num_US")

# Create L1 literacy and numeracy vars (numeric)
kamar$Lit1 <- kamar$Lit1_AS + kamar$Lit1_US
kamar$Num1 <- kamar$Num_AS + kamar$Num_AS

# Create UE num and lit vars (factors)
kamar$UE_Num <- ifelse(kamar$Num1 < 10, "No_Num", "UE_Num")
kamar$Lit <- ifelse(kamar$Lit_Read>=5 & kamar$Lit_Write>=5, "UE_Lit",
                    ifelse(kamar$Lit1<10, "No_L1_Lit", "L1_Lit_only")) 

# Total credits earned for current certificate
kamar$Credits_for_cert <- ifelse(kamar$Prev_Level<20, kamar$Prev_Level+kamar$Current_Level,
                                 kamar$Current_Level+20)
kamar$Rank <- rank(kamar$Credits_for_cert, ties.method = "first") 
kamar$Merit <- kamarRaw[, 19]
kamar$Excellence <- kamarRaw[, 20]

# Plot. UE Numeracy as pch (symbol) and Lit as colour
p <- ggplot(kamar, aes(y=Rank, colour=Lit, label=First_name, label1=Last_name, label2=Tutor)) +
  geom_point(aes(x=Credits_for_cert, pch=UE_Num)) +
  geom_point(aes(x=Credits_for_cert+Int_remain), alpha=0.4, pch=3) +
  geom_vline(xintercept=80, colour="grey") +
  geom_segment(aes(x=Credits_for_cert, xend=Credits_for_cert+Credits_remain, yend=Rank), alpha=0.2) +
  geom_segment(aes(x=Credits_for_cert, xend=Credits_for_cert+Int_remain, yend=Rank), alpha=0.4) +
  labs(x="Credits for Current NCEA Level Certificate", y="", 
       title="+ marks Max total credits with Internals remaining") +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank())

ggplotly(p, tooltip = c("label", "label1", "label2", "x"))
