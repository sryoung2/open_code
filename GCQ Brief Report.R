library(tidyverse)
library(pROC)
library(psych)
library(gridExtra)

#Read in the data
WAISICAR_gift <- read_excel("YOURDATA.xlsx")

#Create a separate dataset for volunteer participants
WAISICAR_gift_split <- split(WAISICAR_gift, WAISICAR_gift$CLINICAL)
WAISICAR_gift_volunteer <- WAISICAR_gift_split$`0`

#Create binary giftedness variables 
WAISICAR_gift_volunteer$gai120 <- ifelse(WAISICAR_gift_volunteer$GAI >= 120, T, F)
WAISICAR_gift$gai120 <- ifelse(WAISICAR_gift$GAI >= 120, T, F)

#Demographics table
demotable <- WAISICAR_gift %>% 
  dplyr::group_by(CLINICAL) %>% 
  dplyr::summarise(  "N (Female)" = paste(dplyr::n(),"(", sum(FEMALE), ")"),
  "Age Mean (SD)"= paste(round(mean(AGE, na.rm = TRUE), 2), 
                         "(", round(sd(AGE, na.rm = TRUE), 2), ")"),
  "GAI Mean (SD)" = paste(round(mean(GAI, na.rm= TRUE), 2), 
                          "(", round(sd(GAI, na.rm= TRUE), 2), ")"),
  "GAI>=120"= sum(gai120),
  "TOTAL16 Mean (SD)"= paste(round(mean(TOTAL16, na.rm = TRUE), 2), 
                             "(", round(sd(TOTAL16, na.rm = TRUE), 2), ")"),
  "TOTAL60 Mean (SD)" = paste(round(mean(na.omit(WAISICAR_gift$TOTAL60)), 2), 
                              "(", round(sd(na.omit(WAISICAR_gift$TOTAL60)), 2), ")"))

demotable <- as.data.frame(t(demotable))
demotable <- demotable[-1, ]

colnames(demotable) <- c(  "Volunteer", "Clinical")

knitr::kable(demotable, caption = "Sample Demographics") %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F)

#Regression analyses
lm16 <- lm(GAI ~ TOTAL16, data = WAISICAR_gift)
lm60 <- lm(GAI ~ TOTAL60, data = WAISICAR_gift_volunteer)

summary(lm16)
summary(lm60)

#AUC/ROC analyses for ICAR16 
roc_16_gai <- roc(WAISICAR_gift$gai120, WAISICAR_gift$TOTAL16, plot=T, ci=T)
roc_16_gai
coords(roc_16_gai, ret=c("threshold","sensitivity","specificity"))
power.roc.test(roc_16_gai)

roc_16_gai_nc <- 
  roc(WAISICAR_gift_volunteer$gai120, WAISICAR_gift_volunteer$TOTAL16, plot=T, ci=T)
roc_16_gai_nc
coords(roc_16_gai_nc, ret=c("threshold","sensitivity","specificity"))
power.roc.test(roc_16_gai_nc)

#AUC/ROC analyses for ICAR60
roc_60_gai <- 
  pROC::roc(WAISICAR_gift_volunteer$gai120, WAISICAR_gift_volunteer$TOTAL60, plot=T, ci=T)
roc_60_gai
coords(roc_60_gai, ret=c("threshold","sensitivity","specificity"))
power.roc.test(roc_60_gai)

# SE/SP calculator for ICAR60 (type cut score below)
cutscore <- 33

TP <- sum(WAISICAR_gift$gai120 == T 
          & WAISICAR_gift$TOTAL60 >= cutscore, na.rm = T)
TN <- sum(WAISICAR_gift$gai120 == F 
          & WAISICAR_gift$TOTAL60 < cutscore, na.rm = T)
FP <- sum(WAISICAR_gift$gai120 == F 
          & WAISICAR_gift$TOTAL60 >= cutscore, na.rm = T)
FN <- sum(WAISICAR_gift$gai120 == T 
          & WAISICAR_gift$TOTAL60 < cutscore, na.rm = T)

sensitivity <- (TP/(TP+FN))
specificity <- (TN/(TN+FP))

#Plot ICAR16 ROC
plot_data_16 <-
  WAISICAR_gift %>%
  dplyr::select(gai120, TOTAL16)

icar16roc_build <-
  ggplot(data = plot_data_16, aes(d = gai120, m = TOTAL16)) +
  plotROC::geom_roc(n.cuts = 15)

icar16roc <-
  icar16roc_build +
  ggtitle("ICAR16") +
  annotate("text", x = .75, y = .25, size = 6,
           label = paste("AUC =", round(plotROC::calc_auc(icar16roc_build)$AUC, 2))) +
  scale_x_continuous("Specificity - 1", breaks = seq(1, 0, by = -.1)) +
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1))

icar16roc

#Plot ICAR60 ROC
plot_data_60 <-
  WAISICAR_gift_volunteer %>%
  dplyr::select(gai120, TOTAL60)

icar60roc_build <-
  ggplot(data = plot_data_60, aes(d = gai120, m = TOTAL60)) +
  plotROC::geom_roc(n.cuts = 23, labelsize = 3)

icar60roc <-
  icar60roc_build +
  ggtitle("ICAR60") +
  annotate("text", x = .75, y = .25, size = 6,
           label = paste("AUC =", round(plotROC::calc_auc(icar60roc_build)$AUC, 2))) +
  scale_x_continuous("Specificity - 1", breaks = seq(0, 1, by = .1)) +
  scale_y_continuous("Sensitivity", breaks = seq(0, 1, by = .1))

icar60roc

#Combine into one figure
combined_plot <- grid.arrange(icar16roc, icar60roc, ncol = 2)
