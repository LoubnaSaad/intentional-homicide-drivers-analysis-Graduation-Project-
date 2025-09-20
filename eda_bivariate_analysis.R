df1<-read.csv("finaldata.csv")
df2<-read.csv("finaldata_after_imp.csv")
colnames(df2)
colnames(df1)[7]<-"Food.insecure"
colnames(df2)[7]<-"Food.insecure"
##
ggpairs(df1[,c(5,6,9)])
summary(df1)
#######################################################################
#BIVARIATE RELATIONS BETW. EXPLANATORIES:
#1) HDI & internet
library(ggplot2)
##before##

# Perform correlation test
cor_test <- cor.test(df1$Internet.rate, df1$HDI, use = "complete.obs")

# Extract r and p-value
r_value1 <- cor_test$estimate
p_value1 <- cor_test$p.value

# Build the plot
ggplot(df1, aes(x = HDI, y = Internet.rate)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,105),xlim = c(0.5,1))+
  geom_text(data = subset(df1, Internet.rate < 50|HDI>0.96),
            aes(label = Country),hjust=0.5, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Internet Rate and HDI",
       subtitle = paste("Pearson Correlation (r) =", round(r_value1, 3), 
                        "| p-value =", signif(p_value1, 3)),
       x = "HDI",
       y = "Internet Rate (%)",)+
  theme_minimal()

##after##
cor_test2 <- cor.test(df2$Internet.rate, df2$HDI)

# Extract r and p-value
r_value2 <- cor_test2$estimate
p_value2 <- cor_test2$p.value

# Build the plot
ggplot(df2, aes(x = HDI, y = Internet.rate)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,105),xlim = c(0.49,1))+
  geom_text(data = subset(df2, Internet.rate < 50|HDI>0.96),
            aes(label = Country),hjust=0.5, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Internet Rate and HDI (after imputation)",
       subtitle = paste("Correlation (r) =", round(r_value2, 3), 
                        "| p-value =", signif(p_value2, 3)),
       x = "HDI",
       y = "Internet Rate (%)",)+ theme_minimal()

#####################################

#2)HDI & GDP

##after only##
cor_test2 <- cor.test(df2$GDP, df2$HDI)

# Extract r and p-value
r_value2 <- cor_test2$estimate
p_value2 <- cor_test2$p.value

# Build the plot
ggplot(df2, aes(x = HDI, y = GDP)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(-15,30),xlim = c(0.48,1))+
  geom_text(data = subset(df2, GDP < -5|HDI>0.96| GDP>20),
            aes(label = Country),hjust=0, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between GDP and HDI",
       subtitle = paste("Correlation (r) =", round(r_value2, 3), 
                        "| p-value =", signif(p_value2, 3)),
       x = "HDI",
       y = "GDP Growth Rate (%)",)+ theme_minimal()
summary(df2)

#3)The Crime with Food Insecurity 

##before##

# Perform correlation test
cor_test3 <- cor.test(df1$Crime.Index, df1$Food.insecure, use = "complete.obs")

# Extract r and p-value
r_value3 <- cor_test3$estimate
p_value3 <- cor_test3$p.value

# Build the plot
ggplot(df1, aes(x = Food.insecure, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(0,35))+
  geom_text(data = subset(df1, Crime.Index >70|Food.insecure> 30|Food.insecure< 10&Crime.Index<20),
            aes(label = Country),hjust=0.3, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Crime Index  and Food insecurity",
       subtitle = paste("Pearson Correlation (r) =", round(r_value3, 3), 
                        "| p-value =", signif(p_value3, 3)),
       x = "Food Insecurity (%)",
       y = "Crime Index",)+
  theme_minimal()

##after##
cor_test33 <- cor.test(df2$Food.insecure, df2$Crime.Index)

# Extract r and p-value
r_value33 <- cor_test33$estimate
p_value33 <- cor_test33$p.value

# Build the plot
ggplot(df2, aes(x = Food.insecure, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(0,45))+
  geom_text(data = subset(df2, Crime.Index >70|Food.insecure >30|Food.insecure< 10&Crime.Index<20),
            aes(label = Country),hjust=0.3, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Crime Index  and Food insecurity (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value33, 3), 
                        "| p-value =", signif(p_value33, 3)),
       x = "Food Insecurity (%)",
       y = "Crime Index",)+
  theme_minimal()


#4)The Crime with GDP

##before##

# Perform correlation test
cor_test4 <- cor.test(df1$Crime.Index, df1$GDP, use = "complete.obs")

# Extract r and p-value
r_value4 <- cor_test4$estimate
p_value4 <- cor_test4$p.value

# Build the plot
ggplot(df1, aes(x = GDP, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(-15,21))+
  geom_text(data = subset(df1, Crime.Index >70|GDP> 20|Crime.Index<20|GDP< -10),
            aes(label = Country),hjust=0.5, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Crime Index  and GDP",
       subtitle = paste("Pearson Correlation (r) =", round(r_value4, 3), 
                        "| p-value =", signif(p_value4, 3)),
       x = "GDP Growth Rate (%)",
       y = "Crime Index",)+
  theme_minimal()

##after##
cor_test44 <- cor.test(df2$GDP, df2$Crime.Index)

# Extract r and p-value
r_value44 <- cor_test44$estimate
p_value44 <- cor_test44$p.value

# Build the plot
ggplot(df2, aes(x = GDP, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(-15,21))+
  geom_text(data = subset(df2, Crime.Index >70|GDP> 20|Crime.Index<20|GDP< -10),
            aes(label = Country),hjust=0.5, vjust = -.5, size = 3.5, color = "black")  +
  labs(title = "Relationship Between Crime Index  and GDP (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value44, 3), 
                        "| p-value =", signif(p_value44, 3)),
       x = "GDP Growth Rate (%)",
       y = "Crime Index",)+
  theme_minimal()

#5)•	Absence of Corruption and Unemployment

##before##

# Perform correlation test
cor_test5 <- cor.test(df1$corruption.score, df1$Unemployment, use = "complete.obs")

# Extract r and p-value
r_value5 <- cor_test5$estimate
p_value5 <- cor_test5$p.value

# Build the plot
ggplot(df1, aes(x = Unemployment, y = corruption.score)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 2.5, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(0,37))+
  geom_text(data = subset(df1, corruption.score >85|corruption.score< 18|Unemployment<1|Unemployment> 35),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Absence ofcorruption and Unemployment",
       subtitle = paste("Pearson Correlation (r) =", round(r_value5, 3), 
                        "| p-value =", signif(p_value5, 3)),
       x = "Unemployment Rate (%)",
       y = "Absence ofcorruption",)+
  theme_minimal()
summary(df1)
##after##
cor_test55 <- cor.test(df2$corruption.score, df2$Unemployment)

# Extract r and p-value
r_value55 <- cor_test55$estimate
p_value55 <- cor_test55$p.value

# Build the plot
ggplot(df2, aes(x = Unemployment, y = corruption.score)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(0,37))+
  geom_text(data = subset(df1, corruption.score >85|corruption.score< 18|Unemployment<1|Unemployment> 35),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Absence ofcorruption and Unemployment imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value55, 3), 
                        "| p-value =", signif(p_value55, 3)),
       x = "Unemployment Rate (%)",
       y = "Absence ofcorruption",)+
  theme_minimal()

#6)	Absence of Corruption and crime rate

##before##

# Perform correlation test
cor_test6 <- cor.test(df1$corruption.score, df1$Crime.Index, use = "complete.obs")

# Extract r and p-value
r_value6 <- cor_test6$estimate
p_value6 <- cor_test6$p.value

# Build the plot
ggplot(df1, aes(x = corruption.score, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 2.5, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(10,100))+
  geom_text(data = subset(df1, corruption.score >85|corruption.score< 18|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Crime Index and Absence of Corruption",
       subtitle = paste("Pearson Correlation (r) =", round(r_value6, 3), 
                        "| p-value =", signif(p_value6, 3)),
       y = "Crime.Index",
       x = "Absence of corruption",)+
  theme_minimal()
summary(df1)
##after##
cor_test66 <- cor.test(df2$corruption.score, df2$Crime.Index)

# Extract r and p-value
r_value66 <- cor_test66$estimate
p_value66 <- cor_test66$p.value

# Build the plot
ggplot(df2, aes(x = corruption.score, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100),xlim = c(10,100))+
  geom_text(data = subset(df2, corruption.score >85|corruption.score< 18|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Crime Index and Absence of Corruption",
       subtitle = paste("Pearson Correlation (r) =", round(r_value66, 3), 
                        "| p-value =", signif(p_value66, 3)),
       y = "Crime.Index",
       x = "Absence of corruption",)+
  theme_minimal()

#7)•	Unemployment with Crime Index


##before##

# Perform correlation test
cor_test7 <- cor.test(df1$Unemployment, df1$Crime.Index, use = "complete.obs")

# Extract r and p-value
r_value7 <- cor_test7$estimate
p_value7 <- cor_test7$p.value

# Build the plot
ggplot(df1, aes(x = Unemployment, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100), xlim = c(0,37))+
  geom_text(data = subset(df1, Unemployment<1|Unemployment> 35|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Crime Index and Unemployment",
       subtitle = paste("Pearson Correlation (r) =", round(r_value7, 3), 
                        "| p-value =", signif(p_value7, 3)),
       y = "Crime.Index",
       x = "Unemployment Rate (%)",)+
  theme_minimal()
summary(df1)
##after##
cor_test77 <- cor.test(df2$Unemployment, df2$Crime.Index)

# Extract r and p-value
r_value77 <- cor_test77$estimate
p_value77 <- cor_test77$p.value

# Build the plot
ggplot(df2, aes(x = Unemployment, y = Crime.Index)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,100), xlim = c(0,37))+
  geom_text(data = subset(df2, Unemployment<1|Unemployment> 35|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Crime Index and Unemployment (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value77, 3), 
                        "| p-value =", signif(p_value77, 3)),
       y = "Crime.Index",
       x = "Unemployment Rate (%)",)+
  theme_minimal()

#8) internet rate with crime 

##before##

# Perform correlation test
cor_test8 <- cor.test(df1$Internet.rate, df1$Crime.Index, use = "complete.obs")

# Extract r and p-value
r_value8 <- cor_test8$estimate
p_value8 <- cor_test8$p.value

# Build the plot
ggplot(df1, aes(x = Crime.Index, y = Internet.rate)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,105), xlim = c(10,85))+
  geom_text(data = subset(df1, Internet.rate < 50|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.9, size = 2.5, color = "black")  +
  labs(title = "Relationship Between Crime Index and Internet Rate",
       subtitle = paste("Pearson Correlation (r) =", round(r_value8, 3), 
                        "| p-value =", signif(p_value8, 3)),
       y = "Internet Rate (%)",
       x = "Crime.Index",)+
  theme_minimal()
summary(df1)
##after##
cor_test88 <- cor.test(df2$Internet.rate, df2$Crime.Index)

# Extract r and p-value
r_value88 <- cor_test88$estimate
p_value88 <- cor_test88$p.value

# Build the plot
ggplot(df2, aes(x = Crime.Index, y = Internet.rate)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,105), xlim = c(10,85))+
  geom_text(data = subset(df2, Internet.rate < 50|Crime.Index >70|Crime.Index<20),
            aes(label = Country),hjust=0.5, vjust = -0.9, size = 2.5, color = "black")  +
  labs(title = "Relationship Between Crime Index and Internet Rate (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value88, 3), 
                        "| p-value =", signif(p_value88, 3)),
       y = "Internet Rate (%)",
       x = "Crime.Index",)+
  theme_minimal()


#9)GDP with corruption score
##before##

# Perform correlation test
cor_test9 <- cor.test(df1$corruption.score, df1$GDP, use = "complete.obs")

# Extract r and p-value
r_value9 <- cor_test9$estimate
p_value9 <- cor_test9$p.value

# Build the plot
ggplot(df1, aes(x = corruption.score, y = GDP)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(-15,21),xlim = c(10,100))+
  geom_text(data = subset(df1, corruption.score >85|corruption.score< 18|GDP> 20|GDP< -10),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between GDP and Absence of Corruption",
       subtitle = paste("Pearson Correlation (r) =", round(r_value9, 3), 
                        "| p-value =", signif(p_value9, 3)),
       y = "GDP Growth Rate (%)",
       x = "Absence of corruption",)+
  theme_minimal()
summary(df1)

#10)GDP with unemployment
##before##

# Perform correlation test
cor_test10 <- cor.test(df1$Unemployment, df1$GDP, use = "complete.obs")

# Extract r and p-value
r_value10 <- cor_test10$estimate
p_value10 <- cor_test10$p.value

# Build the plot
ggplot(df1, aes(x = GDP, y = Unemployment)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,37),xlim = c(-15,21))+
  geom_text(data = subset(df1, Unemployment<1|Unemployment> 35|GDP> 20|GDP< -10),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between GDP and Unemployment",
       subtitle = paste("Pearson Correlation (r) =", round(r_value10, 3), 
                        "| p-value =", signif(p_value10, 3)),
       y = "Unemployment Rate (%)",
       x = "GDP Growth Rate (%)",)+
  theme_minimal()
summary(df1)
#after
cor_test110 <- cor.test(df2$Unemployment, df2$GDP, use = "complete.obs")

# Extract r and p-value
r_value110 <- cor_test110$estimate
p_value110 <- cor_test110$p.value

# Build the plot
ggplot(df2, aes(x = GDP, y = Unemployment)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,37),xlim = c(-15,21))+
  geom_text(data = subset(df2, Unemployment<1|Unemployment> 35|GDP> 20|GDP< -10),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between GDP and Unemployment (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value110, 3), 
                        "| p-value =", signif(p_value110, 3)),
       y = "Unemployment Rate (%)",
       x = "GDP Growth Rate (%)",)+
  theme_minimal()

#11)corruption with health exp


##before##

# Perform correlation test
cor_test12 <- cor.test(df1$corruption.score, df1$Health.expenditure, use = "complete.obs")

# Extract r and p-value
r_value12 <- cor_test12$estimate
p_value12 <- cor_test12$p.value

# Build the plot
ggplot(df1, aes(x = corruption.score, y = Health.expenditure)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 2.5, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,25),xlim = c(10,100))+
  geom_text(data = subset(df1, corruption.score >85|corruption.score< 18|Health.expenditure>15),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Health expenditure and Absence of Corruption",
       subtitle = paste("Pearson Correlation (r) =", round(r_value12, 3), 
                        "| p-value =", signif(p_value12, 3)),
       y = "Health Expenditure",
       x = "Absence of corruption",)+
  theme_minimal()
summary(df1)
##after##

# Perform correlation test
cor_test122 <- cor.test(df2$corruption.score, df2$Health.expenditure, use = "complete.obs")

# Extract r and p-value
r_value122 <- cor_test122$estimate
p_value122 <- cor_test122$p.value

# Build the plot
ggplot(df2, aes(x = corruption.score, y = Health.expenditure)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 2.5, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0,25),xlim = c(10,100))+
  geom_text(data = subset(df2, corruption.score >85|corruption.score< 18|Health.expenditure>15),
            aes(label = Country),hjust=0.5, vjust = -0.7, size = 2.58, color = "black")  +
  labs(title = "Relationship Between Health expenditure and Absence of Corruption (after imputation)",
       subtitle = paste("Pearson Correlation (r) =", round(r_value122, 3), 
                        "| p-value =", signif(p_value122, 3)),
       y = "Health Expenditure",
       x = "Absence of corruption",)+
  theme_minimal()
###########################################################################




#####################################################
#BIIVARIATE RELATIONS WITH RESPONSE:
#unemployment +health.exp+ food inseq + hdi

#unemployment
#before
# Perform correlation test
cor_ttest <- cor.test(df1$Counts.of.Victims, df1$Unemployment, use = "complete.obs")

# Extract r and p-value
rr_value <- cor_ttest$estimate
pp_value <- cor_ttest$p.value

ggplot(df1, aes(x = Unemployment, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(0,37)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Unemployment> 30),
            aes(label = Country), vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Unemployment",
       subtitle = paste("Correlation (r) =", round(rr_value, 3), 
                        "| p-value =", signif(pp_value, 3)),
       y = "Counts of Victims",
       x = "Unemployment Rate (%)",)+
  theme_minimal()


#after

cor_ttest1 <- cor.test(df2$Counts.of.Victims, df2$Unemployment)

# Extract r and p-value
rr_value1 <- cor_ttest1$estimate
pp_value1 <- cor_ttest1$p.value

# Build the plot
ggplot(df2, aes(x = Unemployment, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(0,37)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Unemployment> 30),
            aes(label = Country), vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Unemployment(after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value1, 3), 
                        "| p-value =", signif(pp_value1, 3)),
       y = "Counts of Victims",
       x = "Unemployment Rate (%)",)+
  theme_minimal()


#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Unemployment, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 13),xlim = c(0,36))+
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Unemployment>30),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Unemployment",
       subtitle = "Log(Counts of Victims) vs. Unemployment",
       x = "Unemployment Rate (%)",
       y = "Log(Counts of Victims)")+
  theme_minimal()


colnames(df1)
##############
#Health.expenditure
str(df1)
#before
# Perform correlation test
cor_ttest1 <- cor.test(df1$Counts.of.Victims, df1$Health.expenditure, use = "complete.obs")

# Extract r and p-value
rr_value1 <- cor_ttest1$estimate
pp_value1 <- cor_ttest1$p.value

# Build the plot
ggplot(df1, aes(x = Health.expenditure, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Health.expenditure>20),
            aes(label = Country), vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Health expenditure",
       subtitle = paste("Correlation (r) =", round(rr_value1, 3), 
                        "| p-value =", signif(pp_value1, 3)),
       x = "Health Expenditure",
       y = "Counts of Victims")+  theme_minimal()


#after

cor_ttest11 <- cor.test(df2$Counts.of.Victims, df2$Health.expenditure)

# Extract r and p-value
rr_value11 <- cor_ttest11$estimate
pp_value11 <- cor_ttest11$p.value

# Build the plot
ggplot(df2, aes(x = Health.expenditure, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")  +coord_cartesian(ylim = c(0, 60000)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Health.expenditure>20),
            aes(label = Country), vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Health expenditure(after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value11, 3), 
                        "| p-value =", signif(pp_value11, 3)),
       x = "Health Expenditure",
       y = "Counts of Victims")+  theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Health.expenditure, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(3,22))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000| Counts.of.Victims <2|Health.expenditure>20),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Health Expenditure",
       subtitle = "Log(Counts of Victims) vs. Health.expenditure",
       x = "Health.expenditure",
       y = "Log(Counts of Victims)")+  theme_minimal()

###
#Food.insecure 

#before
# Perform correlation test
cor_ttest12 <- cor.test(df1$Counts.of.Victims, df1$Food.insecure, use = "complete.obs")

# Extract r and p-value
rr_value12 <- cor_ttest12$estimate
pp_value12 <- cor_ttest12$p.value

# Build the plot
ggplot(df1, aes(x = Food.insecure, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Food.insecure>40),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Food insecure",
       subtitle = paste("Correlation (r) =", round(rr_value12, 3), 
                        "| p-value =", signif(pp_value12, 3)),
       x = "Food Insecure",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$Food.insecure)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = Food.insecure, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Food.insecure>40),
            aes(label = Country),hjust=0.00000001, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Food Insecure (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "Food Insecure",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Food.insecure, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(0,45))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Food.insecure>40),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Food Insecure",
       subtitle = "Log(Counts of Victims) vs. Food.insecure",
       x = "Food Insecure",
       y = "Log(Counts of Victims)")+theme_minimal()



#HDI
#before
# Perform correlation test
cor_ttest13 <- cor.test(df1$Counts.of.Victims, df1$HDI, use = "complete.obs")

# Extract r and p-value
rr_value13 <- cor_ttest13$estimate
pp_value13 <- cor_ttest13$p.value

# Build the plot
ggplot(df1, aes(x = HDI, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color = "black") +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|HDI<0.5),
            aes(label = Country), hjust=-0.00000000001,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and HDI",
       subtitle = paste("Correlation (r) =", round(rr_value13, 3), 
                        "| p-value =", signif(pp_value13, 3)),
       x = "HDI",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest133 <- cor.test(df2$Counts.of.Victims, df2$HDI)

# Extract r and p-value
rr_value133 <- cor_ttest133$estimate
pp_value133 <- cor_ttest133$p.value

# Build the plot
ggplot(df2, aes(x = HDI, y = Counts.of.Victims)) +
  geom_point(color = "#722F37", size = 3, alpha = 0.7) +
  geom_smooth(color = "#4A001F") +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000),
            aes(label = Country),hjust=0.00000001, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS HDI (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value133, 3), 
                        "| p-value =", signif(pp_value133, 3)))

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = HDI, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 11),xlim = c(0.45,1))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|HDI<0.5|Counts.of.Victims< 2),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and HDI",
       subtitle = "Log(Counts of Victims) vs. HDI",
       x = "HDI",
       y = "Log(Counts of Victims)")+theme_minimal()


###
#GDP

#before
# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$GDP, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = GDP, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000)) +
geom_text(data = subset(df1, Counts.of.Victims > 20000|GDP< -10|GDP>20),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and GDP",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "GDP Growth Rate (%)",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$GDP)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = GDP, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|GDP>20|GDP< -10),
            aes(label = Country),hjust=0.00000001, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS GDP (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "GDP Growth Rate (%)",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = GDP, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(-15,21))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|GDP>20|GDP< -10),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and GDP",
       subtitle = "Log(Counts of Victims) vs. GDP",
       x = "GDP Growth Rate (%)",
       y = "Log(Counts of Victims)")+theme_minimal()

#############
#corruption

#before
# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$corruption.score, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = corruption.score, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000),xlim = c(15,90)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Absence of corruption",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "Absence of corruption",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$corruption.score)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = corruption.score, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(15,86)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|corruption.score<25),
            aes(label = Country),hjust=0.00000001, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Absence of corruption (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "Absence of corruption",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = corruption.score, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(15,87))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|corruption.score<25),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Absence of corruption",
       subtitle = "Log(Counts of Victims) vs. Absence of corruption",
       x = "Absence of corruption",
       y = "Log(Counts of Victims)")+theme_minimal()


###
#internet rate
#before
# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$Internet.rate, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = Internet.rate, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000),xlim = c(10,100)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Internet.rate<20),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Internet Rate",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "Internet Rate (%)",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$Internet.rate)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = Internet.rate, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(10,100)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Internet.rate<20),
            aes(label = Country),hjust=0.5, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Internet Rate (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "Internet Rate (%)",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Internet.rate, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(10,100))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Internet.rate<20|Counts.of.Victims<2&Internet.rate>99),aes(label = Country), vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Internet Rate",
       subtitle = "Log(Counts of Victims) vs. Internet Rate",
       x = "Internet Rate (%)",
       y = "Log(Counts of Victims)")+theme_minimal()
##########
#Crime index:

#before
# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$Crime.Index, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = Crime.Index, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000),xlim = c(10,90)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Crime.Index<15|Crime.Index>80),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and  Crime Index",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "Crime Index",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$Crime.Index)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = Crime.Index, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(10,90)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Crime.Index<15|Crime.Index>80),
            aes(label = Country),hjust=0.5, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Crime Index (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "Crime Index",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Crime.Index, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(10,90))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Crime.Index<15|Crime.Index>80),aes(label = Country),
            vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Crime Index",
       subtitle = "Log(Counts of Victims) vs. Crime Index",
       x = "Crime Index",
       y = "Log(Counts of Victims)")+theme_minimal()

###
summary(df1)
#rural (have no missing)

# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$rural, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = rural, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000),xlim = c(0,90)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|rural>80),
            aes(label = Country), hjust=0.5,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Rural",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "Rural",
       y = "Counts of Victims")+theme_minimal()


#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = rural, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(0,90))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|rural>80),aes(label = Country),
            vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and  Rural",
       subtitle = "Log(Counts of Victims) vs. Rural",
       x = "Rural",
       y = "Log(Counts of Victims)")+theme_minimal()
##
#temprature:
#before
# Perform correlation test
cor_ttest14 <- cor.test(df1$Counts.of.Victims, df1$Temperature, use = "complete.obs")

# Extract r and p-value
rr_value14 <- cor_ttest14$estimate
pp_value14 <- cor_ttest14$p.value

# Build the plot
ggplot(df1, aes(x = Temperature, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black")   +coord_cartesian(ylim = c(0, 50000),xlim = c(-5,30)) +
  geom_text(data = subset(df1, Counts.of.Victims > 20000|Temperature<0),
            aes(label = Country), hjust=0.2,vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Relationship Between Counts of Victims and Temperature",
       subtitle = paste("Correlation (r) =", round(rr_value14, 3), 
                        "| p-value =", signif(pp_value14, 3)),
       x = "Temperature (Annual mean)",
       y = "Counts of Victims")+theme_minimal()


#after

cor_ttest122 <- cor.test(df2$Counts.of.Victims, df2$Temperature)

# Extract r and p-value
rr_value122 <- cor_ttest122$estimate
pp_value122 <- cor_ttest122$p.value

# Build the plot
ggplot(df2, aes(x = Temperature, y = Counts.of.Victims)) +
  geom_point(color = "#7D0A0A", fill = "#BF3131", size = 3, alpha = 0.7) +
  geom_smooth(color="black") +coord_cartesian(ylim = c(0, 50000),xlim = c(-5,30)) +
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Temperature<0),
            aes(label = Country),hjust=0.5, vjust = -1, size = 3.5, color = "black")  +  
  labs(title = "Counts of Victims VS Temperature (after imputation)",
       subtitle = paste("Correlation (r) =", round(rr_value122, 3), 
                        "| p-value =", signif(pp_value122, 3)),
       x = "Temperature (Annual mean",
       y = "Counts of Victims")+theme_minimal()

#Note: we cant see the pattern well so we will take the log to less the effect og the over high counts:

ggplot(df2, aes(x = Temperature, y = log(Counts.of.Victims ))) +
  geom_point(color = "#7D0A0A", fill = "#BF3131",size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +coord_cartesian(ylim = c(0, 12),xlim = c(-5,30))+
  geom_text(data = subset(df2, Counts.of.Victims > 20000|Temperature<0|Temperature>29),aes(label = Country),
            vjust = -1, size = 2.5, color = "black")  + 
  labs(title = "Log-Transformed Relationship Between Victims and Temperature",
       subtitle = "Log(Counts of Victims) vs. Temperature",
       x = "Temperature (Annual mean)",
       y = "Log(Counts of Victims)")+theme_minimal()



#MULTIVARIATE RELATIONS WITH RESPONSE:
#################################################
#POSSIBLE COMBINATIONS:
#Unemployment +health.exp+ Food.insecure + hdi

##Unemployment +HDI
ggplot(df2, aes(x = HDI , y = Counts.of.Victims, size = Unemployment, color = Food.insecure)) +
  geom_point(alpha = 0.9) +
  scale_size(range = c(1, 7)) +
  scale_color_gradient2(low = "#EEEEEE", mid = "#7D0A0A", high = "#000000",
                        midpoint = median(df2$Food.insecure), name = "Food.insecure") +  theme_minimal() +
  
  labs(title = "Bubble Chart: Counts.of.Victims vs HDI ",
       x = "HDI ",
       y = "Counts.of.Victims",
       size ="Unemployment")

#log victims in the relationship:
ggplot(df2, aes(x = Food.insecure , y = log(Counts.of.Victims), size = Unemployment, color = HDI)) +
  geom_point(alpha = 0.9) +coord_cartesian(ylim = c(0, 12),xlim = c(0.45,35))+
  scale_size(range = c(1, 7)) +
  scale_color_gradient2(low = "#EEEEEE", mid = "#7D0A0A", high = "#000000",
                        midpoint = median(df2$HDI), name = "HDI") +  theme_minimal() +
  
  labs(title = "Bubble Chart: log(Counts.of.Victims) vs Food.insecure ",
       x = "Food.insecure ",
       y = "log(Counts.of.Victims)",
       size ="Unemployment")

###
#log victims in the relationship:
ggplot(df2, aes(x = Unemployment , y = log(Counts.of.Victims), size = Food.insecure, color = HDI)) +
  geom_point(alpha = 0.9) +coord_cartesian(ylim = c(0, 12),xlim = c(0.45,35))+
  scale_size(range = c(1, 7)) +
  scale_color_gradient2(low = "#EEEEEE", mid = "#7D0A0A", high = "#000000",
                        midpoint = median(df2$HDI), name = "HDI") +  theme_minimal() +
  
  labs(title = "Bubble Chart: log(Counts.of.Victims) vs Unemployment ",
       x = "Unemployment ",
       y = "log(Counts.of.Victims)",
       size ="Food.insecure")
###
#log victims in the relationship:
ggplot(df2, aes(x = HDI , y = log(Counts.of.Victims), size = Food.insecure, color = Unemployment)) +
  geom_point(alpha = 0.9) +coord_cartesian(ylim = c(0, 12),xlim = c(0.45,1))+
  scale_size(range = c(1, 7)) +
  scale_color_gradient2(low = "#EEEEEE", mid = "#7D0A0A", high = "#000000",
                        midpoint = median(df2$Unemployment), name = "Unemployment") +  theme_minimal() +
  
  labs(title = "Bubble Chart: log(Counts.of.Victims) vs HDI ",
       x = "HDI ",
       y = "log(Counts.of.Victims)",
       size ="Food.insecure")
summary(df2)
