setwd("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/HJA-Bumble_bee_Networks--MF-")
load("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\data\\spec_net.Rdata")
load("C:/Users/Owner/Downloads/Year_PlantPollinator_Bumblebees.Rdata")
load("C:/Users/Owner/Downloads/Year_PlantPollinator_Bumblebees (1).Rdata")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\prepNets.R")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\misc.R")
source("C:\\Users\\Owner\\University of Oregon Dropbox\\Megan Fenner\\cascades-meadows\\dataPrep\\src\\specialization.R")
install.packages("ggpubr")
install.packages("patchwork")
install.packages("ggforce")
install.packages("lmerTest")
library(dplyr)
library(tidyr)
library(ggpubr)
library(patchwork)
library(ggforce)
library(lme4)
library(lmerTest)

#I decided to test for: PDI, PSI, Species Strength, Closeness, and Effective Partners

which(is.na(sp.lev), arr.ind = TRUE) #gives the specific instances of 
# #where NA values are
names(sp.lev)[colSums(is.na(sp.lev)) > 0] #gives the column names of where NA is, by
# #summing the columns where NA appears, such that if NA is there, it is assigned a value of 1, and 
# #therefore, is then greater than 0.
 
plant_poll_adj <- sp.lev %>% #this row replaces all the NAs in each column with 0s
   mutate(node.specialisation.index.NSI = replace_na(node.specialisation.index.NSI, 0)) %>%
   mutate(weighted.betweenness = replace_na(weighted.betweenness, 0)) %>%
   mutate(closeness = replace_na(closeness, 0)) %>%
   mutate(weighted.closeness = replace_na(weighted.closeness, 0)) %>%
   mutate(d = replace_na(d, 0)) 

#to check for the unique sites- as in following years, 12 sites got reduced to 10
unique(plant_poll_adj$Site)

#only focus on the GenusSpecies data with "Bombus" as the genus
bumblebee_adj <- plant_poll_adj %>%
  #using grepl is a function that acts like a regular expression, looking for instances
  #where there are specific strings of text- in this case, all instances of Bombus +
  #the species name-- this regular expression also removes instances of just "Bombus"
  filter(grepl("^Bombus\\s+\\w+", GenusSpecies))

#checking to see if there are only Bombus in the code
unique(bumblebee_adj$GenusSpecies)

unique(bumblebee_adj$Year)

#the count of some of the bumble bees are low- not many were caught of certain species. 
#Here, I have filtered out the species where the count is greater than or equal to 5. 
Bumble_count <- bumblebee_adj %>%
  group_by(GenusSpecies) %>%
  summarize(occ = n()) %>%
  ungroup() %>%
  filter(occ >= 5)

Bumble_count_more <- bumblebee_adj %>%
  group_by(GenusSpecies) %>%
  summarize(occ = n()) %>%
  ungroup() %>%
  filter(occ >= 10)

#comparing the output to see how many bumble bee species are dropped when filtered 
sort(unique(bumblebee_adj$GenusSpecies))
sort(unique(Bumble_count$GenusSpecies))
sort(unique(Bumble_count_more$GenusSpecies))

bumbles_to_keep <- c(
  "Bombus californicus", "Bombus fernaldae", "Bombus flavidus",
  "Bombus flavifrons", "Bombus insularis", "Bombus melanopygus",
  "Bombus mixtus", "Bombus sitkensis", "Bombus vancouverensis",
  "Bombus vosnesenskii"
)

less_bumbles_to_keep <- c(
  "Bombus californicus",
  "Bombus flavifrons", "Bombus melanopygus",
  "Bombus mixtus", "Bombus vancouverensis",
  "Bombus vosnesenskii"
)

#Using filter allows for the instances of the species listed in bumble_to_keep 
#to be sought in the GenusSpecies column, and using "in" filters for those
#names in the bumblebee_adj data frame.
bumblebees_adj <- bumblebee_adj %>%
  filter(GenusSpecies %in% less_bumbles_to_keep)

#fortuitously, there are now only 10 bumble bee species which are being kept!
sort(unique(bumblebees_adj$GenusSpecies))
#the graphs are not working
bumblebees_adj$Year <- as.numeric(bumblebees_adj$Year)  


#the bumblebees_adj data did not have the complex data- here, we are telling R to
#create a new column using mutate, creating a new column called "Complex", and
#in the complex column, we are assigning the values to correspond with data in
#the site column, such that when there is a certain site, it gets assigned its
#corresponding complex name.
bumblebees_adj <- bumblebees_adj %>%
  mutate(Complex = case_when(
    Site %in% c("CPB", "CPM", "CPR", "CPS") ~ "Carpenter",
    Site %in% c("LM", "LS", "LO", "LB") ~ "Lookout",
    Site %in% c("RP1", "RP2", "M2", "NE") ~ "Frissell",
    TRUE ~ "Other"
  ))


#Effective Partners
ggplot(data= bumblebees_adj, aes( x = Year, y = effective.partners, color = Site, shape = GenusSpecies))+
  geom_jitter()+
  geom_smooth(aes(group = Complex, linetype = Complex), method = "lm", formula = y ~ x, color = "black") + #psuedoreplicating data
  scale_linetype_manual(values = c("Carpenter" = "solid", "Frissell" = "solid", "Lookout" = "solid"))+
  scale_x_continuous(breaks=seq(2011, 2024, by=2))+
  facet_wrap(~ Complex)+
  theme_minimal()+
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size= 20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"), 
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20),
        axis.text.x = element_text( size = 10),
        axis.text.y = element_text(size = 14))+
  labs(y = "Effective Partners", shape = "Bumble bees")+
  scale_shape_discrete(labels = c(expression(italic("Bombus californicus")),
                                  expression(italic("Bombus flavifrons")),
                                  expression(italic("Bombus melanopygus")),
                                  expression(italic("Bombus mixtus")),
                                  expression(italic("Bombus vancouverensis")),
                                  expression(italic("Bombus vosnesenskii")))) +
  guides(linetype = "none") #removes the line legend
effective_partners_model <- lmer(effective.partners ~ scale(Year)*Complex + (1|GenusSpecies) + (1|Site), data = bumblebees_adj)
summary(effective_partners_model)
ggsave("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/graphs_p_values/Effective Partners_noP.jpg", plot = last_plot(), width = 12, height = 8,dpi = 300)

bumblebees_adj %>%
  filter(Complex == "Frissell") %>%
  summarize(unique(GenusSpecies))


#PSI
  
ggplot(data= bumblebees_adj, aes( x = Year, y = PSI, color = Site, shape = GenusSpecies))+
  geom_jitter()+
  geom_smooth(aes(group = Complex, linetype = Complex), method = "lm", formula = y ~ x, color = "black") +
  scale_linetype_manual(values = c("Carpenter" = "dashed", "Frissell" = "dashed", "Lookout" = "dashed"))+
  scale_x_continuous(breaks=seq(2011, 2024, by=2))+
  facet_wrap(~ Complex)+
  theme_minimal()+
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size= 20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"), 
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20),
        axis.text.x = element_text( size = 10),
        axis.text.y = element_text(size = 14))+
  labs(y = "PSI", shape = "Bumble bees")+
  scale_shape_discrete(labels = c(expression(italic("Bombus californicus")),
                                  expression(italic("Bombus flavifrons")),
                                  expression(italic("Bombus melanopygus")),
                                  expression(italic("Bombus mixtus")),
                                  expression(italic("Bombus vancouverensis")),
                                  expression(italic("Bombus vosnesenskii"))))+
  guides(linetype = "none")
PSI_model <-lmer(PSI ~ scale(Year)*Complex + (1|GenusSpecies) + (1|Site), data = bumblebees_adj)
summary(PSI_model)

ggsave("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/graphs_p_values/PSI_noP.jpg", plot = last_plot(), width = 12, height = 8,dpi = 300)

#PDI
  
ggplot(data= bumblebees_adj, aes( x = Year, y = PDI, color = Site, shape = GenusSpecies))+
  geom_jitter()+
  geom_smooth(aes(group = Complex, linetype = Complex), 
                  method = "lm", formula = y ~ x, color = "black") +
  scale_linetype_manual(values = c("Carpenter" = "solid", "Frissell" = "solid", "Lookout" = "solid"))+
  scale_x_continuous(breaks=seq(2011, 2024, by=2))+
  facet_wrap(~ Complex)+ 
  theme_minimal()+
 theme(strip.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size= 20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"), 
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20),
        axis.text.x = element_text( size = 10),
        axis.text.y = element_text(size = 14))+
  labs(y = "PDI", shape = "Bumble bees")+
  scale_shape_discrete(labels = c(expression(italic("Bombus californicus")),
                                  expression(italic("Bombus flavifrons")),
                                  expression(italic("Bombus melanopygus")),
                                  expression(italic("Bombus mixtus")),
                                  expression(italic("Bombus vancouverensis")),
                                  expression(italic("Bombus vosnesenskii"))))+
  guides(linetype = "none") #removes the line legend

PDI_model <-lmer(PDI ~ scale(Year)*Complex + (1|GenusSpecies) + (1|Site), data = bumblebees_adj)
summary(PDI_model)
ggsave("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/graphs_p_values/PDI_noP.jpg", plot = last_plot(), width = 12, height = 8,dpi = 300)

#Closeness
  
ggplot(data= bumblebees_adj, aes( x = Year, y = closeness, color = Site, shape = GenusSpecies))+
  geom_jitter()+
  geom_smooth(aes(group = Complex, linetype = Complex), method = "lm", formula = y ~ x, color = "black") +
  scale_linetype_manual(values = c("Carpenter" = "solid", "Frissell" = "solid", "Lookout" = "solid"))+
  scale_x_continuous(breaks=seq(2011, 2024, by=2))+
  facet_wrap(~ Complex)+
  theme_minimal()+
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size= 20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"), 
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20),
        axis.text.x = element_text( size = 10),
        axis.text.y = element_text(size = 14))+
  labs(y = "Closeness", shape = "Bumble bees")+
  scale_shape_discrete(labels = c(expression(italic("Bombus californicus")),
                                     expression(italic("Bombus flavifrons")),
                                     expression(italic("Bombus melanopygus")),
                                     expression(italic("Bombus mixtus")),
                                     expression(italic("Bombus vancouverensis")),
                                     expression(italic("Bombus vosnesenskii"))))+
  guides(linetype = "none") #removes the line legend
Closeness_model <-lmer(closeness ~ scale(Year)*Complex + (1|GenusSpecies) + (1|Site), data = bumblebees_adj)
summary(Closeness_model)


#only Carpenter is significant
ggsave("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/graphs_p_values/Closeness_noP.jpg", plot = last_plot(), width = 12, height = 8,dpi = 300)

#Species Strength
ggplot(data= bumblebees_adj, aes( x = Year, y = species.strength, color = Site, shape = GenusSpecies))+
  geom_jitter()+
  geom_smooth(aes(group = Complex, linetype = Complex), method = "lm", formula = y ~ x, color = "black") +
  scale_linetype_manual(values = c("Carpenter" = "solid", "Frissell" = "solid", "Lookout" = "solid"))+
  scale_x_continuous(breaks=seq(2011, 2024, by=2))+
  facet_wrap(~ Complex)+
  theme_minimal()+
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size= 20, face = "bold"),
        axis.title.y = element_text(size=20, face = "bold"), 
        legend.title = element_text(size = 24), 
        legend.text = element_text(size = 20),
        axis.text.x = element_text( size = 10),
        axis.text.y = element_text(size = 14))+
  labs(y = "Species Strength", shape = "Bumble bees")+
  scale_shape_discrete(labels = c(expression(italic("Bombus californicus")),
                                  expression(italic("Bombus flavifrons")),
                                  expression(italic("Bombus melanopygus")),
                                  expression(italic("Bombus mixtus")),
                                  expression(italic("Bombus vancouverensis")),
                                  expression(italic("Bombus vosnesenskii"))))+
  guides(linetype = "none")
Species_strength_model <-lmer(species.strength ~ scale(Year)*Complex + (1|GenusSpecies) + (1|Site), data = bumblebees_adj)
summary(Species_strength_model) #slope of the year varies by the complex (Year*Complex)
#treatment complex model- the intercept is Carpenter by default, the changes are the changes
#of each site of it compared to the value outputted. The intercept is the average value of each
#condition
ggsave("C:/Users/Owner/Documents/UO Master's/Classes/Data Science Eco Conserv/Final Project/graphs_p_values/Species_Strength_noP.jpg", plot = last_plot(), width = 12, height = 8,dpi = 300)

