# All H&L sites summary for MPA/CINMS analysis    11/13/2020
# By site, year, species caught .  


rm(list=ls())



library(plyr)
library(devtools)
library(tidyr)
library(dplyr)
library(vegan)



#
#
#

Fishinfo = read.csv("data/All sites data_2020.csv")


#replace NA with 0
Fishinfo[,22:86][is.na(Fishinfo[,22:86])] <-0


#remove unecessary species (N=7)
species.rm = !names(Fishinfo) %in% c("Anemone.unid", "California.Market.Squid", "Giant.plumose.anemone",
                                     "Gorgonian.unident..coral.", "Humboldt.Squid", "Invert.Unidentified",
                                     "Spiny.Sea.Star")
Fishinfo = Fishinfo[,species.rm]      # should have 79 cols




# List groups
target.sp = c("Barred Sand Bass", "Brown Rockfish",        # 32 total
              "California Scorpionfish", "California Sheephead",
              "Canary Rockfish", "Copper Rockfish", 
              "Flag Rockfish", "Gopher Rockfish", "Greenblotched Rockfish",
              "Greenspotted Rockfish", "Ocean Whitefish", "Pink Rockfish",
              "Sanddab Unidentified", "Southern Rock Sole", "Starry Rockfish", 
              "Treefish", "Vermilion Rockfish", "Widow Rockfish",
              "Yellowtail Rockfish", "Bocaccio", "California Yellowtail",
              "Chilipepper", "Cowcod", "Lingcod", "Mexican Rockfish", 
              "Olive Rockfish", "Pacific Bonito", "Yelloweye Rockfish",
              "Bank Rockfish", "Blue Rockfish", "Speckled Rockfish",
              "Squarespot Rockfish")

nontarget.sp = c("Brown Smoothhound", "Freckled Rockfish", 
                 "Gray Smoothhound", "Greenstriped Rockfish",
                 "Honeycomb Rockfish", "Rosy Rockfish", 
                 "White Croaker", "Calico Rockfish", "Lizardfish",
                 "Spiny Dogfish", "Halfbanded Rockfish",
                 "Pacific Mackerel", "Swordspine Rockfish")

carnivore.sp = c("Barred Sand Bass", "Brown Rockfish",
                 "Brown Smoothhound", "California Scorpionfish", 
                 "California Sheephead", "Canary Rockfish",
                 "Copper Rockfish", "Flag Rockfish", "Freckled Rockfish",
                 "Gopher Rockfish", "Gray Smoothhound",
                 "Greenblotched Rockfish", "Greenstriped Rockfish", "Greenspotted Rockfish",
                 "Honeycomb Rockfish", "Ocean Whitefish", "Pink Rockfish",
                 "Rosy Rockfish", "Sanddab Unidentified", "Southern Rock Sole",
                 "Starry Rockfish", "Treefish", "Vermilion Rockfish", 
                 "White Croaker", "Widow Rockfish", "Yellowtail Rockfish")

piscivore.sp = c("Bocaccio", "California Yellowtail", "Chilipepper",
                 "Cowcod", "Lingcod", "Lizardfish",
                 "Mexican Rockfish", "Olive Rockfish",
                 "Pacific Bonito", "Spiny Dogfish", "Yelloweye Rockfish")

planktivore.sp = c("Bank Rockfish", "Blue Rockfish", "Halfbanded Rockfish",
                   "Pacific Mackerel", "Speckled Rockfish", 
                   "Squarespot Rockfish", "Swordspine Rockfish")

# 15 H&L sites fall within state/federal reserve. 1 is defacto military reserve (San Clemente)
MPA.sites = c(48, 59,	62,	97,	152,	180,	181,	182, 
              184,	185,	228,	229,	292,	293,	379,	413)


MPA.Control.sites = c(40,	45,	54,	68,	71,	101,	149,	151,	
                      154,	186,	233,	291,	298,	299,	374,	375,	383)


CINMS.sites = c(40,	43,	45,	48,	180,	181,	182,	184,	185,
                186,	226,	228,	229,	231,	232,	233,	287,
                333,	365,	383,	396,	397,	398,	399,	402,
                413,	419,	508,	512,	514,	516,	517,	518,
                519,	520,	523,	525)


#Sum across species for nmds   # adds 7 cols

Fishinfo2 = Fishinfo %>% mutate(
  Target = (sumrow = Barred.Sand.Bass + Brown.Rockfish + California.Scorpionfish + 
              California.Sheephead + Canary.Rockfish + Copper.Rockfish + 
              Flag.Rockfish + Gopher.Rockfish + Greenblotched.Rockfish + 
              Greenspotted.Rockfish + Ocean.Whitefish + Pink.Rockfish + 
              Sanddab.Unidentified + Southern.Rock.Sole + Starry.Rockfish +  
              Treefish + Vermilion.Rockfish + Widow.Rockfish + California.Yellowtail +
              Yellowtail.Rockfish + Bocaccio + 
              Chilipepper + Cowcod + Lingcod + Mexican.Rockfish +  
              Olive.Rockfish + Pacific.Bonito + Yelloweye.Rockfish + 
              Bank.Rockfish + Blue.Rockfish + Speckled.Rockfish + 
              Squarespot.Rockfish),
  Nontarget = (sumrow = Greenstriped.Rockfish + 
                 Honeycomb.Rockfish + Rosy.Rockfish +  
                 Lizardfish + Brown.Smoothhound + Freckled.Rockfish + 
                 Gray.Smoothhound + Calico.Rockfish + White.Croaker +
                 Spiny.Dogfish + Halfbanded.Rockfish + 
                 Pacific.Mackerel +  Swordspine.Rockfish),
  Carnivore = (sumrow = California.Scorpionfish + Barred.Sand.Bass + Brown.Rockfish +
                 Brown.Smoothhound + Gray.Smoothhound + Freckled.Rockfish +
                 California.Sheephead +  Canary.Rockfish + 
                 Copper.Rockfish +  Flag.Rockfish + Gopher.Rockfish +  
                 Greenblotched.Rockfish +  Greenstriped.Rockfish +  Greenspotted.Rockfish + 
                 Honeycomb.Rockfish +  Ocean.Whitefish +  Pink.Rockfish + 
                 Rosy.Rockfish +  Sanddab.Unidentified +  Southern.Rock.Sole + 
                 Starry.Rockfish +  Treefish +  Vermilion.Rockfish +  
                 Widow.Rockfish +  Yellowtail.Rockfish),
  Piscivore = (sumrow = Bocaccio + Chilipepper + California.Yellowtail +
                 Cowcod +  Lingcod +  Lizardfish + 
                 Mexican.Rockfish +  Olive.Rockfish + 
                 Pacific.Bonito +  Spiny.Dogfish +  Yelloweye.Rockfish),
  Planktivore = (sumrow = Bank.Rockfish + Blue.Rockfish +Halfbanded.Rockfish + 
                   Pacific.Mackerel +  Speckled.Rockfish +  
                   Squarespot.Rockfish + Swordspine.Rockfish),
  Reserve.status = if_else(SiteName %in% MPA.sites, "MPA", 
                           if_else(SiteName %in% MPA.Control.sites, "Control", "NA")),
  CINMS.status = if_else(SiteName %in% CINMS.sites, "Inside.CINMS", "Outside.CINMS")
)



# Add CPUE and diversity calculations     #adds 15 col
# MPA at time of sampling? 1: yes 0:no

Fishinfo2 = Fishinfo2 %>% mutate(
  MPA.when.sampled = if_else((SiteName == 48 | SiteName == 180 | SiteName == 184 | 
                                SiteName == 228 | SiteName ==229 | SiteName == 413) & SurveyYear>2006, 1,
                             if_else((SiteName == 181| SiteName == 182) & SurveyYear>2007, 1,
                                     if_else(SiteName == 185 & SurveyYear >2008, 1,
                                             if_else(SiteName == 97 & SurveyYear>2009,1,
                                                     if_else((SiteName == 59|SiteName ==62 |SiteName == 152|
                                                                SiteName ==292 |SiteName ==293|SiteName ==379) & SurveyYear>2011, 1,0))))),
  CPUE = HookswithFish/ValidHook,
  Target.CPUE = Target/ValidHook,
  Nontarget.CPUE = Nontarget/ValidHook,
  Piscivore.CPUE = Piscivore/ValidHook,
  Carnivore.CPUE = Carnivore/ValidHook,
  Planktivore.CPUE = Planktivore/ValidHook,
  Target.DShannons = diversity(Fishinfo2[,c("Barred.Sand.Bass", "Brown.Rockfish", "California.Yellowtail",
                                            "California.Scorpionfish", "California.Sheephead",
                                            "Canary.Rockfish", "Copper.Rockfish", 
                                            "Flag.Rockfish", "Gopher.Rockfish", "Greenblotched.Rockfish",
                                            "Greenspotted.Rockfish", "Ocean.Whitefish", "Pink.Rockfish",
                                            "Sanddab.Unidentified", "Southern.Rock.Sole", "Starry.Rockfish", 
                                            "Treefish", "Vermilion.Rockfish", "Widow.Rockfish",
                                            "Yellowtail.Rockfish", "Bocaccio", 
                                            "Chilipepper", "Cowcod", "Lingcod", "Mexican.Rockfish", 
                                            "Olive.Rockfish", "Pacific.Bonito", "Yelloweye.Rockfish",
                                            "Bank.Rockfish", "Blue.Rockfish", "Speckled.Rockfish",
                                            "Squarespot.Rockfish")], index = "simpson"),
  Nontarget.DShannons = diversity(Fishinfo2[,c("Greenstriped.Rockfish","Brown.Smoothhound", "Freckled.Rockfish", 
                                               "Gray.Smoothhound", "Calico.Rockfish",
                                               "Honeycomb.Rockfish", "Rosy.Rockfish", 
                                               "Lizardfish", "White.Croaker",
                                               "Spiny.Dogfish", "Halfbanded.Rockfish",
                                               "Pacific.Mackerel", "Swordspine.Rockfish")], index = "simpson"),
  Piscivore.DShannons = diversity(Fishinfo2[,c("Bocaccio", "Chilipepper", "California.Yellowtail",
                                               "Cowcod", "Lingcod", "Lizardfish",
                                               "Mexican.Rockfish", "Olive.Rockfish",
                                               "Pacific.Bonito", "Spiny.Dogfish", "Yelloweye.Rockfish")], index = "simpson"),
  Carnivore.DShannons = diversity(Fishinfo2[,c("Barred.Sand.Bass", "Brown.Rockfish", 
                                               "Brown.Smoothhound", "Gray.Smoothhound", "Freckled.Rockfish",
                                               "California.Scorpionfish", "California.Sheephead", "Canary.Rockfish",
                                               "Copper.Rockfish", "Flag.Rockfish", "Gopher.Rockfish", 
                                               "Greenblotched.Rockfish", "Greenstriped.Rockfish", "Greenspotted.Rockfish",
                                               "Honeycomb.Rockfish", "Ocean.Whitefish", "Pink.Rockfish",
                                               "Rosy.Rockfish", "Sanddab.Unidentified", "Southern.Rock.Sole",
                                               "Starry.Rockfish", "Treefish", "Vermilion.Rockfish", 
                                               "Widow.Rockfish", "Yellowtail.Rockfish")], index = "simpson"),
  Planktivore.DShannons = diversity(Fishinfo2[,c("Bank.Rockfish", "Blue.Rockfish", "Halfbanded.Rockfish",
                                                 "Pacific.Mackerel", "Speckled.Rockfish", 
                                                 "Squarespot.Rockfish", "Swordspine.Rockfish")], index = "simpson"),
  Diversity.Shannon = diversity(Fishinfo2[,c(22:79)], index = "shannon"),
  Diversity.Simpson = diversity(Fishinfo2[,c(22:79)], index = "simpson"),
  Diversity.InvSimpson = diversity(Fishinfo2[,c(22:79)], index = "invsimpson"))



#Don't group "Unknown Fish" anywhere. 


write.csv(Fishinfo2, file = "data/All sites data_Tidied.csv")

################################################################10/6/2020















# Other potential CPUE calculations
# CPUE.FishperMin: TotalFish/(FishingTime/60)
# CPUE.Fishper5min: TotalFish/5
# CPUE.FishperValidHook: TotalFish/ValidHooks
# CPUE.for75hooks: CPUE.FishperValidHook*75



# Summarizing by site (Across all years)
# Most are not calculated yet (i.e. Richness, biomass)   11/20/2020

SiteSumm = Fishinfo2 %>% 
  group_by(SiteName) %>% 
  summarise(avg.Abundance = mean(HookswithFish),
            #avg.Richness = mean(Richness),    
            #avg.Biomass = mean(Biomass),
            #avg.CPUEpermin = mean(CPUE.Fishpermin),
            #avg.CPUEper5min = mean(CPUE.Fishper5min),
            #avg.CPUEperhook = mean(CPUE.Fishpervalidhook),
            #avg.CPUEper75hooks = mean(CPUE.Fishfor75hooks),
            avg.Piscivore = mean(Piscivore),
            avg.Planktivore = mean(Planktivore),
            avg.Carnivore = mean(Carnivore),
            avg.Target = mean(Target),
            avg.Nontarget = mean(Nontarget),
            #avg.PiscivoreBio = mean(Piscivore.bio),
            #avg.PlanktivoreBio = mean(Planktivore.bio),
            #avg.CarnivoreBio = mean(Carnivore.bio),
            #avg.TargetBio = mean(Target.bio),
            #avg.NontargetBio = mean(Nontarget.bio),
            avg.PiscivoreCPUE = mean(Piscivore.CPUE),
            avg.PlanktivoreCPUE = mean(Planktivore.CPUE),
            avg.CarnivoreCPUE = mean(Carnivore.CPUE),
            avg.TargetCPUE = mean(Target.CPUE),
            avg.NontargetCPUE = mean(Nontarget.CPUE),
            avg.ShannonsD = mean(Diversity.Shannon),
            avg.SimpsonsD = mean(Diversity.Simpson),
            avg.Invsimpson = mean(Diversity.InvSimpson)
  )



# write.csv(SiteSumm, file = "SiteSumm.csv")



