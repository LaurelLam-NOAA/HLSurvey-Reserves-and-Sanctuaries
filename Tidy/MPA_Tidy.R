# MPA Data spreadsheet fill   05/01/,2020


rm(list=ls())

library(plyr)
library(devtools)
library(tidyr)
library(dplyr)
library(vegan)



#
#
#
#
# checking email FIXED?!?!

Fishinfo = read.csv("Tidy/2020_All Sites by Year_Fish info_v2.csv")
Site = read.csv("Tidy/2020_All sites_Site Info_v2.csv")

#Species list
#all.sp = levels(Fishinfo$CommonName)

#excluded sp
exclude = c("Gorgonian unident", "California Market Squid")
Fishinfo = Fishinfo[!grepl(paste(exclude, collapse ="|"), Fishinfo$CommonName),]


target.sp = c("Barred Sand Bass", "Brown Rockfish", 
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




#Calculate columns for YearSite_Fishinfo
#AreaDesc2: Same as AreaDesc except combine Conception and San Miguel (conceptionMiguel), and combine Anacapa and Santa Cruz (AnacapaCruz)
#MPAGroup2: Same as MPA group except combine Conception and Richardson Rock (ConceptionRichardson)
#MPA at time of sampling? 1: yes 0:no
#Target: Designate each sp as Target/Nontarget
#Trophic: Designate each sp to its Trophic level


Site2 = Site %>%
  select(SiteName, AreaDesc2:SiteType)

Fishinfo2 = left_join(Fishinfo, Site2, by = "SiteName")




Fishinfo2 = Fishinfo2 %>% mutate(
  MPA.when.sampled = if_else((SiteName == 48 | SiteName == 180 | SiteName == 184 | 
                                SiteName == 228 | SiteName ==229 | SiteName == 413) & SurveyYear>2006, 1,
                             if_else((SiteName == 181| SiteName == 182) & SurveyYear>2007, 1,
                                     if_else(SiteName == 185 & SurveyYear >2008, 1,
                                             if_else(SiteName == 97 & SurveyYear>2009,1,
                                                     if_else((SiteName == 59|SiteName ==62 |SiteName == 152|
                                                                SiteName ==292 |SiteName ==293|SiteName ==379) & SurveyYear>2011, 1,0))))),
  BACI.ControlImpact = if_else(MPA.group3 == "Footprint" & SiteType == "Protected" & SurveyYear>2006, "Protected",
                       if_else(MPA.group3 == "Footprint" & SiteType == "Control" & SurveyYear>2006, "Control",
                              if_else(MPA.group3 == "Richardson Rock" & SiteType == "Protected" & SurveyYear>2008, "Protected",
                              if_else(MPA.group3 == "Richardson Rock" & SiteType == "Control" & SurveyYear>2008, "Control",
                                      if_else(MPA.group3 == "San Clemente" & SiteType == "Control" & SurveyYear>2009, "Control",
                                      if_else(MPA.group3 == "San Clemente" & SiteType == "Protected" & SurveyYear>2009, "Protected",
                                              if_else((MPA.group3 == "Catalina" | MPA.group3 == "Conception" | MPA.group3 == "South Coast") 
                                                      & SiteType == "Protected" & SurveyYear>2011, "Protected",
                                              if_else((MPA.group3 == "Catalina" | MPA.group3 == "Conception" | MPA.group3 == "South Coast") 
                                              & SiteType == "Control" & SurveyYear>2011, "Control",NA_character_)))))))),
  Target = ifelse(CommonName %in% target.sp, "Target",
                  ifelse(CommonName == "Unknown Fish", NA, "Nontarget")),
  Trophic = ifelse(CommonName %in% carnivore.sp, "Carnivore", 
                  ifelse(CommonName %in% piscivore.sp, "Piscivore", 
                         ifelse(CommonName == "Unknown Fish",NA,"Planktivore")))
  )         

#Don't group "Unknown Fish" anywhere. 

write.csv(Fishinfo2, file = "Fishinfo2.csv")

#Summarise biomass by Year, Site, and sp
BiomassYearSite = Fishinfo2 %>% 
  group_by(SiteName, SurveyYear) %>%
  summarise(n=n(),
            Biomass = sum(FishWeight, na.rm = TRUE))

#Check: After Group_by, there should be 475 obs

#Then spread out Target/Nontarget so each row is a Year/Site

BiomassbyTarget = Fishinfo2 %>% 
  group_by(SiteName, SurveyYear, Target) %>%
  summarise(n=n(),
            Biomass = sum(FishWeight, na.rm = TRUE))

BiomassbyTarget2 = BiomassbyTarget %>% 
  select(SiteName:Target, Biomass) %>%                    #Exclude 'n' col
  spread(key = Target, value = Biomass) %>%
  replace_na(list(Target = 0, Nontarget = 0)) %>%       #Turns NAs to 0
  select(SiteName:Target) %>%           #Excludes an extra col "<NA>" that gets added to the end
  rename(Target.bio = Target, Nontarget.bio = Nontarget)    #renaming col so its easier to id when joined




#Same with Trophic
BiomassbyTrophic = Fishinfo2 %>% 
  group_by(SiteName, SurveyYear, Trophic) %>%
  summarise(n=n(),
            Biomass = sum(FishWeight, na.rm = TRUE))


BiomassbyTrophic2 = BiomassbyTrophic %>% 
  select(SiteName:Trophic, Biomass) %>% 
  spread(key = Trophic, value = Biomass) %>% 
  replace_na(list(Carnivore = 0, Piscivore = 0, Planktivore = 0)) %>% 
  select(SiteName:Planktivore) %>% 
  rename(Carnivore.bio = Carnivore, Piscivore.bio = Piscivore, Planktivore.bio = Planktivore)




#Join Biomass datasets together
Biomassdf = join_all(list(BiomassYearSite, BiomassbyTarget2, BiomassbyTrophic2), type ="left", match = "all")



#Adding columns in YearSite







#Calculate columns for YearSite
#AreaDesc2: Same as AreaDesc except combine Conception and San Miguel (conceptionMiguel), and combine Anacapa and Santa Cruz (AnacapaCruz)
#MPAGroup2: Same as MPA group except combine Conception and Richardson Rock (ConceptionRichardson)
#MPAGroup3: MPA grouping by year estblished/area. For BACI Control Impact analysis
#MPA status: Protected or Control
#CPUE.FishperMin: TotalFish/(FishingTime/60)
#CPUE.Fishper5min: TotalFish/5
#CPUE.FishperValidHook: TotalFish/ValidHooks
#CPUE.for75hooks: CPUE.FishperValidHook*75
#Richness calculated BEFORE Imm/Mat Boc, Verm, Gspot joined
#MPA at time of sampling? 1: yes 0:no


#Start with All Sites by Year_CPUE and Species Summary
Yearsite = read.csv("2020_All Sites by Year_CPUE and Species Summary_v2.csv")
Site = read.csv("2020_All sites_Site Info_v2.csv")

tbl_df(Site)
#33 rows total

tbl_df(Yearsite)
#475 rows total


# 1.) Isolate columns from Site to join
# 2.) Join these columns from Site into Yearsite (Yearsite is main df)
#     Joining col = "SiteName"
# 3.) Add in calculated fields (CPUE)
# 4.) Join Imm/Mat verm, boc, gspot
# 5.) Create target/nontarget, trophic level, columns. Calculate Diversity

Site2 = Site %>%
  select(SiteName, AreaDesc2:SiteType, MPA.type)

Yearsite2 = left_join(Yearsite, Site2, by = "SiteName")

Yearsite2 = Yearsite2 %>% mutate(
  CPUE.Fishpermin = TotalFish/(TotalFishingTime.sec/60),
  CPUE.Fishper5min = TotalFish/5,
  CPUE.Fishpervalidhook = TotalFish/ValidHooks,
  CPUE.Fishfor75hooks = CPUE.Fishpervalidhook*75,
  Richness = apply(!is.na(Yearsite2[,13:55]), 1, sum),
  MPA.when.sampled = if_else((SiteName == 48 | SiteName == 180 | SiteName == 184 | 
                                SiteName == 228 | SiteName ==229 | SiteName == 413) & SurveyYear>2006, 1,
                             if_else((SiteName == 181| SiteName == 182) & SurveyYear>2007, 1,
                                     if_else(SiteName == 185 & SurveyYear >2008, 1,
                                             if_else(SiteName == 97 & SurveyYear>2009,1,
                                                     if_else((SiteName == 59|SiteName ==62 |SiteName == 152|
                                                                SiteName ==292 |SiteName ==293|SiteName ==379) & SurveyYear>2011, 1,0))))),
  BACI.ControlImpact = if_else(MPA.group3 == "Footprint" & SiteType == "Protected" & SurveyYear>2006, "Protected",
                               if_else(MPA.group3 == "Footprint" & SiteType == "Control" & SurveyYear>2006, "Control",
                                       if_else(MPA.group3 == "Richardson Rock" & SiteType == "Protected" & SurveyYear>2008, "Protected",
                                               if_else(MPA.group3 == "Richardson Rock" & SiteType == "Control" & SurveyYear>2008, "Control",
                                                       if_else(MPA.group3 == "San Clemente" & SiteType == "Control" & SurveyYear>2009, "Control",
                                                               if_else(MPA.group3 == "San Clemente" & SiteType == "Protected" & SurveyYear>2009, "Protected",
                                                                       if_else((MPA.group3 == "Catalina" | MPA.group3 == "Conception" | MPA.group3 == "South Coast") 
                                                                               & SiteType == "Protected" & SurveyYear>2011, "Protected",
                                                                               if_else((MPA.group3 == "Catalina" | MPA.group3 == "Conception" | MPA.group3 == "South Coast") 
                                                                                       & SiteType == "Control" & SurveyYear>2011, "Control",NA_character_)))))))),
  
  )       
   
               
#Check
check1 = Yearsite2 %>% 
  group_by(SiteType , SiteName) %>% 
  summarise (times.sampled = sum(MPA.when.sampled))
#Looking good


# Read in all imm/mat csvs
#They're all separate since site with no catch are excluded
Imm.Gspot = read.csv("Imm.Gspot.csv")
Mat.Gspot = read.csv("Mat.Gspot.csv")
Imm.Verm = read.csv("Imm.Verm.csv")
Mat.Verm = read.csv("Mat.Verm.csv")
Imm.Boc = read.csv("Imm.Boc.csv")
Mat.Boc = read.csv("Mat.Boc.csv")

#join_all automatically joins by common variables
Yearsite3 = join_all(list(Yearsite2, Mat.Verm, Imm.Verm, Mat.Boc, Imm.Boc, Imm.Gspot, Mat.Gspot), type ="left", match = "all")

#Replace NA with 0 from just the species counts. But don't use imm/mat sp in Diversity indices
Yearsite3[,c(13:55, 69:74)][is.na(Yearsite3[,c(13:55, 69:74)])] = 0


#Create species groups
Yearsite4 = Yearsite3 %>% mutate(
  Target.sp = rowSums(Yearsite3[,c(13:27,29,31,32,36,38:41,43,45:49,51:55)]),
  Nontarget.sp = rowSums(Yearsite3[,c(28,30,33:35,37,42,44,50)]),
  Piscivore = rowSums(Yearsite3[,c(14,16,19,22,24,26,36:38,40,41,54)]),
  Carnivore = rowSums(Yearsite3[,c(17,18,20,21,23,25,27:33,35,39,43:46,49,51:53,55)]),
  Planktivore = rowSums(Yearsite3[,c(13,15,34,42,47,48,50)]),
  Target.CPUE = Target.sp/ValidHooks,
  Nontarget.CPUE = Nontarget.sp/ValidHooks,
  Piscivore.CPUE = Piscivore/ValidHooks,
  Carnivore.CPUE = Carnivore/ValidHooks,
  Planktivore.CPUE = Planktivore/ValidHooks,
  Target.DShannons = diversity(Yearsite3[,c(13:27,29,31,32,36,38:41,43,45:49,51:55)], index = "shannon"),
  Nontarget.DShannons = diversity(Yearsite3[,c(28,30,33:35,37,42,44,50)], index = "shannon"),
  Piscivore.DShannons = diversity(Yearsite3[,c(14,16,19,22,24,26,36:38,40,41,54)], index = "shannon"),
  Carnivore.DShannons = diversity(Yearsite3[,c(17,18,20,21,23,25,27:33,35,39,43:46,49,51:53,55)], index = "shannon"),
  Planktivore.DShannons = diversity(Yearsite3[,c(13,15,34,42,47,48,50)], index = "shannon"),
  Diversity.Shannon = diversity(Yearsite3[,c(13:55)], index = "shannon"),
  Diversity.Simpson = diversity(Yearsite3[,c(13:55)], index = "simpson"),
  Diversity.InvSimpson = diversity(Yearsite3[,c(13:55)], index = "invsimpson")
)


#Join with Biomass data
YearsiteBio = join_all(list(Yearsite4, Biomassdf), type ="left", match = "all")


check3 = YearsiteBio %>% 
  select(TotalFish, n)
#Check if needed
species = Yearsite3[,c(13:55)]
Shannon = as.data.frame(diversity(species, index = "shannon"))
Simpson = as.data.frame(diversity(species, index = "simpson"))
InvSimpson = as.data.frame(diversity(species, index = "invsimpson"))



SiteSumm = YearsiteBio %>% 
  group_by(SiteName) %>% 
  summarise(avg.Abundance = mean(TotalFish),
            avg.Richness = mean(Richness),
            avg.Biomass = mean(Biomass),
            avg.CPUEpermin = mean(CPUE.Fishpermin),
            avg.CPUEper5min = mean(CPUE.Fishper5min),
            avg.CPUEperhook = mean(CPUE.Fishpervalidhook),
            avg.CPUEper75hooks = mean(CPUE.Fishfor75hooks),
            avg.Piscivore = mean(Piscivore),
            avg.Planktivore = mean(Planktivore),
            avg.Carnivore = mean(Carnivore),
            avg.Target = mean(Target.sp),
            avg.Nontarget = mean(Nontarget.sp),
            avg.PiscivoreBio = mean(Piscivore.bio),
            avg.PlanktivoreBio = mean(Planktivore.bio),
            avg.CarnivoreBio = mean(Carnivore.bio),
            avg.TargetBio = mean(Target.bio),
            avg.NontargetBio = mean(Nontarget.bio),
            avg.PiscivoreCPUE = mean(Piscivore.CPUE),
            avg.PlanktivoreCPUE = mean(Planktivore.CPUE),
            avg.CarnivoreCPUE = mean(Carnivore.CPUE),
            avg.TargetCPUE = mean(Target.CPUE),
            avg.NontargetCPUE = mean(Nontarget.CPUE),
            avg.ShannonsD = mean(Diversity.Shannon),
            avg.SimpsonsD = mean(Diversity.Simpson),
            avg.Invsimpson = mean(Diversity.InvSimpson)
            )



write.csv(SiteSumm, file = "SiteSumm.csv")
write.csv(YearsiteBio, file = "YearsiteBio.csv")


