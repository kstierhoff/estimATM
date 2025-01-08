salmon <- c(161931,161974,161975,161976,161977,161979,161980,161989) #trouts&salmon,Pacific salmon unid,pink,chum,coho,sockeye,Chinook,steelhead
eulachon <- 162051

# catch form reporting for all protected species
pro.sp.catch <- catch.all %>%
  filter(species %in% c(salmon, eulachon)) %>%
  left_join(spp.codes) %>% 
  left_join(select(haul.all, haul, orderOcc, netInWaterTime, lat = startLatDecimal, long = startLongDecimal,
                   surfaceTempC, salinityPPM)) %>%
  select(cruise,ship,haul,collection,species,commonName,netInWaterTime,lat,long,remainingSubSampleWtkg,subSampleWtkg,subSampleCount,totalNum,totalWeight,isWtEstimated,hasLF,notes,netSampleType)

write.xlsx(pro.sp.catch, "Cruise_DB/2407RL/2407RL_ProtectedSpeciesCatch_to_report_Compliance.xlsx")

# specific length and weight data for salmon species from specimen table
salmon.specimen <- lengths.all  %>%
  filter(species %in% c(salmon,eulachon)) %>%
  left_join(spp.codes) %>% 
  left_join(select(haul.all, haul, orderOcc, netInWaterTime, equilibriumTime, lat = startLatDecimal, long = startLongDecimal,
                   surfaceTempC, salinityPPM)) %>%
  select(cruise,ship,haul,collection,orderOcc,netInWaterTime,equilibriumTime,lat,long,surfaceTempC,salinityPPM,,species,scientificName,commonName,specimenNumber,individual_ID,forkLength_mm,weightg,notes,wasFrozen,netSampleType,isAlive,adiposeCondition,hasDNAfinClip,hasTag)

write.xlsx(salmon.specimen, "Cruise_DB/2407RL/2407RL_SalmonSpecimens_to_report_Compliance.xlsx")
