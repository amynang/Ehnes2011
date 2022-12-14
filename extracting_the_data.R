# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
library(rJava)      
library(tabulizer)  
library(tidyverse)
library(stringr)

# extract data from supplement
ehnes0 = extract_tables("https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fj.1461-0248.2011.01660.x&file=ELE_1660_sm_Meta-scaling-Appendix.pdf",
                        pages = 2:48)
beepr::beep(9)

# tidyup
ehnes0[[1]] = ehnes0[[1]][,-1]
ehnes = as.data.frame(do.call(rbind, ehnes0))
colnames(ehnes) = ehnes[1,]
ehnes = ehnes[-1,]
ehnes[,1] = gsub("[[:digit:]]", "", ehnes[,1])
ehnes[,1] = gsub(" ", "", ehnes[,1])

names(ehnes)

ehnes = ehnes %>% 
  rename(met.rate.Jh = `J/h`,
         mass.mg = `weight [mg]`,
         temp.C = `Temperature  [°C]`) %>% 
  mutate(met.rate.Jh = met.rate.Jh %>% as.numeric(),
         mass.mg = mass.mg %>% as.numeric(),
         temp.C = temp.C %>% as.numeric())

# Warning message:
# Problem while computing `temp.C = temp.C %>% as.numeric()`.
# ℹ NAs introduced by coercion

# these two were the offending cases, for reasons
ehnes["2370",10] = 2.0
ehnes["2371",10] = 2.0


ehnes = ehnes %>% add_column(.before = "Species",
                             Genus = str_split(ehnes$Species, " ", simplify = T)[,1])
# resolving typos in taxonomy
typo = taxize::resolve(unique(ehnes$Genus),
                       db = 'gnr')
#View(typo$gnr)  
change = typo$gnr[str_detect(typo$gnr$matched_name, " ", negate = TRUE) & 
                    typo$gnr$data_source_title == "National Center for Biotechnology Information" &
                    typo$gnr$score < .6, ]

ehnes = ehnes %>% mutate(Genus = case_when(Genus %in% change$user_supplied_name ~ 
                                             change$matched_name[match(ehnes$Genus,
                                                                       change$user_supplied_name)],
                                           TRUE ~ Genus))
ehnes$Genus[ehnes$Genus == "PSeudophonus"] = "Pseudophonus"
write.csv(ehnes, "Ehnes2011.csv", 
          row.names = F)

