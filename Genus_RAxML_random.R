
#Last updated on Feb 4, 2024

#epang_Random_genus
#Sister species table
getwd()
library(foreach)
#install.packages("phytools")
library(phytools)
library(maps)
library(phangorn)
library(phylotools)
library(ips)
library(dplyr)
library(tidyr)
library(stringr)

### Functions  

# Placing my code in one function extract_Genus (takes one input - RefTree_R_co1_20_1_List or R_80_co1_20_1_List)
extract_Genus <- function(list){
  # use a regex expression to grab the first part of every species name before the underscore character and place in a new list
  # The new list will retain the number of list elements in each sublist that existed previously
  foreach(i=1:length(list)) %do% {
    list[[i]] <- str_extract(list[[i]], "[^_]+")
  }
  
  # Iterate through each list and convert to dataframe format using lapply (very fast function)
  dfList <- lapply(list, function(x) { x <- as.data.frame(x) })
  
  # Need the data.table package for this step, can use rbindlist with idcol = TRUE (this will give an id column corresponding to which list element was used, ex: id=1 for list element 1)
  # Rbindlist is very fast and will combine all the dataframes together quickly
  df <- rbindlist(dfList, idcol=TRUE)
  
  # Edit the column names
  # id column
  colnames(df)[1] <- "id"
  # genus column
  colnames(df)[2] <- "genus"
  
  # remove unneeded vars
  rm(dfList, list)
  
  # return dataframe
  return(df)
}


#read 50 placement fasta files
placement_files <- list.files(path="/home/thanu/Desktop/FishData/New_dataset/RANDOM", pattern="CO1_[0-9]{1,2}_[0-9]{1,2}.fasta", full.names=TRUE, recursive=FALSE)

placementList <- foreach(i=1:length(placement_files)) %do% read.fasta(placement_files[i])

# Now I have a list with each element being its own separate tree, so treeList[[1]] is my first tree 20_1_new, treeList[[2]] is 20_2_new etc.

# Also we can name each list element by substituting out our path from the names of each file

names(placementList) <- gsub("/home/thanu/Desktop/FishData/New_dataset/RANDOM/", "", placement_files)

# This will let us keep track of which list element corresponds to which tree file

names(placementList)

# [1] "CO1_1_1.fasta"   "CO1_1_10.fasta"  "CO1_1_2.fasta"   "CO1_1_3.fasta"   "CO1_1_4.fasta"   "CO1_1_5.fasta"   "CO1_1_6.fasta"   "CO1_1_7.fasta"   "CO1_1_8.fasta"   "CO1_1_9.fasta"  
# [11] "CO1_20_1.fasta"  "CO1_20_10.fasta" "CO1_20_2.fasta"  "CO1_20_3.fasta"  "CO1_20_4.fasta"  "CO1_20_5.fasta"  "CO1_20_6.fasta"  "CO1_20_7.fasta"  "CO1_20_8.fasta"  "CO1_20_9.fasta" 
# [21] "CO1_40_1.fasta"  "CO1_40_10.fasta" "CO1_40_2.fasta"  "CO1_40_3.fasta"  "CO1_40_4.fasta"  "CO1_40_5.fasta"  "CO1_40_6.fasta"  "CO1_40_7.fasta"  "CO1_40_8.fasta"  "CO1_40_9.fasta" 
# [31] "CO1_60_1.fasta"  "CO1_60_10.fasta" "CO1_60_2.fasta"  "CO1_60_3.fasta"  "CO1_60_4.fasta"  "CO1_60_5.fasta"  "CO1_60_6.fasta"  "CO1_60_7.fasta"  "CO1_60_8.fasta"  "CO1_60_9.fasta" 
# [41] "CO1_80_1.fasta"  "CO1_80_10.fasta" "CO1_80_2.fasta"  "CO1_80_3.fasta"  "CO1_80_4.fasta"  "CO1_80_5.fasta"  "CO1_80_6.fasta"  "CO1_80_7.fasta"  "CO1_80_8.fasta"  "CO1_80_9.fasta" 
# we can see the names of the files that correspond to each list element and we can reference any list element by name, for example typing:

placementList$`CO1_20_1.fasta`

#Read 50 tree files
tree_files <- list.files(path="/home/thanu/Desktop/FishData/New_dataset/RANDOM", pattern="^[0-9]{1,2}_[0-9]{1,2}_new", full.names=TRUE, recursive=FALSE)

treeList <- foreach(i=1:length(tree_files)) %do% read.tree(tree_files[i])

# Now I have a list with each element being its own separate tree, so treeList[[1]] is my first tree 20_1_new, treeList[[2]] is 20_2_new etc.

# Also we can name each list element by substituting out our path from the names of each file

names(treeList) <- gsub("/home/thanu/Desktop/FishData/New_dataset/RANDOM", "", tree_files)

# This will let us keep track of which list element corresponds to which tree file

names(treeList)
# [1] "/20_1_new"  "/20_10_new" "/20_2_new"  "/20_3_new"  "/20_4_new"  "/20_5_new"  "/20_6_new"  "/20_7_new"  "/20_8_new"  "/20_9_new" 
# [11] "/40_1_new"  "/40_10_new" "/40_2_new"  "/40_3_new"  "/40_4_new"  "/40_5_new"  "/40_6_new"  "/40_7_new"  "/40_8_new"  "/40_9_new" 
# [21] "/60_1_new"  "/60_10_new" "/60_2_new"  "/60_3_new"  "/60_4_new"  "/60_5_new"  "/60_6_new"  "/60_7_new"  "/60_8_new"  "/60_9_new" 
# [31] "/80_1_new"  "/80_10_new" "/80_2_new"  "/80_3_new"  "/80_4_new"  "/80_5_new"  "/80_6_new"  "/80_7_new"  "/80_8_new"  "/80_9_new" 
# [41] "/99_1_new"  "/99_10_new" "/99_2_new"  "/99_3_new"  "/99_4_new"  "/99_5_new"  "/99_6_new"  "/99_7_new"  "/99_8_new"  "/99_9_new" 

# we can see the names of the files that correspond to each list element and we can reference any list element by name, for example typing:

treeList$`/20_1_new`

# For Robinson-Foulds values, we can use foreach loop to iterate through each tree (There were 40 trees)

species_List <- foreach(i=1:length(placementList)) %do% placementList[[i]]$seq.name
species_List

#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")

#------
##bb 80% tree when placed 20% co1
library(ips)
library(dplyr)
library(tidyr)


#new_code-----------------
#To remove single species in genus groups
Ref_Tree <- as.list(ReferenceTree)
Ref_Tree_Tips <- Ref_Tree$tip.label
class(Ref_Tree_Tips)
Ref_Tree_Tips <- as.data.frame(Ref_Tree_Tips)
df_ReferenceTree<-data.frame(str_extract(Ref_Tree_Tips$Ref_Tree_Tips , "[^_]+"))
colnames(df_ReferenceTree) <- c("genus")
names(df_ReferenceTree)
#Get ununique species by removing single species
genus_more_1<- df_ReferenceTree %>% 
  group_by(genus) %>% 
  filter(n()>=2)
#Get distinct names
Ref_Tree_distinct <- distinct(genus_more_1)#766 obs.

#get the genus name list of co1 placements ()To replace ids with co1 placement genus names

names(placementList$`CO1_20_1.fasta`)
#head(R_co1_20_1$seq.name)
#As I need to get the genus name (1st part of two parts of species name), follow the below
#check placementList order
names(placementList)
# nw <- as.data.frame(str_extract(species_List[[11]],"[^_]+"))
#for all the placement list with genus name
placement_genus_list <- foreach(i=1:length(species_List)) %do% as.data.frame(str_extract(species_List[[i]],"[^_]+"))
#names(placementList) #order
# [1] "CO1_1_1.fasta"   "CO1_1_10.fasta"  "CO1_1_2.fasta"   "CO1_1_3.fasta"   "CO1_1_4.fasta"   "CO1_1_5.fasta"   "CO1_1_6.fasta"   "CO1_1_7.fasta"   "CO1_1_8.fasta"   "CO1_1_9.fasta"  
# [11] "CO1_20_1.fasta"  "CO1_20_10.fasta" "CO1_20_2.fasta"  "CO1_20_3.fasta"  "CO1_20_4.fasta"  "CO1_20_5.fasta"  "CO1_20_6.fasta"  "CO1_20_7.fasta"  "CO1_20_8.fasta"  "CO1_20_9.fasta" 
# [21] "CO1_40_1.fasta"  "CO1_40_10.fasta" "CO1_40_2.fasta"  "CO1_40_3.fasta"  "CO1_40_4.fasta"  "CO1_40_5.fasta"  "CO1_40_6.fasta"  "CO1_40_7.fasta"  "CO1_40_8.fasta"  "CO1_40_9.fasta" 
# [31] "CO1_60_1.fasta"  "CO1_60_10.fasta" "CO1_60_2.fasta"  "CO1_60_3.fasta"  "CO1_60_4.fasta"  "CO1_60_5.fasta"  "CO1_60_6.fasta"  "CO1_60_7.fasta"  "CO1_60_8.fasta"  "CO1_60_9.fasta" 
# [41] "CO1_80_1.fasta"  "CO1_80_10.fasta" "CO1_80_2.fasta"  "CO1_80_3.fasta"  "CO1_80_4.fasta"  "CO1_80_5.fasta"  "CO1_80_6.fasta"  "CO1_80_7.fasta"  "CO1_80_8.fasta"  "CO1_80_9.fasta" 

library(purrr) # using purrr for map function
# map setNames to each dataframe in the list
placement_genus_list_new <- map(placement_genus_list , setNames, nm = "genus")
# # then list2env should work perfectly
# list2env(placement_genus_list_new,envir = .GlobalEnv)

#to get individual dataframes for each replicate
list2env(setNames(placement_genus_list_new, paste0("newdfF", seq_along(placement_genus_list_new))),
         envir=.GlobalEnv)

#Adding ids to the dataframes. id 1 means list element 1 
id <- c(1:45)
co1_1_list <- c(newdfF1,newdfF2,newdfF3,newdfF4,newdfF5,newdfF6,newdfF7,newdfF8,newdfF9,newdfF10)
co1_1_id_list <- foreach(i=1:length(co1_1_list)) %do% data.frame(id,co1_1_list[i])
co1_1_id_list[1]
names(co1_1_id_list[[1]])

id <- c(1:904)
co1_20_list <- c(newdfF11,newdfF12,newdfF13,newdfF14,newdfF15,newdfF16,newdfF17,newdfF18,newdfF19,newdfF20)
co1_20_id_list <- foreach(i=1:length(co1_20_list)) %do% data.frame(id,co1_20_list[i])

id <- c(1:1808)
co1_40_list <- c(newdfF21,newdfF22,newdfF23,newdfF24,newdfF25,newdfF26,newdfF27,newdfF28,newdfF29,newdfF30)
co1_40_id_list <- foreach(i=1:length(co1_40_list)) %do% data.frame(id,co1_40_list[i])
id <- c(1:2712)
co1_60_list <- c(newdfF31,newdfF32,newdfF33,newdfF34,newdfF35,newdfF36,newdfF37,newdfF38,newdfF39,newdfF40)
co1_60_id_list <- foreach(i=1:length(co1_60_list)) %do% data.frame(id,co1_60_list[i])

id <- c(1:3616)
co1_80_list <- c(newdfF41,newdfF42,newdfF43,newdfF44,newdfF45,newdfF46,newdfF47,newdfF48,newdfF49,newdfF50)
co1_80_id_list <- foreach(i=1:length(co1_80_list)) %do% data.frame(id,co1_80_list[i])
# placement_genus_list<- as.data.frame(str_extract(species_List[[i]],"[^_]+"))
# R_co1_20_1_G<-as.data.frame(str_extract(R_co1_20_1$seq.name , "[^_]+"))
#R_co1_20_1_G<-as.data.frame(str_extract(R_co1_20_1 , "[^_]+"))
# class(R_co1_20_1_G)
# colnames(R_co1_20_1_G) <- "genus"
# class(R_co1_20_1_G)

# 
# #Sister species of placement sequences in reference tree
# RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(ReferenceTree,R_co1_20_1_species[i],type="terminal",label=T)
# #Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
# R_80_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(R80_1_tree,R_co1_20_1_species[i],type="terminal",label=T)

#Sister species of placement sequences in reference tree
RefTree_R_co1_20_1_List <- foreach(i=1:length(species_List[[12]])) %do% sister(ReferenceTree,species_List[[12]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(species_List[[12]])) %do% sister(treeList$EPA80_10R_new.nwk,species_List[[12]][i],type="terminal",label=T)
names(placementList) #order
# [1] "CO1_1_1.fasta"   "CO1_1_10.fasta"  "CO1_1_2.fasta"   "CO1_1_3.fasta"   "CO1_1_4.fasta"   "CO1_1_5.fasta"   "CO1_1_6.fasta"   "CO1_1_7.fasta"   "CO1_1_8.fasta"   "CO1_1_9.fasta"  
# [11] "CO1_20_1.fasta"  "CO1_20_10.fasta" "CO1_20_2.fasta"  "CO1_20_3.fasta"  "CO1_20_4.fasta"  "CO1_20_5.fasta"  "CO1_20_6.fasta"  "CO1_20_7.fasta"  "CO1_20_8.fasta"  "CO1_20_9.fasta" 
# [21] "CO1_40_1.fasta"  "CO1_40_10.fasta" "CO1_40_2.fasta"  "CO1_40_3.fasta"  "CO1_40_4.fasta"  "CO1_40_5.fasta"  "CO1_40_6.fasta"  "CO1_40_7.fasta"  "CO1_40_8.fasta"  "CO1_40_9.fasta" 
# [31] "CO1_60_1.fasta"  "CO1_60_10.fasta" "CO1_60_2.fasta"  "CO1_60_3.fasta"  "CO1_60_4.fasta"  "CO1_60_5.fasta"  "CO1_60_6.fasta"  "CO1_60_7.fasta"  "CO1_60_8.fasta"  "CO1_60_9.fasta" 
# [41] "CO1_80_1.fasta"  "CO1_80_10.fasta" "CO1_80_2.fasta"  "CO1_80_3.fasta"  "CO1_80_4.fasta"  "CO1_80_5.fasta"  "CO1_80_6.fasta"  "CO1_80_7.fasta"  "CO1_80_8.fasta"  "CO1_80_9.fasta" 
#Sister species of placement sequences in reference tree#for 80_10 tree
RefTree_R_co1_20_1_List <- foreach(i=1:length(species_List[[12]])) %do% sister(ReferenceTree,species_List[[12]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(species_List[[12]])) %do% sister(treeList$EPA80_10R_new.nwk,species_List[[12]][i],type="terminal",label=T)
#Sister species of placement sequences in reference tree
RefTree_R_co1_20_1_List <- foreach(i=1:length(species_List[[20]])) %do% sister(ReferenceTree,species_List[[20]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(species_List[[20]])) %do% sister(treeList$EPA80_9R_new.nwk,species_List[[20]][i],type="terminal",label=T)
#####

#Load the stringr package, data.tabl, tidyverse & dplyr packages
library(stringr)
library(data.table)
library(tidyverse)
library(dplyr)

# extract_Genus function first
# df_RefTree_R_co1_20_1 <- extract_Genus(RefTree_R_co1_20_1_List)
# df_R_80_co1_20_1 <- extract_Genus(R_80_co1_20_1_List)

# generics::intersect(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )
# generics::setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )
#Function for bb80% (co1 20%) #(For the bb60% have to change only one word in co1_20_id_list to co1_40_id_list)
genus_count <- function(RefTree,placementTree,N){
  df_RefTree <- extract_Genus(RefTree)
  df_placement <- extract_Genus(placementTree)
  #get the rows of placement genus that's not matching with reference tree
  # mismatches <- setdiff(df_RefTree,df_placement)
  nn <- union(df_RefTree ,df_placement)
  inter <- intersect(df_RefTree ,df_placement)
  mismatches <- anti_join(nn,inter)
  #We need the number of genus not rows. So, get the number of placement genus that doesn't match to the placement genus in reference tree
  uniq <- count(unique(mismatches,by="id"))
  #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
  #First I get the genus name to particular the ids 
  mismatch_wt_names <- inner_join(mismatches,co1_20_id_list[[N]],by="id")
  #Get the number of placement genus without matches to the placement genus in reference tree
  unique_mismatches <- unique(mismatch_wt_names,by="genus.y")
  unique_mismatches_count <- count(unique_mismatches)
  #genus.y col in unique_mismatches dataframe has the genus list of mismatches
  #Using that col and set diff function get the monotypic genus
  monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
  monotypic_genus_count <- length(monotypic_genus)
  #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
  match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
  
  mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
  #Let's see whats common in both (mixture set)
  #intersect of id
  mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
  mixtures_count <- length(mixtures)
  #Only in match_monotypic
  All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
  All_match_monotypic_count <- length(All_match_monotypic)
  #Only in mismatch_monotypic
  
  All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
  All_mismatch_monotypic_count <- length(All_mismatch_monotypic)
  results <- data.frame(genus_mismatches=uniq,unique_mismatches_count,monotypic_genus=monotypic_genus_count,mixed_monotypic=mixtures_count,All_match_monotypic=All_match_monotypic_count,All_mismatch_monotypic=All_mismatch_monotypic_count)
  return(results)
}

RefTree_R_co1_20_1_List <- foreach(i=1:length(species_List[[18]])) %do% sister(ReferenceTree,species_List[[18]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(species_List[[18]])) %do% sister(treeList$`/80_7_new`,species_List[[18]][i],type="terminal",label=T)

#For the N (order is,1= 80_1,2=80_10,3=80_2,4=80_3,5=80_4,6=80_5,7=80_6,8=80_7,9=80_8,10=80_9)
monotypic_genus_result <- genus_count(RefTree_R_co1_20_1_List,R_80_co1_20_1_List,8)

# #For 80_2 tree (order is, 80_1,80_10,80_2,80_3,80_4,80_5,80_6,80_7,80_8,80_8,80_9)
# monotypic_genus_result <- genus_count(RefTree_R_co1_20_1_List,R_80_co1_20_1_List,3)
#------------------------
# mismatches <- setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1)
# count(unique_mismatches)
# #Get the number of placement genus without matches to the placement genus in reference tree
# count(unique(mismatches,by="id"))#212
# 
# #In this case since we have 904 co1 placements  in 80% tree the there are 692 matches
# 904-212
# 
# #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
# #First I get the genus name to particular the ids
# mismatch_wt_names <- inner_join(mismatches,co1_20_id_list[[1]],by="id")#580 rows
# 
# #Get the number of placement genus without matches to the placement genus in reference tree
# unique_mismatches <- unique(mismatch_wt_names,by="genus.y")#196
# count(unique_mismatches)
# #R_co1_20_1_G_vec col in unique_mismatches dataframe has the genus list of mismatches
# #Using that col and set diff function get the monotypic genus
# 
# monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
# length(monotypic_genus)
# #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
# match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
# count(match_monotypic)
# mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
# #Let's see whats common in both (mixture set)
# #intersect of id
# mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
# 
# #Only in match_monotypic
# All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
# #Only in mismatch_monotypic
# 
# All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
#--------------
#For bb60% and co1 40%
genus_count <- function(RefTree,placementTree,N){
  df_RefTree <- extract_Genus(RefTree)
  df_placement <- extract_Genus(placementTree)
  #get the rows of placement genus that's not matching with reference tree
  # mismatches <- setdiff(df_RefTree,df_placement)
  nn <- union(df_RefTree ,df_placement)
  inter <- intersect(df_RefTree ,df_placement)
  mismatches <- anti_join(nn,inter)
  #We need the number of genus not rows. So, get the number of placement genus that doesn't match to the placement genus in reference tree
  uniq <- count(unique(mismatches,by="id"))
  #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
  #First I get the genus name to particular the ids 
  mismatch_wt_names <- inner_join(mismatches,co1_40_id_list[[N]],by="id")
  #Get the number of placement genus without matches to the placement genus in reference tree
  unique_mismatches <- unique(mismatch_wt_names,by="genus.y")
  unique_mismatches_count <- count(unique_mismatches)
  #genus.y col in unique_mismatches dataframe has the genus list of mismatches
  #Using that col and set diff function get the monotypic genus
  monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
  monotypic_genus_count <- length(monotypic_genus)
  #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
  match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
  
  mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
  #Let's see whats common in both (mixture set)
  #intersect of id
  mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
  mixtures_count <- length(mixtures)
  #Only in match_monotypic
  All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
  All_match_monotypic_count <- length(All_match_monotypic)
  #Only in mismatch_monotypic
  
  All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
  All_mismatch_monotypic_count <- length(All_mismatch_monotypic)
  results <- data.frame(genus_mismatches=uniq,unique_mismatches_count,monotypic_genus=monotypic_genus_count,mixed_monotypic=mixtures_count,All_match_monotypic=All_match_monotypic_count,All_mismatch_monotypic=All_mismatch_monotypic_count)
  return(results)
}
#Sister species of placement sequences in reference tree#for bb60%
RefTree_R_co1_40_1_List <- foreach(i=1:length(species_List[[28]])) %do% sister(ReferenceTree,species_List[[28]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_60_co1_40_1_List <- foreach(i=1:length(species_List[[28]])) %do% sister(treeList$`/60_7_new`,species_List[[28]][i],type="terminal",label=T)

#Call the function for bb60% and co1 40%
#For the N (order is,1= 60_1,2=60_10,3=60_2,4=60_3,5=60_4,6=60_5,7=60_6,8=60_7,9=60_8,10=60_9)
monotypic_genus_result <- genus_count(RefTree_R_co1_40_1_List,R_60_co1_40_1_List,8)

#for bb40% and co1 60%
genus_count <- function(RefTree,placementTree,N){
  df_RefTree <- extract_Genus(RefTree)
  df_placement <- extract_Genus(placementTree)
  #get the rows of placement genus that's not matching with reference tree
  nn <- union(df_RefTree ,df_placement)
  inter <- intersect(df_RefTree ,df_placement)
  mismatches <- anti_join(nn,inter)
  # mismatches <- setdiff(df_RefTree,df_placement)
  #We need the number of genus not rows. So, get the number of placement genus that doesn't match to the placement genus in reference tree
  uniq <- count(unique(mismatches,by="id"))
  #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
  #First I get the genus name to particular the ids 
  mismatch_wt_names <- inner_join(mismatches,co1_60_id_list[[N]],by="id")
  #Get the number of placement genus without matches to the placement genus in reference tree
  unique_mismatches <- unique(mismatch_wt_names,by="genus.y")
  unique_mismatches_count <- count(unique_mismatches)
  #genus.y col in unique_mismatches dataframe has the genus list of mismatches
  #Using that col and set diff function get the monotypic genus
  monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
  monotypic_genus_count <- length(monotypic_genus)
  #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
  match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
  
  mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
  #Let's see whats common in both (mixture set)
  #intersect of id
  mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
  mixtures_count <- length(mixtures)
  #Only in match_monotypic
  All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
  All_match_monotypic_count <- length(All_match_monotypic)
  #Only in mismatch_monotypic
  
  All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
  All_mismatch_monotypic_count <- length(All_mismatch_monotypic)
  results <- data.frame(genus_mismatches=uniq,unique_mismatches_count,monotypic_genus=monotypic_genus_count,mixed_monotypic=mixtures_count,All_match_monotypic=All_match_monotypic_count,All_mismatch_monotypic=All_mismatch_monotypic_count)
  return(results)
}
#Sister species of placement sequences in reference tree#for bb40%
RefTree_R_co1_60_1_List <- foreach(i=1:length(species_List[[35]])) %do% sister(ReferenceTree,species_List[[35]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_40_co1_60_1_List <- foreach(i=1:length(species_List[[35]])) %do% sister(treeList$`/40_4_new`,species_List[[35]][i],type="terminal",label=T)

#Call the function for bb40% and co1 60%
#For the N (order is,1= 40_1,2=40_10,3=40_2,4=40_3,5=40_4,6=40_5,7=40_6,8=40_7,9=40_8,10=40_9)
monotypic_genus_result <- genus_count(RefTree_R_co1_60_1_List,R_40_co1_60_1_List,5)

#for bb20% and co1 80%
genus_count <- function(RefTree,placementTree,N){
  df_RefTree <- extract_Genus(RefTree)
  df_placement <- extract_Genus(placementTree)
  #get the rows of placement genus that's not matching with reference tree
  nn <- union(df_RefTree ,df_placement)
  inter <- intersect(df_RefTree ,df_placement)
  mismatches <- anti_join(nn,inter)
  # mismatches <- setdiff(df_RefTree,df_placement)
  #We need the number of genus not rows. So, get the number of placement genus that doesn't match to the placement genus in reference tree
  uniq <- count(unique(mismatches,by="id"))
  #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
  #First I get the genus name to particular the ids 
  mismatch_wt_names <- inner_join(mismatches,co1_80_id_list[[N]],by="id")
  #Get the number of placement genus without matches to the placement genus in reference tree
  unique_mismatches <- unique(mismatch_wt_names,by="genus.y")
  unique_mismatches_count <- count(unique_mismatches)
  #genus.y col in unique_mismatches dataframe has the genus list of mismatches
  #Using that col and set diff function get the monotypic genus
  monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
  monotypic_genus_count <- length(monotypic_genus)
  #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
  match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
  
  mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
  #Let's see whats common in both (mixture set)
  #intersect of id
  mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
  mixtures_count <- length(mixtures)
  #Only in match_monotypic
  All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
  All_match_monotypic_count <- length(All_match_monotypic)
  #Only in mismatch_monotypic
  
  All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
  All_mismatch_monotypic_count <- length(All_mismatch_monotypic)
  results <- data.frame(genus_mismatches=uniq,unique_mismatches_count,monotypic_genus=monotypic_genus_count,mixed_monotypic=mixtures_count,All_match_monotypic=All_match_monotypic_count,All_mismatch_monotypic=All_mismatch_monotypic_count)
  return(results)
}
#Sister species of placement sequences in reference tree#for bb20%
RefTree_R_co1_80_1_List <- foreach(i=1:length(species_List[[48]])) %do% sister(ReferenceTree,species_List[[48]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_20_co1_80_1_List <- foreach(i=1:length(species_List[[48]])) %do% sister(treeList$`/20_7_new`,species_List[[48]][i],type="terminal",label=T)

#Call the function for bb40% and co1 60%
#For the N (order is,1= 20_1,2=20_10,3=20_2,4=20_3,5=20_4,6=20_5,7=20_6,8=20_7,9=20_8,10=20_9)
monotypic_genus_result <- genus_count(RefTree_R_co1_80_1_List,R_20_co1_80_1_List,8)

##for bb99% and co1 1%
genus_count <- function(RefTree,placementTree,N){
  df_RefTree <- extract_Genus(RefTree)
  df_placement <- extract_Genus(placementTree)
  #get the rows of placement genus that's not matching with reference tree
  nn <- union(df_RefTree ,df_placement)
  inter <- intersect(df_RefTree ,df_placement)
  mismatches <- anti_join(nn,inter)
  # mismatches <- setdiff(df_RefTree,df_placement)
  #We need the number of genus not rows. So, get the number of placement genus that doesn't match to the placement genus in reference tree
  uniq <- count(unique(mismatches,by="id"))
  #for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
  #First I get the genus name to particular the ids 
  mismatch_wt_names <- inner_join(mismatches,co1_1_id_list[[N]],by="id")
  #Get the number of placement genus without matches to the placement genus in reference tree
  unique_mismatches <- unique(mismatch_wt_names,by="genus.y")
  unique_mismatches_count <- count(unique_mismatches)
  #genus.y col in unique_mismatches dataframe has the genus list of mismatches
  #Using that col and set diff function get the monotypic genus
  monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196
  monotypic_genus_count <- length(monotypic_genus)
  #Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
  match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61
  
  mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
  #Let's see whats common in both (mixture set)
  #intersect of id
  mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10
  mixtures_count <- length(mixtures)
  #Only in match_monotypic
  All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
  All_match_monotypic_count <- length(All_match_monotypic)
  #Only in mismatch_monotypic
  
  All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144
  All_mismatch_monotypic_count <- length(All_mismatch_monotypic)
  results <- data.frame(genus_mismatches=uniq,unique_mismatches_count,monotypic_genus=monotypic_genus_count,mixed_monotypic=mixtures_count,All_match_monotypic=All_match_monotypic_count,All_mismatch_monotypic=All_mismatch_monotypic_count)
  return(results)
}
#Sister species of placement sequences in reference tree#for bb99%
RefTree_R_co1_1_1_List <- foreach(i=1:length(species_List[[8]])) %do% sister(ReferenceTree,species_List[[8]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_99_co1_1_1_List <- foreach(i=1:length(species_List[[8]])) %do% sister(treeList$`/99_7_new`,species_List[[8]][i],type="terminal",label=T)

#Call the function for bb99% and co1 1%
#For the N (order is,1= 99_1,2=99_10,3=99_2,4=99_3,5=99_4,6=99_5,7=99_6,8=99_7,9=99_8,10=99_9)
monotypic_genus_result <- genus_count(RefTree_R_co1_1_1_List,R_99_co1_1_1_List,8)
