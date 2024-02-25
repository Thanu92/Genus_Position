
#Last updated on Jan30, 2024

#epa_RAndom_genus
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
tree_files <- list.files(path="/home/thanu/Desktop/FishData/New_dataset/RANDOM", pattern="EPA[0-9]{1,2}_[0-9]{1,2}R_new.nwk", full.names=TRUE, recursive=FALSE)

treeList <- foreach(i=1:length(tree_files)) %do% read.tree(tree_files[i])

# Now I have a list with each element being its own separate tree, so treeList[[1]] is my first tree 20_1_new, treeList[[2]] is 20_2_new etc.

# Also we can name each list element by substituting out our path from the names of each file

names(treeList) <- gsub("/home/thanu/Desktop/FishData/New_dataset/RANDOM/", "", tree_files)

# This will let us keep track of which list element corresponds to which tree file

names(treeList)
# [1] "EPA20_10R_new.nwk" "EPA20_1R_new.nwk"  "EPA20_2R_new.nwk"  "EPA20_3R_new.nwk"  "EPA20_4R_new.nwk"  "EPA20_5R_new.nwk"  "EPA20_6R_new.nwk"  "EPA20_7R_new.nwk"  "EPA20_8R_new.nwk"  "EPA20_9R_new.nwk" 
# [11] "EPA40_10R_new.nwk" "EPA40_1R_new.nwk"  "EPA40_2R_new.nwk"  "EPA40_3R_new.nwk"  "EPA40_4R_new.nwk"  "EPA40_5R_new.nwk"  "EPA40_6R_new.nwk"  "EPA40_7R_new.nwk"  "EPA40_8R_new.nwk"  "EPA40_9R_new.nwk" 
# [21] "EPA60_10R_new.nwk" "EPA60_1R_new.nwk"  "EPA60_2R_new.nwk"  "EPA60_3R_new.nwk"  "EPA60_4R_new.nwk"  "EPA60_5R_new.nwk"  "EPA60_6R_new.nwk"  "EPA60_7R_new.nwk"  "EPA60_8R_new.nwk"  "EPA60_9R_new.nwk" 
# [31] "EPA80_10R_new.nwk" "EPA80_1R_new.nwk"  "EPA80_2R_new.nwk"  "EPA80_3R_new.nwk"  "EPA80_4R_new.nwk"  "EPA80_5R_new.nwk"  "EPA80_6R_new.nwk"  "EPA80_7R_new.nwk"  "EPA80_8R_new.nwk"  "EPA80_9R_new.nwk" 
# [41] "EPA99_10R_new.nwk" "EPA99_1R_new.nwk"  "EPA99_2R_new.nwk"  "EPA99_3R_new.nwk"  "EPA99_4R_new.nwk"  "EPA99_5R_new.nwk"  "EPA99_6R_new.nwk"  "EPA99_7R_new.nwk"  "EPA99_8R_new.nwk"  "EPA99_9R_new.nwk" 


# we can see the names of the files that correspond to each list element and we can reference any list element by name, for example typing:

treeList$`EPA80_1R_new.nwk`

# For Robinson-Foulds values, we can use foreach loop to iterate through each tree (There were 40 trees)

species_List <- foreach(i=1:length(placementList)) %do% placementList[[i]]$seq.name
species_List

# #Read fasta files of placed co1 on the bb tree
# R_co1_20_1=read.fasta(file = "CO1_20_1.fasta")
# R_co1_20_2=read.fasta(file = "CO1_20_2.fasta")

#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")

# R20_1_tree<-read.tree("EPA20_1R_new.nwk")
# R20_2_tree<-read.tree("EPA20_2R_new.nwk")
# R80_2_tree<-read.tree("EPA80_2R_new.nwk")
# R80_1_tree<-read.tree("EPA80_1R_new.nwk")

#------
#I need the seq.names for downstream analysis. Hence, assign for variables 

# R_co1_20_1_species <- R_co1_20_1$seq.name
# class(R_co1_20_1_species)
# R_co1_20_2_species <- R_co1_20_2$seq.name

getwd()
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
id <- c(1:904)
co1_20_list <- c(newdfF11,newdfF12,newdfF13,newdfF14,newdfF15,newdfF16,newdfF17,newdfF18,newdfF19,newdfF20)
co1_20_id_list <- foreach(i=1:length(co1_20_list)) %do% data.frame(id,co1_20_list[i])



id <- c(1:45)
co1_1_list <- c(newdfF1,newdfF2,newdfF3,newdfF4,newdfF5,newdfF6,newdfF7,newdfF8,newdfF9,newdfF10)
co1_1_id_list <- foreach(i=1:length(co1_1_list)) %do% data.frame(id,co1_1_list[i])
co1_1_id_list[1]
names(co1_1_id_list[[1]])
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
RefTree_R_co1_20_1_List <- foreach(i=1:length(species_List[[11]])) %do% sister(ReferenceTree,species_List[[11]][i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(species_List[[11]])) %do% sister(treeList$EPA80_1R_new.nwk,species_List[[11]][i],type="terminal",label=T)

#The following function I made is not working
# sister <- function(x,y){
#   treeList <- foreach(i=1:length(species_List[[x]])) %do% sister(y,species_List[[x]][i],type="terminal",label=T)
#   return(treeList)
# }

#####

# you will need the stringr package, data.tabl, tidyverse & dplyr packages
library(stringr)
library(data.table)
library(tidyverse)
library(dplyr)

# extract_Genus function first
df_RefTree_R_co1_20_1 <- extract_Genus(RefTree_R_co1_20_1_List)
df_R_80_co1_20_1 <- extract_Genus(R_80_co1_20_1_List)

# generics::intersect(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )
# generics::setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )

mismatches <- setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1)
#Get the number of placement genus without matches to the placement genus in reference tree
unique(mismatches,by="id")#212

#In this case since we have 904 co1 placements  in 80% tree the there are 692 matches
904-212

#for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
#First I convert the ids to genus name
# class(mismatches)
# dim(mismatches)
# class(placement_Genus)
# dim(placement_Genus)
mismatch_wt_names <- inner_join(mismatches,co1_20_id_list[[1]],by="id")#580 rows

#Get the number of placement genus without matches to the placement genus in reference tree
unique_mismatches <- unique(mismatch_wt_names,by="genus.y")#196

#R_co1_20_1_G_vec col in unique_mismatches dataframe has the genus list of mismatches
#Using that col and set diff function get the monotypic genus

monotypic_genus <- setdiff(unique_mismatches$genus.y,Ref_Tree_distinct)#196

#Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x==mismatch_wt_names$genus.y, ]#61

mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus.x!=mismatch_wt_names$genus.y, ]#519
#Let's see whats common in both (mixture set)
#intersect of id
mixtures <- intersect(match_monotypic$genus.y,mismatch_monotypic$genus.y)#10

#Only in match_monotypic
All_match_monotypic <- setdiff(match_monotypic$genus.y,mismatch_monotypic$genus.y)#42
#Only in mismatch_monotypic

All_mismatch_monotypic <- setdiff(mismatch_monotypic$genus.y,match_monotypic$genus.y)#144

#------------
#get the genus name list of co1 placements ()To replace ids with co1 placement genus names
names(R_co1_20_2)

#head(R_co1_20_1$seq.name)
#As I need to get the genus name (1st part of two parts of species name), follow the below
R_co1_20_2_G<-as.data.frame(str_extract(R_co1_20_2$seq.name , "[^_]+"))
#R_co1_20_1_G<-as.data.frame(str_extract(R_co1_20_1 , "[^_]+"))
class(R_co1_20_2_G)
colnames(R_co1_20_2_G) <- "genus"
class(R_co1_20_2_G)

#I want to use the genus name column in co1 placements in the sister species dataframe column names (df_RefTree_R_co1_20_1_new, df_R_80_co1_20_1_new).
#Coz all the sister species in each column are the sis species of given co1 sequence genus/ species. So can rename column names with co1 placement genus name
R_co1_20_2_G_vec <- as.vector(R_co1_20_2_G$genus)
#create a dataframe
id <- c(1:904)
placement_Genus <- data.frame(id,R_co1_20_2_G_vec)
names(placement_Genus)
#Sister species of placement sequences in reference tree
RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(ReferenceTree,R_co1_20_2_species[i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(R80_2_tree,R_co1_20_2_species[i],type="terminal",label=T)

#####  

# We need the stringr package, data.tabl, tidyverse & dplyr packages
library(stringr)
library(data.table)
library(tidyverse)
library(dplyr)

# extract_Genus function first
df_RefTree_R_co1_20_2 <- extract_Genus(RefTree_R_co1_20_2_List)
df_R_80_co1_20_2 <- extract_Genus(R_80_co1_20_2_List)

# generics::intersect(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )
# generics::setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1 )
#Get the different rows of two dataframes (Here differences should be based on reference tree)
mismatches <- setdiff(df_RefTree_R_co1_20_2,df_R_80_co1_20_2)#3771 rows
#Get the number of placement genus without matches to the placement genus in reference tree
unique(mismatches,by="id")#211

#In this case since we have 904 co1 placements  in 80% tree the there are 693 matches
904-211

#for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
#First I convert the ids to genus name
class(mismatches)
dim(mismatches)
class(placement_Genus)
dim(placement_Genus)
mismatch_wt_names <- inner_join(mismatches,placement_Genus,by="id")#3771 rows

#Get the number of placement genus without matches to the placement genus in reference tree
unique_mismatches <- unique(mismatch_wt_names,by="R_co1_20_2_G_vec")#201

#R_co1_20_2_G_vec col in unique_mismatches dataframe has the genus list of mismatches
#Using that col and set diff function get the monotypic genus
monotypic_genus <- setdiff(unique_mismatches$R_co1_20_2_G_vec,Ref_Tree_distinct)#201

#Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus==mismatch_wt_names$R_co1_20_2_G_vec, ]#55

mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus!=mismatch_wt_names$R_co1_20_2_G_vec, ]#3716

#Let's see what's common in both (mixture set)
#intersect of id
mixtures <- intersect(match_monotypic$R_co1_20_2_G_vec,mismatch_monotypic$R_co1_20_2_G_vec)#8

#Only in match_monotypic
All_match_monotypic <- setdiff(match_monotypic$R_co1_20_2_G_vec,mismatch_monotypic$R_co1_20_2_G_vec)#40

#Only in mismatch_monotypic
All_mismatch_monotypic <- setdiff(mismatch_monotypic$R_co1_20_2_G_vec,match_monotypic$R_co1_20_2_G_vec)#153
