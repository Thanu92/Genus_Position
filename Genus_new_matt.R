
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

### Function from matt's previous code (it's easy to get the dataframes with ids)

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

#Read fasta files of placed co1 on the bb tree (I have 50 trees in this set, but I will attach two fasta files and trees for you)
#Are there any easy way to read all the trees and fasta files at once? If so, please add that function as well.Thank you!
#Can i use foreach? But it returns list right? here read.fasta gave a dataframe for me. So I did downstream analysis according to that
#Feel free to use any easy function as long as I get results at the end, similar to the results of this code
R_co1_20_1=read.fasta(file = "CO1_20_1.fasta")
R_co1_20_2=read.fasta(file = "CO1_20_2.fasta")
# R_co1_20_3=read.fasta(file = "CO1_20_3.fasta")
# R_co1_20_4=read.fasta(file = "CO1_20_4.fasta")
# R_co1_20_5=read.fasta(file = "CO1_20_5.fasta")
# R_co1_20_6=read.fasta(file = "CO1_20_6.fasta")
# R_co1_20_7=read.fasta(file = "CO1_20_7.fasta")
# R_co1_20_8=read.fasta(file = "CO1_20_8.fasta")
# R_co1_20_9=read.fasta(file = "CO1_20_9.fasta")
# R_co1_20_10=read.fasta(file = "CO1_20_10.fasta")
# R_co1_40_1=read.fasta(file = "CO1_40_1.fasta")
# R_co1_40_2=read.fasta(file = "CO1_40_2.fasta")
# R_co1_40_3=read.fasta(file = "CO1_40_3.fasta")
# R_co1_40_4=read.fasta(file = "CO1_40_4.fasta")
# R_co1_40_5=read.fasta(file = "CO1_40_5.fasta")
# R_co1_40_6=read.fasta(file = "CO1_40_6.fasta")
# R_co1_40_7=read.fasta(file = "CO1_40_7.fasta")
# R_co1_40_8=read.fasta(file = "CO1_40_8.fasta")
# R_co1_40_9=read.fasta(file = "CO1_40_9.fasta")
# R_co1_40_10=read.fasta(file = "CO1_40_10.fasta")
# R_co1_60_1=read.fasta(file = "CO1_60_1.fasta")
# R_co1_60_2=read.fasta(file = "CO1_60_2.fasta")
# R_co1_60_3=read.fasta(file = "CO1_60_3.fasta")
# R_co1_60_4=read.fasta(file = "CO1_60_4.fasta")
# R_co1_60_5=read.fasta(file = "CO1_60_5.fasta")
# R_co1_60_6=read.fasta(file = "CO1_60_6.fasta")
# R_co1_60_7=read.fasta(file = "CO1_60_7.fasta")
# R_co1_60_8=read.fasta(file = "CO1_60_8.fasta")
# R_co1_60_9=read.fasta(file = "CO1_60_9.fasta")
# R_co1_60_10=read.fasta(file = "CO1_60_10.fasta")
# R_co1_80_1=read.fasta(file = "CO1_80_1.fasta")
# R_co1_80_2=read.fasta(file = "CO1_80_2.fasta")
# R_co1_80_3=read.fasta(file = "CO1_80_3.fasta")
# R_co1_80_4=read.fasta(file = "CO1_80_4.fasta")
# R_co1_80_5=read.fasta(file = "CO1_80_5.fasta")
# R_co1_80_6=read.fasta(file = "CO1_80_6.fasta")
# R_co1_80_7=read.fasta(file = "CO1_80_7.fasta")
# R_co1_80_8=read.fasta(file = "CO1_80_8.fasta")
# R_co1_80_9=read.fasta(file = "CO1_80_9.fasta")
# R_co1_80_10=read.fasta(file = "CO1_80_10.fasta")
# R_co1_1_1=read.fasta(file = "CO1_1_1.fasta")
# R_co1_1_2=read.fasta(file = "CO1_1_2.fasta")
# R_co1_1_3=read.fasta(file = "CO1_1_3.fasta")
# R_co1_1_4=read.fasta(file = "CO1_1_4.fasta")
# R_co1_1_5=read.fasta(file = "CO1_1_5.fasta")
# R_co1_1_6=read.fasta(file = "CO1_1_6.fasta")
# R_co1_1_7=read.fasta(file = "CO1_1_7.fasta")
# R_co1_1_8=read.fasta(file = "CO1_1_8.fasta")
# R_co1_1_9=read.fasta(file = "CO1_1_9.fasta")
# R_co1_1_10=read.fasta(file = "CO1_1_10.fasta")



#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")
#Here, the same as previous comments. I would like to know any easy function to read all 50 at once. 
R80_1_tree<-read.tree("EPA80_1R_new.nwk")
R80_2_tree<-read.tree("EPA80_2R_new.nwk")
# R20_1_tree<-read.tree("EPA20_1R_new.nwk")
# R20_2_tree<-read.tree("EPA20_2R_new.nwk")
# R20_3_tree<-read.tree("EPA20_3R_new.nwk")
# R20_4_tree<-read.tree("EPA20_4R_new.nwk")
# R20_5_tree<-read.tree("EPA20_5R_new.nwk")
# R20_6_tree<-read.tree("EPA20_6R_new.nwk")
# R20_7_tree<-read.tree("EPA20_7R_new.nwk")
# R20_8_tree<-read.tree("EPA20_8R_new.nwk")
# R20_9_tree<-read.tree("EPA20_9R_new.nwk")
# R20_10_tree<-read.tree("EPA20_10R_new.nwk")
# R40_1_tree<-read.tree("EPA40_1R_new.nwk")
# R40_2_tree<-read.tree("EPA40_2R_new.nwk")
# R40_3_tree<-read.tree("EPA40_3R_new.nwk")
# R40_4_tree<-read.tree("EPA40_4R_new.nwk")
# R40_5_tree<-read.tree("EPA40_5R_new.nwk")
# R40_6_tree<-read.tree("EPA40_6R_new.nwk")
# R40_7_tree<-read.tree("EPA40_7R_new.nwk")
# R40_8_tree<-read.tree("EPA40_8R_new.nwk")
# R40_9_tree<-read.tree("EPA40_9R_new.nwk")
# R40_10_tree<-read.tree("EPA40_10R_new.nwk")
# R60_1_tree<-read.tree("EPA60_1R_new.nwk")
# R60_2_tree<-read.tree("EPA60_2R_new.nwk")
# R60_3_tree<-read.tree("EPA60_3R_new.nwk")
# R60_4_tree<-read.tree("EPA60_4R_new.nwk")
# R60_5_tree<-read.tree("EPA60_5R_new.nwk")
# R60_6_tree<-read.tree("EPA60_6R_new.nwk")
# R60_7_tree<-read.tree("EPA60_7R_new.nwk")
# R60_8_tree<-read.tree("EPA60_8R_new.nwk")
# R60_9_tree<-read.tree("EPA60_9R_new.nwk")
# R60_10_tree<-read.tree("EPA60_10R_new.nwk")
# # R60_1_tree<-read.tree("epa80_1S.nwk")
# R80_2_tree<-read.tree("EPA80_2R_new.nwk")
# R80_3_tree<-read.tree("EPA80_3R_new.nwk")
# R80_4_tree<-read.tree("EPA80_4R_new.nwk")
# R80_5_tree<-read.tree("EPA80_5R_new.nwk")
# R80_6_tree<-read.tree("EPA80_6R_new.nwk")
# R80_7_tree<-read.tree("EPA80_7R_new.nwk")
# R80_8_tree<-read.tree("EPA80_8R_new.nwk")
# R80_9_tree<-read.tree("EPA80_9R_new.nwk")
# R80_10_tree<-read.tree("EPA80_10R_new.nwk")
# R80_1_tree<-read.tree("EPA80_1R_new.nwk")
# class(R80_1_tree)
# R99_1_tree<-read.tree("EPA99_1R_new.nwk")
# R99_2_tree<-read.tree("EPA99_2R_new.nwk")
# R99_3_tree<-read.tree("EPA99_3R_new.nwk")
# R99_4_tree<-read.tree("EPA99_4R_new.nwk")
# R99_5_tree<-read.tree("EPA99_5R_new.nwk")
# R99_6_tree<-read.tree("EPA99_6R_new.nwk")
# R99_7_tree<-read.tree("EPA99_7R_new.nwk")
# R99_8_tree<-read.tree("EPA99_8R_new.nwk")
# R99_9_tree<-read.tree("EPA99_9R_new.nwk")
# R99_10_tree<-read.tree("EPA99_10R_new.nwk")
#------
#I need the seq.names for downstream analysis. Here, I got dataframes from fasta files. So, Iused this method. 
#Again would like to know a simple way to run all 50 lines
R_co1_20_1_species <- R_co1_20_1$seq.name
class(R_co1_20_1_species)
R_co1_20_2_species <- R_co1_20_2$seq.name
# R_co1_20_3_species <- R_co1_20_3$seq.name
# R_co1_20_4_species <- R_co1_20_4$seq.name
# R_co1_20_5_species <- R_co1_20_5$seq.name
# R_co1_20_6_species <- R_co1_20_6$seq.name
# R_co1_20_7_species <- R_co1_20_7$seq.name
# R_co1_20_8_species <- R_co1_20_8$seq.name
# R_co1_20_9_species <- R_co1_20_9$seq.name
# R_co1_20_10_species <- R_co1_20_10$seq.name
# R_co1_40_1_species <- R_co1_40_1$seq.name
# R_co1_40_2_species <- R_co1_40_2$seq.name
# R_co1_40_3_species <- R_co1_40_3$seq.name
# R_co1_40_4_species <- R_co1_40_4$seq.name
# R_co1_40_5_species <- R_co1_40_5$seq.name
# R_co1_40_6_species <- R_co1_40_6$seq.name
# R_co1_40_7_species <- R_co1_40_7$seq.name
# R_co1_40_8_species <- R_co1_40_8$seq.name
# R_co1_40_9_species <- R_co1_40_9$seq.name
# R_co1_40_10_species <- R_co1_40_10$seq.name
# R_co1_60_1_species <- R_co1_60_1$seq.name
# R_co1_60_2_species <- R_co1_60_2$seq.name
# R_co1_60_3_species <- R_co1_60_3$seq.name
# R_co1_60_4_species <- R_co1_60_4$seq.name
# R_co1_60_5_species <- R_co1_60_5$seq.name
# R_co1_60_6_species <- R_co1_60_6$seq.name
# R_co1_60_7_species <- R_co1_60_7$seq.name
# R_co1_60_8_species <- R_co1_60_8$seq.name
# R_co1_60_9_species <- R_co1_60_9$seq.name
# R_co1_60_10_species <- R_co1_60_10$seq.name
# R_co1_80_1_species <- R_co1_80_1$seq.name
# R_co1_80_2_species <- R_co1_80_2$seq.name
# R_co1_80_3_species <- R_co1_80_3$seq.name
# R_co1_80_4_species <- R_co1_80_4$seq.name
# R_co1_80_5_species <- R_co1_80_5$seq.name
# R_co1_80_6_species <- R_co1_80_6$seq.name
# R_co1_80_7_species <- R_co1_80_7$seq.name
# R_co1_80_8_species <- R_co1_80_8$seq.name
# R_co1_80_9_species <- R_co1_80_9$seq.name
# R_co1_80_10_species <- R_co1_80_10$seq.name
# R_co1_1_1_species <- R_co1_1_1$seq.name
# R_co1_1_2_species <- R_co1_1_2$seq.name
# R_co1_1_3_species <- R_co1_1_3$seq.name
# R_co1_1_4_species <- R_co1_1_4$seq.name
# R_co1_1_5_species <- R_co1_1_5$seq.name
# R_co1_1_6_species <- R_co1_1_6$seq.name
# R_co1_1_7_species <- R_co1_1_7$seq.name
# R_co1_1_8_species <- R_co1_1_8$seq.name
# R_co1_1_9_species <- R_co1_1_9$seq.name
# R_co1_1_10_species <- R_co1_1_10$seq.name
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
names(R_co1_20_1)

#head(R_co1_20_1$seq.name)
#As I need to get the genus name (1st part of two parts of species name), follow the below
R_co1_20_1_G<-as.data.frame(str_extract(R_co1_20_1$seq.name , "[^_]+"))
#R_co1_20_1_G<-as.data.frame(str_extract(R_co1_20_1 , "[^_]+"))
class(R_co1_20_1_G)
colnames(R_co1_20_1_G) <- "genus"
class(R_co1_20_1_G)

#I want to use the genus name column in co1 placements in the sister species dataframe column names (df_RefTree_R_co1_20_1_new, df_R_80_co1_20_1_new).
#Coz all the sister species in each column are the sis species of given co1 sequence genus/ species. So can rename column names with co1 placement genus name
R_co1_20_1_G_vec <- as.vector(R_co1_20_1_G$genus)
#create a dataframe
id <- c(1:904)
placement_Genus <- data.frame(id,R_co1_20_1_G_vec)

#Sister species of placement sequences in reference tree (Can we have his in a function? Coz, I have the same two lines for 50 more trees.
#When I tried my own function there was an atomic issue)
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(ReferenceTree,R_co1_20_1_species[i],type="terminal",label=T)
#Sister species for placement sequeneces(20%) after placing on bb tree(80% bb tree)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(R80_1_tree,R_co1_20_1_species[i],type="terminal",label=T)

#####

# you will need the stringr package, data.tabl, tidyverse & dplyr packages
library(stringr)
library(data.table)
library(tidyverse)
library(dplyr)

# extract_Genus function first
df_RefTree_R_co1_20_1 <- extract_Genus(RefTree_R_co1_20_1_List)
df_R_80_co1_20_1 <- extract_Genus(R_80_co1_20_1_List)

#Let's get the mismatches based on reference tree and co1 placement names
mismatches <- setdiff(df_RefTree_R_co1_20_1,df_R_80_co1_20_1)
#Get the number of placement genus without matches to the placement genus in reference tree
unique(mismatches,by="id")#212

#In this case since we have 904 co1 placements  in 80% tree the there are 692 matches
904-212

#for the downstream analysis, I need mismatches and check the genus with single species in mismatch genus list
#First I convert the ids to genus name as I have to deal with genus name when removing genus with single species
class(mismatches)
dim(mismatches)
class(placement_Genus)
dim(placement_Genus)
mismatch_wt_names <- inner_join(mismatches,placement_Genus,by="id")#580 rows

#Get the number of placement genus without matches to the placement genus in reference tree
unique_mismatches <- unique(mismatch_wt_names,by="R_co1_20_1_G_vec")#196

#R_co1_20_1_G_vec col in unique_mismatches dataframe has the genus list of mismatches
#Using that col and set diff function get the monotypic genus

monotypic_genus <- setdiff(unique_mismatches$R_co1_20_1_G_vec,Ref_Tree_distinct)

#Now let's see the number of monotypic placement genus with clade genus that all matches, zero matches and mixtures (close enough monotypic)
match_monotypic <- mismatch_wt_names[mismatch_wt_names$genus==mismatch_wt_names$R_co1_20_1_G_vec, ]#61

mismatch_monotypic <- mismatch_wt_names[mismatch_wt_names$genus!=mismatch_wt_names$R_co1_20_1_G_vec, ]#519
#Let's see whats common in both (mixture set)
#intersect of id
mixtures <- intersect(match_monotypic$R_co1_20_1_G_vec,mismatch_monotypic$R_co1_20_1_G_vec)#10

#Only in match_monotypic
All_match_monotypic <- setdiff(match_monotypic$R_co1_20_1_G_vec,mismatch_monotypic$R_co1_20_1_G_vec)#42
#Only in mismatch_monotypic

All_mismatch_monotypic <- setdiff(mismatch_monotypic$R_co1_20_1_G_vec,match_monotypic$R_co1_20_1_G_vec)#144

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

###-----------

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
