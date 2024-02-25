#Sep 19, 2023 edited Jan 4, 2024

#Sister species table
getwd()
library(foreach)
#install.packages("phytools")
library(phytools)
library(maps)
library(phangorn)
library(phylotools)
library(stringr)
#Read fasta files of placed co1 on the bb tree
R_co1_20_1=read.fasta(file = "CO1_20_1.fasta")
R_co1_20_2=read.fasta(file = "CO1_20_2.fasta")
R_co1_20_3=read.fasta(file = "CO1_20_3.fasta")
R_co1_20_4=read.fasta(file = "CO1_20_4.fasta")
R_co1_20_5=read.fasta(file = "CO1_20_5.fasta")
R_co1_20_6=read.fasta(file = "CO1_20_6.fasta")
R_co1_20_7=read.fasta(file = "CO1_20_7.fasta")
R_co1_20_8=read.fasta(file = "CO1_20_8.fasta")
R_co1_20_9=read.fasta(file = "CO1_20_9.fasta")
R_co1_20_10=read.fasta(file = "CO1_20_10.fasta")
R_co1_40_1=read.fasta(file = "CO1_40_1.fasta")
R_co1_40_2=read.fasta(file = "CO1_40_2.fasta")
R_co1_40_3=read.fasta(file = "CO1_40_3.fasta")
R_co1_40_4=read.fasta(file = "CO1_40_4.fasta")
R_co1_40_5=read.fasta(file = "CO1_40_5.fasta")
R_co1_40_6=read.fasta(file = "CO1_40_6.fasta")
R_co1_40_7=read.fasta(file = "CO1_40_7.fasta")
R_co1_40_8=read.fasta(file = "CO1_40_8.fasta")
R_co1_40_9=read.fasta(file = "CO1_40_9.fasta")
R_co1_40_10=read.fasta(file = "CO1_40_10.fasta")
R_co1_60_1=read.fasta(file = "CO1_60_1.fasta")
R_co1_60_2=read.fasta(file = "CO1_60_2.fasta")
R_co1_60_3=read.fasta(file = "CO1_60_3.fasta")
R_co1_60_4=read.fasta(file = "CO1_60_4.fasta")
R_co1_60_5=read.fasta(file = "CO1_60_5.fasta")
R_co1_60_6=read.fasta(file = "CO1_60_6.fasta")
R_co1_60_7=read.fasta(file = "CO1_60_7.fasta")
R_co1_60_8=read.fasta(file = "CO1_60_8.fasta")
R_co1_60_9=read.fasta(file = "CO1_60_9.fasta")
R_co1_60_10=read.fasta(file = "CO1_60_10.fasta")
R_co1_80_1=read.fasta(file = "CO1_80_1.fasta")
R_co1_80_2=read.fasta(file = "CO1_80_2.fasta")
R_co1_80_3=read.fasta(file = "CO1_80_3.fasta")
R_co1_80_4=read.fasta(file = "CO1_80_4.fasta")
R_co1_80_5=read.fasta(file = "CO1_80_5.fasta")
R_co1_80_6=read.fasta(file = "CO1_80_6.fasta")
R_co1_80_7=read.fasta(file = "CO1_80_7.fasta")
R_co1_80_8=read.fasta(file = "CO1_80_8.fasta")
R_co1_80_9=read.fasta(file = "CO1_80_9.fasta")
R_co1_80_10=read.fasta(file = "CO1_80_10.fasta")
R_co1_1_1=read.fasta(file = "CO1_1_1.fasta")
R_co1_1_2=read.fasta(file = "CO1_1_2.fasta")
R_co1_1_3=read.fasta(file = "CO1_1_3.fasta")
R_co1_1_4=read.fasta(file = "CO1_1_4.fasta")
R_co1_1_5=read.fasta(file = "CO1_1_5.fasta")
R_co1_1_6=read.fasta(file = "CO1_1_6.fasta")
R_co1_1_7=read.fasta(file = "CO1_1_7.fasta")
R_co1_1_8=read.fasta(file = "CO1_1_8.fasta")
R_co1_1_9=read.fasta(file = "CO1_1_9.fasta")
R_co1_1_10=read.fasta(file = "CO1_1_10.fasta")



#read the refernce tree
ReferenceTree <- read.tree("RAxML_bestTree.FR100_new")
#Read the 40 trees generated using Random samples
R20_1_tree<-read.tree("20_1_new")
R20_2_tree<-read.tree("20_2_new")
R20_3_tree<-read.tree("20_3_new")
R20_4_tree<-read.tree("20_4_new")
R20_5_tree<-read.tree("20_5_new")
R20_6_tree<-read.tree("20_6_new")
R20_7_tree<-read.tree("20_7_new")
R20_8_tree<-read.tree("20_8_new")
R20_9_tree<-read.tree("20_9_new")
R20_10_tree<-read.tree("20_10_new")
R40_1_tree<-read.tree("40_1_new")
R40_2_tree<-read.tree("40_2_new")
R40_3_tree<-read.tree("40_3_new")
R40_4_tree<-read.tree("40_4_new")
R40_5_tree<-read.tree("40_5_new")
R40_6_tree<-read.tree("40_6_new")
R40_7_tree<-read.tree("40_7_new")
R40_8_tree<-read.tree("40_8_new")
R40_9_tree<-read.tree("40_9_new")
R40_10_tree<-read.tree("40_10_new")
R60_1_tree<-read.tree("60_1_new")
R60_2_tree<-read.tree("60_2_new")
R60_3_tree<-read.tree("60_3_new")
R60_4_tree<-read.tree("60_4_new")
R60_5_tree<-read.tree("60_5_new")
R60_6_tree<-read.tree("60_6_new")
R60_7_tree<-read.tree("60_7_new")
R60_8_tree<-read.tree("60_8_new")
R60_9_tree<-read.tree("60_9_new")
R60_10_tree<-read.tree("60_10_new")
R60_1_tree<-read.tree("60_1_new")
R80_2_tree<-read.tree("80_2_new")
R80_3_tree<-read.tree("80_3_new")
R80_4_tree<-read.tree("80_4_new")
R80_5_tree<-read.tree("80_5_new")
R80_6_tree<-read.tree("80_6_new")
R80_7_tree<-read.tree("80_7_new")
R80_8_tree<-read.tree("80_8_new")
R80_9_tree<-read.tree("80_9_new")
R80_10_tree<-read.tree("80_10_new")
R80_1_tree<-read.tree("80_1_new")
R99_1_tree<-read.tree("99_1_new")
R99_2_tree<-read.tree("99_2_new")
R99_3_tree<-read.tree("99_3_new")
R99_4_tree<-read.tree("99_4_new")
R99_5_tree<-read.tree("99_5_new")
R99_6_tree<-read.tree("99_6_new")
R99_7_tree<-read.tree("99_7_new")
R99_8_tree<-read.tree("99_8_new")
R99_9_tree<-read.tree("99_9_new")
R99_10_tree<-read.tree("99_10_new")
class(R99_1_tree)
#------
R_co1_20_1_species <- R_co1_20_1$seq.name
R_co1_20_2_species <- R_co1_20_2$seq.name
R_co1_20_3_species <- R_co1_20_3$seq.name
R_co1_20_4_species <- R_co1_20_4$seq.name
R_co1_20_5_species <- R_co1_20_5$seq.name
R_co1_20_6_species <- R_co1_20_6$seq.name
R_co1_20_7_species <- R_co1_20_7$seq.name
R_co1_20_8_species <- R_co1_20_8$seq.name
R_co1_20_9_species <- R_co1_20_9$seq.name
R_co1_20_10_species <- R_co1_20_10$seq.name
R_co1_40_1_species <- R_co1_40_1$seq.name
R_co1_40_2_species <- R_co1_40_2$seq.name
R_co1_40_3_species <- R_co1_40_3$seq.name
R_co1_40_4_species <- R_co1_40_4$seq.name
R_co1_40_5_species <- R_co1_40_5$seq.name
R_co1_40_6_species <- R_co1_40_6$seq.name
R_co1_40_7_species <- R_co1_40_7$seq.name
R_co1_40_8_species <- R_co1_40_8$seq.name
R_co1_40_9_species <- R_co1_40_9$seq.name
R_co1_40_10_species <- R_co1_40_10$seq.name
R_co1_60_1_species <- R_co1_60_1$seq.name
R_co1_60_2_species <- R_co1_60_2$seq.name
R_co1_60_3_species <- R_co1_60_3$seq.name
R_co1_60_4_species <- R_co1_60_4$seq.name
R_co1_60_5_species <- R_co1_60_5$seq.name
R_co1_60_6_species <- R_co1_60_6$seq.name
R_co1_60_7_species <- R_co1_60_7$seq.name
R_co1_60_8_species <- R_co1_60_8$seq.name
R_co1_60_9_species <- R_co1_60_9$seq.name
R_co1_60_10_species <- R_co1_60_10$seq.name
R_co1_80_1_species <- R_co1_80_1$seq.name
R_co1_80_2_species <- R_co1_80_2$seq.name
R_co1_80_3_species <- R_co1_80_3$seq.name
R_co1_80_4_species <- R_co1_80_4$seq.name
R_co1_80_5_species <- R_co1_80_5$seq.name
R_co1_80_6_species <- R_co1_80_6$seq.name
R_co1_80_7_species <- R_co1_80_7$seq.name
R_co1_80_8_species <- R_co1_80_8$seq.name
R_co1_80_9_species <- R_co1_80_9$seq.name
R_co1_80_10_species <- R_co1_80_10$seq.name
R_co1_1_1_species <- R_co1_1_1$seq.name
R_co1_1_2_species <- R_co1_1_2$seq.name
R_co1_1_3_species <- R_co1_1_3$seq.name
R_co1_1_4_species <- R_co1_1_4$seq.name
R_co1_1_5_species <- R_co1_1_5$seq.name
R_co1_1_6_species <- R_co1_1_6$seq.name
R_co1_1_7_species <- R_co1_1_7$seq.name
R_co1_1_8_species <- R_co1_1_8$seq.name
R_co1_1_9_species <- R_co1_1_9$seq.name
R_co1_1_10_species <- R_co1_1_10$seq.name
getwd
##bb 80% tree when placed 20% co1
library(ips)
library(dplyr)
library(tidyr)
#------------------
#To remove single species in genus groups
Ref_Tree <- as.list(ReferenceTree)
Ref_Tree_Tips <- Ref_Tree$tip.label
class(Ref_Tree_Tips)
Ref_Tree_Tips <- as.data.frame(Ref_Tree_Tips)
df_ReferenceTree<-data.frame(str_extract(Ref_Tree_Tips$Ref_Tree_Tips , "[^_]+"))
colnames(df_ReferenceTree) <- c("genus")
names(df_ReferenceTree)
#Get ununique species by removing single species
#Ref_Tree_distinct <- distinct(df_ReferenceTree)
rm(df_RederenceTree)
genus_more_1<- df_ReferenceTree %>% 
  group_by(genus) %>% 
  filter(n()>=2)
#Get distinct names
Ref_Tree_distinct <- distinct(genus_more_1)#766 obs.

#-----------------------

#separate(RefTree_R_co1_20_1_List, col )
#sister(tree, "t4", type = "terminal", label = T)
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(ReferenceTree,R_co1_20_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_20_1_species)) %do% sister(R80_1_tree,R_co1_20_1_species[i],type="terminal",label=T)
class(RefTree_R_co1_20_1_List)
#df_RefTree_R_co1_20_1_List <- data.frame(RefTree_R_co1_20_1_List)#diff size column error
#When converting a list to a dataframe there's an error called different size column error.
#To solve that I used one liner with plyr
#t to transpose mt dataframe for a better analysis
df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#98 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#97 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:904,each=98)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:904,each=97)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #714
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #190

# df_R_80_co1_20_1_new_t_t <- data.frame(t(df_R_80_co1_20_1_new_t))
# uniq1_df_R_80_co1_20_1_new_t_t <- unique(df_R_80_co1_20_1_new_t_t)
# uniq_df_R_80_co1_20_1_new_t <- unique(df_R_80_co1_20_1_new_t)
# comparedf(df_RefTree_R_co1_20_1_new_t,df_R_80_co1_20_1_new_t)
# comparedf(df_RefTree_R_co1_20_1_new,df_R_80_co1_20_1_new)
# class(df_RefTree_R_co1_20_1_new_t)

# hh <- df_RefTree_R_co1_20_1_new[1:5,]
# ii <- df_R_80_co1_20_1_new[1:5,]
# library(janitor)
# compare_df_cols(hh,ii,return = "match")
# library(diffdf)
# diffdf(hh,ii)
# #-------Example
# 
# df1 <- data.frame(id = paste0("person", 1:3),
#                   a = c("a", "b", "c"),
#                   b = c(1, 3, 4),
#                   c = c("f", "e", "d"),
#                   row.names = paste0("rn", 1:3),
#                   stringsAsFactors = FALSE)
# df2 <- data.frame(id = paste0("person", 3:1),
#                   a = c("c", "b", "a"),
#                   b = c(1, 3, 4),
#                   d = paste0("rn", 1:3),
#                   row.names = paste0("rn", c(1,3,2)),
#                   stringsAsFactors = FALSE)
# comparedf(df1,df2)
# summary(comparedf(df1,df2))
# #-----
# library(arsenal)
# comparedf(df_RefTree_R_co1_20_1_new,df_R_80_co1_20_1_new)
# #----
# 
# ibrary(dplyr)
# dfff <- df_RefTree_R_co1_20_1_new %>%
#   mutate(across(everything(), ~ replace(.x, is.na(.x), "")))
# dff <- as.list(dfff)
# unique1 <- unique(df_RefTree_R_co1_20_1_new$genus)
# 
# df_RefTree_R_co1_20_1_new  <- as.list(df_R_80_co1_20_1_new)
# df_R_80_co1_20_1_new <- as.list(df_R_80_co1_20_1_new)

# 


# df_RefTree_R_co1_20_1 %>% drop_na("genus")
# max.length <- max(sapply(RefTree_R_co1_20_1_List , length))
# ## Add NA values to list elements
# db <- lapply(RefTree_R_co1_20_1_List , function(v) { c(v, rep(NA, max.length-length(v)))})
# ## Rbind
# db <- data.frame(do.call(rbind, db))
# max.length <- max(sapply(R_80_co1_20_1_List , length))
# ## Add NA values to list elements
# db_80_20_1 <- lapply(R_80_co1_20_1_List, function(v) { c(v, rep(NA, max.length-length(v)))})
# ## Rbind
# db_80_20_1  <- data.frame(do.call(rbind, db_80_20_1 ))
# 
# db1 <- sub("\\_.*", "", db)
# 
# RefTree_R_co1_20_1 <- unlist(RefTree_R_co1_20_1_List)
# sub("(.*)\\_.*","\\1",RefTree_R_co1_20_1_List)
# lapply(X = RefTree_R_co1_20_1_List, sub("(.*)\\_.*","\\1",RefTree_R_co1_20_1_List))
# sub("\\_.*", "", RefTree_R_co1_20_1_List)
# Filter(function(x) sub("(.*)\\_.*","\\1", x), RefTree_R_co1_20_1_List)
# RefTree_R_co1_20_1_List[1]
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_1_List [(RefTree_R_co1_20_1_List %in% R_80_co1_20_1_List )] #401
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_1_List [!(RefTree_R_co1_20_1_List %in% R_80_co1_20_1_List)] #503

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(ReferenceTree,R_co1_20_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_20_2_species)) %do% sister(R80_2_tree,R_co1_20_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#692 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:904,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:904,each=692)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #720
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #184


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_2_List [(RefTree_R_co1_20_2_List %in% R_80_co1_20_2_List )] #393
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_2_List [!(RefTree_R_co1_20_2_List %in% R_80_co1_20_2_List)] #511

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_20_3_species)) %do% sister(ReferenceTree,R_co1_20_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_20_3_species)) %do% sister(R80_3_tree,R_co1_20_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#315 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#314 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:904,each=315)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:904,each=314)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #706
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #198

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_3_List [(RefTree_R_co1_20_3_List %in% R_80_co1_20_3_List )] #358
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_3_List [!(RefTree_R_co1_20_3_List %in% R_80_co1_20_3_List)] #546

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_20_4_species)) %do% sister(ReferenceTree,R_co1_20_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_20_4_species)) %do% sister(R80_4_tree,R_co1_20_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#183 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:904,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:904,each=183)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #703
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #201

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_4_List [(RefTree_R_co1_20_4_List %in% R_80_co1_20_4_List )] #364
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_4_List [!(RefTree_R_co1_20_4_List %in% R_80_co1_20_4_List)] #540

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_20_5_species)) %do% sister(ReferenceTree,R_co1_20_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_20_5_species)) %do% sister(R80_5_tree,R_co1_20_5_species[i],type="terminal",label=T)
df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#1536 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#4204 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:904,each=1536)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:904,each=4204)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #681
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #223

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_5_List [(RefTree_R_co1_20_5_List %in% R_80_co1_20_5_List )] #382
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_5_List [!(RefTree_R_co1_20_5_List %in% R_80_co1_20_5_List)] #522

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_20_6_species)) %do% sister(ReferenceTree,R_co1_20_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_20_6_species)) %do% sister(R80_6_tree,R_co1_20_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#315 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#4517 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:904,each=315)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:904,each=4517)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #671
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #233

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_6_List [(RefTree_R_co1_20_6_List %in% R_80_co1_20_6_List )] #337
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_6_List [!(RefTree_R_co1_20_6_List %in% R_80_co1_20_6_List)] #567

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_20_7_species)) %do% sister(ReferenceTree,R_co1_20_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_20_7_species)) %do% sister(R80_7_tree,R_co1_20_7_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#1404 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#284 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:904,each=1404)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:904,each=284)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #681
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #223

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_7_List [(RefTree_R_co1_20_7_List %in% R_80_co1_20_7_List )] #384
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_7_List [!(RefTree_R_co1_20_7_List %in% R_80_co1_20_7_List)] #520

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_20_8_species)) %do% sister(ReferenceTree,R_co1_20_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_20_8_species)) %do% sister(R80_8_tree,R_co1_20_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#122 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:904,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:904,each=122)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #714
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #190

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_8_List [(RefTree_R_co1_20_8_List %in% R_80_co1_20_8_List )] #389
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_8_List [!(RefTree_R_co1_20_8_List %in% R_80_co1_20_8_List)] #515

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_20_9_species)) %do% sister(ReferenceTree,R_co1_20_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_20_9_species)) %do% sister(R80_9_tree,R_co1_20_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#1536 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#4418 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:904,each=1536)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:904,each=4418)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #704
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #200
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_9_List [(RefTree_R_co1_20_9_List %in% R_80_co1_20_9_List )] #394
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_9_List [!(RefTree_R_co1_20_9_List %in% R_80_co1_20_9_List)] #510

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_20_10_species)) %do% sister(ReferenceTree,R_co1_20_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_20_10_species)) %do% sister(R80_10_tree,R_co1_20_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#65 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:904,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:904,each=65)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #691
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #213

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_20_10_List [(RefTree_R_co1_20_10_List %in% R_80_co1_20_10_List )] #367
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_20_10_List [!(RefTree_R_co1_20_10_List %in% R_80_co1_20_10_List)] #537

#bb 60% tree when placed 40% co1

#To get easy coding, I rename RefTree_R_co1_40_1_List as RefTree_R_co1_20_1_List******
#R_60_co1_40_1_List as R_80_co1_20_1_List*******

RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_40_1_species)) %do% sister(ReferenceTree,R_co1_40_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_40_1_species)) %do% sister(R60_1_tree,R_co1_40_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#4519 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#4517 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:1808,each=4517)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1376
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #432
# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_1_List [(RefTree_R_co1_40_1_List %in% R_60_co1_40_1_List )] #627
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_1_List [!(RefTree_R_co1_40_1_List %in% R_60_co1_40_1_List )] #1181

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_40_2_species)) %do% sister(ReferenceTree,R_co1_40_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_40_2_species)) %do% sister(R60_2_tree,R_co1_40_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#4515 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:1808,each=4515)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1365
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #443

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_2_List [(RefTree_R_co1_40_2_List %in% R_60_co1_40_2_List )] #582
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_2_List [!(RefTree_R_co1_40_2_List %in% R_60_co1_40_2_List )] #1226

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_40_3_species)) %do% sister(ReferenceTree,R_co1_40_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_40_3_species)) %do% sister(R60_3_tree,R_co1_40_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#1536 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#314 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:1808,each=1536)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:1808,each=314)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1370
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #438


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_3_List [(RefTree_R_co1_40_3_List %in% R_60_co1_40_3_List )] #573
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_3_List [!(RefTree_R_co1_40_3_List %in% R_60_co1_40_3_List )] #1235

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_40_4_species)) %do% sister(ReferenceTree,R_co1_40_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_40_4_species)) %do% sister(R60_4_tree,R_co1_40_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#4516 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:1808,each=4516)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1379
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #429

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_4_List [(RefTree_R_co1_40_4_List %in% R_60_co1_40_4_List )] #588
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_4_List [!(RefTree_R_co1_40_4_List %in% R_60_co1_40_4_List )] #1220

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_40_5_species)) %do% sister(ReferenceTree,R_co1_40_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_40_5_species)) %do% sister(R60_5_tree,R_co1_40_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#1536 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#316 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:1808,each=1536)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:1808,each=316)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1335
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #473


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_5_List [(RefTree_R_co1_40_5_List %in% R_60_co1_40_5_List )] #578
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_5_List [!(RefTree_R_co1_40_5_List %in% R_60_co1_40_5_List )] #1230

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_40_6_species)) %do% sister(ReferenceTree,R_co1_40_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_40_6_species)) %do% sister(R60_6_tree,R_co1_40_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#430 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#314 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:1808,each=430)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:1808,each=314)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1349
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #459

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_6_List [(RefTree_R_co1_40_6_List %in% R_60_co1_40_6_List )] #575
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_6_List [!(RefTree_R_co1_40_6_List %in% R_60_co1_40_6_List )] #1233

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_40_7_species)) %do% sister(ReferenceTree,R_co1_40_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_40_7_species)) %do% sister(R60_7_tree,R_co1_40_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#4519 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#98 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:1808,each=98)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1407
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #401


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_7_List [(RefTree_R_co1_40_7_List %in% R_60_co1_40_7_List )] #606
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_7_List [!(RefTree_R_co1_40_7_List %in% R_60_co1_40_7_List )] #1202

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_40_8_species)) %do% sister(ReferenceTree,R_co1_40_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_40_8_species)) %do% sister(R60_8_tree,R_co1_40_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#4430 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:1808,each=4430)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1386
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #422

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_8_List [(RefTree_R_co1_40_8_List %in% R_60_co1_40_8_List )] #610
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_8_List [!(RefTree_R_co1_40_8_List %in% R_60_co1_40_8_List )] #1198

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_40_9_species)) %do% sister(ReferenceTree,R_co1_40_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_40_9_species)) %do% sister(R60_9_tree,R_co1_40_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#1536 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#4512 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:1808,each=1536)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:1808,each=4512)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1355
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #453

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_9_List [(RefTree_R_co1_40_9_List %in% R_60_co1_40_9_List )] #575
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_9_List [!(RefTree_R_co1_40_9_List %in% R_60_co1_40_9_List )] #1233

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_40_10_species)) %do% sister(ReferenceTree,R_co1_40_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_40_10_species)) %do% sister(R60_10_tree,R_co1_40_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#4471 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:1808,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:1808,each=4471)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1379
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #429

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_40_10_List [(RefTree_R_co1_40_10_List %in% R_60_co1_40_10_List )] #587
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_40_10_List [!(RefTree_R_co1_40_10_List %in% R_60_co1_40_10_List )] #1221

#RefTree_biasco1List[sapply(names(RefTree_biasco1List), function(x) !identical(RefTree_biasco1List[[x]], biasco1List[[x]]))] 

#bb 40% tree when placed 60% co1
#To get easy coding, I rename RefTree_R_co1_60_1_List as RefTree_R_co1_20_1_List******
#R_40_co1_60_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_60_1_species)) %do% sister(ReferenceTree,R_co1_60_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_60_1_species)) %do% sister(R40_1_tree,R_co1_60_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#4519 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#291 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:2712,each=291)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1839
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #873

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_1_List [(RefTree_R_co1_60_1_List %in% R_40_co1_60_1_List )] #585
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_1_List [!(RefTree_R_co1_60_1_List %in% R_40_co1_60_1_List )] #2127

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_60_2_species)) %do% sister(ReferenceTree,R_co1_60_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_60_2_species)) %do% sister(R40_2_tree,R_co1_60_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#4515 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:2712,each=4515)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1819
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #893

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_2_List [(RefTree_R_co1_60_2_List %in% R_40_co1_60_2_List )] #543
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_2_List [!(RefTree_R_co1_60_2_List %in% R_40_co1_60_2_List )] #2169


RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_60_3_species)) %do% sister(ReferenceTree,R_co1_60_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_60_3_species)) %do% sister(R40_3_tree,R_co1_60_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#4519 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#481 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:2712,each=481)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1850
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #862

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_3_List [(RefTree_R_co1_60_3_List %in% R_40_co1_60_3_List )] #592
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_3_List [!(RefTree_R_co1_60_3_List %in% R_40_co1_60_3_List )] #2120

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_60_4_species)) %do% sister(ReferenceTree,R_co1_60_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_60_4_species)) %do% sister(R40_4_tree,R_co1_60_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#4506 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:2712,each=4506)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1870
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #842

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_4_List [(RefTree_R_co1_60_4_List %in% R_40_co1_60_4_List )] #600
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_4_List [!(RefTree_R_co1_60_4_List %in% R_40_co1_60_4_List )] #2212

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_60_5_species)) %do% sister(ReferenceTree,R_co1_60_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_60_5_species)) %do% sister(R40_5_tree,R_co1_60_5_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#314 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:2712,each=314)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1824
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #888

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_5_List [(RefTree_R_co1_60_5_List %in% R_40_co1_60_5_List )] #569
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_5_List [!(RefTree_R_co1_60_5_List %in% R_40_co1_60_5_List )] #2143

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_60_6_species)) %do% sister(ReferenceTree,R_co1_60_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_60_6_species)) %do% sister(R40_6_tree,R_co1_60_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#1219 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#691 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:2712,each=1219)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:2712,each=691)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1890
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #822

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_6_List [(RefTree_R_co1_60_6_List %in% R_40_co1_60_6_List )] #567
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_6_List [!(RefTree_R_co1_60_6_List %in% R_40_co1_60_6_List )] #2145

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_60_7_species)) %do% sister(ReferenceTree,R_co1_60_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_60_7_species)) %do% sister(R40_7_tree,R_co1_60_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#4519 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#213 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:2712,each=213)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1839
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #873

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_7_List [(RefTree_R_co1_60_7_List %in% R_40_co1_60_7_List )] #584
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_7_List [!(RefTree_R_co1_60_7_List %in% R_40_co1_60_7_List )] #2128

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_60_8_species)) %do% sister(ReferenceTree,R_co1_60_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_60_8_species)) %do% sister(R40_8_tree,R_co1_60_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#3992 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:2712,each=3992)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1875
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #837

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_8_List [(RefTree_R_co1_60_8_List %in% R_40_co1_60_8_List )] #591
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_8_List [!(RefTree_R_co1_60_8_List %in% R_40_co1_60_8_List )] #2121

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_60_9_species)) %do% sister(ReferenceTree,R_co1_60_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_60_9_species)) %do% sister(R40_9_tree,R_co1_60_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#4509 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:2712,each=4509)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1781
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #931


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_9_List [(RefTree_R_co1_60_9_List %in% R_40_co1_60_9_List )] #553
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_9_List [!(RefTree_R_co1_60_9_List %in% R_40_co1_60_9_List )] #2159

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_60_10_species)) %do% sister(ReferenceTree,R_co1_60_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_60_10_species)) %do% sister(R40_10_tree,R_co1_60_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#4467 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:2712,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:2712,each=4467)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1864
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #848


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_60_10_List [(RefTree_R_co1_60_10_List %in% R_40_co1_60_10_List )] #603
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_60_10_List [!(RefTree_R_co1_60_10_List %in% R_40_co1_60_10_List )] #2109


#bb 20% tree when placed 80% co1
#To get easy coding, I rename RefTree_R_co1_80_1_List as RefTree_R_co1_20_1_List******
#R_20_co1_80_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_80_1_species)) %do% sister(ReferenceTree,R_co1_80_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_80_1_species)) %do% sister(R20_1_tree,R_co1_80_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#4519 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#4363 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:3616,each=4363)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1725
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #1891

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_1_List [(RefTree_R_co1_80_1_List %in% R_20_co1_80_1_List )] #299
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_1_List [!(RefTree_R_co1_80_1_List %in% R_20_co1_80_1_List )] #3317

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_80_2_species)) %do% sister(ReferenceTree,R_co1_80_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_80_2_species)) %do% sister(R20_2_tree,R_co1_80_2_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#4519 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#4399 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:3616,each=4399)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1729
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #1887

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_2_List [(RefTree_R_co1_80_2_List %in% R_20_co1_80_2_List )] #290
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_2_List [!(RefTree_R_co1_80_2_List %in% R_20_co1_80_2_List )] #3326

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_80_3_species)) %do% sister(ReferenceTree,R_co1_80_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_80_3_species)) %do% sister(R20_3_tree,R_co1_80_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#4519 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#4517 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:3616,each=4517)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1762
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #1854

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_3_List [(RefTree_R_co1_80_3_List %in% R_20_co1_80_3_List )] #325
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_3_List [!(RefTree_R_co1_80_3_List %in% R_20_co1_80_3_List )] #3291

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_80_4_species)) %do% sister(ReferenceTree,R_co1_80_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_80_4_species)) %do% sister(R20_4_tree,R_co1_80_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#4519 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#4507 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:3616,each=4507)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1768
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #1848

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_4_List [(RefTree_R_co1_80_4_List %in% R_20_co1_80_4_List )] #335
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_4_List [!(RefTree_R_co1_80_4_List %in% R_20_co1_80_4_List )] #3281

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_80_5_species)) %do% sister(ReferenceTree,R_co1_80_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_80_5_species)) %do% sister(R20_5_tree,R_co1_80_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#4519 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#2509 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:3616,each=2509)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1772
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #1844

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_5_List [(RefTree_R_co1_80_5_List %in% R_20_co1_80_5_List )] #344
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_5_List [!(RefTree_R_co1_80_5_List %in% R_20_co1_80_5_List )] #3272

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_80_6_species)) %do% sister(ReferenceTree,R_co1_80_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_80_6_species)) %do% sister(R20_6_tree,R_co1_80_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#4519 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#4515 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:3616,each=4515)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1731
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #1885

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_6_List [(RefTree_R_co1_80_6_List %in% R_20_co1_80_6_List )] #260
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_6_List [!(RefTree_R_co1_80_6_List %in% R_20_co1_80_6_List )] #3356

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_80_7_species)) %do% sister(ReferenceTree,R_co1_80_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_80_7_species)) %do% sister(R20_7_tree,R_co1_80_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#4519 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#4478 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:3616,each=4478)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1775
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #1841

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_7_List [(RefTree_R_co1_80_7_List %in% R_20_co1_80_7_List )] #307
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_7_List [!(RefTree_R_co1_80_7_List %in% R_20_co1_80_7_List )] #3309

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_80_8_species)) %do% sister(ReferenceTree,R_co1_80_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_80_8_species)) %do% sister(R20_8_tree,R_co1_80_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#4519 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#4511 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:3616,each=4511)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1772
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #1844

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_8_List [(RefTree_R_co1_80_8_List %in% R_20_co1_80_8_List )] #316
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_8_List [!(RefTree_R_co1_80_8_List %in% R_20_co1_80_8_List )] #3300

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_80_9_species)) %do% sister(ReferenceTree,R_co1_80_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_80_9_species)) %do% sister(R20_9_tree,R_co1_80_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#4519 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#588 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:3616,each=588)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1744
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #1872


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_9_List [(RefTree_R_co1_80_9_List %in% R_20_co1_80_9_List )] #312
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_9_List [!(RefTree_R_co1_80_9_List %in% R_20_co1_80_9_List )] #3304

RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_80_10_species)) %do% sister(ReferenceTree,R_co1_80_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_80_10_species)) %do% sister(R20_10_tree,R_co1_80_10_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#691 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:3616,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:3616,each=691)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1726
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #1890


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_80_10_List [(RefTree_R_co1_80_10_List %in% R_20_co1_80_10_List )] #335
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_80_10_List [!(RefTree_R_co1_80_10_List %in% R_20_co1_80_10_List )] #3281
#---------------------


#bb 99% tree when placed 1% co1
#To get easy coding, I rename RefTree_R_co1_1_1_List as RefTree_R_co1_20_1_List******
#R_99_co1_1_1_List as R_80_co1_20_1_List*******
RefTree_R_co1_20_1_List <- foreach(i=1:length(R_co1_1_1_species)) %do% sister(ReferenceTree,R_co1_1_1_species[i],type="terminal",label=T)
R_80_co1_20_1_List <- foreach(i=1:length(R_co1_1_1_species)) %do% sister(R99_1_tree,R_co1_1_1_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_1_List <- t(plyr::ldply(RefTree_R_co1_20_1_List, rbind))#15 rows
df_R_80_co1_20_1_List <- t(plyr::ldply(R_80_co1_20_1_List, rbind))#15 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_1<-data.frame(str_extract(df_RefTree_R_co1_20_1_List , "[^_]+"))
df_R_80_co1_20_1<-data.frame(str_extract(df_R_80_co1_20_1_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_1) <- "genus"
colnames(df_R_80_co1_20_1) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_1_new <- data.frame(split(df_RefTree_R_co1_20_1,rep(1:45,each=15)))
class(df_RefTree_R_co1_20_1_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_1_new <- data.frame(split(df_R_80_co1_20_1,rep(1:45,each=15)))
class(df_R_80_co1_20_1_new)
df_RefTree_R_co1_20_1_new[is.na(df_RefTree_R_co1_20_1_new)] <- ""
df_RefTree_R_co1_20_1_new_t <- data.frame(t(df_RefTree_R_co1_20_1_new))
is.na(df_RefTree_R_co1_20_1_new_t)
df_R_80_co1_20_1_new[is.na(df_R_80_co1_20_1_new)] <- ""
df_R_80_co1_20_1_new_t<- data.frame(t(df_R_80_co1_20_1_new))
#uniq_R_80_co1_20_1_new <- apply(df_R_80_co1_20_1_new_t[], 1, unique)
#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_1_new<- apply(df_RefTree_R_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_1_new<- apply(df_R_80_co1_20_1_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_1_new)

similar <- uniq_RefTree_R_co1_20_1_new [(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #36
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_1_new [!(uniq_RefTree_R_co1_20_1_new %in% uniq_R_80_co1_20_1_new )] #9

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_1_List [(RefTree_R_co1_1_1_List %in% R_99_co1_1_1_List )] #24
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_1_List [!(RefTree_R_co1_1_1_List %in% R_99_co1_1_1_List)] #21

RefTree_R_co1_20_2_List <- foreach(i=1:length(R_co1_1_2_species)) %do% sister(ReferenceTree,R_co1_1_2_species[i],type="terminal",label=T)
R_80_co1_20_2_List <- foreach(i=1:length(R_co1_1_2_species)) %do% sister(R99_2_tree,R_co1_1_2_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_2_List <- t(plyr::ldply(RefTree_R_co1_20_2_List, rbind))#15 rows
df_R_80_co1_20_2_List <- t(plyr::ldply(R_80_co1_20_2_List, rbind))#25 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_2<-data.frame(str_extract(df_RefTree_R_co1_20_2_List , "[^_]+"))
df_R_80_co1_20_2<-data.frame(str_extract(df_R_80_co1_20_2_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_2) <- "genus"
colnames(df_R_80_co1_20_2) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_2_new <- data.frame(split(df_RefTree_R_co1_20_2,rep(1:45,each=15)))
class(df_RefTree_R_co1_20_2_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_2_new <- data.frame(split(df_R_80_co1_20_2,rep(1:45,each=25)))
class(df_R_80_co1_20_2_new)
df_RefTree_R_co1_20_2_new[is.na(df_RefTree_R_co1_20_2_new)] <- ""
df_RefTree_R_co1_20_2_new_t <- data.frame(t(df_RefTree_R_co1_20_2_new))
is.na(df_RefTree_R_co1_20_2_new_t)
df_R_80_co1_20_2_new[is.na(df_R_80_co1_20_2_new)] <- ""
df_R_80_co1_20_2_new_t<- data.frame(t(df_R_80_co1_20_2_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_2_new<- apply(df_RefTree_R_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_2_new<- apply(df_R_80_co1_20_2_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_2_new)

similar <- uniq_RefTree_R_co1_20_2_new [(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #31
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_2_new [!(uniq_RefTree_R_co1_20_2_new %in% uniq_R_80_co1_20_2_new )] #14

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_2_List [(RefTree_R_co1_1_2_List %in% R_99_co1_1_2_List )] #22
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_2_List [!(RefTree_R_co1_1_2_List %in% R_99_co1_1_2_List)] #23

RefTree_R_co1_20_3_List <- foreach(i=1:length(R_co1_1_3_species)) %do% sister(ReferenceTree,R_co1_1_3_species[i],type="terminal",label=T)
R_80_co1_20_3_List <- foreach(i=1:length(R_co1_1_3_species)) %do% sister(R99_3_tree,R_co1_1_3_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_3_List <- t(plyr::ldply(RefTree_R_co1_20_3_List, rbind))#22 rows
df_R_80_co1_20_3_List <- t(plyr::ldply(R_80_co1_20_3_List, rbind))#27 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_3<-data.frame(str_extract(df_RefTree_R_co1_20_3_List , "[^_]+"))
df_R_80_co1_20_3<-data.frame(str_extract(df_R_80_co1_20_3_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_3) <- "genus"
colnames(df_R_80_co1_20_3) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_3_new <- data.frame(split(df_RefTree_R_co1_20_3,rep(1:45,each=22)))
class(df_RefTree_R_co1_20_3_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_3_new <- data.frame(split(df_R_80_co1_20_3,rep(1:45,each=27)))
class(df_R_80_co1_20_3_new)
df_RefTree_R_co1_20_3_new[is.na(df_RefTree_R_co1_20_3_new)] <- ""
df_RefTree_R_co1_20_3_new_t <- data.frame(t(df_RefTree_R_co1_20_3_new))
is.na(df_RefTree_R_co1_20_3_new_t)
df_R_80_co1_20_3_new[is.na(df_R_80_co1_20_3_new)] <- ""
df_R_80_co1_20_3_new_t<- data.frame(t(df_R_80_co1_20_3_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_3_new<- apply(df_RefTree_R_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_3_new<- apply(df_R_80_co1_20_3_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_3_new)

similar <- uniq_RefTree_R_co1_20_3_new [(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #35
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_3_new [!(uniq_RefTree_R_co1_20_3_new %in% uniq_R_80_co1_20_3_new )] #10

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_3_List [(RefTree_R_co1_1_3_List %in% R_99_co1_1_3_List )] #23
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_3_List [!(RefTree_R_co1_1_3_List %in% R_99_co1_1_3_List)] #22

RefTree_R_co1_20_4_List <- foreach(i=1:length(R_co1_1_4_species)) %do% sister(ReferenceTree,R_co1_1_4_species[i],type="terminal",label=T)
R_80_co1_20_4_List <- foreach(i=1:length(R_co1_1_4_species)) %do% sister(R99_4_tree,R_co1_1_4_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_4_List <- t(plyr::ldply(RefTree_R_co1_20_4_List, rbind))#12 rows
df_R_80_co1_20_4_List <- t(plyr::ldply(R_80_co1_20_4_List, rbind))#16 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_4<-data.frame(str_extract(df_RefTree_R_co1_20_4_List , "[^_]+"))
df_R_80_co1_20_4<-data.frame(str_extract(df_R_80_co1_20_4_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_4) <- "genus"
colnames(df_R_80_co1_20_4) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_4_new <- data.frame(split(df_RefTree_R_co1_20_4,rep(1:45,each=12)))
class(df_RefTree_R_co1_20_4_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_4_new <- data.frame(split(df_R_80_co1_20_4,rep(1:45,each=16)))
class(df_R_80_co1_20_4_new)
df_RefTree_R_co1_20_4_new[is.na(df_RefTree_R_co1_20_4_new)] <- ""
df_RefTree_R_co1_20_4_new_t <- data.frame(t(df_RefTree_R_co1_20_4_new))
is.na(df_RefTree_R_co1_20_4_new_t)
df_R_80_co1_20_4_new[is.na(df_R_80_co1_20_4_new)] <- ""
df_R_80_co1_20_4_new_t<- data.frame(t(df_R_80_co1_20_4_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_4_new<- apply(df_RefTree_R_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_4_new<- apply(df_R_80_co1_20_4_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_4_new)

similar <- uniq_RefTree_R_co1_20_4_new [(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #36
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_4_new [!(uniq_RefTree_R_co1_20_4_new %in% uniq_R_80_co1_20_4_new )] #9

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_4_List [(RefTree_R_co1_1_4_List %in% R_99_co1_1_4_List )] #21
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_4_List [!(RefTree_R_co1_1_4_List %in% R_99_co1_1_4_List)] #24

RefTree_R_co1_20_5_List <- foreach(i=1:length(R_co1_1_5_species)) %do% sister(ReferenceTree,R_co1_1_5_species[i],type="terminal",label=T)
R_80_co1_20_5_List <- foreach(i=1:length(R_co1_1_5_species)) %do% sister(R99_5_tree,R_co1_1_5_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_5_List <- t(plyr::ldply(RefTree_R_co1_20_5_List, rbind))#36 rows
df_R_80_co1_20_5_List <- t(plyr::ldply(R_80_co1_20_5_List, rbind))#83 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_5<-data.frame(str_extract(df_RefTree_R_co1_20_5_List , "[^_]+"))
df_R_80_co1_20_5<-data.frame(str_extract(df_R_80_co1_20_5_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_5) <- "genus"
colnames(df_R_80_co1_20_5) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_5_new <- data.frame(split(df_RefTree_R_co1_20_5,rep(1:45,each=36)))
class(df_RefTree_R_co1_20_5_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_5_new <- data.frame(split(df_R_80_co1_20_5,rep(1:45,each=83)))
class(df_R_80_co1_20_5_new)
df_RefTree_R_co1_20_5_new[is.na(df_RefTree_R_co1_20_5_new)] <- ""
df_RefTree_R_co1_20_5_new_t <- data.frame(t(df_RefTree_R_co1_20_5_new))
is.na(df_RefTree_R_co1_20_5_new_t)
df_R_80_co1_20_5_new[is.na(df_R_80_co1_20_5_new)] <- ""
df_R_80_co1_20_5_new_t<- data.frame(t(df_R_80_co1_20_5_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_5_new<- apply(df_RefTree_R_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_5_new<- apply(df_R_80_co1_20_5_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_5_new)

similar <- uniq_RefTree_R_co1_20_5_new [(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #37
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_5_new [!(uniq_RefTree_R_co1_20_5_new %in% uniq_R_80_co1_20_5_new )] #8

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_5_List [(RefTree_R_co1_1_5_List %in% R_99_co1_1_5_List )] #25
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_5_List [!(RefTree_R_co1_1_5_List %in% R_99_co1_1_5_List)] #20

RefTree_R_co1_20_6_List <- foreach(i=1:length(R_co1_1_6_species)) %do% sister(ReferenceTree,R_co1_1_6_species[i],type="terminal",label=T)
R_80_co1_20_6_List <- foreach(i=1:length(R_co1_1_6_species)) %do% sister(R99_6_tree,R_co1_1_6_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_6_List <- t(plyr::ldply(RefTree_R_co1_20_6_List, rbind))#6 rows
df_R_80_co1_20_6_List <- t(plyr::ldply(R_80_co1_20_6_List, rbind))#32 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_6<-data.frame(str_extract(df_RefTree_R_co1_20_6_List , "[^_]+"))
df_R_80_co1_20_6<-data.frame(str_extract(df_R_80_co1_20_6_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_6) <- "genus"
colnames(df_R_80_co1_20_6) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_6_new <- data.frame(split(df_RefTree_R_co1_20_6,rep(1:45,each=6)))
class(df_RefTree_R_co1_20_6_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_6_new <- data.frame(split(df_R_80_co1_20_6,rep(1:45,each=32)))
class(df_R_80_co1_20_6_new)
df_RefTree_R_co1_20_6_new[is.na(df_RefTree_R_co1_20_6_new)] <- ""
df_RefTree_R_co1_20_6_new_t <- data.frame(t(df_RefTree_R_co1_20_6_new))
is.na(df_RefTree_R_co1_20_6_new_t)
df_R_80_co1_20_6_new[is.na(df_R_80_co1_20_6_new)] <- ""
df_R_80_co1_20_6_new_t<- data.frame(t(df_R_80_co1_20_6_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_6_new<- apply(df_RefTree_R_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_6_new<- apply(df_R_80_co1_20_6_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_6_new)

similar <- uniq_RefTree_R_co1_20_6_new [(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #37
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_6_new [!(uniq_RefTree_R_co1_20_6_new %in% uniq_R_80_co1_20_6_new )] #8

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_6_List [(RefTree_R_co1_1_6_List %in% R_99_co1_1_6_List )] #31
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_6_List [!(RefTree_R_co1_1_6_List %in% R_99_co1_1_6_List)] #14

RefTree_R_co1_20_7_List <- foreach(i=1:length(R_co1_1_7_species)) %do% sister(ReferenceTree,R_co1_1_7_species[i],type="terminal",label=T)
R_80_co1_20_7_List <- foreach(i=1:length(R_co1_1_7_species)) %do% sister(R99_7_tree,R_co1_1_7_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_7_List <- t(plyr::ldply(RefTree_R_co1_20_7_List, rbind))#30 rows
df_R_80_co1_20_7_List <- t(plyr::ldply(R_80_co1_20_7_List, rbind))#6 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_7<-data.frame(str_extract(df_RefTree_R_co1_20_7_List , "[^_]+"))
df_R_80_co1_20_7<-data.frame(str_extract(df_R_80_co1_20_7_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_7) <- "genus"
colnames(df_R_80_co1_20_7) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_7_new <- data.frame(split(df_RefTree_R_co1_20_7,rep(1:45,each=30)))
class(df_RefTree_R_co1_20_7_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_7_new <- data.frame(split(df_R_80_co1_20_7,rep(1:45,each=6)))
class(df_R_80_co1_20_7_new)
df_RefTree_R_co1_20_7_new[is.na(df_RefTree_R_co1_20_7_new)] <- ""
df_RefTree_R_co1_20_7_new_t <- data.frame(t(df_RefTree_R_co1_20_7_new))
is.na(df_RefTree_R_co1_20_7_new_t)
df_R_80_co1_20_7_new[is.na(df_R_80_co1_20_7_new)] <- ""
df_R_80_co1_20_7_new_t<- data.frame(t(df_R_80_co1_20_7_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_7_new<- apply(df_RefTree_R_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_7_new<- apply(df_R_80_co1_20_7_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_7_new)

similar <- uniq_RefTree_R_co1_20_7_new [(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #39
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_7_new [!(uniq_RefTree_R_co1_20_7_new %in% uniq_R_80_co1_20_7_new )] #6

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_7_List [(RefTree_R_co1_1_7_List %in% R_99_co1_1_7_List )] #32
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_7_List [!(RefTree_R_co1_1_7_List %in% R_99_co1_1_7_List)] #13

RefTree_R_co1_20_8_List <- foreach(i=1:length(R_co1_1_8_species)) %do% sister(ReferenceTree,R_co1_1_8_species[i],type="terminal",label=T)
R_80_co1_20_8_List <- foreach(i=1:length(R_co1_1_8_species)) %do% sister(R99_8_tree,R_co1_1_8_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_8_List <- t(plyr::ldply(RefTree_R_co1_20_8_List, rbind))#5 rows
df_R_80_co1_20_8_List <- t(plyr::ldply(R_80_co1_20_8_List, rbind))#7 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_8<-data.frame(str_extract(df_RefTree_R_co1_20_8_List , "[^_]+"))
df_R_80_co1_20_8<-data.frame(str_extract(df_R_80_co1_20_8_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_8) <- "genus"
colnames(df_R_80_co1_20_8) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_8_new <- data.frame(split(df_RefTree_R_co1_20_8,rep(1:45,each=5)))
class(df_RefTree_R_co1_20_8_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_8_new <- data.frame(split(df_R_80_co1_20_8,rep(1:45,each=7)))
class(df_R_80_co1_20_8_new)
df_RefTree_R_co1_20_8_new[is.na(df_RefTree_R_co1_20_8_new)] <- ""
df_RefTree_R_co1_20_8_new_t <- data.frame(t(df_RefTree_R_co1_20_8_new))
is.na(df_RefTree_R_co1_20_8_new_t)
df_R_80_co1_20_8_new[is.na(df_R_80_co1_20_8_new)] <- ""
df_R_80_co1_20_8_new_t<- data.frame(t(df_R_80_co1_20_8_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_8_new<- apply(df_RefTree_R_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_8_new<- apply(df_R_80_co1_20_8_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_8_new)

similar <- uniq_RefTree_R_co1_20_8_new [(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #34
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_8_new [!(uniq_RefTree_R_co1_20_8_new %in% uniq_R_80_co1_20_8_new )] #11

# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_8_List [(RefTree_R_co1_1_8_List %in% R_99_co1_1_8_List )] #21
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_8_List [!(RefTree_R_co1_1_8_List %in% R_99_co1_1_8_List)] #24

RefTree_R_co1_20_9_List <- foreach(i=1:length(R_co1_1_9_species)) %do% sister(ReferenceTree,R_co1_1_9_species[i],type="terminal",label=T)
R_80_co1_20_9_List <- foreach(i=1:length(R_co1_1_9_species)) %do% sister(R99_9_tree,R_co1_1_9_species[i],type="terminal",label=T)

df_RefTree_R_co1_20_9_List <- t(plyr::ldply(RefTree_R_co1_20_9_List, rbind))#240 rows
df_R_80_co1_20_9_List <- t(plyr::ldply(R_80_co1_20_9_List, rbind))#22 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_9<-data.frame(str_extract(df_RefTree_R_co1_20_9_List , "[^_]+"))
df_R_80_co1_20_9<-data.frame(str_extract(df_R_80_co1_20_9_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_9) <- "genus"
colnames(df_R_80_co1_20_9) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_9_new <- data.frame(split(df_RefTree_R_co1_20_9,rep(1:45,each=240)))
class(df_RefTree_R_co1_20_9_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_9_new <- data.frame(split(df_R_80_co1_20_9,rep(1:45,each=22)))
class(df_R_80_co1_20_9_new)
df_RefTree_R_co1_20_9_new[is.na(df_RefTree_R_co1_20_9_new)] <- ""
df_RefTree_R_co1_20_9_new_t <- data.frame(t(df_RefTree_R_co1_20_9_new))
is.na(df_RefTree_R_co1_20_9_new_t)
df_R_80_co1_20_9_new[is.na(df_R_80_co1_20_9_new)] <- ""
df_R_80_co1_20_9_new_t<- data.frame(t(df_R_80_co1_20_9_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_9_new<- apply(df_RefTree_R_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_9_new<- apply(df_R_80_co1_20_9_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_9_new)

similar <- uniq_RefTree_R_co1_20_9_new [(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #33
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_9_new [!(uniq_RefTree_R_co1_20_9_new %in% uniq_R_80_co1_20_9_new )] #12


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_9_List [(RefTree_R_co1_1_9_List %in% R_99_co1_1_9_List )] #24
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_9_List [!(RefTree_R_co1_1_9_List %in% R_99_co1_1_9_List)] #21


RefTree_R_co1_20_10_List <- foreach(i=1:length(R_co1_1_10_species)) %do% sister(ReferenceTree,R_co1_1_10_species[i],type="terminal",label=T)
R_80_co1_20_10_List <- foreach(i=1:length(R_co1_1_10_species)) %do% sister(R99_10_tree,R_co1_1_10_species[i],type="terminal",label=T)


df_RefTree_R_co1_20_10_List <- t(plyr::ldply(RefTree_R_co1_20_10_List, rbind))#4519 rows
df_R_80_co1_20_10_List <- t(plyr::ldply(R_80_co1_20_10_List, rbind))#63 rows

#extract string before underscore "_" in the species name. 
#I need only the genus name. Hence from Ambloplites_rupestris I need Ambloplites, the string before the underscore.
df_RefTree_R_co1_20_10<-data.frame(str_extract(df_RefTree_R_co1_20_10_List , "[^_]+"))
df_R_80_co1_20_10<-data.frame(str_extract(df_R_80_co1_20_10_List , "[^_]+"))
colnames(df_RefTree_R_co1_20_10) <- "genus"
colnames(df_R_80_co1_20_10) <- "genus"
#divide the one column dataframe into 98 columns
#using data.frame, I convert the list to a dataframe (904 co1 genus and each has 98 values, so the full set divide by 98)
df_RefTree_R_co1_20_10_new <- data.frame(split(df_RefTree_R_co1_20_10,rep(1:45,each=4519)))
class(df_RefTree_R_co1_20_10_new)
#df_R_80_co1_20_1 has the same 904 genus names but each has 97 values (genus names)
df_R_80_co1_20_10_new <- data.frame(split(df_R_80_co1_20_10,rep(1:45,each=63)))
class(df_R_80_co1_20_10_new)
df_RefTree_R_co1_20_10_new[is.na(df_RefTree_R_co1_20_10_new)] <- ""
df_RefTree_R_co1_20_10_new_t <- data.frame(t(df_RefTree_R_co1_20_10_new))
is.na(df_RefTree_R_co1_20_10_new_t)
df_R_80_co1_20_10_new[is.na(df_R_80_co1_20_10_new)] <- ""
df_R_80_co1_20_10_new_t<- data.frame(t(df_R_80_co1_20_10_new))

#unique to get one name from duplicated names and "apply" to use it for all the rows
#If there are leading, lagging spaces, use trimws to remove those
uniq_RefTree_R_co1_20_10_new<- apply(df_RefTree_R_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
uniq_R_80_co1_20_10_new<- apply(df_R_80_co1_20_10_new_t[], 1, function(x) unique(trimws(x)))
class(uniq_R_80_co1_20_10_new)

similar <- uniq_RefTree_R_co1_20_10_new [(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #32
#get the number of species doesn't have similar genus name comparing with reference tree
dissimilar <- uniq_RefTree_R_co1_20_10_new [!(uniq_RefTree_R_co1_20_10_new %in% uniq_R_80_co1_20_10_new )] #13


# #get the number of species have similar sisters by comparing with reference tree
# RefTree_R_co1_1_10_List [(RefTree_R_co1_1_10_List %in% R_99_co1_1_10_List )] #23
# #get the number of species doesn't have similar sisters by comparing with reference tree
# RefTree_R_co1_1_10_List [!(RefTree_R_co1_1_10_List %in% R_99_co1_1_10_List)] #22


# #checking
# tree<-rtree(n=12)
# plotTree(tree,node.numbers=TRUE)
# getSisters(tree,"t4",mode="label")
# getSisters(tree,23,"label")$tips

