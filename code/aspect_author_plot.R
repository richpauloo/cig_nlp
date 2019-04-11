# Code to make a network plot of ASPECT authors
# Jane Carlen, February 2019

# TO DO:s
# more network analysis. Path lengths? Clustering coefficient? Over time?
# When I load bibtek I get message "Some BibTeX entries may have been dropped. The result could be malformed. Review the .bib file and make sure every single entry starts with a '@'." Doesn't say which.

# ------------------------------------------------------------------------------------------
# 0. Setup  ----

library(readxl)
library(bibtex)
library(stringr)
library(network)
library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(scales)
library(forcats)
library(reshape2)
library(visNetwork)
library(RColorBrewer)
library(grDevices)
library(cowplot)

setwd("~/Documents/DSI/cig_citation/")

# ---------------------------- Original Data for Talk ---------------------------------------
# 1. Aspect data  ----

authors = read_xlsx("protoNeptworkA_jc_edited.xlsx", sheet = 3); head(authors)

# Late-add authors added below
#J. Perry-Houts  (hack Participant 2015) & L. Karlstrom are co-authors
#L. Schuurmans. Thesis (no co-authors)

# from: https://docs.google.com/document/d/1jCXNRsIAjVjS5Dz2PqwhZKlsodXaKSXbdz9jQArCHso/edit
# "Status: ASPECT - NEED TO ADD"

# Three more papers with the following co-authors:
# Eva Bredow	Single author. This is her dissertation
# Giacomo Corti,, Raffaello Cioni, Zara Franceschini, Federico Sani, Stéphane Scaillet, Paola Molin, Ilaria Isola, Francesco Mazzarini, Sascha Brune , Derek Keir, Asfaw Erbello, Ameha Muluneh, Finnigan Illsley-Kemp, Anne Glerum
# S. Liu and S.D. King

# -  Clean author names  ----
aspect.bib = read.bib("bibtex_export only pubs.bib", encoding = "latin1") #had to add school placeholder 'test' for some articles so they'd load
aspect.authors.list = lapply(aspect.bib, function(x) as.character(x$author)); length(unique(unlist(aspect.authors.list))) 
# two authors get grouped when we use last name only
# From checking excelt, S. Zhang != N. Zhang,  but ch C. O\\textquoterightNeill == C. J. O\\textquoterightNeill ?
aspect.authors.list = sapply(aspect.authors.list, function(x) {
  x[str_detect(pattern = "Neill", x)] = "C. O'Neill"; x
  x[str_detect(pattern = "H. Blom", x)] = "C. Blom"; x
  x[str_detect(pattern = "I. R. Rose", x)] = "I. Rose"; x
  x[str_detect(pattern = "M. R. T. Fraters", x)] = "M. Fraters"; x
  x[str_detect(pattern = "Molly Kathryn Ellowitz", x)] = "M. K. Ellowitz"; x
  #Z-X is Zheng-Xiang  
}) #now 60 unique
sort(table(unlist(aspect.authors.list)))
#sapply(unlist(aspect.authors.list), iconv, to="ASCII//TRANSLIT")

# Add new authors

# Late-add authors
#J. Perry-Houts  (hack Participant 2015) & L. Karlstrom are co-authors
#L. Schuurmans. Thesis (no co-authors)
aspect.authors.list$"Perry-Houts+Karlstrom2018" = c("J. Perry-Houts", "L. Karlstrom")
aspect.authors.list$"Schuurmans2018" = "L. Schuurmans"
# from: https://docs.google.com/document/d/1jCXNRsIAjVjS5Dz2PqwhZKlsodXaKSXbdz9jQArCHso/edit
aspect.authors.list$"Bredow" = "E. Bredow"
aspect.authors.list$"Corti+etal" = c("G. Corti", "R. Cioni", "Z. Franceschini", "F. Sani", "S. Scaillet", "P. Molin", "I. Isola", "F. Mazzarini", "S. Brune",
"D. Keir", "A. Erbello", "A. Muluneh", "F. Illsley-Kemp", "A. Glerum")
aspect.authors.list$"Liu+King" = c("S. Liu", "S.D. King")
aspect.authors.list$"Grove" = c("R.R. Grove")
aspect.authors.list$"Perry-Houts" = c("J. Perry-Houts")

# -  Convert to co-author network  ----
author.pairs = lapply(aspect.authors.list, function(x) { if(length(x) > 1) {t(combn(x, 2))} else {NULL}})
author.pairs = data.frame(do.call("rbind", author.pairs))
author.pairs$count = 1
author.pairs = aggregate(author.pairs$count, list(author.pairs$X1, author.pairs$X2), sum)
names(author.pairs)[3] = "co_authorships"
write.csv(author.pairs, "author_pairs.csv", row.names = F)
aspect.net = graph_from_data_frame(author.pairs, directed = F, vertices = as.vector(unique(unlist(aspect.authors.list))))
plot(aspect.net)

# -  Make plot and add colors ----

# Special authors
V(aspect.net)$name_last = sapply(str_split(V(aspect.net)$name, " "), last)
red = c("Bangerth", "Heister", "Gassmöller", "Dannberg") #Original Lead Developers
brown = c("Glerum", "Fraters", "Austermann", "Naliboff") #Added Principal Developers
color.aspect = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(aspect.net, "name")))]
color.aspect[V(aspect.net)$name_last%in% brown] = "Added Principal Developers"
red.rgb = "#990033"
brown.rgb = "#ff5050"
V(aspect.net)$Author_type =  color.aspect
V(aspect.net)$Papers = table(unlist(aspect.authors.list))[V(aspect.net)$name]
E(aspect.net)$co_authorships

author.plot = ggraph(aspect.net, layout = "kk") + 
  geom_edge_link(aes(width = co_authorships), alpha = 1, color = "gray") +
  # for some reason edge width not working like it doesn in the plots below
  scale_edge_width(name = "Number of co-authorships", range=c(.5,3), breaks = c(1,2,3)) +
  geom_node_point(aes(fill = Author_type, size = Papers), shape = 21, stroke = .5, alpha = .8) + 
  scale_fill_manual(values =  c(red.rgb, brown.rgb, "white"),
                    guide = guide_legend(title = "Author type", override.aes = list(size = 5), order = 1)) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(), 
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16)
        ) +
  ggtitle("ASPECT Co-author Relationships")
author.plot

ggsave(plot = author.plot, "aspect_author_plot_no_labels.png", height = 6, width  = 10)

# Add labels
author.plot = author.plot + geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold")
ggsave(plot = author.plot, "aspect_author_plot.png", height = 6, width  = 10)

# 2. Hack data  ----

hack = read_xlsx("protoNetworkA_jc_edited.xlsx", sheet = 4)[,1:7] #only care about hacks in first columns, not workships
dim(hack)

#Add one new hack participant (also added to authorlist)
hack = rbind(hack, c("J. Perry-Houts", "Jonathan Perry-Houts, University of Oregon", NA, "X", NA, NA, NA))

hack = hack %>% mutate(author = sapply(strsplit(Full_Name,","), first),
                       author_last = sapply(str_split(str_trim(author), " "), last),
                       name = paste(str_extract(author, "[A-Z]{1}"), ". ", author_last, sep = ""), #formatting like aspect authors
                       affiliation = str_trim(sapply(strsplit(hack$Full_Name,","), nth, 2)),
                       affiliation = str_replace(affiliation, "UC", "University of California"),
                       affiliation = str_replace(affiliation, "GFZ-Potsdam", "GFZ Potsdam"),
                       affiliation = str_replace(affiliation, "Clemson$", "Clemson University"))


# -  Fix hack names to agree with Aspect names as necessary ----
# some last names in the hack authors appear more than once, but they're all unique authors with unique first initial
hack$name = str_replace(hack$name, pattern = "Gassmoeller", replace = "Gassmöller")
hack$name = str_replace(hack$name, pattern  = "Huettig", replace = "Hüttig")
hack$name = str_replace(hack$name, pattern  = "E. Puckett", replace = "E. G. Puckett")
hack$name = str_replace(hack$name, pattern  = "J. Robey", replace = "J. M. Robey")
hack$name = str_replace(hack$name, pattern  = "L. Kellogg", replace = "L. H. Kellogg")
hack$name = str_replace(hack$name, pattern  = "S. Cox", replace = "S. P. Cox")
hack$name_last = sapply(str_split(hack$name, " "), last)
hack$name[hack$name %in% V(aspect.net)$name]

# -  Make hack net ----

# Find co-hack attendance and filter out authors who didn't go to any (only workshops) 
hack.pairs = apply(hack[, 3:7], 2, function(x) {
  t(combn(hack$name[rowSums(hack[,3:7]=="X", na.rm = T) > 0 & !is.na(x)], 2))
})

# Individual-year networks
hack.nets = lapply(hack.pairs, function(x) {
  graph_from_edgelist(x, directed = F)
})
#lapply(hack.nets, plot)

# Overall network
hack.pairs = data.frame(do.call("rbind", hack.pairs), year = rep(c("hack_2014","hack_2015","hack_2016","hack_2017", "hack_2018"), times = sapply(hack.pairs, nrow)), stringsAsFactors = F)
hack.pairs$count = 1
co.hacks = aggregate(hack.pairs$count, list(hack.pairs$X1, hack.pairs$X2), sum)
names(hack.pairs) = c("Group.1", "Group.2", "Relationship_type", "Count")
hack.pairs = left_join(hack.pairs, co.hacks, by = c("Group.1","Group.2"))
names(hack.pairs)[5] = "co_hacks"
write.csv(hack.pairs, "hack_pairs.csv", row.names = F)
hack.net = graph_from_data_frame(hack.pairs, directed = F)

# Color authors by type
color.hack = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, V(hack.net)$name))]
color.hack[rowSums(sapply(brown, grepl, vertex_attr(hack.net, "name"))) == 1] = "Added Principal Developers"
V(hack.net)$Author_type = color.hack

# Size nodes by hacks attended
Hacks_attended =  rowSums(hack[,3:7]=="X", na.rm = T)[rowSums(hack[,3:7]=="X", na.rm = T)>0]
names(Hacks_attended) = hack$name[rowSums(hack[,3:7]=="X", na.rm = T)>0]
Hacks_attended = Hacks_attended[V(hack.net)$name]
V(hack.net)$Hacks_attended = Hacks_attended

plot(hack.net, edge.width = .3, edge.color = (hue_pal()(5))[as.numeric(as.factor(E(hack.net)$Relationship_type))], vertex.color = (hue_pal()(3))[as.numeric(as.factor(color.hack))], vertex.size = V(hack.net)$Hacks_attended)

# -  Make nice hack plot ----

set.seed(1) # so plot doesn't move when I re-plot since x and ylim are
hack.plot = ggraph(hack.net, layout = 'nicely') + 
  geom_edge_fan(aes(color = Relationship_type), width = .5, alpha = 1) + #width = x
  scale_edge_color_brewer(palette = "Pastel1", name =  "Relationship type") +
  #scale_edge_color_manual(values = c(rgb(85/255, 142/255, 213/255), 
   #                             rgb(149/255, 55/255, 53/255), 
    #                            rgb(119/255, 47/255, 60/255), 
     #                           rgb(49/255, 133/255, 156/255), 
      #                          rgb(228/255, 108/255, 10/255), 
       #                         rgb(191/255, 153/255, 0/255)
        #                        )) +
  geom_node_point(aes(fill = Author_type, size = Hacks_attended, shape = Author_type), shape = 21, stroke = .5, alpha = .5)  +
  scale_fill_manual(values = c(red.rgb, brown.rgb, "gray"),
                    guide = guide_legend(title = "Author type", override.aes = list(size = 5), order = 1)) + 
  #scale_edge_width(range = c(.1,1.5)) + <- put in if we do geom_edge instead
  scale_size(range = c(4,9), name = "Hack attended") + 
  theme_bw() +
  xlim(0, 3.5) + 
  ylim(-2, 2) + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16)
        ) +
  ggtitle("Co-hack Attendance Relationships")
hack.plot

ggsave(plot = hack.plot, "hack_plot_no_labels.png", height = 8, width  = 12)

hack.plot.label = hack.plot + geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold")
ggsave(plot = hack.plot.label, "hack_plot.png", height = 8, width  = 12)

  
# 3. Make combined net ----

# Merged hacks into aggregate so it's not too busy
names(author.pairs)[3] = "co_" #make co_* name match so we can rbind
names(hack.pairs)[5] = "co_"
author_hack.pairs = cbind( rbind(author.pairs, hack.pairs[,c("Group.1", "Group.2", "co_")]),  
                           Relationship_type = c(rep("co-authorship", nrow(author.pairs)),
                                                 rep("co-hacks", nrow(hack.pairs))) )
names(author_hack.pairs)[3] = "Number_of_relationship_type"
write.csv(author_hack.pairs, "author_hack_pairs.csv", row.names = F)
geo.net = graph_from_data_frame(author_hack.pairs,
                                vertices = unique(c(V(hack.net)$name, V(aspect.net)$name)),
                                directed = F)

#    Make nice combined plot  ----

# Stable layout from earlier plots

layoutMatrix1 = 
  rbind(cbind(name = hack.plot$data$name, hack.plot$data[,1:2]),
        cbind(name = author.plot$data$name, author.plot$data[,1:2]))
layoutMatrix1 = layoutMatrix1[!duplicated(layoutMatrix1$name),]
# Check ordering
layoutMatrix1[,1] == V(geo.net)$name
layoutMatrix1 = data.frame(x = layoutMatrix1[,2], y = layoutMatrix1[,3])

V(geo.net)$name_last = sapply(str_split(V(geo.net)$name, " "), last)
# Special authors
# see "red" and "brown" created above
color2 = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, V(geo.net)$name))]
color2[rowSums(sapply(brown, grepl, vertex_attr(geo.net, "name"))) == 1] = "Added Principal Developers"
color2[V(geo.net)$name_last  %in% brown] = "Added Principal Developers"

V(geo.net)$Author_type =  color2
V(geo.net)$Papers = table(unlist(aspect.authors.list))[V(geo.net)$name] 
V(geo.net)$Papers = replace_na(V(geo.net)$Papers, replace = 0)

geo.plot = ggraph(geo.net, layout = "manual", node.position = data.frame(layoutMatrix1)) + 
  geom_edge_fan(aes(color = Relationship_type, linetype = Relationship_type,
                    width = Number_of_relationship_type), alpha = 1) +
  scale_edge_color_manual(values = c("gray", brewer.pal(5, "Pastel1")[2]), name = "Relationship type") +
  scale_edge_width(range = c(.5,2.5), name = "Number of relationship type") +
  scale_edge_linetype(c("solid", "twodash"), name = "Relationship type") + 
  geom_node_point(aes(fill = Author_type, size = Papers), shape = 21, stroke = .5, alpha = .5)  + 
  scale_fill_manual(values = c(red.rgb, brown.rgb, "gray"),
                    guide = guide_legend(title = "Author type", override.aes = list(size = 5), order = 1)) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        title = element_text(size = 16)
        ) + 
  ggtitle("Co-hack and ASPECT Co-author Relationships")

geo.plot
ggsave(plot = geo.plot, "author_and_hack_plot_no_labels.png", height = 6, width = 12)

geo.plot.label = geo.plot + geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold")
ggsave(plot = geo.plot.label, "author_and_hack_plot.png", height = 6, width  = 12)

# 3b. Make a joined hack and hack + author plot ----

ggsave(plot_grid(hack.plot, geo.plot), filename = "hack_and_hackAuthor_plots.png", width = 18, height = 7)

# 4. Centrality measures ####

# Preliminary work, need to check how these igraph measures account for disconnected components, valued edges and mutli-edges

#Change the net and title for each of the three plots generated

net.list = list(hack.net, aspect.net, geo.net)
title.vec = c("Hack_Network_Centrality_Measures", "Author_Network_Centrality_Measures", "Hack_and_Author_Network_Centrality_Measures")

for (i in 1:3) {

  net = net.list[[i]]
  title1 = title.vec[i]
  
centrality_measures = data.frame(
  name = V(net)$name,
  degree = rank(centralization.degree(net)$res),
  page_rank = rank(page.rank(net)$vector),
  betweenness = rank(betweenness(net)),
  degree_rank = as.character(rank(centralization.degree(net)$res)),
  stringsAsFactors = F
)

ggplot(melt(centrality_measures, idvars = c('name', 'degree_rank'), value.name = "rank") %>% 
         transform(degree_rank = round(as.numeric(degree_rank))) %>%
         group_by(variable), aes(x = fct_reorder(name, degree_rank), y = rank)) +
  geom_line(aes(group = name)) +
  geom_point(aes(color = variable)) +
  theme(axis.text.x = element_text(angle = 90, size = 4, vjust = -.01), axis.ticks.x = element_blank()) +
  ggtitle(title1) +
  ylab("rank: higher is better")
  
ggsave(title1, device = "png")
  
}
# 5. Interactive (draft) ####

net1 = geo.net
V(net1)$color =  c(brown.rgb, red.rgb, "gray")[as.numeric(as.factor(V(net1)$Author_type))]
V(net1)$color[V(net1)$name %in% V(hack.net)$name & V(net1)$color == "gray"] = "green" 
V(net1)$label = V(net1)$name
V(net1)$label.color = "black"
V(net1)$size= 6
V(net1)$label.cex = .3

visIgraph(net1, layout = "layout.norm", idToLabel= FALSE, layoutMatrix = as.matrix(layoutMatrix1)) %>%
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "all", degree = list(from = 0, to = 0)),
               nodesIdSelection = list(enabled = TRUE)) %>%
  visInteraction(dragNodes = TRUE, 
                 dragView = TRUE, 
                 zoomView = TRUE)



#----------------------------- Complete Data for Paper  -------------------------------------

# NOTE:
# Any names that should have a different spelling, etc should be changed in cig_allauthors_JAC 2
# Names that are incorrectly formatted, jumbled or grouped with others should be fixed directly in the bibtek

# 1b. Read in bibtek files (older) to make list of author names for cleaning ----

#devtools::install_github("https://github.com/ropensci/bib2df")
library(bib2df)

#have to get multiple authors by hand because code doesn't expect authors with "and" to new line
bib_2_df <- function(file) {
  bib <- readLines(file)
  bib <- paste(bib, collapse = "\n")
  bib = gsub(bib, pattern = "\nand", replacement = " and ")
  bib = str_split(bib, "\n")[[1]]
  bib = bib2df:::bib2df_gather(bib) #from inside bib2
  return(bib)
}

# Older versions were used to make author name spreadsheet then cleaned by Lorraine
cig_2015 = bib_2_df(file = "data/2015 bibtex_export v2.bib")
cig_2016 = bib_2_df(file = "data/2016 bibtex_export v2.bib")
cig_2017 = bib_2_df(file = "data/2017 bibtex_export v3.bib")
cig_2018 = bib_2_df(file = "data/2018 bibtex_export v3.bib")

# make one big data frame
allnames = c(names(cig_2015), names(cig_2016), names(cig_2017), names(cig_2018))
keepnames = names(table(allnames))[table(allnames)==4]

# use columns in all years
cig_allyears = rbind(cig_2015[,keepnames], cig_2016[,keepnames], cig_2017[,keepnames], cig_2018[,keepnames])

# only keep colums with data 
cig_allyears  = cig_allyears[,!apply(cig_allyears, 2, function (x) sum(!is.na(x)))==0]

#Make new author fields to go in spreadsheet
max.authors = max(sapply(cig_allyears$AUTHOR, function(x) {length(unlist(str_split(x, ",")))}))
allauthors = data.frame(nrow(cig_allyears), max.authors)
allauthors = t(do.call("cbind", lapply(str_split(cig_allyears$AUTHOR, " and "), "[", 1:max.authors)))
colnames(allauthors) = paste("AUTHOR_", 1:64, sep = "")
cig_allyears = cbind(cig_allyears, allauthors)
write.csv(cig_allyears, "cig_allyears.csv")
write.csv(table(allauthors), "cig_allauthors.csv")

# 2b. Read in bibtek files (newest) to make plot ----

# FIXED latest bibtek files based on "Instructions" column in cig_all_authors_JAC 2.csv (based on Lorraine's version of that file) except Li. P said "THROW THIS ONE OUT" why? I just changed it to Li, P.

# Lorrain's list for the analysis (added 2016 back in)
    # https://docs.google.com/document/d/1jCXNRsIAjVjS5Dz2PqwhZKlsodXaKSXbdz9jQArCHso/edit
    # "These should have the .bibs for the years 2010-2018."

cig_2015 = bib_2_df(file = "data/2015 bibtex_export v4.2.bib") #Higher version numbers reflect my final cleaning
cig_2016 = bib_2_df(file = "data/2016 bibtex_export v2.2.bib")
cig_2017 = bib_2_df(file = "data/2017 bibtex_export v5.2.bib")
cig_2018 = bib_2_df(file = "data/2018 bibtex_export v5.2.bib")

#Add dummy  columns to 2017 and 2018 for keywordds
optkeywordvars = names(cig_2015[grepl("OPTKEYWORDS", x = names(cig_2015))])
optkeywordvars[!optkeywordvars %in% names(cig_2017)]
optkeywordvars[!optkeywordvars %in% names(cig_2018)]

cig_2017 = cig_2017 %>% mutate("OPTKEYWORDS.2" = NA,
                               "OPTKEYWORDS.3" = NA,
                               "OPTKEYWORDS.4" = NA,
                               "OPTKEYWORDS.5" = NA)
cig_2018 = cig_2018 %>% mutate("OPTKEYWORDS.2" = NA,
                               "OPTKEYWORDS.3" = NA,
                               "OPTKEYWORDS.4" = NA,
                               "OPTKEYWORDS.5" = NA)

# Find common column names so we can rbind
allnames = c(names(cig_2015), names(cig_2016), names(cig_2017), names(cig_2018))
keepnames = names(table(allnames))[table(allnames)==4]

# use columns in all years
cig_allyears = rbind(cig_2015[,keepnames], cig_2016[,keepnames], cig_2017[,keepnames], cig_2018[,keepnames])
cig_allyears  = cig_allyears[,!apply(cig_allyears, 2, function (x) sum(!is.na(x)))==0]
write.csv(cig_allyears, "cig_allyears_v5.csv", row.names = F)

# Do we only want articles?
table(cig_allyears$CATEGORY)

# -  Clean author names  ----

# Author names always separted by " and "
split_names = sapply(cig_allyears$AUTHOR, strsplit, " and ")
split_names = sapply(split_names, trimws)
unique_names = unique(unlist(split_names))
  
# -  Use spreadsheet of fixed names to unify them ----
final_names = read.csv("cig_allauthors_JAC 2.csv", stringsAsFactors = F)
modified_names = filter(final_names, FinalName!="" |  Instructions!="" | Note.to.Self!="" )[,2:6]
#View(modified_names)

# Which names have replacements in final_names?
sum(unique_names %in%  modified_names$allauthors)
# View(filter(modified_names,  modified_names$allauthors %in% unique_names) %>%
#  filter(!duplicated(allauthors)))
modified_name_dict = (filter(modified_names,  modified_names$allauthors %in% unique_names) %>%
  filter(!duplicated(allauthors)))[,c("allauthors", "FinalName")]
modified_name_dict[modified_name_dict$FinalName == "", "FinalName"] =
  modified_name_dict[modified_name_dict$FinalName == "", "allauthors"]

# Replace names with modified as necessary
split_names2 = lapply(split_names, function(x) {x[x %in% modified_name_dict$allauthors] = 
  modified_name_dict$FinalName[modified_name_dict$allauthors %in% x]
  x})

# -  Convert to co-author network  ----

author.pairs = lapply(split_names2, function(x) { if(length(x) > 1) {t(combn(x, 2))} else {NULL}})
author.pairs = data.frame(do.call("rbind", author.pairs))
author.pairs$count = 1
author.pairs = aggregate(author.pairs$count, list(author.pairs$X1, author.pairs$X2), sum)
names(author.pairs)[3] = "co_authorships"
write.csv(author.pairs, "author_pairs_final.csv", row.names = F)
author.net = graph_from_data_frame(author.pairs, directed = F, 
                                   vertices = as.vector(unique(unlist(split_names2))))
plot(author.net)

# -  Make plot and add colors ----

# Special authors
V(author.net)$name_last = gsub(x = sapply(str_split(V(author.net)$name, " "), first), ",", "")
red = c("Bangerth", "Heister", "Gassm.*ller", "Dannberg") #Original Lead Developers
brown = c("Glerum", "Fraters", "Austermann", "Naliboff") #Added Principal Developers
color.aspect = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(author.net, "name")))]
color.aspect[V(author.net)$name_last%in% brown] = "Added Principal Developers"
red.rgb = "#990033"
brown.rgb = "#ff5050"
V(author.net)$Author_type =  color.aspect
V(author.net)$Papers = table(unlist(split_names2))[V(author.net)$name]
E(author.net)$co_authorships
  
author.plot = ggraph(author.net, layout = "kk") + 
  geom_edge_link(aes(width = co_authorships), alpha = 1, color = "gray") +
  # for some reason edge width not working like it doesn in the plots below
  scale_edge_width(name = "Number of co-authorships") + #, range=c(.2,2.8), breaks = c(1,2,3)) +
  geom_node_point(aes(fill = Author_type, size = Papers, text = name), shape = 21, stroke = .5, alpha = .5) + 
  scale_fill_manual(values =  c(red.rgb, brown.rgb, "white"), 
                    guide = guide_legend(title = "Author_type", order = 1)) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Co-author Relationships 2010 - 2018")

author.plot

# interactive plot instead of labels ----
# Too many nodes for labels

# plotly::ggplotly(author.plot) # can't do edges yet

#for visNetwork
V(author.net)$color = c(red.rgb, brown.rgb, rgb(0,0,0,.3))[as.factor(V(author.net)$Author_type)]
V(author.net)$title = V(author.net)$name
layoutMatrix1 = author.plot$data[,1:2]

author.visNet = visIgraph(author.net, layout = "layout.norm", idToLabel= TRUE, 
          layoutMatrix = as.matrix(layoutMatrix1)) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", hover = FALSE,
                                     degree = 1, hideColor = rgb(0,1,0,.05)),
             nodesIdSelection = list(enabled = TRUE, main = "Select author",
                                     values = sort(V(author.net)$title))) %>%
  visInteraction(dragNodes = TRUE, 
                 dragView = TRUE, 
                 zoomView = TRUE,
                 multiselect = TRUE)

  
author.visNet$x$main = list(text = "Co-author Relationships 2010 - 2018", style = "font-family: Arial, Helvetica, sans-serif;font-size:16px;text-align:center;")
#author.visNet$x$nodes$color = list(hover = "#000000")
warnings()
author.visNet

#---------------------------- Keyword plots  ------------------------------------------------

# package list ----
# i'll ignore case in searching for these
packagelist = c(
"ASPECT", #focus
"Ellipsis3d",
"SEISMIC.*CPML", #had to add slash to remove error
"AxiSEM",
"FLEXWIN",
"SELEN",
"Burnman",
"Gale", 
"SNAC",
"Calypso",
"MAG", 
"SPECFEM", #focus - anything starting with specfem*
"CitcomCU",
"CitcomS", # CitcomCU, CitcomS - seperate and together plot. In together plot code authors by whether they’re CU, S or both
"Citcom", # For together plot
"Mineos",
"SW4",
"PyLith", #focus
"Virtual Quake", #Change to Virtual Quake?
"ConMan",
"RELAX")

PyLith_tutorial= read_xlsx("~/Documents/DSI/cig_citation/data/Tutorial atttendees.xlsx", sheet = "Authors only")
names(PyLith_tutorial)

# subset plot ----
for (package in packagelist) {
  
  print(package)
  
  # subset data 
  subset_index = colSums(apply(cig_allyears %>% select(contains("OPTKEYWORDS")), 1, 
                               grepl, pattern = package, ignore.case = TRUE)) > 0
  
  split_names_sub = split_names2[subset_index]
  
  if (package == "Citcom") {
    subset_index1 = colSums(apply(cig_allyears %>% select(contains("OPTKEYWORDS")), 1, 
                                 grepl, pattern = "CitcomS", ignore.case = TRUE)) > 0
    split_names_sub1 = split_names2[subset_index1]
    
    subset_index2 = colSums(apply(cig_allyears %>% select(contains("OPTKEYWORDS")), 1, 
                                  grepl, pattern = "CitcomCU", ignore.case = TRUE)) > 0
    split_names_sub2 = split_names2[subset_index2]
    
    unique(unlist(split_names_sub2)) %in% unique(unlist(split_names_sub1))
    
    split_names_sub = split_names2[subset_index1 | subset_index2]
    
    
  }
  
  # subset net 
  author.pairs = lapply(split_names_sub, function(x) { if(length(x) > 1) {t(combn(x, 2))} else {NULL}})
  author.pairs = data.frame(do.call("rbind", author.pairs))
  author.pairs$count = 1
  author.pairs = aggregate(author.pairs$count, list(author.pairs$X1, author.pairs$X2), sum)
  names(author.pairs)[3] = "co_authorships"
  package.net = graph_from_data_frame(author.pairs, directed = F, 
                                     vertices = as.vector(unique(unlist(split_names_sub))))
  V(package.net)$Papers = table(unlist(split_names_sub))[V(package.net)$name]
  E(package.net)$co_authorships
  
  # if ASPECT, special authors/colors 
  if (package == "ASPECT") {
    V(package.net)$name_last = gsub(x = sapply(str_split(V(package.net)$name, " "), first), ",", "")
    red = c("Bangerth", "Heister", "Gassm.*ller", "Dannberg") #Original Lead Developers, gassmoller has weird     characters
    brown = c("Glerum", "Fraters", "Austermann", "Naliboff") #Added Principal Developers
    color.aspect = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(package.net, "name")))]
    color.aspect[V(package.net)$name_last%in% brown] = "Added Principal Developers"
    red.rgb = "#990033"
    brown.rgb = "#ff5050"
    V(package.net)$Author_type =  color.aspect
  }
  # if Citcom, special authors/colors
  if (package == "Citcom") {
    tmp = V(package.net)$name %in% unique(unlist(split_names_sub1)) 
    tmp[tmp] = "Both"
    tmp[V(package.net)$name %in% unique(unlist(split_names_sub2)) ] = "CitcomS"
    tmp[V(package.net)$name %in% unique(unlist(split_names_sub1)) & 
          V(package.net)$name %in% unique(unlist(split_names_sub2)) ] = "CitcomCU"
    V(package.net)$Author_type = tmp
  }
  if (package == "PyLith") {
    V(package.net)$name_last = gsub(x = sapply(str_split(V(package.net)$name, " "), first), ",", "")
    red = c("Aagaard", "Knepley", "Williams") #develoepr
    brown = PyLith_tutorial$Last
    color.aspect = c("Other", "Developer")[1 + rowSums(sapply(red, grepl, vertex_attr(package.net, "name")))]
    color.aspect[V(package.net)$name_last%in% brown & !V(package.net)$name_last%in% red] = "Participant"
    V(package.net)$Author_type = color.aspect
  }
  
  #fix package name for title in some cases
  if (package == "SEISMIC.*CPML") package = "SEISMIC_CPML" 
  if (package == "Virtual.*California") package = "Virtual California"
  
  # plot
  package.plot = ggraph(package.net, layout = "kk") + 
    geom_edge_link(aes(width = co_authorships), alpha = 1, color = "gray") +
    # for some reason edge width not working like it doesn in the plots below
    scale_edge_width(name = "Co-authorships",
                     range = c(.5, 2.8),
                     breaks = 1:max(E(package.net)$co_authorships) 
                     ) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)) +
    ggtitle(paste(package, "Co-author Relationships"))
  
  if (package == "ASPECT") {
    package.plot = package.plot + 
      geom_node_point(aes(fill = Author_type, size = Papers), color = "black", shape = 21,
                      stroke = .5, alpha = .5) + 
      scale_fill_manual(values =  c(red.rgb, brown.rgb, "white"),
                      guide = guide_legend(title = "Author type", order = 1))
  } else 
  if (package == "PyLith") {
      package.plot = package.plot + 
        geom_node_point(aes(fill = Author_type, size = Papers), color = "black", shape = 21,
                        stroke = .5, alpha = .5) + 
        scale_fill_manual(values =  c("darkred", "white", "pink"),
                          guide = guide_legend(title = "Author type", order = 1))
  } else 
  if (package == "Citcom"){
    package.plot = package.plot + 
      geom_node_point(aes(fill = Author_type, size = Papers), color = "black", shape = 21,
                      stroke = .5, alpha = .7) + 
      scale_fill_manual(values =  c("darkred", "white", "pink"), 
                        guide = guide_legend(title = "Author type", order = 1))
    
  } else {
    package.plot = package.plot + 
      geom_node_point(aes(size = Papers), color = "black", fill = "lightblue", shape = 21,
                    stroke = .5, alpha = .5)
  }
  
  #Adjust breaks for paper range
  breaks1 = 1:max( V(package.net)$Papers)
  if (max( V(package.net)$Papers) > 5) {
    breaks1 = round(seq(1, max( V(package.net)$Papers), length.out = 5))
  }
  
  package.plot = package.plot + scale_size(name = "Papers",
             range = c(1,6), 
             breaks = breaks1 )
  
ggsave(package.plot, filename = paste0("package_plots/", package, "_co_author_relationships", ".png"), width = 9)

package.plot = package.plot + geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, 
                                             size = 2, fontface = "bold")

ggsave(package.plot, filename = paste0("package_plots/LABELS_", package, "_co_author_relationships", ".png"), width = 9)


}


