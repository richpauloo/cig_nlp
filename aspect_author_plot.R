# Code to make a network plot of ASPECT authors
# Jane Carlen, February 2019

# TO DO:
# more network analysis. Path lengths? Clustering coefficient? Over time?

# 1. Setup  ----

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

setwd("~/Documents/DSI/cig_citation/")

# 2. Aspect data  ----

authors = read_xlsx("protoNetworkA_jc_edited.xlsx", sheet = 3); head(authors)

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

# Change Aspect authors to agree with hack list used later


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
brown = c("Glerum", "Fraters", "Austermann") #Added Principal Developers
color.aspect = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(aspect.net, "name")))]
color.aspect[V(aspect.net)$name_last%in% brown] = "Added Principal Developers"

V(aspect.net)$Author_type =  color.aspect
V(aspect.net)$Papers = table(unlist(aspect.authors.list))[V(aspect.net)$name]
E(aspect.net)$co_authorships

author.plot = ggraph(aspect.net, layout = "kk") + 
  geom_edge_link(aes(width = co_authorships), alpha = 1, color = "gray") +
  # for some reason edge width not working like it doesn in the plots below
  scale_edge_width(name = "Number of co-authorships", range=c(.2,2.8), breaks = c(1,2,3)) +
  geom_node_point(aes(fill = Author_type, size = Papers), shape = 21, stroke = .5) + 
  scale_fill_manual(values = c("#654321", "red", "white"),
                    guide = guide_legend(title = "Author_type", order = 1)) + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  ggtitle("ASPECT Co-author Relationships")
author.plot

ggsave(plot = author.plot, "aspect_author_plot.png", height = 10, width  = 10)

# 3. Hack data  ----

hack = read_xlsx("protoNetworkA_jc_edited.xlsx", sheet = 4)[,1:7] #only care about hacks in first columns, not workships
dim(hack)

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
V(hack.net)$Hacks_attended = rowSums(hack[,3:7]=="X", na.rm = T)[rowSums(hack[,3:7]=="X", na.rm = T)>0]

plot(hack.net, edge.width = .3, edge.color = (hue_pal()(5))[as.numeric(as.factor(E(hack.net)$Relationship_type))], vertex.color = (hue_pal()(3))[as.numeric(as.factor(color.hack))], vertex.size = V(hack.net)$Hacks_attended)

# -  Make nice hack plot ----

hack.plot = ggraph(hack.net, layout = 'nicely') + 
  geom_edge_fan(aes(color = Relationship_type), width = .2) + #width = x
  geom_node_point(aes(fill = Author_type, size = Hacks_attended), shape = 21, stroke = .5)  +
  scale_fill_manual(values = c("#654321", "red", "gray"),
                    guide = guide_legend(title = "Author_type", order = 1)) + 
  #scale_edge_width(range = c(.1,1.5)) + <- put in if we do geom_edge instead
  scale_size(range = c(4,9)) + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
  ggtitle("Co-hack Attendance Relationships")
hack.plot

ggsave(plot = hack.plot, "hack_plot.png", height = 10, width  = 14)

# 4. Make combined net ----

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

V(geo.net)$name_last = sapply(str_split(V(geo.net)$name, " "), last)
# Special authors
# see "red" and "brown" created above
color2 = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, V(geo.net)$name))]
color2[rowSums(sapply(brown, grepl, vertex_attr(geo.net, "name"))) == 1] = "Added Principal Developers"
color2[V(geo.net)$name_last  %in% brown] = "Added Principal Developers"

V(geo.net)$Author_type =  color2
V(geo.net)$Papers = table(unlist(aspect.authors.list))[V(geo.net)$name] 
V(geo.net)$Papers = replace_na(V(geo.net)$Papers, replace = 0)

geo.plot = ggraph(geo.net, layout = 'kk') + 
  geom_edge_fan(aes(color = Relationship_type, linetype = Relationship_type,
                    width = Number_of_relationship_type), alpha = .7) +
  scale_edge_width(range = c(.2,1)) +
  geom_node_point(aes(fill = Author_type, size = Papers), shape = 21, stroke = .5)  + 
  scale_fill_manual(values = c("#654321", "red", "white"),
                    guide = guide_legend(title = "Author_type", order = 1)) + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + 
  ggtitle("Co-hack and ASPECT Co-author Relationships")

geo.plot

ggsave(plot = geo.plot, "author_and_hack_plot.png", height = 10, width  = 14)

# 5. Centrality measures ####

# Preliminary work, need to check how these igraph measures account for disconnected components, valued edges and mutli-edges

#Change the net and title for each of the three plots generated
net = geo.net; title1 = "Hack_and_Author_Network_Centrality_Measures"

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
  
