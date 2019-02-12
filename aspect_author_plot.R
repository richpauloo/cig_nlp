# Code to make a network plot of ASPECT authors
# Jane Carlen, February 2019

# We can add number of hacks attended as a color in the network plot
# for next thursday:
# hackathon relationships
# reflect number of hackathons

# 1. Load packages  ----

library(readxl)
library(bibtex)
library(stringr)
library(network)
library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)

# 2. Load data  ----
setwd("~/Documents/DSI/cig_citation/")
authors = read_xlsx("protoNetworkA.xlsx", sheet = 3); head(authors)

# 3. Clean author names  ----
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


# 4. Convert to co-author network  ----
author.pairs = lapply(aspect.authors.list, function(x) { if(length(x) > 1) {t(combn(x, 2))} else {NULL}})
author.pairs = data.frame(do.call("rbind", author.pairs))
author.pairs$count = 1
author.pairs = aggregate(author.pairs$count, list(author.pairs$X1, author.pairs$X2), sum)
aspect.net = graph_from_data_frame(author.pairs, directed = F, vertices = as.vector(unique(unlist(aspect.authors.list))))
plot(aspect.net)

# 5. View plot and add colors ----

# Special authors
red = c("Bangerth", "Heister", "Gassmöller", "Dannberg")
brown = c("Glerum", "Fraters", "Austermann")
color1 = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(aspect.net, "name")))]
color1[V(aspect.net)$name_last  %in% brown] = "Added Principal Developers"

V(aspect.net)$color =  color1
V(aspect.net)$Papers = table(unlist(aspect.authors.list))[V(aspect.net)$name]
V(aspect.net)$name_last = sapply(str_split(V(aspect.net)$name, " "), last)
#write.csv(cbind(V(aspect.net)$name, V(aspect.net)$name_last), "~/Documents/DSI/cig_citation/authors_aspect.csv", row.names = F)

author.plot = ggraph(aspect.net, layout = "auto") + 
  geom_edge_link(color = "gray", aes(width = x)) +
  # edge width for number of co-authorships -- why do 1-3 look so similar in legend?
  scale_edge_width(name = "Edge width (co-authorships)",
                   range = c(.5,1.5), breaks = c(0.5,1,1.5), labels = c("1", "2", "3")) +
  geom_node_point(aes(color = color, size = Papers)) + 
  scale_color_discrete(name = "Author type") + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
author.plot

ggsave("aspect_author_plot.png", height = 10, width  = 10)

# 6. Add hack information ----

hack = read_xlsx("protoNetworkA.xlsx", sheet = 4)[,1:7] #only care about hacks in first columns, not workships
dim(hack)

hack = hack %>% mutate(author = sapply(strsplit(X__1,","), first),
                       author_last = sapply(str_split(str_trim(author), " "), last),
                       name = paste(str_extract(author, "[A-Z]{1}"), ". ", author_last, sep = ""), #formatting like aspect authors
                       affiliation = str_trim(sapply(strsplit(hack$X__1,","), nth, 2)),
                       affiliation = str_replace(affiliation, "UC", "University of California"),
                       affiliation = str_replace(affiliation, "GFZ-Potsdam", "GFZ Potsdam"),
                       affiliation = str_replace(affiliation, "Clemson$", "Clemson University"))


#    Fix hack names to agree with Aspect names as necessary ----
# some last names in the hack authors appear more than once, but they're all unique authors with unique first initial
hack$name = str_replace(hack$name, pattern = "Gassmoeller", replace = "Gassmöller")
hack$name = str_replace(hack$name, pattern  = "Huettig", replace = "Hüttig")
hack$name = str_replace(hack$name, pattern  = "E. Puckett", replace = "E. G. Puckett")
hack$name = str_replace(hack$name, pattern  = "J. Robey", replace = "J. M. Robey")
hack$name = str_replace(hack$name, pattern  = "L. Kellogg", replace = "L. H. Kellogg")
hack$name = str_replace(hack$name, pattern  = "S. Cox", replace = "S. P. Cox")
hack$name_last = sapply(str_split(hack$name, " "), last)
hack$name[hack$name %in% V(aspect.net)$name]

#    make hack net ----

hack.pairs = apply(hack[,3:7], 2, function(x) {
  t(combn(hack$name[!is.na(x)], 2))
})
hack.pairs = data.frame(do.call("rbind", hack.pairs), year = rep(c("hack_2014","hack_2015","hack_2016","hack_2017", "hack_2018"), times = sapply(hack.pairs, nrow)))
hack.pairs$count = 1
hack.pairs = aggregate(hack.pairs$count, list(hack.pairs$X1, hack.pairs$X2, hack.pairs$year), sum)

hack.net = graph_from_data_frame(hack.pairs, directed = F) #don't include all names because only ones in pairs were at a hack
plot(hack.net, edge.width = .3)

#    make nice hack plot (?)----

# 7. Make combined net ----

author_hack.pairs = cbind( rbind(author.pairs, hack.pairs[,c("Group.1", "Group.2", "x")]),  
                           Relationship_type = c(rep("co-authorship", nrow(author.pairs)), as.character(hack.pairs$Group.3)))
geo.net = graph_from_data_frame(author_hack.pairs, vertices = unique(c(V(hack.net)$name, V(aspect.net)$name)), directed = F)
V(geo.net)$name_last = sapply(str_split(V(geo.net)$name, " "), last)
plot(geo.net, edge.size = .5, edge.curve = TRUE:FALSE, edge.color = c("gray", "red"))

#    Make combined plot nicer ----

# Special authors
# see "red" and "brown" created above
color2 = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, V(geo.net)$name))]
color2[rowSums(sapply(brown, grepl, vertex_attr(geo.net, "name"))) == 1] = "Added Principal Developers"
color2[V(geo.net)$name_last  %in% brown] = "Added Principal Developers"

V(geo.net)$Author_type =  color2
V(geo.net)$Papers = table(unlist(aspect.authors.list))[V(geo.net)$name] 
V(geo.net)$Papers = replace_na(V(geo.net)$Papers, replace = 0)

geo.plot = ggraph(geo.net, layout = 'kk') + 
  geom_edge_fan(width = .3, aes(color = Relationship_type)) +
  geom_node_point(aes(fill = Author_type, size = Papers), shape = 21, stroke = .5)  + 
  scale_fill_manual(values = c("black", "red", "gray")) + 
  #scale_edge_width(name = "Edge width", range = c(.1,1), breaks = c(0.5, 1, 1.5, 2, 2.5), labels = c("1", "2", "3", "4", "5")) + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5, fontface = "bold") + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
geo.plot

ggsave("geo_plot.png", height = 10, width  = 14)
