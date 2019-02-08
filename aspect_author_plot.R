# Code to make a network plot of ASPECT authors
# Jane Carlen, February 2019

# 1. Load packages  ----

library(readxl)
library(bibtex)
library(stringr)
library(network)
library(igraph)
library(ggraph)

# 2. Load data  ----
setwd("~/Documents/DSI/cig_citation/")
authors = read_xlsx("protoNetworkA.xlsx", sheet = 3); head(authors)

# 3. Clean author names  ----
aspect.bib = read.bib("bibtex_export only pubs.bib")
aspect.authors.last = lapply(aspect.bib, function(x) as.character(x$author$family)); length(unique(unlist(aspect.authors.last)))
aspect.authors.list = lapply(aspect.bib, function(x) as.character(x$author)); length(unique(unlist(aspect.authors.list))) 
# two authors get grouped when we use last name only
# From checking excelt, S. Zhang != N. Zhang,  but ch C. O\\textquoterightNeill == C. J. O\\textquoterightNeill ?
aspect.authors.list = sapply(aspect.authors.list, function(x) {x[str_detect(pattern = "Neill", x)] = "C. O'Neill"; x}) #now 60 unique

# 4. Convert to co-author network  ----
author.pairs = lapply(aspect.authors.list, function(x) { if(length(x) > 1) {t(combn(x, 2))} else {NULL}})
author.pairs = data.frame(do.call("rbind", author.pairs))
author.pairs$count = 1
author.pairs = aggregate(author.pairs$count, list(author.pairs$X1, author.pairs$X2), sum)
author.net = graph_from_data_frame(author.pairs, directed = F)
plot(author.net)

# 5. View plot and add colors ----

# Special authors
red = c("Bangerth", "Heister", "Gassmöller", "Dannberg")
brown = c("Glerum", "Fraters", "Austermann")
color1 = c("Other", "Original Lead Developers")[1 + rowSums(sapply(red, grepl, vertex_attr(author.net, "name")))]
color1[rowSums(sapply(brown, grepl, vertex_attr(author.net, "name"))) == 1] = "Added Principal Developers"
V(author.net)$color =  color1
V(author.net)$papers = degree(author.net)

author.plot = ggraph(author.net, layout = "auto") + 
  geom_edge_link(color = "gray", aes(width = x)) +
  # edge width for number of co-authorships -- why do 1-3 look so similar in legend?
  scale_edge_width(name = "Edge width (co-authorships)",
                   range = c(.5,1.5), breaks = c(0.5,1,1.5), labels = c("1", "2", "3")) +
  geom_node_point(aes(color = color, size = papers)) + 
  scale_color_discrete(name = "Author type") + 
  geom_node_text(aes(label = name), repel = TRUE, segment.alpha = .5) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
author.plot

ggsave("aspect_author_plot.png", height = 10, width  = 10)

# for next thursday
# hackathon relationships
# reflect number of hackathons