###########################
# Network plotting script #
# Author:       Dillon O  #
# Last updated: May 2018  #
###########################

# INSTALL PACKAGES
# install.packages("igraph")
# install.packages("network")
# install.packages("sna")
# install.packages("ndtv")

# LOAD PACKAGES
library(igraph)   # For reading 
library(network)  # For
library(sna)      # For
library(ndtv)     # For

# READ DATA

## Read whole community
# test.community <- LoadCommunity('web_community')

## Read data piece by piece
nodes <- read.csv("Desktop/food-web-metaanalysis/data/nodes.csv", header=T, as.is=T)
links <- read.csv("Desktop/food-web-metaanalysis/data/trophic.links.csv", header=T, as.is=T)

## Turn links data into a directed network
net <- graph.data.frame(links, nodes, directed=T)
net
#DN = directed named graph
#numbers following DN--- are nodes and edges

E(net) # The edges of the "net" object
V(net) # The vertices of the "net" object
E(net)$strength # Edge attribute "links"
V(net)$group # Vertex attribute "nodes"

plot(net) # not a pretty picture!

## Function to wrap long strings
# Source: http://stackoverflow.com/a/7367534/496488
wrap_strings <- function(vector_of_strings,width){
  as.character(sapply(vector_of_strings, FUN=function(x){
    paste(strwrap(x, width=width), collapse="\n")
  }))
}

# Apply the function to wrap the node labels
V(net)$label= wrap_strings(V(net)$label, 12)

## Shrink font
V(net)$group.cex = 0.8

# Function to increase node separation (for explanatory details, see the link below)
# Source: http://stackoverflow.com/a/28722680/496488
layout.by.attr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  net <- graph.edgelist(get.edgelist(net)) # create a lightweight copy of graph w/o the attributes.
  E(net)$weight <- 1
  V(net)$size <- deg*3
  
  attr <- cbind(id=1:vcount(net), val=wc)
  net <- net + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  
  l <- layout(net, weights=E(net)$weight)[1:vcount(net),]
  return(l)
}
plot(net, vertex.shape="circle",edge.arrow.size=.4)

#USE TKPLOT TO SET GRAPH NODE COORDINATES. USE MCoords (saves those coordinates) in future plotting commands to use the layout
capabilities("tcltk")
system("ls -ld /usr/local /usr/local/lib /usr/local/lib/libtcl*")

Coord <- tkplot(net, vertex.size=3, 
                edge.arrow.size=0.5, edge.color="black")
MCoords <- tkplot.getcoords(Coord)
plot(net, layout=MCoords, vertex.size=5, 
      edge.arrow.size=0.5, edge.color="black")


#back to simplified net

net <- simplify(net, remove.multiple = F, remove.loops = T)

plot(net, edge.arrow.size=.4,vertex.label=NA)

#colors
# If you don't have R ColorBrewer already, you will need to install it:
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

display.brewer.pal(8, "Spectral")

#back to plot

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1)


# Generate colors base on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$group.type]
deg <- links$ref
V(net)$size <- deg*3

# Set edge width based on weight:
E(net)$width <- E(net)$strength/6

#change arrow size and edge color:
E(net)$arrow.size <- .2 
E(net)$edge.color <- "gray80" 
E(net)$width <- 1+E(net)$weight/12
plot(net)



#legend
plot(net)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)
plot(net, edge.arrow.size=.4, edge.curved=.1)
#this will start coloring lines (right now based on origin, but could use if duplicate links based on direction...)
edge.col=ifelse(E(net)$direction > 0, "blue","red")


plot(net, edge.color=edge.col, edge.curved=.1,layout=MCoords)


## new plot with spread nodes, colored lines, and legend
plot(net, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

#network layouts
l <- layout.circle(net) 
plot(net, layout=l)

#Fruchterman-Reingold is one of the most used force-directed layout algorithms out there.
#Force-directed layouts try to get a nice-looking graph where edges are similar in length and cross each other as little as possible. They simulate the graph as a physical system. Nodes are electrically charged particles that repulse each other when they get too close. The edges act as springs that attract connected nodes closer together. As a result, nodes are evenly distributed through the chart area, and the layout is intuitive in that nodes which share more connections are closer to each other.

l <- layout.fruchterman.reingold(net, repulserad=vcount(net)^3, area=vcount(net)^2.4)
par(mfrow=c(1,2), mar=c(0,0,0,0)) # plot two figures - 1 row, 2 columns 
plot(net, layout=layout.fruchterman.reingold)
plot(net, layout=l)

dev.off() # shut off the graphic device to clear the two-figure configuration.


layouts <- grep("^layout\\.", ls("package:igraph"), value=TRUE)
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama", layouts)]
par(mfrow=c(3,3))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(net))
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }
dev.off()


hist(links$strength)
mean(links$strength) 
sd(links$strength)

#this isnt working yet
E(net)$width <- 1.5
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$group=="management")+1],
     vertex.color="gray40", layout=layout.circle)

# Community detection based on label propagation:
clp <- cluster_label_prop(net)
class(clp)
# Community detection returns an object of class "communities" # which igraph knows how to plot:
plot(clp, net)


dist.from.carbon <- distances(net, v=V(net)[nodes=="carbon"], to=V(net), weights=NA)
# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.carbon)+1)
col <- col[dist.from.carbon+1]
plot(net, vertex.color=col, vertex.label=dist.from.carbon, edge.arrow.size=.6, vertex.label.color="white")


#heatmap work
netm <- get.adjacency(net, attr="strength", sparse=F) 
colnames(netm) <- V(net)
rownames(netm) <- V(net)

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,32:1], Rowv = NA, Colv = NA, col = palf(100),
       scale="none", margins=c(10,10) )
     

# 2 level newtork plot reingold tilford (currently too condensed)
co <- layout.reingold.tilford(net, root='grazing')
plot(net, layout=co)


## plotting sub networks as multiple panels in fig (first is complete net, following are sub)
#adjust links values first
# Set edge width based on weight:
links$width <- links$strength/6

#change arrow size and edge color:
links$arrow.size <- .2 
links$edge.color <- "gray80" 
links$width <- 1+links$weight/12

edge.col=ifelse(links$direction > 0, "blue","red")



# create subnets
graze <- subset(links, resource == "grazing")
subnet1 <- graph.data.frame(graze, directed=T)

tree <- subset(links, resource == "tree_shrub_presence")
subnet2 <- graph.data.frame(tree, directed=T)

grazetree <- subset(links, resource == "grazing_tree_shrub_presence")
subnet3 <- graph.data.frame(grazetree, directed=T)

riparian <- subset(links, resource == "riparian_restoration")
subnet4 <- graph.data.frame(riparian, directed=T)

composition <- subset(links, resource == "grassland_composition")
subnet5 <- graph.data.frame(composition, directed=T)

hedge <- subset(links, resource == "hedgerow_planting")
subnet6 <- graph.data.frame(hedge, directed=T)

orgamend <- subset(links, resource == "organic_amendment")
subnet7 <- graph.data.frame(orgamend, directed=T)

# Generate colors base on media type for subnets:
links$group.type <- V(subnet1)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet1, mode="all")
V(subnet1)$size <- deg*3

# Generate colors base on media type for subnets:
links$group.type <- V(subnet2)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet2, mode="all")
V(subnet2)$size <- deg*3

# Generate colors base on media type for subnets:
links$group.type <- V(subnet3)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet3, mode="all")
V(subnet3)$size <- deg*3

# Generate colors base on media type for subnets:
links$group.type <- V(subnet4)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet4, mode="all")
V(subnet4)$size <- deg*3

# Generate colors base on media type for subnets:
links$group.type <- V(subnet5)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet5, mode="all")
V(subnet5)$size <- deg*3

# Generate colors base on media type for subnets:
links$group.type <- V(subnet6)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet6, mode="all")
V(subnet6)$size <- deg*3


# Generate colors base on media type for subnets:
links$group.type <- V(subnet7)$group.type
colrs <- c("gray50", "tomato", "gold")
links$color <- colrs[V(net)$group.type]
deg <- degree(subnet7, mode="all")
V(subnet7)$size <- deg*3



par(mfrow=c(4,2), mar=c(0,0,0,0)) # plot two figures - 4 rows, 2 columns 

plot(net, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet1, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet2, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet3, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet4, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet5, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet6, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 
plot(subnet7, vertex.shape="circle", edge.arrow.size=.4, edge.color=edge.col,edge.curved=.1)
legend(x=-1.5, y=-1.1, c("management","soil_property", "outcome"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1) 

dev.off() # shut off the graphic device to clear the two-figure configuration.

