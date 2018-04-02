install.packages("networkD3")
install.packages("data.tree")
library(networkD3)
library(data.tree)

nodes <- read.csv("~/Desktop/web_community/nodes.csv", header=T, as.is=T)
links <- read.csv("~/Desktop/web_community/trophic.links.csv", header=T, as.is=T)


net <- data.frame(links,nodes, directed = T)

simpleNetwork(links)


#not working yet
forceNetwork(Links = links, Nodes = nodes, Source = "resource", Target = "consumer",
             Value = "direction", NodeID = "node",
             Group = "group", opacity = 0.8)

sankeyNetwork(Links = links, Nodes = nodes, Source = "resource",
              Target = "consumer", Value = "strength", NodeID = "node",
              units = "NA", fontSize = 12, nodeWidth = 30)
## end non-working section


# collapsible tree plots as form of heirarchy plot
# Install package from CRAN:
install.packages("collapsibleTree")
library(collapsibleTree)

collapsibleTree(
  nodes,
  hierarchy = c("group", "node"),
  width = 800
)


collapsibleTree(links,
                hierarchy = c("resource","consumer"),
                width =800)
