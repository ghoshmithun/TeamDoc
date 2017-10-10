
transition_count <- read.table("C:/Users/mghosh/Desktop/WebPage_Transition_Count.txt",header=T,sep=",")
transition_count <- transition_count[,c('home',	'division',	'product','category',	'zoom',	'view_larger','quicklook','productimages','alt_view_prod_detail','rewards',	'signin','order_history',	'inlinebagadd',	'shopping_bag')]

colnames(transition_count)

row.names(transition_count) <- colnames(transition_count)

x<-as.matrix(transition_count)

require(igraph)

y<- graph_from_adjacency_matrix(x, mode = c( "undirected"), weighted = NULL, diag = TRUE,
                                add.colnames = NULL, add.rownames = NA)
plot(y)
y1 <- graph_from_adjacency_matrix(x, mode = c( "directed"), weighted = NULL, diag = TRUE,
                            add.colnames = NULL, add.rownames = NA)
plot.igraph(y1,edge.arrow.size=0.3,mark.groups=c("home"))

l <- layout.circle(y1)
plot(y1, layout=l)

l <- layout.fruchterman.reingold(y1, repulserad=vcount(y1)^3, 
                                 area=vcount(y1)^2.4)
#par(mfrow=c(1,2),  mar=c(0,0,0,0)) # plot two figures - 1 row, 2 columns
p<- list()
p[1]<-plot(y1, layout=layout.fruchterman.reingold)
p[2]<-plot.igraph(y1, layout=l,edge.arrow.size=0.3,)
pdf("C:\Users\mghosh\Box Sync\14.ClickpathAnalysis")
dev.off()


y2<- graph_from_adjacency_matrix(x, mode = c( "directed"), weighted = NULL, diag = TRUE,
                                 add.colnames = NULL, add.rownames = NA)
plot(y2, edge.arrow.size=.4)

install.packages("GGally")
library(GGally)
devtools::install_github("briatte/ggnet")
library(ggnet)
require(ggplot2)
library(network)
library(sna)
require(scales)

ggnet2(y2, color = "mode", palette = col, edge.size = "weights")

# Using Plotly

library(plotly)
packageVersion('plotly')
library(plotly)
library(igraph)


G <- upgrade_graph(y)
L <- layout.circle(G)

#Create Vertices and Edges
vs <- V(G)
es <- as.data.frame(get.edgelist(G))

Nv <- length(vs)
Ne <- length(es[1]$V1)

edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

p <- layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="karate-network-r")
chart_link