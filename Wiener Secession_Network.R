# install packages
install.packages("flextable")
install.packages("GGally")
install.packages("ggraph")
install.packages("igraph")
install.packages("Matrix")
install.packages("network")
install.packages("quanteda")
install.packages("sna")
install.packages("tidygraph")
install.packages("tidyverse")
install.packages("tm")
install.packages("tibble")
install.packages("quanteda.textplots")
install.packages("gutenbergr")

# activate packages
library(flextable)
library(GGally)
library(ggraph)
library(gutenbergr)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
library(readxl)


net_dat <- read_excel("/Users/rebekaerdo/Documents/Digital Humanities/2024 Sommer/Data Analysis Project/Dataset/Wien_Secession_Netzwerk.xlsx")
View(net_dat)

net_cmx <- crossprod(table(net_dat[3:2]))
diag(net_cmx) <- 0
net_df <- as.data.frame(net_cmx)
View(net_df)

# create a document feature matrix
net_dfm <- quanteda::as.dfm(net_df)
head(net_dfm)
# create feature co-occurrence matrix
net_fcm <- quanteda::fcm(net_dfm, tri = F)
# inspect data
head(net_fcm)

quanteda.textplots::textplot_network(
  x = net_fcm,                    # a fcm or dfm object
  min_freq = 0.5,                   # frequency count threshold or proportion for co-occurrence frequencies (default = 0.5)
  edge_alpha = 0.5,                 # opacity of edges ranging from 0 to 1.0 (default = 0.5)
  edge_color = "gray",            # color of edges that connect vertices (default = "#1F78B4")
  edge_size = 2,                    # size of edges for most frequent co-occurrence (default = 2)
  # calculate the size of vertex labels for the network plot
  vertex_labelsize = net_dfm %>%
    # convert the dfm object to a data frame
    quanteda::convert(to = "data.frame") %>% 
    # exclude the 'doc_id' column
    dplyr::select(-doc_id) %>%
    # calculate the sum of row values for each row
    rowSums() %>%
    # apply the natural logarithm to the resulting sums
    log(),
  vertex_color = "#4D4D4D",         # color of vertices (default = "#4D4D4D")
  vertex_size = 2                   # size of vertices (default = 2)
)
# create a new data frame 'va' using the 'net_dat' data
net_dat %>%
  # rename the 'person' column to 'node' and 'occurrences' column to 'n'
  dplyr::rename(node = Person,
                n = Occurences,
                nat = Nationality,
                year = Year) %>%
  # group the data by the 'node' column
  dplyr::group_by(node) %>%
  # summarize the data, calculating the total occurrences ('n') for each 'node'
  dplyr::summarise(n = sum(n), nat = first(nat)) -> va

View(net_dat)

# create a new data frame 'ed' using the 'dat' data
ed <- net_df %>%
  # add a new column 'from' with row names
  dplyr::mutate(from = rownames(.)) %>%
  # reshape the data from wide to long format using 'gather'
  tidyr::gather(to, n, 'Adler, Angela': 'Zwickle, Hubert von') %>%
  # remove zero frequencies
  dplyr::filter(n > 0)

# Identify nodes that appear in the filtered edge list
filtered_nodes <- unique(c(ed$from, ed$to))

# Filter 'va' to keep only nodes that are in 'filtered_nodes'
va <- va %>% 
  dplyr::filter(node %in% filtered_nodes)

ig <- igraph::graph_from_data_frame(d=ed, vertices=va, directed = FALSE)

tg <- tidygraph::as_tbl_graph(ig) %>% 
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=name)

# set seed (so that the exact same network graph is created every time)
set.seed(12345)

# create a graph using the 'tg' data frame with the Fruchterman-Reingold layout
tg %>%
  ggraph::ggraph(layout = 'kk') +
  
  # Add arcs for edges with color according to year
  geom_edge_arc(colour = "grey",
                lineend = "round",
                strength = 0.2,
                aes(edge_width = ed$n,
                    alpha = ed$n)) +
  
  # Add points for nodes with size based on log-transformed 'n' and color based on nationality
  geom_node_point(aes(color = va$nat), size = log(va$n) * 2) +
  
  # add text labels for nodes with various aesthetics
  geom_node_text(aes(label = name),
                 repel = TRUE, 
                 point.padding = unit(0.2, "lines"), 
                 size = sqrt(va$n), 
                 colour = "black") +
  
  # Adjust edge and node color scales
  scale_edge_width(range = c(0.2, 2.5)) +
  scale_edge_alpha(range = c(0.2, .3)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(va$nat)), name = "Set2")) +
  
  # set graph background color to white
  theme_graph(background = "white") +
  
  # adjust legend position to the top
  theme(legend.position = "top", 
        # suppress legend title
        legend.title = element_blank()) +
  
  # remove edge width and alpha guides from the legend
  guides(edge_width = FALSE,
         edge_alpha = FALSE)

# Calculate betweenness centrality
V(ig)$betweenness <- igraph::betweenness(ig, directed = FALSE)
V(ig)$betweenness #30.93

# Calculate closeness centrality
V(ig)$closeness <- igraph::closeness(ig, normalized = TRUE)
V(ig)$closeness #1

# Calculate degree centrality
V(ig)$degree <- igraph::degree(ig, mode = "all")
V(ig)$degree #36

# Get the node with the highest betweenness centrality
top_betweenness_node <- which.max(V(ig)$betweenness)
top_betweenness <- V(ig)$name[top_betweenness_node]

# Get the node with the highest closeness centrality
top_closeness_node <- which.max(V(ig)$closeness)
top_closeness <- V(ig)$name[top_closeness_node]

# Get the node with the highest degree centrality
top_degree_node <- which.max(V(ig)$degree)
top_degree <- V(ig)$name[top_degree_node]

# Print the results
cat("Node with highest betweenness centrality: ", top_betweenness, "\n")
cat("Node with highest closeness centrality: ", top_closeness, "\n")
cat("Node with highest degree centrality: ", top_degree, "\n")
