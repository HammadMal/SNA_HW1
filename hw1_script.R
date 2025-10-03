  install.packages("igraph")
  library(igraph)
  install.packages("leaflet")
  library(leaflet)
  
  g1 = read_graph("karachi_bus_network.graphml", format = "graphml")
  summary(g1)
  V(g1)$color= "blue"

  
  
  num_vertices = vcount(g1)
  network_density = round(edge_density(g1), 4)
  avg_path_length = round(mean_distance(g1), 2)
  network_diameter = diameter(g1)
  
  # print(num_vertices)
  # print(num_edges)
  # print(network_density)
  # print(avg_path_length)
  # print(network_diameter)
  
  global_clustering = round(transitivity(g1, type="global"), 4)
  local_clustering = round(transitivity(g1, type="average"), 4)
  
  # print(local_clustering)
  
  
  deg = degree(g1)
  btw = betweenness(g1)
  cls = closeness(g1, normalized = TRUE)
  print(cls)
  # print(deg)
  # print(btw)
  # print(cls)
  
  
  components_info = components(g1)
  num_components = components_info$no
  largest_component = max(components_info$csize)
  
  #Formatting and print suggestions adapted from Claude Sonnet 4
  # (Anthropic, 2024), conversation on 9/27/2025
  
  print(paste("\n=== BASIC NETWORK METRICS ==="))
  print(paste("Number of Vertices:", num_vertices))
  print(paste("Number of Edges:", num_edges))
  print(paste("Network Density:", network_density))
  print(paste("Average Path Length:", avg_path_length))
  print(paste("Network Diameter:", network_diameter))
  
  print(paste("\n=== CENTRALITY MEASURES ==="))
  print(paste("Mean Degree:", mean(deg)))
  print(paste("Max Degree:", max(deg)))
  print(paste("Mean Betweenness:", mean(btw)))
  print(paste("Max Betweenness:", max(btw)))
  print(paste("Mean Closeness:", mean(cls)))
  print(paste("Max Closeness:", max(cls)))
  
  print(paste("\n=== CLUSTERING ANALYSIS ==="))
  print(paste("Global Clustering:", global_clustering))
  print(paste("Local Clustering:", local_clustering))
  
  print(paste("\n=== CONNECTIVITY ANALYSIS ==="))
  print(paste("Number of Components:", num_components))
  print(paste("Largest Component Size:", largest_component))
  
  
  #Degree Distribution Analysis
  deg_dist = degree_distribution(g1)
  mean_deg = mean(deg)
  median_deg = median(deg)
  
  # print(deg_dist)
  # print(mean_deg)
  # print(median_deg)
  
  print(paste("\n\n=== DEGREE DISTRIBUTION ANALYSIS ==="))
  print(paste("Mean Degree", mean_deg))
  print(paste("Mean Degree:", mean_deg))
  print(paste("Median Degree:", median_deg))
  print(paste("Degree Range:", min(deg), "to", max(deg)))
  
  
  #Degree Centrality
  print("\n=== TOP 10 NODES BY DEGREE CENTRALITY ===")
  top_deg = head(sort(deg, decreasing = TRUE), 10)
  print(top_deg)
  
  # Betweenness Centrality: 
  print("\n=== TOP 10 NODES BY BETWEENNESS CENTRALITY ===")
  top_btw = head(sort(btw, decreasing = TRUE), 10)
  print(top_btw)
  
  #Closeness Centrality
  print("\n=== TOP 10 NODES BY CLOSENESS CENTRALITY ===")
  top_cls = head(sort(cls, decreasing = TRUE), 10)
  print(top_cls)
  
  
  #Eigenvector centrality
  eigen_cent = eigen_centrality(g1)$vector
  print("\n=== EIGENVECTOR CENTRALITY STATISTICS ===")
  print(paste("Mean Eigenvector Centrality:", round(mean(eigen_cent), 4)))
  print(paste("Max Eigenvector Centrality:", round(max(eigen_cent), 4)))
  
  
  print("\n=== TOP 10 NODES BY EIGENVECTOR CENTRALITY ===")
  top_eigen = head(sort(eigen_cent, decreasing = TRUE), 10)
  print(top_eigen)
  
  
  # PageRank Centrality
  PR = page.rank(g1)$vector
  top.pagerank = head(sort(PR, decreasing = TRUE),10)
  top.pagerank

  
  
  #Community Detection
  communities_louvain = cluster_louvain(as.undirected(g1))
  communities_walktrap = cluster_walktrap(as.undirected(g1))
  
  print("\n=== COMMUNITY DETECTION ===")
  print(paste("Number of Communities (Louvain):", length(communities_louvain)))
  print(paste("Modularity (Louvain):", round(modularity(communities_louvain), 4)))
  print(paste("Number of Communities (Walktrap):", length(communities_walktrap)))
  print(paste("Modularity (Walktrap):", round(modularity(communities_walktrap), 4)))
  
  
  #Visualizations
  
  #Geographic Distribution
  
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lng = V(g1)$longi, 
      lat = V(g1)$lati,
      radius = 5,  
      color = "blue",
      fillColor = "blue",
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2
    ) %>%
    setView(lng = mean(V(g1)$longi), 
            lat = mean(V(g1)$lati), 
            zoom = 12)
  
  
  # Community Structure Visualization
  dev.new()
  plot(communities_louvain, 
       as.undirected(g1),
       vertex.label = NA,
       vertex.size = 5,
       edge.arrow.size = 0.3,
       main = "Community Structure (Louvain Method)")
  
  #Walk trap
  dev.new()
  plot(communities_walktrap, 
       as.undirected(g1),
       vertex.label = NA,
       vertex.size = 5,
       edge.arrow.size = 0.3,
       main = "Community Structure (Walktrap Method)")
  
  
  # Degree distribution histogram
  dev.new()
  hist(deg, breaks=30, main="Degree Distribution", 
       xlab="Degree", ylab="Frequency", col="lightblue")
  
  # Degree Centrality Visualization
  dev.new()
  deg_colors = colorRampPalette(c("white", "red"))(100)
  deg_rank = cut(deg, breaks = 100, labels = FALSE)
  V(g1)$deg_color = deg_colors[deg_rank]
  
  
  plot(g1,
       vertex.size = 12 * sqrt(deg/max(deg)),
       vertex.color = V(g1)$deg_color,
       vertex.label = V(g1)$name,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(g1),
       main = "Karachi Bus Network\n(Node size proportional to degree)")
  
  
  legend("topright", 
         legend = c("High", "Medium", "Low"),
         pt.cex = c(1.5, 1, 0.7),
         pch = 21,
         pt.bg = c("red", "orange", "white"),
         title = "Degree",
         bty = "n",
         cex = 0.8)
  
  
  # Betweenness Centrality Visualization
  dev.new()
  btw_colors = colorRampPalette(c("white", "yellow", "orange"))(100)
  btw_rank = cut(btw, breaks = 100, labels = FALSE)
  V(g1)$btw_color = btw_colors[btw_rank]
  
  plot(g1,
       vertex.size = 12 * sqrt(btw/max(btw)),
       vertex.color = V(g1)$btw_color,
       vertex.label = NA,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(g1),
       main = "Karachi Bus Network\n(Node size proportional to betweenness centrality)")
  
  legend("topright", 
         legend = c("High", "Medium", "Low"),
         pt.cex = c(1.5, 1, 0.7),
         pch = 21,
         pt.bg = c("orange", "yellow", "white"),
         title = "Betweenness",
         bty = "n",
         cex = 0.8)
  
  
  # Eigenvector Centrality Visualization
  dev.new()
  eigen_colors = colorRampPalette(c("white", "lightgreen", "green", "darkgreen"))(100)
  eigen_rank = cut(eigen_cent, breaks = 100, labels = FALSE)
  V(g1)$eigen_color = eigen_colors[eigen_rank]
  
  plot(g1,
       vertex.size = 12 * sqrt(eigen_cent/max(eigen_cent)),
       vertex.color = V(g1)$eigen_color,
       vertex.label = NA,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(g1),
       main = "Karachi Bus Network\n(Node size proportional to eigenvector centrality)")
  
  legend("topright", 
         legend = c("High", "Medium", "Low"),
         pt.cex = c(1.5, 1, 0.7),
         pch = 21,
         pt.bg = c("darkgreen", "green", "lightgreen"),
         title = "Eigenvector",
         bty = "n",
         cex = 0.8)
  
  
  # PageRank Centrality Visualization
  dev.new()
  PR_colors = colorRampPalette(c("lightblue", "blue", "darkblue"))(100)
  PR_rank = cut(PR, breaks = 100, labels = FALSE)
  V(g1)$PR_color = PR_colors[PR_rank]
  
  plot(g1,
       vertex.size = 12 * sqrt(PR/max(PR)),
       vertex.color = V(g1)$PR_color,
       vertex.label = NA,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(g1),
       main = "Karachi Bus Network\n(Node size proportional to PageRank centrality)")
  
  legend("topright", 
         legend = c("High", "Medium", "Low"),
         pt.cex = c(1.5, 1, 0.7),
         pch = 21,
         pt.bg = c("darkblue", "blue", "lightblue"),
         title = "PageRank",
         bty = "n",
         cex = 0.8)
  
  
  # Closeness Centrality Visualization
  dev.new()
  cls_clean = cls
  cls_clean[is.nan(cls_clean) | is.infinite(cls_clean)] = 0
  
  cls_colors = colorRampPalette(c("white", "pink", "hotpink"))(100)
  cls_rank = cut(cls_clean, breaks = 100, labels = FALSE)
  V(g1)$cls_color = cls_colors[cls_rank]
  
  plot(g1,
       vertex.size = 12 * sqrt(cls_clean/max(cls_clean)),
       vertex.color = V(g1)$cls_color,
       vertex.label = NA,
       edge.arrow.size = 0.3,
       layout = layout_with_fr(g1),
       main = "Karachi Bus Network\n(Node size proportional to closeness centrality)")
  
  legend("topright", 
         legend = c("High", "Medium", "Low"),
         pt.cex = c(1.5, 1, 0.7),
         pch = 21,
         pt.bg = c("hotpink", "pink", "white"),
         title = "Closeness",
         bty = "n",
         cex = 0.8)
  
  
