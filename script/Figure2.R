rm(list=ls())

# Code to create Figure 2:
# Main phylogenetic tree and map of Africa with zoom in of east africa to show viral sequences by lineage

# Libraries
library(rnaturalearth)
library(ggtree)
library(tidytree)
library(ggplot2)
library(sf)
library(sp) # library(rgdal)
library(phangorn)
library(castor)
library(ape)
library(treeio)
library(patchwork)  
library(ggsci)
library(ggspatial)
library(grid)
library(gridExtra) 
library(tidyverse)

# Data
tree <- ape::read.nexus("Data/All_cases_combined/updated.nex") # phylogeny
metadata <- read.csv("Data/All_cases_combined/all_combined_supplimentary_table.csv") # metadata
tree$tip.label <- gsub("_.*","", tree$tip.label) # remove awkward characters & spaces from labels
tree$tip.label <- gsub("/.*","", tree$tip.label)
tree <-ladderize(tree)

plot(tree) # length(test$tip.label)
length(tree$tip.label)

# Extract lineage info & assign colours
lineage_info <- data.frame(case = unique(metadata$case), colour = NA)
lineage_info <- lineage_info[order(lineage_info$case),]
lineage_info$colour <- pal_startrek("uniform")(4) # c("yellow","blue","green","red")

# Set up plotting 
human_cases <- grep(c("Z0826253|Z0828879|HB002|2021153276|SD846"), metadata$ID) # cases 1,2,3,4,5
metadata$tipshape <- "animal"; metadata$tipshape[human_cases] <- "human" # use large cross for human cases
metadata$tipsize <- 1.2; metadata$tipsize[human_cases] <- 2 # use large cross for human cases
metadata$cases <- metadata$case; metadata$cases[which(metadata$case == "3_5")] <- "3 & 5"
# metadata$alpha <- .8; metadata$alpha[human_cases] <- 1 
# Apply date to the tips
metadata <- metadata %>%
  dplyr::mutate(
    tiplab = case_when(
      ID == "Z0826253" ~ "2018/02/27",
      ID == "Z0828879" ~ "2018/07/18",
      ID == "HB002" ~ "2019/09/16",
      ID == "2021153276" ~ "2022/08/29",
      ID == "SD846" ~ "2022/10/03",
      TRUE ~ ""
    )
  )
#------------------------------------------------
#TREE

# Create main phylogeny
main_tree <- ggtree::ggtree(tree, colour = "grey50", ladderize = T, size = 0.1) %<+% metadata # size changes branch thickness

# Manually annotate important bootstraps according to .contree file viewed in FigTree
node_labels<-rep("",length(main_tree$data$node))
node_labels[c(263,418,419,267)]<-"100"
node_labels[c(264,356)]<-"99"
node_labels[c(266)]<-"97"

# Full tree with annotations
main_tree <- main_tree +
  ggtree::geom_tippoint(aes(fill = factor(cases), shape = factor(tipshape), size = tipsize), color = "grey50") +
  scale_fill_manual("", guide = "legend",values=c(lineage_info$colour), labels = c("1", "2", "3 & 5", "4")) +
  scale_shape_manual(values = c("human" = 23, "animal" = 21))+
  ggtree::geom_text(aes(label=tiplab), hjust=-.2, vjust=0.7, size=1.9, col = "grey30") +
  geom_text(aes(label=node_labels), hjust=-.2, size = 1.9, col = "grey30") +
  # ggtree::geom_tiplab(aes(color = tiplab)) +
  # ggtree::geom_tiplab(offset = .6, hjust = .5) +
  scale_size_continuous(range = c(1, 2.5)) +
  geom_treescale(x = 0.03, y = -15, offset = 3, linesize = 0.25, fontsize = 4) + 
  coord_cartesian(clip="off") +
  guides(fill = guide_legend(override.aes = list(shape = 21), title = "Case lineages:", nrow=2, byrow=TRUE),
         shape = guide_legend(title = "Type:", nrow=2, byrow=TRUE),
         size = "none") +
  theme(plot.margin = unit(c(14,8,13,8), "mm")) +
  theme(legend.key.size = unit(1,"line"), # change legend key size, title & font size
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8), legend.position="bottom", legend.justification = "right") 
main_tree

ggsave("Figs/main_phylogeny.pdf", main_tree, width = 4, height = 6, units="in")


#_________________________________________________________________________
# MAPS
# Assign metadata colours
metadata$colour <- NA
metadata$colour[which(metadata$case == "1")] <- lineage_info$colour[1]
metadata$colour[which(metadata$case == "2")] <- lineage_info$colour[2]
metadata$colour[which(metadata$case == "3_5")] <- lineage_info$colour[3]
metadata$colour[which(metadata$case == "4")] <- lineage_info$colour[4]
metadata <- metadata[order(metadata$colour),]
# metadata_sf <- st_as_sf(metadata, coords = c("longitude","latitude"))

# GIS data
world <- ne_countries(scale = "medium", returnclass = "sf") # world <- map_data("world", returnclass = "sf")
districts <- read_sf("Data/Shapefiles/afr_g2014_2013_2.shp") # For Africa!

# AFRICA map
africa_map <- ggplot(data = world) +
  geom_sf() + 
  geom_point(data = metadata, 
             aes(x = longitude, y = latitude, shape = tipshape, fill = cases, size = tipsize*2.5), colour = "grey50") +
  scale_shape_manual(values=c(21,23)) +
  scale_fill_manual(values=c(lineage_info$colour)) +
  scale_size_continuous(range = c(1, 3)) +
  coord_sf(xlim = c(-20, 55), ylim = c(-40, 40), expand = FALSE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none")
africa_map
## ggsave("Figs/main_africa_map.pdf", africa_map, width = 4, height = 6, units="in")

# EAST AFRICA map
countries <- data.frame(name = c("Tanzania","Kenya"),
                        long = c(35, 38),
                        lat = c(-6, 1))

eafrica <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = metadata, 
             aes(x = longitude, y = latitude, shape = tipshape, fill = cases, size = tipsize*2.5), colour = "grey50") +
  scale_shape_manual(values=c(21,23)) +
    scale_fill_manual(values=c(lineage_info$colour)) +
    scale_size_continuous(range = c(1, 3)) +
  coord_sf(xlim = c(29, 41.7), ylim = c(-12, 5.5), expand = FALSE) +
  geom_text(data = countries, aes(x = long, y = lat, label = name)) + 
  annotation_scale(location = "tl", width_hint = 0.25) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
eafrica
## ggsave("Figs/east_africa_map.pdf", eafrica, width = 4, height = 6, units="in")

eafrica_blank <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(29, 41.7), ylim = c(-12, 5.5), expand = FALSE) +
  geom_text(data = countries, aes(x = long, y = lat, label = name)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
eafrica_blank
ggsave("Figs/east_africa_blank_map.pdf", eafrica_blank, width = 4, height = 6, units="in")


# Save as a 3 panel plot
fig2 <- africa_map + eafrica + main_tree + plot_layout(widths = c(1.1, .75, .95)); fig2
fig2 <- fig2+plot_annotation(tag_levels = 'A')
ggsave("Figs/fig2_phylogeny_map_test.pdf", plot = fig2, width = 11, height = 6, units="in")

