rm(list=ls())

# Code to create Figure 3:
# Zoomed in map to show cases coloured by lineage spread across the border of TZA & KEN
# Subtrees of each lineage and nearest cases to the human case

# Libraries
library(rnaturalearth)
library(ggtree)
library(tidytree)
library(ggplot2)
library(sf)
library(sp) 
library(phangorn)
library(castor)
library(ape)
library(treeio)
library(patchwork)  
library(ggsci)
library(ggspatial)
library(caper)

# Fixes completed:
# labelled human case dates on trees
# Resize points to be smaller/ larger than human case based on relative timing
# Outlined points with lighter/ darker shade than human case based on relative timing
# standardized scale of tree (0.002) & changed height to be a bit easier to read/ compare
# Import Gadm data for districts in Tz
# COLLAPSE uninformative clades and label with information on numbers of cases pre/post each human case
# Fix points on tree to show outline & use correct shapes (as per Fig 2)
# Create a composite legend

#________________________________________________________________________________________________________
# Case colours
case_col <- pal_startrek("uniform")(4)
human_case_dates <- c("2018/02/27", "2018/07/18", "2019/09/16", "2022/08/29", "2022/09/08") # human case dates

related1_dates<- c("2003/03/31")
related2_dates<- c("2017/09/12","2021/11/11")
related35_dates<- c("2019/03/22", "2021/10/13")
related4_dates<- c("2021/11/11")

# TREES
#### case 1 needs pre-2018 (light), 2018 (dark); zoom in Africa & show countries & 2 dates
#  collapse upper subclade, annotate how many cases in collapses clade - indicate n from 200x and 20XX
tree1 <- ape::read.nexus("Data/Case1/case1.nex")
metadata1 <- read.csv("Data/Case1/case1.csv")
case1 <- which(metadata1$ID == "Z0826253")
before1 <- which(metadata1$ID == "KT119642")

tree1$tip.label<-gsub("_.*","",tree1$tip.label)
tree1$tip.label<-gsub("/.*","",tree1$tip.label)
tree1 <- ladderize(tree1)

plot(tree1)
'%notin%'<-Negate("%in%")
which(tree1$tip.label %notin% metadata1$ID)
which(metadata1$ID %notin% tree1$tip.label)

metadata1$timing <- NA
metadata1$timing[which(metadata1$year < metadata1$year[case1])] <- "before"
metadata1$timing[which(metadata1$year == "2018")] <- "2018"
metadata1$alpha <- NA
metadata1$alpha[which(metadata1$year < metadata1$year[case1])] <- 0.8
metadata1$alpha[which(metadata1$year == "2018")] <- 1
metadata1$tipshape <- "animal"; metadata1$tipshape[case1] <- "human" ; metadata1$tipshape[before1] <- "before"
metadata1$tipsize <- 2.5; metadata1$tipsize[c(case1, before1)] <- 2.55
metadata1$tiplab <- ""; metadata1$tiplab[case1] <- human_case_dates[1]; metadata1$tiplab[before1]<-related1_dates[1]


# case1 tree
case1_tree <- ggtree::ggtree(tree1, colour = "grey50", ladderize = T, size = 0.25) %<+% metadata1 
case1_tree <- case1_tree +
  scale_color_manual(values=c("grey10","grey80")) +
  #  geom_tippoint(aes(shape = tipshape, size = tipsize), color="grey50") +
  ggtree::geom_tippoint(aes(shape = tipshape, size = tipsize), fill = case_col[1], colour = "grey50") +
  ggtree::geom_text(aes(label=tiplab), hjust=-.3, vjust=0.25, size=1.7, col = "grey10") +
  scale_shape_manual(values=c(21,24,23)) +
  scale_size_continuous(range = c(2, 4)) +
  coord_cartesian(clip="off") +
  geom_treescale(x = 0.015, y = 80, offset = 1, width = 0.002, linesize = 0.25, fontsize = 4) + 
  guides(#shape = guide_legend(title = "Cases"), 
    shape = "none", size = "none", color = "none") + 
  theme(plot.margin = unit(c(14,8,13,8), "mm")) +
  theme(legend.key.size = unit(1,"line"), # change legend key size, title & font size
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8), legend.position="bottom") 
case1_tree

# Identify node to collapse from: 
##case1_tree + geom_text(aes(label=node), hjust=-.3)
node1 <-153

collapse1<-tree1$tip.label[(caper::clade.members(node1, tree1))]
min1<-min(metadata1$year[which(metadata1$ID %in% collapse1)])
max1<-max(metadata1$year[which(metadata1$ID %in% collapse1)])

case1_tree<-scaleClade(case1_tree, node1, .2) %>% collapse(node1, 'min', fill=case_col[1])+
  geom_cladelabel(node=node1, label= paste(length(collapse1), " tips (", min1, "-", max1,")", sep = ""), 
                  fontsize = 2, offset = 0.02)
case1_tree
##ggsave("Figs/case1_phylogeny.pdf", case1_tree, width = 3, height = 4, units="in")

################# 
# Case 2
#### case 2 needs pre-2018 (light), 2018 (dark), post 2018 (v dark - black?), Kenya counties (need to see scale)
# Case 2: before = turquoise1, year_of = blue, after = navy
#  collapse upper subclade - with all the dark blue
tree2 <- ape::read.nexus("Data/Case2/case2.nex")
metadata2 <- read.csv("Data/Case2/case2.csv")
case2 <- which(metadata2$ID == "Z0828879")
before2 <- which(metadata2$ID == "2011100186")
after2 <- which(metadata2$ID == "Z0826447")

tree2$tip.label<-gsub("_.*","",tree2$tip.label)
tree2$tip.label<-gsub("/.*","",tree2$tip.label)
tree2 <- ladderize(tree2)

plot(tree2)
'%notin%'<-Negate("%in%")
which(tree2$tip.label %notin% metadata2$ID)
which(metadata2$ID %notin% tree2$tip.label)
 
metadata2$tipshape <- "animal"; metadata2$tipshape[case2] <- "human" ; metadata2$tipshape[after2]<-"after" ; 
metadata2$tipshape[before2]<-"before"

metadata2$tiplab <- ""; metadata2$tiplab[case2] <- human_case_dates[2]; metadata2$tiplab[before2]<-related2_dates[1];
metadata2$tiplab[after2]<-related2_dates[2]

metadata2$timing <- "after"
metadata2$timing[which(metadata2$year < metadata2$year[case2])] <- "before"
metadata2$timing[which(metadata2$year == "2018")] <- "2018"
metadata2$tipsize <- 2 
metadata2$tipsize[c(case2,after2, before2)] <- 2.5

# case2 tree
case2_tree <- ggtree::ggtree(tree2, colour = "grey50", ladderize = T, size = 0.25) %<+% metadata2 
case2_tree <- case2_tree +
  scale_color_manual(values=c("grey10","grey80")) +
  #  geom_tippoint(aes(shape = tipshape, size = tipsize), color="grey50") +
  ggtree::geom_tippoint(aes(shape = tipshape, size = tipsize), fill = case_col[2], colour = "grey50") +
  ggtree::geom_text(aes(label=tiplab), hjust=-.3, vjust=0.25, size=1.7, col = "grey10") +
  scale_shape_manual(values=c(25,21,24,23)) +
  scale_size_continuous(range = c(2, 4)) +
  coord_cartesian(clip="off") +
  geom_treescale(x = 0.004, y = 20, offset = 1, width = 0.002, linesize = 0.25, fontsize = 4) + 
  guides(#shape = guide_legend(title = "Cases"), 
    shape = "none", size = "none", color = "none") + 
  theme(plot.margin = unit(c(14,30,13,8), "mm")) +
  theme(legend.key.size = unit(1,"line"), # change legend key size, title & font size
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8), legend.position="bottom") 
case2_tree

# Identify node to collapse from: 
## case2_tree + geom_text(aes(label=node), hjust=-.3)
node2<-50

collapse2<-tree2$tip.label[(caper::clade.members(node2, tree2))]
min2<-min(metadata2$year[which(metadata2$ID %in% collapse2)])
max2<-max(metadata2$year[which(metadata2$ID %in% collapse2)])

case2_tree<-scaleClade(case2_tree, node2, .2) %>% collapse(node2, 'min', fill=case_col[2])+
  geom_cladelabel(node=node2, label= paste(length(collapse2), " tips (", min2, "-", max2, ")", sep = ""), 
                  fontsize = 2, offset = 0.002)
case2_tree
##ggsave("Figs/case2_phylogeny.pdf", case2_tree, width = 3, height = 4, units="in")


#__________________________________
## CASE 3/5
#### cases 3 & 5 needs pre-2019 (light), 2019 (med), 2020-2021 (dark), 2022 (v dark - black?), Kenya & Tz (need to better label cases)
# Make before 2019 as white - to latest time period in orange
# collapse upper clade
tree3_5 <- ape::read.nexus("Data/Case3_5/case3_5.nex") 
metadata3_5 <- read.csv("Data/Case3_5/case3_5_subset.csv")
case3 <- which(metadata3_5$ID == "HB002")
case5 <- which(metadata3_5$ID == "SD846")
before35 <- which(metadata3_5$ID == "SD743")
after3 <- which(metadata3_5$ID == "Z0083236")

tree3_5$tip.label<-gsub("_.*","",tree3_5$tip.label)
tree3_5$tip.label<-gsub("/.*","",tree3_5$tip.label)
tree3_5 <- ladderize(tree3_5)

plot(tree3_5)
'%notin%'<-Negate("%in%")
which(tree3_5$tip.label %notin% metadata3_5$ID)
which(metadata3_5$ID %notin% tree3_5$tip.label)

before <- which(metadata3_5$year < "2020")
y2020 <- which(metadata3_5$year == "2020")
y2021 <- which(metadata3_5$year == "2021")
y2022 <- which(metadata3_5$year == "2022")
metadata3_5$timing <- "NA"
metadata3_5$timing[before] <- "before"
metadata3_5$timing[y2020] <- "2020"
metadata3_5$timing[y2021] <- "2021"
metadata3_5$timing[y2022] <- "2022"
metadata3_5$tipshape <- "animal"
metadata3_5$tipshape[case3] <- "human"
metadata3_5$tipshape[case5] <- "human"
metadata3_5$tipshape[before35] <- "before"
metadata3_5$tipshape[after3] <- "after"
metadata3_5$tipsize <- 2.5; 
metadata3_5$tipsize[c(case3, case5, before35, after3)] <- 3.5
metadata3_5$tiplab <- ""; 
metadata3_5$tiplab[case3] <- human_case_dates[3]
metadata3_5$tiplab[case5] <- human_case_dates[5]
metadata3_5$tiplab[before35] <- related35_dates[1]
metadata3_5$tiplab[after3] <- related35_dates[2]


# case3_5 tree
case3_5_tree <- ggtree::ggtree(tree3_5, colour = "grey50", ladderize = T, size = 0.25) %<+% metadata3_5 
case3_5_tree <- case3_5_tree +
  scale_color_manual(values=c("grey10","grey80")) +
  #  geom_tippoint(aes(shape = tipshape, size = tipsize), color="grey50") +
  ggtree::geom_tippoint(aes(shape = tipshape, size = tipsize), fill = case_col[3], colour = "grey50") +
  ggtree::geom_text(aes(label=tiplab), hjust=-.3, vjust=0.25, size=1.7, col = "grey10") +
  scale_shape_manual(values=c(25,21,24,23)) +
  scale_size_continuous(range = c(2, 4)) +
  coord_cartesian(clip="off") +
  geom_treescale(x = 0.004, y = 20, offset = 1, width = 0.002, linesize = 0.25, fontsize = 4) + 
  guides(#shape = guide_legend(title = "Cases"), 
    shape = "none", size = "none", color = "none") + 
  theme(plot.margin = unit(c(14,25,13,8), "mm")) +
  theme(legend.key.size = unit(1,"line"), # change legend key size, title & font size
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8), legend.position="bottom") 
case3_5_tree

# Identify node to collapse from: 
##case3_5_tree + geom_text(aes(label=node), hjust=-.3)
node3_5<-60

collapse3_5<-tree3_5$tip.label[(caper::clade.members(node3_5, tree3_5))]
min3_5<-min(metadata3_5$year[which(metadata3_5$ID %in% collapse3_5)])
max3_5<-max(metadata3_5$year[which(metadata3_5$ID %in% collapse3_5)])

case3_5_tree<-scaleClade(case3_5_tree, node3_5, .2) %>% collapse(node3_5, 'min', fill=case_col[3])+
  geom_cladelabel(node=node3_5, label= paste(length(collapse3_5), " tips (", min3_5, "-", max3_5, ")", sep = ""), 
                  fontsize = 2, offset = 0.001)
case3_5_tree
##ggsave("Figs/case3_5_phylogeny.pdf", case3_5_tree, width = 3, height = 4, units="in")

#______________________________________________________
# CASE 4
#### cases 4 needs pre-2022(light), 2022 (dark), Kenya 
# collapse lower clade
tree4 <- ape::read.nexus("Data/Case4/case4.nex")
metadata4 <- read.csv("Data/Case4/case4.csv")
tree4$tip.label<-gsub("_.*","",tree4$tip.label)
tree4$tip.label<-gsub("/.*","",tree4$tip.label)
tree4 <- ladderize(tree4)

plot(tree4)
'%notin%'<-Negate("%in%")
which(tree4$tip.label %notin% metadata4$ID)
which(metadata4$ID %notin% tree4$tip.label)

case4 <- which(metadata4$ID == "2021153276")
before4<-which(metadata4$ID == "Z00861847")

metadata4$timing <- NA
metadata4$timing[which(metadata4$year < metadata4$year[case4])] <- "before"
metadata4$timing[which(metadata4$year == "2022")] <- "2022"
metadata4$alpha <- NA
metadata4$alpha[which(metadata4$year < metadata4$year[case4])] <- 0.8
metadata4$alpha[which(metadata4$year == "2022")] <- 1
metadata4$tipshape <- "animal"; metadata4$tipshape[case4] <- "human"; metadata4$tipshape[before4] <- "before"
metadata4$tipsize <- 2; metadata4$tipsize[c(case4, before4)] <- 3
metadata4$tiplab <- ""; metadata4$tiplab[case4] <- human_case_dates[4]; metadata4$tiplab[before4] <- related4_dates[1]


# case4 tree
case4_tree <- ggtree::ggtree(tree4, colour = "grey50", ladderize = T, size = 0.25) %<+% metadata4 
case4_tree <- case4_tree +
  scale_color_manual(values=c("grey10","grey80")) +
  #  geom_tippoint(aes(shape = tipshape, size = tipsize), color="grey50") +
  ggtree::geom_tippoint(aes(shape = tipshape, size = tipsize), fill = case_col[4], colour = "grey50") +
  ggtree::geom_text(aes(label=tiplab), hjust=-.3, vjust=0.25, size=1.7, col = "grey10") +
  scale_shape_manual(values=c(21,24,23)) +
  scale_size_continuous(range = c(2, 4)) +
  coord_cartesian(clip="off") +
  geom_treescale(x = 0, y = 15, offset = 1, width = 0.002, linesize = 0.25, fontsize = 4) + 
  guides(#shape = guide_legend(title = "Cases"), 
    shape = "none", size = "none", color = "none") + 
  theme(plot.margin = unit(c(14,29,13,8), "mm")) +
  theme(legend.key.size = unit(1,"line"), # change legend key size, title & font size
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8), legend.position="bottom") 
case4_tree

# Identify node to collapse from: 
##case4_tree + geom_text(aes(label=node), hjust=-.3)
node4<-18

collapse4<-tree4$tip.label[(caper::clade.members(node4, tree4))]
min4<-min(metadata4$year[which(metadata4$ID %in% collapse4)])
max4<-max(metadata4$year[which(metadata4$ID %in% collapse4)])

case4_tree<-scaleClade(case4_tree, node4, .2) %>% collapse(node4, 'min', fill=case_col[4])+
  geom_cladelabel(node=node4, label= paste(length(collapse4), " tips (", min4, "-", max4, ")", sep = ""), 
                  fontsize = 2, offset = 0.0015)
case4_tree
##ggsave("Figs/case4_phylogeny.pdf", case4_tree, width = 3, height = 4, units="in")

# # Save as a 4 x 1 panel plot
fig3 <- case1_tree + case2_tree + case3_5_tree + case4_tree + plot_layout(widths = c(2.6, 2.6, 2.6, 0.8)); fig3
##ggsave("Figs/fig3_phylogenies.pdf", plot = fig3, width = 12, height = 7, units="in")
# #___________________________________________________________________

# MAPS
# Change this as necessary. Tz, Ken and EAf all available here
world <- ne_countries(scale = "medium", returnclass = "sf") 
KEN <-read_sf("Data/Shapefiles/gadm41_KEN_0.shp")
KEN_provinces <-read_sf("Data/Shapefiles/gadm41_KEN_1.shp")
KEN_counties <-read_sf("Data/Shapefiles/gadm41_KEN_2.shp")
TZA <-read_sf("Data/Shapefiles/gadm41_TZA_0.shp")
TZA_regions <-read_sf("Data/Shapefiles/gadm41_TZA_1.shp")
TZA_districts <-read_sf("Data/Shapefiles/gadm41_TZA_2.shp")
# TZA_districts <-read_sf("Data/Shapefiles/gadm40_TZA_1.shp")
# Probably woudl be useful to download districts too and overlay (thin grey lines)

# KEN <- ggplot(KEN_counties) + geom_sf(fill = "white"); KEN
# TZA <- ggplot(TZA_districts) + geom_sf(fill = "white"); TZA 

countries <- data.frame(name = c("Tanzania","Kenya"),
                        long = c(35, 38), lat = c(-6, 1))

######## Casemap 1
case1_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = metadata1, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize, color = timing)) +
  scale_color_manual(values=c("grey10", "grey80")) +
  geom_point(data = metadata1, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*.99), col = case_col[1]) +
  coord_sf(xlim = c(2, 42), ylim = c(-35, 10), expand = FALSE) +
  geom_text(data = countries, aes(x = long, y = lat, label = name)) + 
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
case1_map

######## Casemap 2
countries_zoom2 <- data.frame(name = c("Tanzania","Kenya"),
                             long = c(34, 36), lat = c(-1.5, 0.5))

case2_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = KEN_counties, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = KEN_provinces, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA_districts, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = TZA_regions, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA, fill = "transparent", color = "black", linewidth =1) +
  geom_sf(data = KEN, fill = "transparent", color = "black", linewidth =1) +
  # geom_sf(data = world, fill = "transparent", color = "black") +
  geom_point(data = metadata2, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize, color = timing)) +
  scale_color_manual(values=c("grey50","grey10","grey90")) + 
  scale_size(range=c(1.5,3)) +
  geom_point(data = metadata2, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*.8), col = case_col[2]) +
  coord_sf(xlim = c(33, 37), ylim = c(-2, 1.5), expand = FALSE) +
  # coord_sf(xlim = c(32, 40), ylim = c(-5.5, 1), expand = FALSE) +
  geom_text(data = countries_zoom2, aes(x = long, y = lat, label = name), size = 4) + 
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
# case2_map

##### Casemap 3&5 tree
countries_zoom <- data.frame(name = c("Tanzania","Kenya"),
                             long = c(35, 38), lat = c(-4, 0))

case3_5_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = KEN_counties, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = KEN_provinces, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA_districts, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = TZA_regions, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA, fill = "transparent", color = "black", linewidth =1) +
  geom_sf(data = KEN, fill = "transparent", color = "black", linewidth =1) +
  # geom_sf(data = world, fill = "transparent", color = "black") +
  geom_point(data = metadata3_5, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize, color = timing)) +
  scale_color_manual(values=c("grey80","grey70","grey10","grey90")) + scale_size(range=c(1.5,3)) +
  geom_point(data = metadata3_5, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*.8), col = case_col[3]) +
  coord_sf(xlim = c(32, 40), ylim = c(-5.5, 1), expand = FALSE) +
  geom_text(data = countries_zoom, aes(x = long, y = lat, label = name), size = 4) + 
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
# case3_5_map

######## Casemap 4
case4_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = KEN_counties, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = KEN_provinces, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = KEN, fill = "transparent", color = "black", linewidth =1) +
  # geom_sf(data = world, fill = "transparent", color = "black") +
  geom_point(data = metadata4, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize, color = timing)) +
  scale_color_manual(values=c("grey10", "grey80")) + scale_size(range=c(1.5,3)) +
  geom_point(data = metadata4, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*.99), col = case_col[4]) +
  coord_sf(xlim = c(33, 37), ylim = c(-2, 1.5), expand = FALSE) +
  geom_text(data = countries_zoom2, aes(x = long, y = lat, label = name)) + 
  annotation_scale(location = "bl", width_hint = 0.25) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="none") 
# case4_map

# Save as a 4 x 2 panel plot
fig3_maps <- case1_map + case2_map + case3_5_map + case4_map + plot_layout(widths = c(1, 1, 1, 1))
# fig3_maps
ggsave("Figs/fig3_maps.pdf", plot = fig3_maps, width = 12, height = 7, units="in")

# #_________________________________________
# fig3_all <- fig3 / fig3_maps; fig3_all
# ggsave("Figs/fig3_all.pdf", plot = fig3_all, width = 10, height = 8, units="in")
# 
# #_________________________________________
# Cases map zoomed in!
countries_zoom3 <- data.frame(name = c("TANZANIA","KENYA"),
                             long = c(35, 36), lat = c(-3, -1.5))

map_text1 <- data.frame(name = c("Serengeti","Ngorongoro","Moshi", "Kisumu", "Siaya", "Makueni"),
                       long = c(34.8, 35.5, 37.5, 34.8, 34.6, 38), 
                       lat = c(-2, -2.8,-3.1, -0.1, 0.15, -2.5))
map_text2 <- data.frame(name = c("Rareida", "Bondo", "Gem"),
                       long = c(34.46, 34.16, 34.45), 
                       lat = c(-0.3, -0.25, 0.0))
map_text3 <- data.frame(name = c("Nairobi", "Arusha"),
                        long = c(37.1, 36.8), 
                        lat = c(-1.3, -3.3))

# ZOOM MAP
cases_zoom_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data = KEN_counties, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = KEN_provinces, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA_districts, fill = "grey95", color = "grey80", linewidth =0.1) +
  geom_sf(data = TZA_regions, fill = "transparent", color = "grey80", linewidth =0.75) +
  geom_sf(data = TZA, fill = "transparent", color = "black", linewidth =1) +
  geom_sf(data = KEN, fill = "transparent", color = "black", linewidth =1) +
  
  # case 3 & 5 (GREENS)
  geom_point(data = metadata3_5, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*1.1, fill = "Case 3 & 5"), 
             colour = "grey50") +

  # case 2 (BLUE)
  geom_point(data = metadata2, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*1.37, fill = "Case 2"), 
             colour = "grey50") +
 
  # case 4 (YELLOW)
  geom_point(data = metadata4, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*1.37, fill = "Case 4"), 
            colour = "grey50") +
  
    # case 1 (RED)
  geom_point(data = metadata1, aes(x = longitude, y = latitude, shape = tipshape, size = tipsize*1.37, fill = "Case 1"), 
             colour = "grey50") +
  
  # Key map text
  geom_text(data = map_text1, aes(x = long, y = lat, label = name), size = 3) + 
  geom_text(data = map_text2, aes(x = long, y = lat, label = name), size = 2.5) + 
  geom_text(data = map_text3, aes(x = long, y = lat, label = name), size = 4) + 
  geom_text(data = countries_zoom3, aes(x = long, y = lat, label = name), size = 4.5) + 
  
  # colour & size
  scale_color_manual(values=c("grey80","grey70","grey10","grey90", "grey50","grey10","grey90", "grey10", "grey80", "grey10", "grey80")) +
  scale_fill_manual(values = c(case_col))+
  scale_size(range=c(2,4.5)) +
  scale_shape_manual(values=c(25,21,24,23), 
                     labels = c("Subsequent", "Animal", "Antecedent", "Human")) +
  
  
  # Scales
  coord_sf(xlim = c(33.5, 38.5), ylim = c(-4, 0.5), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  
  # Guides
  guides(fill = guide_legend(override.aes = list(shape = 21), title = "Closest relatives:", nrow=2, byrow=TRUE),
         shape = guide_legend(title = "Case type:", nrow=2, byrow=TRUE),
         size = "none") +
  
  # theme
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.text.x =element_blank(), axis.text.y =element_blank(),
        axis.ticks.x =element_blank(), axis.ticks.y =element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.position="top") 
# cases_zoom_map
ggsave("Figs/fig3_cases_zoom_map.pdf", plot = cases_zoom_map, width = 10, height = 8, units="in")

# # new zoom layout
# layout <- "
# AAAAAABBB
# AAAAAABBB
# AAAAAACC#
# AAAAAADD#
# AAAAAAE##
# "
# fig3_zoom <- cases_zoom_map + case1_tree + case2_tree + case3_5_tree + case4_tree +
#   plot_layout(design = layout)
# fig3_zoom
# ggsave("Figs/fig3_zoom.pdf", plot = fig3_zoom, width = 10, height = 15, units="in")

# Alternative:
layout <- "
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
AAAAAAAAAAA
BBBCCDD####
BBBCCDDEE##
BBBCCDDEE##
"

fig3_zoom2 <- cases_zoom_map + case1_tree + case2_tree + case3_5_tree + case4_tree +
  plot_layout(design = layout) 
# fig3_zoom2
ggsave("Figs/fig3_zoom2.pdf", plot = fig3_zoom2, width = 11, height = 12, units="in")

