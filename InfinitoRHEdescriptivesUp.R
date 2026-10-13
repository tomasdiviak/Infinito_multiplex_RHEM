###### Infinito mafia network - descriptives from hyperevent data #####


# 1) size distribution per event type (avg, sd, range) + histogram (facets)
# 2) create a one-mode projection of the combined network + each type
# 3) describe the projections: # actors + events, dense+centr, avg+SD deg, clust
# 4) sociograms for the four networks in the same layout + leaders
# 5) lineplot of events of each type over time (months)



# loading the data
setwd("C:/Users/msraqtd4/OneDrive - The University of 
      Manchester/práce/postdoc/DualityEventsActors")

#library(openxlsx)

# actual events start on line 2154 
# two dates 29/02/2009 had to be switched to 28/02 (no 29/02 in 2009)
#dat <- read.xlsx("InfinitoEventsFinal.xlsx", sheet = 1, colNames = TRUE)
#dat

library(readxl)

# robustness check: sheet 2 = less expansive lasting events, sheet 3 = more
dat <- read_excel("InfinitoEventsFinalUpRevise.xlsx", sheet = 3, col_names = TRUE,
                  col_types = c("text", "date", "text", "numeric", "text", "text"))

# extract the events only
ev <- dat[2154:nrow(dat), ]

# nr of nodes and nr of events (N120 + R052 REMOVED)
length(unique(ev$ID)) # 346; update 347; 346 // revision: 346, 346
length(unique(ev$Cod_ev)) # 1001; update 1000 (line 63); 1000// revision: 998, 997

# size distribution of all events + per event type
library(tidyverse)

# distribution of event sizes in total
# NEED TO ADJUST FOR SAME Cod_ev OCCURING ON DIFFERENT date!!!!!
#ev |> count(Cod_ev) |> 
  #summarise(avg = mean(n), SD = sd(n), min = min(n), max = max(n))
# 2.73  2.54     1    44 (???)

ev |> count(Cod_ev, date) |> 
  summarise(avg = mean(n), SD = sd(n), min = min(n), max = max(n))
# update: 2.72  2.22     1    25!!!!! correct
# revision: 2.60  2.12     1    25 ///  2.71  2.19     1    25

ev |> group_by(type) |> 
  group_by(Cod_ev) |> 
  count(type)

table(ev$type)

#table(ev$type)

# size distribution per event type
# NEED TO ADJUST FOR SAME Cod_ev OCCURING ON DIFFERENT date!!!!!
ev |> group_by(type) |> 
  count(Cod_ev, date) |> 
  summarise(avg = mean(n), SD = sd(n), min = min(n), max = max(n))

# save the counts in a data frame for visualisation
evCounts <- ev |> group_by(type) |> count(Cod_ev, date) # 1008, adjusted for date
# revision: 1131 // 1023

# visualisation w/ ggplot2
library(ggplot2)

# different facets - does not work due to different dispersion
ggplot(data = evCounts, aes(x = n)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  facet_wrap(~ type) +
  scale_x_continuous(breaks = seq(min(evCounts$n), 
                                  max(evCounts$n), 
                                  by = 1)) + # Ensures integer breaks
  labs(title = "Distribution of the number of actors per event", 
       x = "number of actors per event") +
  theme_bw()

# visualisation - histograms

# selecting meeting events
mtng <- evCounts |>  # 526; 526 // revision: 526; 526
  filter(type == "mtng")

h1 <- ggplot(data = mtng, aes(x = n)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  labs(title = "Distribution of event sizes - meetings", x = "event size") +
  theme_bw()

# selecting meeting events
call <- evCounts |> # 384; 383 // revision: 381; 381
  filter(type == "call")

h2 <- ggplot(data = call, aes(x = n)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  labs(title = "Distribution of event sizes - calls", x = "event size") +
  scale_x_continuous(breaks = seq(min(call$n), 
                                  max(call$n), 
                                  by = 1)) + # Ensures integer breaks
  theme_bw()

# selecting meeting events
chrg <- evCounts |> # 99 // revision: 224; 116
  filter(type == "chrg")

h3 <- ggplot(data = chrg, aes(x = n)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) + 
  labs(title = "Distribution of event sizes - offences", x = "event size") +
  theme_bw()

# using patchwork to arrange the plots into 3x1 or 1x3
library(patchwork)

# 3 by 1
h1 / h2 / h3 # does NOT work well

# 1 by 3
h1 | h2 | h3 # works better, maybe fix the y axes - ggplots needs to be modified


# attributes do not change with the revision
# selecting leaders variable
ld <- dat |> 
  filter(type == "leader")

sum(ld$weight) # 35; update 34

# selecting gender variable
gn <- dat |> 
  filter(type == "gndr")

sum(gn$weight, na.rm = TRUE) # NO WOMEN, update 11 (there were NAs!!!)

# selecting age variable
age <- dat |> 
  filter(type == "age")

summary(age$weight) # min 33, med 56, mean 57.7, max 98

# selecting kin variable
kin <- dat |> 
  filter(type == "kin")

kin |> 
  group_by(Cod_ev) |> 
  count(weight) |> 
  arrange(desc(n))# 210 dyads, one triad = 213 kinship dyads

# selecting locale variable
loc <- dat |> 
  filter(type == "locale")

table(loc$weight) # 19 = 34 actors, 17 = 30 actors
summary(loc$weight) # 109 NAs
length(unique(loc$weight)) # 19 different localis

# creating an igraph attribute
ld2 <- ld |> 
  select(ID, weight) |> 
  rename("leader" = "weight") |> 
  mutate(leader = leader + 1)


# formatting an igraph edgelist
el <- ev |> 
  select(ID, Cod_ev, type) |> 
  count(ID, Cod_ev, type) |> 
  rename("from" = "ID", "to" = "Cod_ev", "weight" = "n")
el

# formatting an igraph edgelist
el2 <- ev |> 
  select(ID, Cod_ev, type, date) |> 
  count(ID, Cod_ev, type, date) |> 
  rename("from" = "ID", "to" = "Cod_ev", "weight" = "n") |> 
  select(from, to, type, weight)
el2

# converting to a graph object
library(igraph)

evG <- graph_from_data_frame(el2, directed = FALSE) # accounting for eventXdate
evG

is_bipartite(evG) # not bipartite

# assigning a new attribute based on bipartite mapping
V(evG)$type <- bipartite_mapping(evG)$type
is_bipartite(evG) # TRUE

# actor-actor projections
library(manynet)

# combined graph of all types 
combG <- to_mode1(evG)
combG

# manynet 'to_mode1()' does NOT preserve edge types
# first subset edge types, THEN do projections

# meetings
mtngG <- subgraph.edges(evG, eids = E(evG)[E(evG)$type == "mtng"], 
                        delete.vertices = FALSE)
mtngGpr <- to_mode1(mtngG)
mtngGpr

# calls
callG <- subgraph.edges(evG, eids = E(evG)[E(evG)$type == "call"], 
                        delete.vertices = FALSE)
callGpr <- to_mode1(callG)
callGpr

# offences
chrgG <- subgraph.edges(evG, eids = E(evG)[E(evG)$type == "chrg"], 
                        delete.vertices = FALSE)
chrgGpr <- to_mode1(chrgG)
chrgGpr

# binding the projections to a list 
grL <- list(combG, mtngGpr, callGpr, chrgGpr)

# defining ad hoc functions to calculate on the list
avgdeg <- function(x) {
  avgdeg <- mean(degree(x, mode = "all"))
  print(avgdeg)
}

sddeg <- function(x) {
  sddeg <- sd(degree(x, mode = "all"))
  print(sddeg)
}

avgstrng <- function(x) {
  avgstrng <- mean(strength(x, mode = "all"), na.rm = TRUE)
  print(avgstrng)
}

sdstrng <- function(x) {
  sdstrng <- sd(strength(x, mode = "all"), na.rm = TRUE)
  print(sdstrng)
}

nrisolates <- function(x) {
  nriso <- sum(node_is_isolate(x))
  print(nriso)
}

nractive <- function(x) {
  nract <- gorder(x) - sum(node_is_isolate(x))
  print(nract)
}

# looping descriptives over the list
sapply(grL, gorder)
sapply(grL, nrisolates)
sapply(grL, nractive)
sapply(grL, edge_density)
sapply(grL, centralization.degree, mode = "total")
sapply(grL, transitivity, type = "global")
sapply(grL, mean_distance)
sapply(grL, avgdeg) 
sapply(grL, sddeg)
sapply(grL, avgstrng)
sapply(grL, sdstrng)

# multiplex visualisation 
library(ggraph)

# one isolate
V(combG)[degree(combG) == 0] # N120

ev |> filter(ID == "N120") # R052, charge!!!
ev |> filter(Cod_ev == "R052") # single person charge --> removed (06/05/25)


# combined network
# adding leadership
combGAtt <- set_vertex_attr(combG, name = "leader", value = ld$weight)

# creating a layout based on the combined network
l <- layout_with_fr(combGAtt)

# ggraph sociogram
s1 <- ggraph(combGAtt, layout = l) +
  geom_edge_link(color = "gray50") +   
  geom_node_point(color = V(combGAtt)$leader+1, size = 3) +
  labs(title = "Sociogram - meetings, calls, and co-offenses combined") +
  theme_void()
s1

# meeting network
# adding leadership
mtngGprAtt <- set_vertex_attr(mtngGpr, name = "leader", value = ld$weight)

# ggraph sociogram
s2 <- ggraph(mtngGprAtt, layout = l) +
  geom_edge_link(color = "gray50") +   
  geom_node_point(color = V(mtngGprAtt)$leader+1, size = 3) +
  labs(title = "Sociogram - meetings only") +
  theme_void()
s2

# calls network
# adding leadership
callGprAtt <- set_vertex_attr(callGpr, name = "leader", value = ld$weight)

# ggraph sociogram
s3 <- ggraph(callGprAtt, layout = l) +
  geom_edge_link(color = "gray50") +   
  geom_node_point(color = V(callGprAtt)$leader+1, size = 3) +
  labs(title = "Sociogram - calls only") +
  theme_void()
s3

# co-offences network
# adding leadership
chrgGprAtt <- set_vertex_attr(chrgGpr, name = "leader", value = ld$weight)

# ggraph sociogram
s4 <- ggraph(chrgGprAtt, layout = l) +
  geom_edge_link(color = "gray50") +   
  geom_node_point(color = V(chrgGprAtt)$leader+1, size = 3) +
  labs(title = "Sociogram - co-offences only") +
  theme_void()
s4

# patchwork to arrange the plots 2x2
(s1 | s2) /
  (s3 | s4)


# creating a timeline with months and filling empty months
evDates <- ev |> 
  select(date, type, Cod_ev)

datesMnth <- evDates |>  
  group_by(type) |> 
  count(month = floor_date(date, "month")) |>  
  complete(month = seq.Date(min(as.Date(month)), max(as.Date(month)), 
                            by = "month")) |>  
  replace_na(list(n = 0)) 

# visualising it with a lineplot
ggplot(aes(month, n, color = type), data = datesMnth) +
  geom_line() + 
  theme_bw() + 
  labs(x = "Months", y = "Number of events weighted by their size")

# descriptive statistics
# pivot to wide format: one column per 'type', rows = months
datesWide <- datesMnth |>
  pivot_wider(names_from = type, values_from = n)

# computing the correlation matrix on the type columns
corMat <- datesWide |>
  select(-month) |>
  cor(use = "pairwise.complete.obs")
corMat # callXoff .71, callXmeet .74, meetX .45
# revise1: callXoff .767, callXmeet .737, meetXcall .582
# revise2: callXoff .648, callXmeet .737, meetXcall .461

# computing avg and SD across months
mean(datesMnth$n) # 21.54 // revise: 21.54; 21.795
sd(datesMnth$n) # 29.71 // revise: 29.71; 29.706

# by types of events
datesMnth |> 
  group_by(type) |> 
  summarise(avg = mean(n), SD = sd(n)) 
# call: 19.2(18.1), chrg: 6.5(9), mtng: 35.1(40.2)
# revise1: call: 19.1(18.0), chrg: 11.9(13.1), mtng: 35.1(40.3)
# revise2: call: 19.1(18.0), chrg: 7.42(10.1), mtng: 35.1(40.3)