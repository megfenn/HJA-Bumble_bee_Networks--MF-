knitr::purl("C:/Users/Owner/University of Oregon Dropbox/Megan Fenner/cascades-meadows/dataPrep/1_specimenPrep.R")
source(spec.net)
save(spec.net, file = "spec.net.RData")
spec.net.data <- load("spec.net.RData")

load("spec.net.RData")  # loads spec.net into environment
HJA_bumblebees <- spec.net[spec.net$Genus == "Bombus", ] #this worked! It created a dataframe HJA 
#that only has the data for Bombus. Question is... can I then group?
HJA_bumblebees <- HJA_bumblebees[ , c("Year", "Site", "SampleRound", "Date", "SiteSampleYear", "GenusSpecies", 
                                      "PlantGenus", "PlantGenusSpecies", "Genus", "Complex")]
#so what was happening before was it wasn't recognizing the names of the columns when I was telling
#it to look for these columns, as is listed above. So, now we've created a vector that is all
#the names of the columns. 
cols_to_keep <- c("Year", "Site", "SampleRound", "Date", "SiteSampleYear", 
                  "GenusSpecies", "PlantGenus", "PlantGenusSpecies", 
                  "Genus", "Complex")
#Now, we say for in the HJA_bumblebees data, for all the rows, intersect the column names that
#we have established with the names in the HJA data set. 
HJA_bumblebees <- HJA_bumblebees[, intersect(cols_to_keep, names(HJA_bumblebees))]

#Function that might be used to create the adjacency matrix:
makeNets <- function(spec.dat, net.type,
                     species=c("Plant", "Pollinator"),
                     poll.groups="all", mean.by.year=FALSE,
                     ...){
  ## 1. spec.data: the specimen data, can be all groups, only bees
  ## etc.
  ## 2. net.type: a character string, "YearSR"= create networks by year
  ## and sampling round or "Year" by year.
  ## 3. species: a vector with two entries, c("Plant", "Pollinator"),
  ## or c("Pollinator", "Parasite")
  ## 4. poll.groups: a character string for naming the
  ## networks. Should correspond to what specimen data subset was
  ## passed in, i.e., "all", "bees"
  spec.dat$YearSR <- paste(spec.dat$Year, spec.dat$SampleRound, sep=".")
  nets <- breakNet(spec.dat, site='Site', year=net.type,
                   mean.by.year=mean.by.year, ...)
  nets <- lapply(nets, bipartite::empty)
  
  ## graphs
  nets.graph <- lapply(nets, graph_from_incidence_matrix,
                       weighted =   TRUE, directed = FALSE)
  nets.graph <-  lapply(nets.graph, function(x){
    vertex_attr(x)$type[vertex_attr(x)$type] <- species[2]
    vertex_attr(x)$type[vertex_attr(x)$type
                        != species[2]] <- species[1]
    return(x)
  })
}




makeNets <- function(spec.dat, net.type,
                     species=c("Plant", "Pollinator"),
                     poll.groups="all", mean.by.year=FALSE,
                     ...){
  ## 1. spec.data: the specimen data, can be all groups, only bees
  ## etc.
  ## 2. net.type: a character string, "YearSR"= create networks by year
  ## and sampling round or "Year" by year.
  ## 3. species: a vector with two entries, c("Plant", "Pollinator"),
  ## or c("Pollinator", "Parasite")
  ## 4. poll.groups: a character string for naming the
  ## networks. Should correspond to what specimen data subset was
  ## passed in, i.e., "all", "bees"
  spec.dat$YearSR <- paste(spec.dat$Year, spec.dat$SampleRound, sep=".")
  nets <- breakNet(spec.dat, site='Site', year=net.type,
                   mean.by.year=mean.by.year, ...)
  nets <- lapply(nets, bipartite::empty)
  
  ## graphs
  nets.graph <- lapply(nets, graph_from_incidence_matrix,
                       weighted =   TRUE, directed = FALSE)
  nets.graph <-  lapply(nets.graph, function(x){
    vertex_attr(x)$type[vertex_attr(x)$type] <- species[2]
    vertex_attr(x)$type[vertex_attr(x)$type
                        != species[2]] <- species[1]
    return(x)
  })
  
  ## unweighted
  nets.graph.uw <- lapply(nets, graph_from_incidence_matrix,
                          directed = FALSE)
  nets.graph.uw <-  lapply(nets.graph.uw, function(x){
    vertex_attr(x)$type[vertex_attr(x)$type] <- species[2]
    vertex_attr(x)$type[vertex_attr(x)$type
                        != species[2]] <- species[1]
    return(x)
  })
  
  years <- sapply(strsplit(names(nets), "[.]"), function(x) x[[2]])
  sites <- sapply(strsplit(names(nets), "[.]"), function(x) x[[1]])
  
  if(net.type == "YearSR" & mean.by.year == FALSE){
    SRs <- sapply(strsplit(names(nets), "[.]"), function(x) x[[3]])
  } else{
    SRs <- NA
  }
  if(mean.by.year){
    net.ty <- "Year"
  } else {
    net.ty <- net.type
  }
  
  save(nets.graph,nets.graph.uw, nets, years, sites, SRs,
       file=sprintf("../data/networks/%s_%s_%s.Rdata", net.ty,
                    paste(species, collapse=""), poll.groups
       ))
  
  ## species stats
  sp.lev <- calcSpec(nets)
  save(sp.lev,
       file=sprintf('../data/splevel_network_metrics/%s_%s_%s.Rdata',
                    net.ty,
                    paste(species, collapse=""), poll.groups
       ))
  return(sp.lev)
}


bumblebee_adj <- makeNets(HJA_bumblebees, "YearSR")  
graph_bumbles <- graph_from_biadjacency_matrix(bumblebee_adj, weighted = TRUE)



