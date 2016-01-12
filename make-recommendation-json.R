#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   make-recommendation-json.R                          ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################


options(stringsAsFactors=FALSE)

# libraries
library(dplyr)
library(magrittr)
library(memoise)
library(jsonlite)





ARTIST_LIST <- c("Belle and Sebastian", "The Smiths", "The Magnetic Fields",
                 "P:ano", "The Cure", "Stars", "Bad Religion",
                 "Elvis Costello", "The Beatles", "Radiohead",
                 "Guided by Voices", "NOFX", "The Strokes", "Morrissey",
                 "Joy Division", "New Order", "Depeche Mode", "Wu-Tang Clan",
                 "The New Pornographers", "Peter Tosh", "Cocteau Twins",
                 "The Sisters of Mercy", "Lowlife", "Tokyo Police Club",
                 "Johann Sebastian Bach", "Echo and the Bunnymen", "Beirut",
                 "Gary Numan", "They Might Be Giants", "KRS-One",
                 "Little Nemo", "Rancid", "The Coup", "And Also The Trees",
                 "Pet Shop Boys", "No Kids", "Dead Can Dance", "Arcade Fire",
                 "dead prez", "Kate Bush", "INDK", "The Divine Comedy",
                 "The Velvet Underground", "Anti-Flag", "Broken Social Scene",
                 "Metric", "Talking Heads", "Pixies", "Public Image Ltd.",
                 "Sex Pistols", "The Psychedelic Furs", "The Church",
                 "Black Uhuru", "Bob Marley", "A Tribe Called Quest",
                 "Busta Rhymes", "Leaders of the New School", "Choking Victim",
                 "The National", "Ghostface Killah", "Rakim", "De La Soul",
                 "Public Enemy", "Raekwon", "Boogie Down Productions",
                 "Operation Ivy", "Catch 22", "The Kinks", "Bauhaus",
                 "Masta Killa")



create_artist_query_url_lfm <- function(artist_name){
  prefix <- "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist="
  postfix <- "&api_key=c2e57923a25c03f3d8b317b3c8622b43&format=json"
  encoded_artist <- URLencode(artist_name)
  return(paste0(prefix, encoded_artist, postfix))
}


get_tag_frame_lfm <- function(an_artist){
  print(paste0("Attempting to fetch: ", an_artist))
  artist_url <- create_artist_query_url_lfm(an_artist)
  json <- fromJSON(artist_url)
  return(as.vector(json$toptags$tag[,"name"]))
}

mem_get_tag_frame_lfm <- memoise(get_tag_frame_lfm)

mem_get_tag_frame_lfm("La Banda Gorda")



artists_tags <- sapply(ARTIST_LIST, mem_get_tag_frame_lfm)
names(artists_tags) <- ARTIST_LIST



cmbs <- combn(ARTIST_LIST, 2)
comparisons <- data.frame(t(cmbs))
names(comparisons) <- c("artist1", "artist2")


jaccard_index <- function(tags1, tags2){
  length(intersect(tags1, tags2))/length(union(tags1, tags2))
}

comparisons$similarity <- apply(comparisons, 1,
  function(arow){
    jaccard_index(artists_tags[[unlist(arow[1])]],
                  artists_tags[[unlist(arow[2])]])
  }) 



get_top_n <- function(comparisons, N, artist, threshold){
  comparisons %<>%
    filter(artist1==artist | artist2==artist) %>%
    arrange(desc(similarity))
  other_artist <- ifelse(comparisons$similarity>threshold,
                         ifelse(comparisons$artist1==artist,
                                comparisons$artist2, comparisons$artist1),
                         "None")
  return(other_artist[1:N])
}


# need "name,group,first,second,third"
nodes <- sapply(ARTIST_LIST, function(x) get_top_n(comparisons, 3, x, 0.25))
nodes <- data.frame(t(nodes))
names(nodes) <- c("first", "second", "third")
nodes$name <- row.names(nodes)
row.names(nodes) <- NULL
nodes$group <- 1


# need "source,target,node1,node2,weight"

# find the 0-indexed index
lookup_number <- function(name) which(name==ARTIST_LIST)-1

strong_links <- comparisons %>%
  filter(similarity > 0.25) %>%
  rename(node1 = artist1, node2 = artist2, weight=similarity)
strong_links$source <- sapply(strong_links$node1, lookup_number)
strong_links$target <- sapply(strong_links$node2, lookup_number)




object <- list("nodes"=nodes,
               "links"=strong_links)

sink("artists.json")
toJSON(object, dataframe="rows", pretty=TRUE)
sink()



