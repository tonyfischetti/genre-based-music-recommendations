#!/usr/bin/Rscript --vanilla

###########################################################
##                                                       ##
##   make-recommendation-json.R                          ##
##                                                       ##
##                Author: Tony Fischetti                 ##
##                        tony.fischetti@gmail.com       ##
##                                                       ##
###########################################################

# workspace cleanup
rm(list=ls())

# options
options(echo=TRUE)
options(stringsAsFactors=FALSE)

# cli args
args <- commandArgs(trailingOnly=TRUE)

# libraries
library(dplyr)
library(magrittr)
library(memoise)
library(jsonlite)


##
## FUNCTIONS
##

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
  retframe <- data.frame(json$toptags$tag[,c("count", "name")])
  names(retframe)[1] <- an_artist
  return(retframe)
}

mem_get_tag_frame_lfm <- memoise(get_tag_frame_lfm)


# Failed experiment to improve recommendations
# by employing degree of tag fit
sim_coef <- function(frame1, frame2){
  comb <- merge(frame1, frame2, all=TRUE)
  comb <- select(comb, -name)
  comb[is.na(comb)] <- 0
  denom <- sum(comb[,1]) * sum(comb[,2])
  numer <- sum(comb[,1] * comb[,2])
  return(numer/denom)
}
mem_sim_coef <- memoise(sim_coef)



jaccard_index <- function(frame1, frame2){
  one <- frame1[["name"]]
  two <- frame2[["name"]]
  length(intersect(one, two))/length(union(one, two))
}

mem_jaccard_index <- memoise(jaccard_index)

lookup_number <- function(name) which(name==our_artists)-1

get_genre_code <- function(name) ARTISTS[which(ARTISTS$artists==name),3]

get_top_3 <- function(the_pairs, artist, key){
  the_pairs %<>% filter(artist1==artist | artist2==artist)
  another <- the_pairs
  # don't even look at this, it's very late
  another %<>%
    rename(artisttmp = artist2) %>%
    rename(artist2 = artist1) %>%
    rename(artist1 = artisttmp) %>%
    select_("artist1", "artist2", key) %>%
    rbind(the_pairs) %>%
    filter(artist1 == artist) %>%
    arrange_(key)
  get_name <- function(arow){
    if(arow[[key]] > 0.25)
      return(arow[2])
    return("None")
  }
  first <- get_name(another[69,])
  second <- get_name(another[68,])
  third <- get_name(another[67,])
  return(as.vector(unlist(c(first, second, third))))
}

##
##
##


ARTISTS <- read.csv("./artists.tsv", sep="\t")

our_artists <- ARTISTS$artists
our_artists_tags <- lapply(our_artists, mem_get_tag_frame_lfm)
names(our_artists_tags) <- our_artists

codes <-  as.numeric(factor(ARTISTS$my_genres))
ARTISTS$genre.code <- as.vector(codes)



cbms <- combn(our_artists, 2)

art_pairs <- data.frame(t(rbind(cbms)))
names(art_pairs) <- c("artist1", "artist2")

art_pairs$jacc <- apply(art_pairs, 1,
  function(arow){
    mem_jaccard_index(our_artists_tags[[unlist(arow[1])]],
                      our_artists_tags[[unlist(arow[2])]])
  }) 





sink("json.txt")

cat("{\n    \"nodes\": [\n")

lapply(our_artists, function(x){
       recs <- get_top_3(art_pairs, x, "jacc")
       cat(paste0("        { \"name\": \"", x, "\", \"group\": ",
                  get_genre_code(x), ", \"first\": \"", recs[1],
                  "\", \"second\": \"", recs[2], "\", \"third\": \"",
                  recs[3], "\" },\n"))
})

cat("    ],\n    \"links\": [\n")

apply(art_pairs, 1, function(arow){
      if(arow[5] > 0.25){
        cat(paste0("        {\"source\": ",
                   lookup_number(unlist(arow[1])),
                   ", \"target\": ",
                   lookup_number(unlist(arow[2])),
                   ", \"weight\": ",
                   arow[5],
                   ", \"node1\": \"",
                   arow[1],
                   "\", \"node2\": \"",
                   arow[2],
                   "\"},\n")) 
      } else{
        cat("")
      } })
 cat("\n    ]\n}\n")
sink()





