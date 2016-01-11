
# genre-based-music-recommendations

![Alt text](./sample-image.png?raw=true "Title")

## Research on combining open source "tag" (genre) metadata to inform musical artist recommendations

`make-recommendation-json.R` reads the artist/manually-labeled-genre pairs from
`artists.csv`, uses the last.fm API to get a list of stranger-submitted tags,
using jaccard's similarity index to quantify the similarity between each
pair of 70 artists, and spits out pseudo-json in `json.txt` which requires
minimal editing to make valid json. `index.html` then reads the valid json,
`artists.json` to construct a d3 visualization of the artist similarity
network. The d3 visualization includes tooltips that give artist
recommendations (the three most similar other artists).

Why I didn't write the R script to spit out valid JSON right from the get
is still unclear. I'd wager it's laziness.
