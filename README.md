
# genre-based-music-recommendations

![Alt text](./sample-image.png?raw=true "Title")

## Research on combining open source "tag" (genre) metadata to inform musical artist recommendations

See [this blog post](http://www.onthelambda.com/2016/01/11/genre-based-music-recommendations-using-open-data-and-the-problem-with-recommender-systems/)

Uses the last.fm API to get a list of stranger-submitted tags,
using jaccard's similarity index to quantify the similarity between each
pair of 70 artists, and spits out `artists.json`. `index.html` then reads
that json to construct a d3 visualization of the artist similarity
network. The d3 visualization includes tooltips that give artist
recommendations (the three most similar other artists).

