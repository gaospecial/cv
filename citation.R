library(scholar)
library(jsonlite)
library(ggplot2)
library(ggstance)
library(ggimage)
library(ggtree)


id <- 'DO5oG40AAAAJ'

profile <- tryCatch(get_profile(id), error = function(e) return(NULL))
if (!is.null(profile)) {
    cat(toJSON(profile), file ="profile.json")
}

citation <- tryCatch(get_citation_history(id), error = function(e) return(NULL))

if (!is.null(citation)) {
    cat(toJSON(citation), file = "citation.json")
}

citation <- fromJSON("citation.json")
citation$year <- factor(citation$year)
p <- ggplot(citation, aes(cites, year)) + 
    geom_barh(stat='identity', fill = "#96B56C") + 
    geom_text2(aes(label=cites, subset = cites > 500), hjust=1.1, size=5) + 
    theme_minimal(base_size=14) + xlab(NULL) + ylab(NULL) +
    theme_transparent() +
    labs(caption = "data from Google Scholar")

ggsave(p, file = "citation.png", width=3.5, height=9)


library(magick)
p <- image_read("citation.png")
p <- image_transparent(p, "white")
image_write(p, path="citation.png")