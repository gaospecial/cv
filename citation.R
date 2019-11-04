library(scholar)
library(jsonlite)
library(ggplot2)
library(ggstance)
library(ggimage)
library(ggtree)

readRenviron("proxy.Renviron")

id <- '9hAQ1LYAAAAJ'

profile <- tryCatch(get_profile(id), error = function(e) stop(paste("Could not get Google scholar profile for id: ",id,e,sep="\t")))
if (!is.null(profile)) {
    profile$date <- Sys.Date()
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
    labs(caption = "data from Google Scholar") +
    scale_x_continuous(position="top") +
    theme_minimal(base_size=14) + xlab(NULL) + ylab(NULL) +
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype="dashed"),
          plot.caption=element_text(colour='grey30')) +
    theme_transparent() 

ggsave(p, file = "citation.png", width=2.5, height=3, bg = "transparent")


## library(magick)
## p <- image_read("citation.png")
## p <- image_transparent(p, "white")
## image_write(p, path="citation.png")
