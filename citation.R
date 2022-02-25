library(scholar)
library(jsonlite)
library(ggplot2)
library(ggstance)
library(ggimage)
library(ggtree)

readRenviron(".Renviron")
set_scholar_mirror("https://sc.panda321.com/")
id <- '9hAQ1LYAAAAJ'

devtools::load_all("../myCitation/")
chromever = "96.0.4664.45"
geckover = "0.30.0"
port = 4445L
killtask_by_port(port)
driver<- RSelenium::rsDriver(browser = "firefox",
                             port = port,
                             chromever = NULL,
                             geckover = geckover,
                             phantomver = NULL)
browser <- driver[["client"]]

url = build_scholar_publication_url(id)
browser$navigate(url)

pageSource = browser$getPageSource()[[1]]

get_profile_pageSource = function (pageSource) 
{
  require(rvest)
  require(stringr)
    if (is.null(pageSource)) 
        return(NA)
    page <- pageSource %>% read_html()
    input = page %>% html_elements("input")
    id = input[[which(html_attr(input, "name") == "user")]] %>% html_attr("value")
    tables <- page %>% html_table()
    stats <- tables[[1]]
    rows <- nrow(stats)
    name <- page %>% html_nodes(xpath = "//*/div[@id='gsc_prf_in']") %>% 
        html_text()
    bio_info <- page %>% html_nodes(xpath = "//*/div[@class='gsc_prf_il']") %>% 
        html_text()
    interests <- page %>% html_nodes(xpath = "//*/div[@id='gsc_prf_int']") %>% 
        html_children() %>% html_text()
    affiliation <- bio_info[1]
    specs <- iconv(bio_info[2], from = "UTF8", to = "ASCII")
    specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
    homepage <- page %>% html_nodes(xpath = "//*/div[@id='gsc_prf_ivh']//a/@href") %>% 
        html_text()
    return(list(id = id, name = name, affiliation = affiliation, 
                total_cites = as.numeric(as.character(stats[rows - 2, 
                                                            2])), 
                h_index = as.numeric(as.character(stats[rows - 1, 2])), 
                i10_index = as.numeric(as.character(stats[rows, 2])), 
                fields = specs, 
                homepage = homepage, 
                interests = interests
    ))
}

profile <- tryCatch(get_profile_pageSource(pageSource), error = function(e) stop(paste("Could not get Google scholar profile for id: ",id,e,sep="\t")))
if (!is.null(profile)) {
    profile$date <- Sys.Date()
    xfun::write_utf8(toJSON(profile), "profile.json")
}

  
get_citation_history_pageSource = function (pageSource) 
{

    if (is.null(pageSource)) 
        return(NA)
    page <- pageSource %>% read_html()
    years <- page %>% html_nodes(xpath = "//*/span[@class='gsc_g_t']") %>% 
        html_text() %>% as.numeric()
    vals <- page %>% html_nodes(xpath = "//*/span[@class='gsc_g_al']") %>% 
        html_text() %>% as.numeric()
    if (length(years) > length(vals)) {
        style_tags = page %>% html_nodes(css = ".gsc_g_a") %>% 
            html_attr("style")
        zindices = as.integer(stringr::str_match(style_tags, 
                                                 "z-index:([0-9]+)")[, 2])
        allvals = integer(length = length(years))
        allvals[zindices] = vals
        vals = rev(allvals)
    }
    df <- data.frame(year = years, cites = vals)
    return(df)
}


citation <- tryCatch(get_citation_history_pageSource(pageSource), error = function(e) return(NULL))

if (!is.null(citation)) {
    xfun::write_utf8(toJSON(citation), "citation.json")
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
