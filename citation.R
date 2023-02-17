library(scholar)
library(jsonlite)
library(ggplot2)
library(ggstance)
library(ggimage)
library(ggtree)

readRenviron(".Renviron")
# set_scholar_mirror("https://sc.panda321.com/")
id <- '9hAQ1LYAAAAJ'

devtools::load_all("../myCitation/")
url = build_scholar_publication_url(id)

if (FALSE){
  chromever = "110.0.5481.77"
  geckover = "0.30.0"
  port = 4445L
  killtask_by_port(port)
  driver<- RSelenium::rsDriver(browser = "chrome",
                               port = port,
                               chromever = chromever,
                               geckover = NULL,
                               phantomver = NULL)
  browser <- driver[["client"]]
  
  browser$navigate(url)
  
  pageSource = browser$getPageSource()[[1]]
}


html = rvest::read_html(url)

process_scholar_page = function(html){
  require(rvest)
  require(stringr)
  input = html %>% html_elements("input")
  id = input[[which(html_attr(input, "name") == "user")]] %>% html_attr("value")
  tables <- html %>% html_table()
  stats <- tables[[1]]
  rows <- nrow(stats)
  name <- html %>% html_nodes(xpath = "//*/div[@id='gsc_prf_in']") %>% 
    html_text()
  bio_info <- html %>% html_nodes(xpath = "//*/div[@class='gsc_prf_il']") %>% 
    html_text()
  interests <- html %>% html_nodes(xpath = "//*/div[@id='gsc_prf_int']") %>% 
    html_children() %>% html_text()
  affiliation <- bio_info[1]
  specs <- iconv(bio_info[2], from = "UTF8", to = "ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
  homepage <- html %>% html_nodes(xpath = "//*/div[@id='gsc_prf_ivh']//a/@href") %>% 
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

get_profile_pageSource = function (pageSource){
  if (is.null(pageSource)) 
        return(NA)
    page <- pageSource %>% read_html()
  return(page)
}


profile = process_scholar_page(html)
if (!is.null(profile)) {
    profile$date <- Sys.Date()
    xfun::write_utf8(toJSON(profile), "profile.json")
}

  
get_citation_history = function (html){
    years <- html %>% html_nodes(xpath = "//*/span[@class='gsc_g_t']") %>% 
        html_text() %>% as.numeric()
    vals <- html %>% html_nodes(xpath = "//*/span[@class='gsc_g_al']") %>% 
        html_text() %>% as.numeric()
    if (length(years) > length(vals)) {
        style_tags = html %>% html_nodes(css = ".gsc_g_a") %>% 
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


citation <- get_citation_history(html)

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
