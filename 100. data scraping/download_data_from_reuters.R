# parse command line arguments
suppressMessages({
  library(argparser)
  library(magrittr)
})
argv <- arg_parser("Scrape www.reuters.com") %>%
  add_argument("--from",  help="page number (start)",    default=1L) %>%
  add_argument("--to",    help="page number (end)",      default=30L) %>%
  add_argument("--cache", help="number of pages cached", default=10L) %>%
  add_argument("--db",    help="sqlite database path",   default="") %>%
  add_argument("--csv",   help="output csv file",        default="") %>%
  parse_args()


# settings
suppressMessages({
  library(progress)
  library(foreach)
  library(doParallel)
  library(readr)
  library(dplyr)
  library(purrr)
  library(rvest)
})
cl <- makeCluster(detectCores())
registerDoParallel(cl)


# global constants
BASE_URL    = "http://www.reuters.com/news/archive/marketsNews?page="
DB_PATH     = ifelse(argv$db == "", tempfile(tmpdir=".", fileext=".sqlite3"), argv$db)
PAGE_START  = argv$from
PAGE_END    = argv$to
CACHE_PAGES = argv$cache
OUTPUT_CSV  = ifelse(argv$csv == "", FALSE, TRUE)
CSV_PATH    = argv$csv


# functions
## create empty stories object
empty_stories <- function(size=0) {
  data.frame(
    headline_url = character(size),
    story_url    = character(size),
    title        = character(size),
    text         = character(size),
    time         = character(size)
  )
}

##  scrape stories in the given page
get_stories <- function(page_url) {
  tryCatch({
    stories <-
      read_html(page_url, encoding = "UTF-8") %>%
      html_nodes("article.story") %>%
      map(function(s) {
        url   <- s %>% html_node(".story-title a") %>% html_attr("href")
        title <- s %>% html_node(".story-title a") %>% html_text()
        text  <- s %>% html_node("p") %>% html_text()
        time  <- s %>% html_node(".timestamp") %>% html_text()
        tibble(headline_url=page_url, story_url=url, title=title, text=text, time=time)
      }) %>% bind_rows()
    stories
  }, error=function(e) {
    print(page_url)
    print(e)
    empty_stories()
  })
}


# setup database
db <- src_sqlite(DB_PATH, create=TRUE)
stories_db <- copy_to(db, empty_stories(), name="stories", temporary = FALSE)


# start scraping
pb <- progress_bar$new(
  total  = (PAGE_END - PAGE_START + 1) %/% CACHE_PAGES, #PAGE_END - PAGE_START - 1,
  format = "scraping [:bar] :percent in :elapsed"
)

for(i in seq(PAGE_START, PAGE_END, CACHE_PAGES)) {
  stories <- foreach(j=seq(i, min(i+CACHE_PAGES-1, PAGE_END)),
                     .combine=bind_rows, .packages = c("dplyr", "purrr", "rvest")) %dopar%
                     {
                       page_url <- paste0(BASE_URL, j)
                       get_stories(page_url)
                     }
  try({
    db_insert_into(db$con, "stories", stories)
  })
  pb$tick()
}


# save as a csv file
if(OUTPUT_CSV) {
  output_tbl <- tbl(db, "stories") %>% collect()
  write_csv(output_tbl, "cs.csv")
}
