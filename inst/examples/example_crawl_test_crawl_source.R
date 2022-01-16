
setwd("~/crawlR")

# setwd("~/craw/test")
# setwd("E:/R_scripts/crawlR")
# setwd("Z:/System Volume Information/brian/Documents/crawlR/")
devtools::document()
devtools::install()
3
# library(crawlR)
# library(dplyr)
# library(future)
# library(promises)
# load("Z:/System Volume Information/brian/Documents/prco.RData")
# load("~/inteliScrape/iir_url.rda")
# load("Z:/System Volume Information/brian/Documents/Signal_old/iir_url.rda")
# load("E:/R_scripts/inteliScrape/iir_url.rda")
# load("C:/Users/brobinson/Google Drive/prco.RData")

load("E:/R_scripts/inteliScrape/iir_url.rda")
seedsau<- (iir_url$WEB_SITE[iir_url$PHYS_COUNTRY %in% c('AU','NZ')])

top_companies <- as.data.frame(table(na.omit(prco$COMPANY_ID)))
summary(top_companies$Freq)
top_companies <- as.character(top_companies$Var1[top_companies$Freq>=1 ])

top_owner <- as.data.frame(table(na.omit(prco$OWNER_ID)))
summary(top_owner$Freq)
top_owner <- as.character(top_owner$Var1[top_owner$Freq>=1 ])

seeds<- iir_url$WEB_SITE[iir_url$COMPANY_ID %in% c(top_owner,top_companies)]
seeds<-unique(c(seedsau,seeds))

seeds<-gsub('^www','http://www',seeds)
seeds<-seeds[!is.na(seeds)]
seeds<-seeds[!duplicated(xml2::url_parse(seeds)$server)]
seeds<-seeds[!is.na(xml2::url_parse(seeds)$server)]
seeds<-seeds[!is.na(xml2::url_parse(seeds)$scheme)]
seeds<-seeds[!(xml2::url_parse(seeds)$server == "")]
seeds<-seeds[!(xml2::url_parse(seeds)$scheme == "")]
seeds<-seeds[!(grepl("^[0-9]",xml2::url_parse(seeds)$server) & grepl("[0-9]$",xml2::url_parse(seeds)$server))]
seeds <-gsub('(http://)([^w]{3})','\\1www.\\2',seeds)
seeds <-gsub('(https://)([^w]{3})','\\1www.\\2',seeds)


seeds<-seeds[!(grepl("^[0-9]",xml2::url_parse(seeds)$server) )]
res_seeds<- readLines('E:\\R_scripts\\crawlR\\inst\\scripts\\research_urls.txt')
seeds<-unique(c(seeds))
writeLines(seeds, con ='E:\\R_scripts\\crawlR\\inst\\scripts\\seeds_top_owner_comp.txt')

# library(readr)
# iir_urls_big_list <- read_csv("E:/vm_share/iir_urls_big_list.csv", col_names = FALSE)
# seeds <- iir_url$WEB_SITE[1:6500]
rm(iir_url)
rm(tmp)
rm(prco)




seeds <- readLines('./inst/scripts/seeds_all.txt')
seeds<-unique(seeds)



## Setup
work_dir <- '~/crawl/test/'
out_dir <- paste0(work_dir)
max_concurr = 150        # max concurrent connections - total
max_concurr_invest = 70  # invest pages (10-k) are text heavy - too many connect may timeout
max_host = 1             # max concurrent connections - per host
crawl_delay = 30         # delay in seconds between sucessvie requests same host
timeout = Inf            # total time for crawling ALL urls
topN=15000
seeds_only =T


seeds_res <- readLines('./inst/scripts/research_urls.txt')
## Get initial pages
crawlR::crawlR(
  seeds = seeds_res,
  work_dir = work_dir,
  out_dir =  out_dir,
  max_concurr = max_concurr,
  max_host = max_host,
  timeout = timeout,
  external_site = F,
  crawl_delay=crawl_delay,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = NULL,
  depth = 1,
  queue_scl = 1,
  topN=NULL,
  max_urls_per_host = 10,
  n_threads = 1,
  parser = crawlR:::parse_content,
  seeds_only=T,
  log_file = 'crawl_log.txt',
  crawl_int = 1)




## Get Projects/News links
depth <- 1
topN <- 5000
out_dir <- paste0(work_dir,'projects/')

projFiltIn <- list()
projFiltIn[paste0(1:depth)]<-"study|studies|portfolio|project|press|announce|news|completed|future|past|complete|construction|current";

projFiltOut <- list()
projFiltOut[paste0(1:depth)]<-"\\.xml|facebook|linkedin|instagram|career|job|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact|team|quarter|annual";

crawlR::crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,
  max_concurr = max_concurr,
  max_host = max_host,
  timeout = timeout,
  external_site = F,
  crawl_delay=crawl_delay,
  max_size = 4e6,
  regExOut = projFiltOut,
  regExIn = projFiltIn,
  depth = depth,
  queue_scl = 1,
  topN=topN,
  max_urls_per_host = 5,
  n_threads = 4,
  parser = crawlR:::parse_content,
  seeds_only=F)


## Get Investment Articles
depth<-3
topN <- 5000
out_dir <- paste0(work_dir,'invest/')

investFiltIn <- list()
investFiltIn[paste(1:depth)]<-"pdf|report|quarterly-report|annual-report|finance|invest|asset|holding|10k|10-k|10q|10-q|quarterly|earning|annual";

investFiltOut <- list()
investFiltOut[paste(1:depth)]<-"facebook|linkedin|instagram|career|job|contact|about|openings|team";

crawlR::crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,
  max_concurr = max_concurr,
  max_host = max_host,
  timeout = timeout,
  external_site = F,
  crawl_delay=crawl_delay,
  max_size = 4e6,
  regExOut = investFiltOut,
  regExIn = investFiltIn,
  depth = depth,
  queue_scl = 1,
  topN=topN,
  max_urls_per_host = 10,
  n_threads = 4,
  parser = crawlR:::parse_content)


# ## Get Initial Contact information
# depth <- 1
# topN <- NULL
# out_dir <- paste0(work_dir,'contact/')
#
# contactFiltIn <- list()
# contactFiltIn[paste(1:depth)]<-"contact"
#
# contactFiltOut <- list()
# contactFiltOut[paste0(1:depth)]<-paste0(
#   "report|quarterly-report|annual-report|finance|invest|asset|",
#   "holding|10k|10-k|10q|10-q|quarterly|earning|annual|",
#   "study|studies|portfolio|project|press|announce|event|news|",
#   "completed|future|past|complete|construction|current");
#
# crawlR(
#   seeds = NULL,
#   work_dir = work_dir,
#   out_dir =  out_dir,
#   max_concurr = max_concurr,
#   max_host = max_host,
#   timeout = timeout,
#   external_site = F,
#   crawl_delay=crawl_delay,
#   max_size = 4e6,
#   regExOut = contactFiltOut,
#   regExIn = contactFiltIn,
#   depth = depth,
#   queue_scl = 1,
#   topN=NULL,
#   max_urls_per_host = 15,
#   n_threads = 2,
#   parser = crawlR:::parse_contact)




















































## Get initial pages
crawlR::crawlR(
  seeds = seedsAll[1:100],
  work_dir = work_dir,
  out_dir =  out_dir,
  max_concurr = 200,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=10,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = NULL,
  depth = 1,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 1,
  n_threads = 1,
  parser = crawlR:::parse_content)








## Get Projects

regExFiltIn <- list()
regExFiltIn[['1']]<-"study|studies|portfolio|project|press|announce|event|news|completed|future|past|complete|construction|current";
regExFiltIn[['2']]<-"study|studies|portfolio|project|press|announce|event|news|completed|future|past|complete|construction|current";
regExFiltIn[['3']]<-"study|studies|portfolio|project|press|announce|event|news|completed|future|past|complete|construction|current";

regExFiltOut <- list()
regExFiltOut[['1']]<-"career|job|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";
regExFiltOut[['2']]<-"career|job|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";
regExFiltOut[['3']]<-"career|job|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";

out_dir <- paste0(work_dir,'projects/')

crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 200,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=10,
  max_size = 4e6,
  regExOut = regExFiltOut,
  regExIn = regExFiltIn,
  depth = 4,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 15,
  n_threads = 2,
  parser = crawlR:::parse_content
)


## Get Investment Articles

regExFiltIn <- list()
regExFiltIn[['1']]<-"report|quarterly-report|annual-report|finance|invest|asset|holding|10k|10-k|10q|10-q";
regExFiltIn[['2']]<-"report|quarterly-report|annual-report|finance|invest|asset|holding|10k|10-k|10q|10-q";
regExFiltIn[['3']]<-"report|quarterly-report|annual-report|finance|invest|asset|holding|10k|10-k|10q|10-q";

regExFiltOut <- list()
regExFiltOut[['1']]<-"career|job|contact";
regExFiltOut[['2']]<-"career|job|contact";
regExFiltOut[['3']]<-"career|job|contact";

out_dir <- paste0(work_dir,'invest/')

crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 100,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=10,
  max_size = 4e6,
  regExOut = regExFiltOut,
  regExIn = regExFiltIn,
  depth = 3,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 15,
  n_threads = 2,
  parser = crawlR:::parse_content
)



regExFiltIn <- list()
regExFiltIn[['1']]<-"career|job|opening|hiring|opening|work|team|join"
regExFiltIn[['2']]<-"career|job|opening|hiring|opening|work|team|join"
regExFiltIn[['3']]<-"career|job|opening|hiring|opening|work|team|join"

regExFiltOut <- list()
regExFiltOut[['1']]<-"project|press|announce|event|news|completed|future|past|complete|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";
regExFiltOut[['2']]<-"project|press|announce|event|news|completed|future|past|complete|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";
regExFiltOut[['3']]<-"project|press|announce|event|news|completed|future|past|complete|finance|invest|asset|holding|10k|10-k|10q|10-q|earnings|contact";



out_dir <- paste0(work_dir,'career/')

crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 200,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=10,
  max_size = 4e6,
  regExOut = regExFiltOut,
  regExIn = regExFiltIn,
  depth = 2,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 15,
  n_threads = 2,
  parser = crawlR:::parse_content
)


regExFiltIn <- list()
regExFiltIn[['1']]<-"contact|locations"
regExFiltIn[['2']]<-"contact|locations"
regExFiltIn[['3']]<-"contact|locations"

out_dir <- paste0(work_dir,'contact/')

crawlR(
  seeds = NULL,
  work_dir = work_dir,
  out_dir =  out_dir,  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 200,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=10,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFiltIn,
  depth = 1,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 15,
  n_threads = 2,
  parser = crawlR:::parse_contact
)



#rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join"





























# this_dir<-'~/crawl/fetch_20190820202710/parsed'
# these_files<- list.files(this_dir,full.names = T)
this_func<-function(this_file){

  this_file<-these_files[100]
  con <- gzfile(this_file)
  this_file_reg<-readLines(con=con)
  test$insert(this_file)
  this_file<-jsonlite::fromJSON(readLines(con=con))
  this_file$headers<-curl::parse_headers_list(rawToChar(this_file$headers))
  this_file<-jsonlite::fromJSON(readLines(con=con))

}

url
id
lastModified
updatedDate
usedoc
type
title

crawlR(
seeds = seeds,
out_dir = '~/crawl/',
max_concurr = 200,
max_host = 1,
timeout = Inf,
external_site = F,
sitemaps = F,
crawl_delay=3,
max_size = 3e6,
regExOut = regExFilt,
depth = 4,
queue_scl = 1,
topN=50000,
max_urls_per_host = 10,
parser = parseR
)
# seeds = unlist(power_urls)[1:5000];
# seeds<- (unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
# seeds_parse <- xml2::url_parse(seeds)

seeds <- iir_urls_big_list$X1
pages<-crawlR(
  seeds = seeds,
  timeout = Inf,
  max_concurr = 150,
  max_host = 1,
  out_dir = 'E:/crawl/',
  crawl_delay=3,
  max_size = 3e6,
  regExOut = regExFilt,
  max_slots = 100,
  depth=4,
  parser= NULL,
  queue_scl=10)

## Options
##

links$wait_table <- data.frame(
  root = names(as.list(linkDB)),
  dt   = initTime,
  stringsAsFactors = F)

# setup initial links
for(i in ls(linkDB)){
  links$url[[i]] <- unlist(linkDB[[i]])
}

inF <- "~/crawl/crawlR_bbeff7055f67995db75fcc454ce1e165.html"
temp<-rtika::tika_json_text(input=inF)
temp<-jsonlite::fromJSON(temp)

batch <- c(
  system.file("extdata", "jsonlite.pdf", package = "rtika"),
  system.file("extdata", "curl.pdf", package = "rtika"),
  system.file("extdata", "table.docx", package = "rtika"),
  system.file("extdata", "xml2.pdf", package = "rtika"),
  system.file("extdata", "R-FAQ.html", package = "rtika"),
  system.file("extdata", "calculator.jpg", package = "rtika"),
  system.file("extdata", "tika.apache.org.zip", package = "rtika")
)

list.files("~/crawl", full.names = T)
text = tika(batch,output='text',output_dir = "~/crawl/parsed", threads=)
cat(substr(text[1],45,450))
