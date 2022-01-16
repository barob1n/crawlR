


# iir_url <- iir_url[iir_url$COMPANY_STATUS %in% 'C',]
#
# con <- oracleData::db_connection_myreports()
# q <- paste("select  project_id, company_id from IIRLIVE.projects_company as c
#            left join pec_view p on
#            p.project_id = c.company_id")
#
# iir_pr_comp <- ROracle::dbGetQuery(con, q)
# q <- paste("select PROJECT_ID, OWNER_ID, PLANT_PARENT_ID, INDUSTRY_CODE from iirlive.pec_view where WORLD_REGION_DESC = 'North America' ")#where project_id = 300040636
# res <- ROracle::dbGetQuery(con, q)
#
# ag <- aggregate(PROJECT_ID ~ COMPANY_ID, res,FUN=NROW)
# ag2 <- aggregate(PROJECT_ID ~ PLANT_PARENT_ID, res,FUN=NROW)
#
# ag <- ag[ag$COMPANY_ID %in% iir_url$COMPANY_ID,]
#
# links <- ag$COMPANY_ID[order(ag$PROJECT_ID,decreasing = T)]
# links <- iir_url$WEB_SITE[iir_url$COMPANY_ID %in% links[1:1000]]
# links <-ifelse(grepl('^www',links), paste0('https://',links),links )
# links <-ifelse(grepl('^co',links), paste0('https://',links),links )
# seeds <- gsub('http://','https://', links[1:100,1])
#
# library(crawlR)
# seeds = c('https://www.ford.com','https://www.ge.com','https://www.shell.com' )


# comps_pow <- comps[!is.na(comps$WEB_SITE),]
# # comps<- iir_url
# comps <- comps[!is.na(comps$WEB_SITE),]
#
# comps_th <- iir_url[iir_url$PHYS_COUNTRY %in% 'TH'& !is.na(iir_url$WEB_SITE), ]

# power_urls <- read.csv("E:/vm_share/power_urls.txt",  col.names = FALSE)

con<-oracleData::db_connection_myreports()
comps <- ROracle::dbGetQuery(con, "select COMPANY_ID, company_name, phys_country, phys_state, phys_city, web_site
                             from iirlive.companies where record_status = 'E' and industry_code = '01'")

# load("~/inteliScrape/iir_url.rda")
 load("Z:/System Volume Information/brian/Documents/Signal/iir_url.rda")
# load("E:/R_scripts/inteliScrape/iir_url.rda")
pr <- readRDS("~/all_data/Projects ALL - Global - from Oracle.rds")
pl <- readRDS("~/all_data/Plants ALL - Global - from Oracle - FEWER COLS.rds")
#pr <- oracleData::getProjects(columns=c("OWNER_ID"))
#con <- oracleData::db_connection_myreports()
#pl <- ROracle::dbGetQuery(con," select COMPANY_NAME OWNER_NAME, ULTIMATE_PARENT_NAME PARENTNAME from iirlive.plant_view_mv")
pl$OWNER_NAME<-as.character(pl$OWNER_NAME)
pl$PARENTNAME<-as.character(pl$PARENTNAME)
theseIds<-c(iir_url$COMPANY_ID[match(pl$OWNER_NAME, iir_url$COMPANY_NAME)],
            iir_url$COMPANY_ID[match(pl$PARENTNAME, iir_url$COMPANY_NAME)],
            pr$OWNER_ID)
theseIds <- data.frame(ids=theseIds,freq=1)
theseIds<-aggregate(freq~ids,theseIds,FUN=sum)
#theseIds<-table(theseIds);
theseIds<-theseIds[order(theseIds$freq,decreasing = T),]
#View(theseIds)
rm(list=c('pl','pr'))
gc()


# install.packages('dplyr')
# install.packages('future')
# install.packages('promises')
# install.packages('rvest')
# install.packages('xml2')
# install.packages('Rtools')
# install.packages('data.table')
# install.packages('rtika')

#  setwd("~/crawlR")
 setwd("E:/R_scripts/crawlR")
#  setwd("Z:/System Volume Information/brian/Documents/crawlR/")
devtools::document()
devtools::install()
3
library(crawlR)
library(dplyr)
library(future)
library(promises)
#library(chromote)
source('E:/R_scripts/crawlR/R/updateR.R', echo=TRUE)
source('E:/R_scripts/crawlR/R/injectR.R', echo=TRUE)
source('E:/R_scripts/crawlR/R/fetchR.R', echo=TRUE)
source('E:/R_scripts/crawlR/R/crawlR_helpers.R', echo=TRUE)
source('E:/R_scripts/crawlR/R/generateR.R', echo=TRUE)

source('~/crawlR/R/updateR.R', echo=TRUE)
source('~/crawlR/R/injectR.R', echo=TRUE)
source('~/crawlR/R/fetchR.R', echo=TRUE)
source('~/crawlR/R/crawlR_helpers.R', echo=TRUE)
source('~/crawlR/R/generateR.R', echo=TRUE)

regExFilt <- list()
regExFilt[['2']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['3']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['4']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['5']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";


load("E:/R_scripts/inteliScrape/iir_url.rda")
#seeds<- (iir_url$WEB_SITE[match(as.numeric(theseIds$ids),iir_url$COMPANY_ID)])
seeds<- (iir_url[iir_url$PHYS_COUNTRY %in% c('AU','NZ'),])
seeds<-gsub('^www','http://www',seeds$WEB_SITE)
seeds<-seeds[!is.na(seeds)]
seeds<-seeds[!duplicated(xml2::url_parse(seeds)$server)]


#plan(multiprocess, workers = 5)
#seeds = comps$WEB_SITE[1:20000];
#seeds<- (unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
#seeds_parse <- xml2::url_parse(seeds)

# library(readr)
# iir_urls_big_list <- read_csv("E:/vm_share/iir_urls_big_list.csv", col_names = FALSE)
# seeds <- iir_urls_big_list$X1[1:20000]



seeds = seeds[1:300]
work_dir = 'E:/data_lake/crawl/AU2/'
out_dir =  'E:/data_lake/crawl/AU2/'  # 'E:/data_lake/crawl/AU/'  #
max_concurr = 70
max_host = 1
timeout = 7200
external_site = F
crawl_delay=5
max_size = 4e6
regExOut = NULL
regExIn = regExFilt
depth = 1
queue_scl = 1
topN=50000
max_urls_per_host = 15
parser = crawlR:::parse_content


regExFilt <- list()
regExFilt[['2']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['3']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['4']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['5']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";



## Get initial pages
crawlR(
  seeds = seeds,
  work_dir = 'E:/data_lake/crawl/AU2/',
  out_dir =  'E:/data_lake/crawl/AU2/',  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 100,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=5,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFilt,
  depth = 1,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 15,
  parser = crawlR:::parse_content
)




regExFilt <- list()
regExFilt[['1']]<-"contact|locations"
regExFilt[['2']]<-"contact|locations"
regExFilt[['3']]<-"contact|locations"

crawlR(
  seeds = NULL,
  work_dir = 'E:/data_lake/crawl/AU2/',
  out_dir =  'E:/data_lake/crawl/AU2/contact',  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 150,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=5,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFilt,
  depth = 1,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 2,
  n_threads = 4,
  parser = crawlR:::parse_contact
)



regExFilt <- list()
regExFilt[['1']]<-"career|job|opening|hiring|opening|work|team|join"
regExFilt[['2']]<-"career|job|opening|hiring|opening|work|team|join"
regExFilt[['3']]<-"career|job|opening|hiring|opening|work|team|join"



crawlR(
  seeds = seeds,
  work_dir = 'E:/data_lake/crawl/AU2/',
  out_dir =  'E:/data_lake/crawl/AU2/career',  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 100,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=5,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFilt,
  depth = 4,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 10,
  parser = crawlR:::parse_content
)


regExFilt <- list()
regExFilt[['2']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['3']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['4']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";
regExFilt[['5']]<-"rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join";




crawlR(
  seeds = seeds,
  work_dir = 'E:/data_lake/crawl/AU2/',
  out_dir =  'E:/data_lake/crawl/AU2/projects',  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 100,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=5,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFilt,
  depth = 4,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 10,
  parser = crawlR:::parse_content
)


rss|feed|asset|holdings|structure|capabilit|article|career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project|opening|hiring|opening|work|complet|team|join"





regExFilt <- list()
regExFilt[['1']]<-"contact|locations"
regExFilt[['2']]<-"contact|locations"
regExFilt[['3']]<-"contact|locations"

crawlR(
  seeds = NULL,
  work_dir = 'E:/data_lake/crawl/AU2/',
  out_dir =  'E:/data_lake/crawl/AU2/projects',  # 'E:/data_lake/crawl/AU/',  #
  max_concurr = 100,
  max_host = 1,
  timeout = 7200,
  external_site = F,
  crawl_delay=5,
  max_size = 4e6,
  regExOut = NULL,
  regExIn = regExFilt,
  depth = 1,
  queue_scl = 1,
  topN=50000,
  max_urls_per_host = 2,
  parser = crawlR:::parse_contact
)



























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
max_concurr = 100,
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
