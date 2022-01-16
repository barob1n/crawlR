


iir_url <- iir_url[iir_url$COMPANY_STATUS %in% 'C',]

con <- oracleData::db_connection_myreports()
q <- paste("select  project_id, company_id from IIRLIVE.projects_company as c
           left join pec_view p on
           p.project_id = c.company_id")

iir_pr_comp <- ROracle::dbGetQuery(con, q)
q <- paste("select PROJECT_ID, OWNER_ID, PLANT_PARENT_ID, INDUSTRY_CODE from iirlive.pec_view where WORLD_REGION_DESC = 'North America' ")#where project_id = 300040636
res <- ROracle::dbGetQuery(con, q)

ag <- aggregate(PROJECT_ID ~ COMPANY_ID, res,FUN=NROW)
ag2 <- aggregate(PROJECT_ID ~ PLANT_PARENT_ID, res,FUN=NROW)

ag <- ag[ag$COMPANY_ID %in% iir_url$COMPANY_ID,]

links <- ag$COMPANY_ID[order(ag$PROJECT_ID,decreasing = T)]
links <- iir_url$WEB_SITE[iir_url$COMPANY_ID %in% links[1:1000]]
links <-ifelse(grepl('^www',links), paste0('https://',links),links )
links <-ifelse(grepl('^co',links), paste0('https://',links),links )
seeds <- gsub('http://','https://', links[1:100,1])

library(crawlR)
seeds = c('https://www.ford.com','https://www.ge.com','https://www.shell.com' )

con<-oracleData::db_connection_myreports()
comps <- ROracle::dbGetQuery(con, "select COMPANY_ID, company_name, phys_country, phys_state, phys_city, web_site
                             from iirlive.companies where record_status = 'E' and industry_code = '01'")
comps_pow <- comps[!is.na(comps$WEB_SITE),]
# comps<- iir_url


seeds<-comps$WEB_SITE[comps$INDUSTRY_CODE %in% '01']
comps_th <- comps[comps$PHYS_COUNTRY %in% 'TH' & !is.na(comps$WEB_SITE),]

comps_th <- iir_url[iir_url$PHYS_COUNTRY %in% 'TH'& !is.na(iir_url$WEB_SITE), ]

# power_urls <- read.csv("E:/vm_share/power_urls.txt",  col.names = FALSE)
regExFilt <- list()
regExFilt[['2']]<-"career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project";
regExFilt[['3']]<-"career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project";
regExFilt[['4']]<-"career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project";
regExFilt[['5']]<-"career|job|invest|opportunit|financ|quarterly|earning|spending|financial|shares|10k|10-k|10q|10-q|news|announce|press|event|project";



#www.www1.nationalgridus.com
# seeds = unlist(power_urls)[2501:10000];
# seeds<- (unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
# seeds_parse <- xml2::url_parse(seeds)
timeout = 7200;
max_concurr=100;
max_host = 1;
out_dir = '~/crawl/';
crawl_delay = 3;
max_size = 2e6;
urlRegExFilterOut = NULL;
max_slots=100;
depth=4;
parser=NULL;
queue_scl=10;
save_to_disk<-F

setwd("E:/wamp/apache2/htdocs/crawlR")
devtools::document()
devtools::install()
library(crawlR)

seeds = unlist(power_urls)[5001:10000];
seeds<- (unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
seeds_parse <- xml2::url_parse(seeds)
pages<-crawlR(
  seeds = seeds,
  timeout = Inf,
  max_concurr = 50,
  max_host = 1,
  out_dir = 'E:/crawl/crawl2/',
  crawl_delay=3,
  max_size = 3e6,
  urlRegExFilterOut = regExFilt,
  max_slots = 100,
  depth=4,
  parser= NULL,
  queue_scl=4)

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
