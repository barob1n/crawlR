


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
comps_th <- comps[comps$PHYS_COUNTRY %in% 'TH' & !is.na(comps$WEB_SITE),]

comps_th <- iir_url[!is.na(iir_url$WEB_SITE), ]

#power_urls <- read_csv("E:/vm_share/power_urls.txt",  col_names = FALSE)
seeds<- comps$WEB_SITE[1:5000]
out_dir="E:/crawl/"
fetch_list=NULL
crawl_delay=2
max_concurr=200
max_host=1
timeout=Inf
save_to_disk=F
return=T
queue_scl=10
comments=""

 library(lineprof)

seeds = unlist(power_urls)[1:5000];
seeds<- (unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
seeds_parse <- xml2::url_parse(seeds)
pages<-crawlR(
  seeds = seeds,
  timeout = Inf,
  max_concurr = 50,
  max_host = 1,
  out_dir = '~/crawl/',
  crwal_delay = 5,
  max_size = 2e6,
  urlRegExFilter = NULL,
  max_slots=100,
  depth=2)

## Options
##
out_dir="~/crawl/"
fetch_list=NULL
crawl_delay=NULL
max_concurr=200
max_host=1
timeout=Inf
save_to_disk=F
return=T
queue_scl=5
comments=""


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
