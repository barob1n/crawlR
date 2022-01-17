#' Inject Seed List into CrawlDB.
#'
#'
#'
#' @param out_dir  Output Directory directory for this crawl.
#' @param work_dir (Required) Working Directory directory for this crawl.
#' @param seeds Url's to inject.
#' @param log_file Name of log file. If null, writes to stdout().
#' @return Returns character vector of links, or error message.
#' @export
#'
injectR <- function(out_dir = NULL,
                    work_dir=NULL,
                    seeds = NULL,
                    log_file = NULL
                    ){

  tryCatch({

    log_con<-set_log_file(log_file)

    if(is.null(out_dir))     stop('Output directory not provided.')
    if(is.null(work_dir))    stop('Work directory not provided.')
    if(!dir.exists(work_dir))stop('Work directory does not exist.')
    if(is.null(seeds))       stop('A seed list must be provided.')

    crawlDB <- DBI::dbConnect(RSQLite::SQLite(), paste0(work_dir,"crawlDB.sqlite"))
    if(NROW(DBI::dbListObjects(crawlDB))>0){
      linkDB <- dplyr::tbl(crawlDB, "linkDB")
    }else{
      linkDB <- NULL
    }

    ## Create a linkDB from list of seeds
    seeds<-na.omit(unlist(lapply(unlist(seeds),function(x) normalize_url(x))))
    seeds<- ifelse(!grepl('^http',seeds), paste0('http://', seeds), seeds)
    seeds<-gsub('/$','',seeds)
    ip <- gsub('[0-9.]','',seeds)
    seeds <- seeds[ip!=""]

    seeds<-xml2::url_parse(unlist(seeds)) %>%
      mutate(url= paste0(scheme,"://",server,path,'?',query)) %>%
      mutate(url=gsub('\\?$|/$','',url)) %>%
      mutate(crawled=0,
             next_crawl=as.numeric(Sys.Date()),
             is_seed=1,
             depth=0,
             crawl_int=1,
             last_crawl=as.numeric(Sys.Date())) %>%
      filter(!(scheme == "" | server == "")) %>%
      filter(!duplicated(url))

    seeds$score<-0

    col_order <- c('scheme','server','port','user','path','query',
                   'fragment','crawled','next_crawl','url',
                   'is_seed','depth','crawl_int','last_crawl','score')


    seeds<-seeds[!duplicated(seeds$url),]

    if(NROW(DBI::dbListObjects(crawlDB))>0){

      q <- paste("
        insert or replace into linkDB
          select
          seeds.scheme,
          seeds.server,
          seeds.port,
          seeds.user,
          seeds.path,
          seeds.query,
          seeds.fragment,
          seeds.crawled,
          seeds.next_crawl,
          seeds.url,
          seeds.is_seed,
          seeds.depth,
          seeds.crawl_int,
          seeds.last_crawl,
          seeds.score
        from seeds")

      # CREATE UNIQUE INDEX unq_sap_id ON sap(id);
      DBI::dbWriteTable(crawlDB,'seeds',seeds[,col_order],temporary=T,append=F)
      DBI::dbExecute(crawlDB, q)
      DBI::dbExecute(crawlDB,'drop table seeds')
    }else{
      DBI::dbWriteTable(crawlDB,'linkDB',seeds[,col_order],append=T)
      DBI::dbExecute(crawlDB,'CREATE UNIQUE INDEX url ON linkDB(url)')
    }

  }, error = function(e){
    e<-paste('injectR:',e)
    writeLines(e, con=log_con)
    class(e) <- 'error'
    e

  }, finally = {
    DBI::dbDisconnect(crawlDB)
    if(class(log_con)[1]=="file") close(log_con)
  })
}


