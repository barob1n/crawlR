#' Update crawlDB
#'
#' @description
#' Updates the crawlDB with links discovered during crawl. The crawlDB must already exist.
#'
#'
#' @param work_dir (Required) Working directory for this crawl.
#' @param out_dir (Required) Working directory for this crawl.
#' @param log_file Name of log file. If null, writes to stdout().
#' @param overwrite If true, all crawled links will be entered into DB as fresh unseen links.
#' @param score_func Function to score urls
#' @return Returns character vector of links, or error message.
#' @export
#'
updateR <- function(work_dir = NULL,
                    out_dir=NULL,
                    log_file = NULL,
                    overwrite = F,
                    score_func=NULL
                    ){

  ## set up logging
  log_con<-crawlR:::set_log_file(log_file)

  ## enviroment to hold models used by scoring function
  score_func_env<-new.env()

  tryCatch({

    if(is.null(work_dir)) stop('Work directory not provided.')
    if(is.null(out_dir)) stop('Output directory not provided.')
    if(!dir.exists(work_dir)) stop('Work directory does not exist.')
    if(!file.exists(links)){stop("can't find previous runs fetched_links file.")}

    ## JSON file with extracted links
    links<-paste0(out_dir,"fetched_links.json.gz")

    ## file handler for links
    fh_links<-suppressWarnings(gzfile(links,open='rb'))

    load(paste0(out_dir,"fetch_list.rda"))

    ## remove the already fetech seeds
    fetch_list$url<-gsub('/$','',fetch_list$url)

    crawlDB <- DBI::dbConnect(RSQLite::SQLite(), paste0(work_dir,"crawlDB.sqlite"))
    fetched_links<-NULL

    cnt<-0
    ## read until end
    while(TRUE){

      ## cnt used for printing after certain number
      cnt<-cnt+1

      ## grab links output from parse phase
      fetched_links<-suppressWarnings(readLines(con=fh_links,n=100))

      ## break at end
      if(NROW(fetched_links)==0) break;

      ## parse links from json
      fetched_links<- lapply(fetched_links,function(x){
        x<-try({jsonlite::fromJSON(x)},silent=T)
        if(class(x)=='try-error'){
          return(NULL)
        }else{
          return(x)
        }
      })

      ## clean/format links
      fetched_links<-do.call(dplyr::bind_rows ,fetched_links)
      if(is.null(fetched_links$port)) fetched_links$port<-""
      fetched_links$url<-normalize_url(fetched_links$url)
      fetched_links<-fetched_links[!fetched_links$url %in% fetch_list$url,]
      fetched_links$crawled <- 0
      fetched_links$next_crawl <- as.numeric(Sys.Date())
      fetched_links$is_seed <- 0
      fetched_links$last_crawl <- as.numeric(Sys.Date())

      ## if no scoring function then score = 0.
      if(is.null(score_func)){
        fetched_links$score <- 0
        writeLines(paste("UpdateR: No Scoring function.  "), con=log_con)
      }else{

        ## scored in batches incase huge number of links are found
        n_rows<-NROW(fetched_links)
        n<-20000
        idx<-ceiling(n_rows/n)

        ## scored only on path. this might be too restrictive.
        urls<-unlist(fetched_links$path)

        fetched_links$score<-unlist(
          lapply(1:idx,function(i){
            if(cnt%%100==0) writeLines(paste("UpdateR: Scoring URL's. Batch #:",cnt), con=log_con)
            st<-(i-1)*n+1
            en<-min(n_rows,i*n)
            return(score_func(urls[st:en],url_vectorizer,glm_url_model,url_tfidf,url_lsa))
          })
        )
      }

      ## Must be in correct order before inserting into linkDB
      col_order <- c(
        'scheme', 'server', 'port', 'user', 'path', 'query',
        'fragment','crawled', 'next_crawl', 'url', 'is_seed',
        'depth','crawl_int', 'last_crawl','score')

      fetched_links <- fetched_links[,col_order]

      ## remove duplicates
      dup <- duplicated(fetched_links$url)
      DBI::dbWriteTable(
        crawlDB,
        'fetched_links',
        fetched_links[!dup,],
        overwrite=F, temporary=T, append=T)
    }

    ## unload files used by score func
    rm(list=ls(score_func_env),envir=score_func_env)

    if(overwrite){q<-" insert or replace into linkDB "}else{ q<-" insert or ignore into linkDB  "}
    q <- paste(q,"
      select
        fetched_links.scheme,
        fetched_links.server,
        fetched_links.port,
        fetched_links.user,
        fetched_links.path,
        fetched_links.query,
        fetched_links.fragment,
        fetched_links.crawled,
        fetched_links.next_crawl,
        fetched_links.url,
        fetched_links.is_seed,
        fetched_links.depth,
        fetched_links.crawl_int,
        fetched_links.last_crawl,
        fetched_links.score
      from fetched_links")
    DBI::dbExecute(crawlDB,q)

  },
  error = function(e){
    e<-paste('updateR:',e)
    writeLines(e, con=log_con)
    class(e) <- 'error'
    e
  },
  finally ={
    rm(fetched_links)
    DBI::dbDisconnect(crawlDB)
    if(class(log_con)[1]=="file")close(log_con)
  })


}

