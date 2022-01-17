#' Generate Fetch List of Url's from linkDB
#'
#'
#'
#' @param out_dir Output directory for this crawl.
#' @param work_dir (Required) Working directory for this crawl.
#' @param regExOut RegEx URL filter - omit links with these keywords.
#' @param regExIn  RegEx URL filter - keep links with these keywords.
#' @param max_depth maximum depth for selected url's
#' @param topN Choose these top links.
#' @param external_site Logical. If False, host outside the seed list will NOT be crawled.
#' @param max_urls_per_host Max URL's to generate per host.
#' @param crawl_delay crawl delay for requests to the same host
#' @param log_file Name of log file. If null, writes to stdout().
#' @param seeds_only gen only seeds
#' @param min_score minimum score for url
#' @return Returns character vector of links, or error message.
#' @export
#'
generateR <- function(out_dir=NULL,
                      work_dir=NULL,
                      regExOut=NULL,
                      regExIn=NULL,
                      max_depth =NULL,
                      topN = NULL,
                      external_site=F,
                      max_urls_per_host=10,
                      crawl_delay=NULL,
                      log_file=NULL,
                      seeds_only=F,
                      min_score=0
                      ){

  ## create output directory
  this_dir <- paste0(out_dir,"fetch_",gsub(":|-| ","",Sys.time()),"/")
  dir.create(this_dir)

  tryCatch({

    log_con<-set_log_file(log_file)

    ## Check values
    if(is.null(work_dir)){stop('Work directory was not provided.')}
    if(is.null(out_dir)){stop('Output directory was not provided.')}

    ## these should already be here
    crawlDB <- DBI::dbConnect(RSQLite::SQLite(), paste0(work_dir,"crawlDB.sqlite"))
    today   <- as.numeric(Sys.Date())

    ## set next crawl date
    get_next_crawl <- function(today, tbl,crawl_int=NULL){
      if(is.null(crawl_int)) crawl_int<-100*365
      if(!is.null(tbl)) tbl <- paste0(tbl,".")
      paste0( tbl,'crawled = 0 ')
    }

    ## do filt in
    get_filtin <- function(regExIn, tbl){
      if(is.null(regExIn)) return(NULL)
      if(!is.null(tbl)) tbl <- paste0(tbl,".")
      val <- paste0(" ",tbl,"path like ")
      q <- strsplit(regExIn,'\\|')[[1]]
      q <- paste0(val," '%",q,"%' ", collapse=' OR ')
      q <- paste( " ( ", q, " ) ")
      return(q)
    }

    ## do filter out
    get_filtout <- function(regExOut, tbl){
      if(is.null(regExOut)) return(NULL)
      if(!is.null(tbl)) tbl <- paste0(tbl,".")
      val <- paste0(" ",tbl,"path not like ")
      q   <- strsplit(regExOut,'\\|')[[1]]
      q   <- paste0(val," '%",q,"%' ", collapse=' AND ')
      q   <- paste( "   ", q, "   ")
      return(q)
    }

    ## get limit
    get_subquery<- function(max_urls_per_host, tbl=NULL){
      if(is.null(max_urls_per_host)) return(NULL)
      paste(" limit ",max_urls_per_host )
    }

    ## get max_depth
    get_max_depth<- function(max_depth, tbl=NULL){
      if(is.null(max_depth)) return(NULL)
      paste(" and depth <= ",max_depth )
    }

    ## get max_urls_per_host
    get_limit <- function(max_urls_per_host, tbl=NULL){
      if(is.null(max_urls_per_host)) return(NULL)
      paste(" limit ",max_urls_per_host )
    }



    ## get query
    generate_query <- function(q="select * from linkDB ",
                               next_crawl=NULL,
                               max_depth=NULL,
                               fin=NULL,
                               fout=NULL,
                               seeds_only=F,
                               subquery=NULL,
                               limit=NULL,
                               ext_site=external_site,
                               min_score=0

                               ){
      if(!ext_site) q <-paste(
        q,
        'inner join (select t3.server from linkDB  t3 where t3.is_seed=1) t2 on t1.server = t2.server')

      q <-paste(q, 'where', next_crawl)
      q <-paste(q,  max_depth)

      if(!is.null(fin) & !is.null(fout)) q <- paste(q,'and ( ', fin, 'and', fout,' and t1.score >= ', min_score, ' or t1.depth = 0 )')
      else if(!is.null(fin))  q <- paste(q,'and (', fin, ' and t1.score >= ', min_score, ' or t1.depth = 0 )')
      else if(!is.null(fout)) q <- paste(q,'and (', fout,' and t1.score >= ', min_score, ' or t1.depth = 0 )')

      if(seeds_only)          q <- paste(q,'and t1.is_seed=1')
      if(!is.null(limit))     q <- paste(q, ' ', limit)


      return(q)
    }


    # filter both th inner sub query and outer query to speed it up
    # order by score desc to get best scoring, then by depth asc
    # to get newest.
    q <-generate_query(
      q = paste("select
                ROW_NUMBER () OVER (
                PARTITION BY t1.server
                ORDER BY score desc, depth asc ) RowNum,
                t1.*
                from linkDB t1 "), # ORDER BY is_seed desc,
      next_crawl=get_next_crawl(today, 't1'),
      max_depth=get_max_depth(max_depth, 't1'),
      fin=get_filtin(regExIn, 't1'),
      fout=get_filtout(regExOut, 't1'),
      seeds_only=seeds_only,
      subquery=NULL,
      limit=NULL,
      ext_site=external_site,
      min_score=min_score)



    this_date<-as.numeric(Sys.Date())
    q <- paste("create temporary table fetch_list as ",q,
               " order by score desc, depth asc" )
    st<-Sys.time()
    writeLines(paste('GenerateR: Begining generate query - ',st), con=log_con)

    # get the fetch list
    DBI::dbExecute(crawlDB,q)

    DBI::dbExecute(crawlDB,paste0("delete from fetch_list where Rownum > ",max_urls_per_host))
    DBI::dbExecute(crawlDB,paste0(
      "delete from fetch_list where url not in (
         select url from fetch_list ",get_limit(topN), " )")) # order by is_seed desc, RANDOM()
    fetch_list<- DBI::dbGetQuery(crawlDB, paste0('select * from fetch_list'))
    fetch_list$RowNum <- NULL

    # writeLines(paste(fetch_list$is_seed), con=log_con)

    et <- Sys.time()
    writeLines(paste('GenerateR: Finished generate query - ',et), con=log_con)
    writeLines(paste('GenerateR: Found',NROW(fetch_list), 'urls.'), con=log_con)
    writeLines(paste('GenerateR: Query time - ',round(as.numeric(et)-as.numeric(st)),'seconds.'), con=log_con)

    next_crawl <- as.numeric(Sys.Date() + 10)
    DBI::dbExecute(crawlDB,
                     paste("update linkDB set crawled = 1",
                           ",next_crawl = ", as.numeric(Sys.Date()),
                           ",last_crawl = ", as.numeric(Sys.Date()),
                           "where url in ( select t2.url from fetch_list t2)"))
    #########################

    DBI::dbExecute(crawlDB, " drop table fetch_list")

    fetch_list <- create_fetch_list(fetch_list,crawl_delay)
    save(fetch_list,file=paste0(this_dir,'fetch_list.rda'))
    f_out<-gzfile(paste0(this_dir,'fetch_list.csv.gz'))
    write.csv(fetch_list, file=f_out)

  }, error = function(e){
    writeLines(paste('GenerateR: ',e), con=log_con)
    paste('GenerateR: ',e)
    class(e) <- 'error'
    e

  }, finally = {
    DBI::dbDisconnect(crawlDB)
    if(class(log_con)[1]=="file")close(log_con)


  })


}


