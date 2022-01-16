

getwd()

f <- readLines("E:/R_scripts/crawlR/README_template.Rmd")
writeLines(f, con= "E:/R_scripts/crawlR/README.Rmd")


#f <- list.files("./man/", full.names = T)
funcs <- c('crawlR','injectR','generateR','fetchR','parse_content', 'parse_contact','parseExt','parseR','get_links','updateR','writeR')
f<-paste0('./man/',funcs,'.Rd')
#f<- f[grepl('injectR','generateR','fetchR','parse_contact','parseExt','parseR','get_links','updateR','newpage','writeR')]
#f <- f[grepl('Rd', f)]
idx <- which(grepl('crawlR.Rd',f))
f1<-f[idx]
f<-f[-idx]
Rd2md::Rd2markdown( paste0(f1),"E:/R_scripts/crawlR/README.Rmd", append=T )

con <- file("E:/R_scripts/crawlR/README.Rmd")
f1 <- readLines(con)
close(con )
f1<-gsub('## Examples', '## Current Crawl Configuration',f1)
con <- file("E:/R_scripts/crawlR/README.Rmd")
writeLines(c(f1,'\\newpage'), con= con)
close(con )

lapply(f, function(x){
  Rd2md::Rd2markdown( paste0(x),"E:/R_scripts/crawlR/README.Rmd", append=T )
  con <- file("E:/R_scripts/crawlR/README.Rmd", open="at")
 writeLines('\\newpage',con= con)
  close(con )
})

f <- readLines("E:/R_scripts/crawlR/README.Rmd")
f<-gsub('# `','# ',f)
f<-gsub('`\\:',':',f)
con <- file("E:/R_scripts/crawlR/README.Rmd")
writeLines(f, con= con)
close(con)
