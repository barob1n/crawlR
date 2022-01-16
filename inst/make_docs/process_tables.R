df<-tabulizer::extract_tables(file = 'C:\\Users\\brian\\Documents\\house_docs\\inspection_costs_to_fix.pdf',pages=8)



names(df) <- paste(1:length(df))
library(dplyr)
f <-function(x){
  x
}
df_stk<-NULL
for(i in 1:(length(df)-1)){
print(paste0('X',1:NCOL(df[[i]])))
  df[[i]]<-data.frame(df[[i]])
  colnames(df[[i]])   <-paste0('X',1:NCOL(df[[i]])) #<- df[[i]][1,]  
  df_stk<-dplyr::bind_rows(df_stk,df[[i]])
}

df_stk<-dplyr::bind_rows(df)

df_stk<-do.call(rbind,df[2:4])
write.csv(df,)
