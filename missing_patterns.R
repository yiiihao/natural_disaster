plot_patterns <- function(df,percent=TRUE) {
  
  nrow = nrow(df)
  ncol = ncol(df)
  
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  test <- missing_patterns %>% 
    mutate(index = row_number()) %>%
    gather(key, value, -c(index,count)) %>%
    group_by(index) %>%
    mutate(label = ifelse(sum(value)==0, "complete", "pass")) %>%
    ungroup() %>%
    mutate(label = ifelse(label=="complete", "complete",ifelse(value==TRUE, "true", "false") ))
  
  if(!percent){
    test1 <- data.frame(count = colSums(is.na(df))) %>% 
      rownames_to_column("columns") %>% 
      arrange(desc(count))
    level <- test1$columns
    lab1 <- "num rows missing"
    ylim1 <- max(test1$count)
    
    test2 <- test %>%
      group_by(index) %>%
      summarise(count=mean(count), complete = ifelse(sum(value)==0,"yes","no"))
    lab2 <- "row count"
    xlim2 <- max(test2$count)
  }
  
  else{
    test1 <- data.frame(count = colSums(is.na(df))/nrow*100) %>% 
      rownames_to_column("columns") %>% 
      arrange(desc(count))
    level <- test1$columns
    lab1 <- "% rows missing"
    ylim1 <- 100
    
    test2 <- test %>%
      group_by(index) %>%
      summarise(count=mean(count)/nrow*100, complete = ifelse(sum(value)==0,"yes","no"))
    lab2 <- "% rows"
    xlim2 <-100
  }
  
  p2 <- ggplot(test1,aes(x=factor(columns,levels = level),y=count)) + 
    geom_bar(stat="identity",fill="skyblue") + 
    labs(y=lab1,x="", title = "Missing value patterns") +
    ylim(0,ylim1) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  index <- test2[test2$complete=="yes",]$index
  cols <- c("false" = "gray83", "true" = "rosybrown2", "complete" = "grey")
  p1 <- ggplot(test,aes(x=factor(key,levels = level),y=fct_rev(factor(index)),fill=label)) +
    geom_tile(color="white") + 
    scale_fill_manual(values = cols) +
    labs(x="variables",y="missing patterns") +
    theme(legend.position = "none")
  
  npattern <- nrow(missing_patterns)
  if(length(index)>=1){
    for(i in index){
      p1 <- p1 + geom_text(x=ncol/2+0.5, y=npattern+1-i, label="complete cases",size=5)
    }
  }
  
  p3 <- ggplot(test2, aes(x=count, y=fct_rev(factor(index)),fill=complete)) + 
    scale_fill_manual(values=c("skyblue", "deepskyblue3")) +
    geom_bar(stat="identity") +
    labs(x=lab2,y="") +
    xlim(0,xlim2) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    theme(legend.position = "none")
  
  layout <- "
  BBBBBBBBBB##
  AAAAAAAAAACC
  AAAAAAAAAACC
  AAAAAAAAAACC
  "
  return(p1 + p2 + p3 + 
           plot_layout(design = layout))
}
