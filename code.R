library(rvest)

#global variables
classes <- c("Flyweight","Bantamweight","Featherweight","Lightweight","Welterweight","Middleweight",
               "Light_Heavyweight","Heavyweight","Women_Strawweight","Women_Bantamweight")
fighters <- NULL
data <- NULL

#collects urls of fighter links
list_urls <- function(){
  root <- c("http://www.ufc.ca/fighter/Weight_Class/")
  pages <- NULL
  for (i in 1:length(classes)){
    u <- paste(root,classes[i],sep="")
    page <- read_html(u)
    links <- html_nodes(page,".step")
    for (j in 1:length(links)){
      temp <- as.character(links[j])
      temp <- sub(".*?/(.*?)asc.*", "\\1", temp)
      temp <- gsub("amp;","",temp)
      temp <- paste(root,temp,"asc",sep="")
      u <- c(u,temp)
    }#for j
    u <- data.frame(Class = classes[i],Links = u)
    pages <- rbind(pages,u)
  }#for i
  pages
}#list_urls

#collects urls of individual fighters
fighter_urls <- function(pages){
  fu <- NULL
  for (i in 1:nrow(pages)){
    p <- read_html(as.character(pages$Links[i]))
    links <- html_nodes(p,".fighter-name")
    for (j in 1:length(links)){
      temp <- as.character(links[j])
      temp <- sub(".*?/(.*?)\".*", "\\1", temp)
      temp <- data.frame(Link=temp,Class=pages$Class[i])
      fu <- rbind(fu,temp)
    }#for j
  }#for i
  fu
}#fighter_urls

#does error checking for values
check_value <- function(fighter,class,w,h,r){
  if (length(w)==0) {w <- NA}
  if (length(h)==0) {h <- NA}
  if (length(r)==0) {r <- NA}
  data.frame(fighter=fighter,Class=class,Weight.kg=w,Height.cm=h,Reach.cm=r)
}#check_value

#collects info on fighters
get_data <- function(){
  pages <- list_urls()
  fighters <<- fighter_urls(pages)
  w <- NULL; h <- NULL; r <- NULL; dat <- NULL
  root <- "http://www.ufc.ca/fighter/"
  for (i in 1:nrow(fighters)){
    u <- paste(root,as.character(fighters$Link[i]),sep="")
    p <- read_html(u)
    w <- html_nodes(p,"#fighter-weight")
    w <- as.numeric(sub(".*?[(](.*?)k.*", "\\1", w))
    h <- html_nodes(p,"#fighter-height")
    h <- as.numeric(sub(".*?[(](.*?)c.*", "\\1", h))
    r <- html_nodes(p,"#fighter-reach")
    r <- 2.54*as.numeric(sub(".*?[>](.*?)\".*", "\\1", r))
    temp <- check_value(fighters$Link[i],fighters$Class[i],w,h,r)
    dat <- rbind(dat,temp)
  }#for i
  data <<- na.omit(dat)
  write.csv(data,file="data.csv",row.names=FALSE,quote=FALSE)
}#get_data



